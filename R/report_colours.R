# SU/NHS categorical colours
# SU: _brand.yml. NHS: https://service-manual.nhs.uk/design-system/styles/colour
# Fixed colours by organisation code
hybrid_palette <- c(
  su_blue = "#5881C1", orange = "#ED8B00", dark_green = "#006747",
  purple = "#330072", pink = "#AE2573", su_yellow = "#F9BD07",
  aqua_green = "#00A499", dark_red = "#8A1538", light_blue = "#41B6E6",
  light_green = "#78BE20", dark_blue = "#003087", dark_pink = "#7C2855",
  green = "#009639", aqua_blue = "#00A9CE", blue = "#005EB8",
  yellow = "#FAE100", charcoal = "#2C2825", grey = "#959595")

organisation_colour_key <- c(
  # Main trusts
  RNA = "#5881C1", RXK = "#ED8B00", RBK = "#006747",
  RL4 = "#330072", RRK = "#AE2573", RQ3 = "#F9BD07",
  # Other cluster providers
  RRJ = "#00A499", RXT = "#8A1538", RYW = "#41B6E6",
  RYA = "#78BE20", RYK = "#003087",
  # ICBs
  QHL = "#5881C1", QUA = "#ED8B00", QNC = "#006747",
  QWU = "#330072", QGH = "#AE2573", QOC = "#F9BD07", QJ2 = "#00A499",
  CLUSTER = "#2C2825")

status_colour_key <- c(Other = "#7C2855", Home = "#009639",
  `No booking` = "#00A9CE", `No delivery` = "#2C2825", Unknown = "#959595")

local_authority_colour_key <- local({
  areas <- read.csv("config/local_authorities.csv", stringsAsFactors = FALSE)
  stats::setNames(toupper(areas$colour), toupper(areas$area_code))
})

# Repeatable fallback shades for unlisted codes
extended_identity_colour <- function(key) {
  vapply(key, function(value) {
    hash <- 0
    for (byte in utf8ToInt(value)) hash <- (hash * 131 + byte) %% 2147483647
    anchors <- hybrid_palette[1:14]
    anchor <- grDevices::col2rgb(anchors[1 + hash %% length(anchors)]) / 255
    shade <- 0.78 + ((hash %/% length(anchors)) %% 401) / 1000
    rgb <- if (shade <= 1) anchor * shade else anchor + (1 - anchor) * (shade - 1)
    grDevices::rgb(rgb[1], rgb[2], rgb[3])
  }, character(1), USE.NAMES = FALSE)
}

identity_colours <- function(codes, priority = character()) {
  original <- as.character(codes)
  key <- toupper(trimws(sub("^(trust|icb)-", "", original, ignore.case = TRUE)))
  colours <- unname(organisation_colour_key[key])
  # Configured LA colours
  area_colours <- unname(local_authority_colour_key[key])
  colours[!is.na(area_colours)] <- area_colours[!is.na(area_colours)]
  status <- rep(NA_character_, length(key))
  status[grepl("^(OTHER($| TRUST| PROVIDER| ICB)|ALL OTHER )", key)] <- "Other"
  status[grepl("^HOME($| BIRTH| DELIVERY)", key)] <- "Home"
  status[grepl("^NO (RECORDED )?BOOKING", key)] <- "No booking"
  status[grepl("^NO (RECORDED )?DELIVERY", key)] <- "No delivery"
  status[is.na(key) | key %in% c("", "NA", "N/A", "NULL", "<NA>") |
    grepl("^(UNKNOWN|MISSING|NOT (STATED|KNOWN|RECORDED)|L999)", key)] <- "Unknown"
  colours[!is.na(status)] <- unname(status_colour_key[status[!is.na(status)]])
  missing <- is.na(colours)
  # Fixed colours first; frequent external organisations keep their shades
  priority <- toupper(trimws(sub("^(trust|icb)-", "", priority, ignore.case = TRUE)))
  external <- unique(key[missing])
  external <- c(intersect(priority, external), sort(setdiff(external, priority)))
  fallback <- stats::setNames(extended_identity_colour(external), external)
  used <- unique(c(organisation_colour_key, local_authority_colour_key, status_colour_key))
  for (code in external) {
    if (fallback[[code]] %in% used) {
      # Unused colours with the greatest separation from those already assigned
      grid <- expand.grid(h = seq(0, 360, length.out = max(121L, length(external) + 1L))[-1],
        c = c(45, 65), l = c(40, 60, 80))
      candidates <- setdiff(unique(c(hybrid_palette, grDevices::hcl(grid$h, grid$c, grid$l))), used)
      lab <- grDevices::convertColor(t(grDevices::col2rgb(c(candidates, used))) / 255,
        from = "sRGB", to = "Lab")
      proposed <- lab[seq_along(candidates), , drop = FALSE]
      assigned <- lab[length(candidates) + seq_along(used), , drop = FALSE]
      distances <- outer(proposed[, 1], assigned[, 1], "-")^2 +
        outer(proposed[, 2], assigned[, 2], "-")^2 +
        outer(proposed[, 3], assigned[, 3], "-")^2
      fallback[[code]] <- candidates[which.max(apply(distances, 1, min))]
    }
    used <- c(used, fallback[[code]])
  }
  colours[missing] <- unname(fallback[key[missing]])
  stats::setNames(colours, original)
}

organisation_line_colours <- function(keys) identity_colours(keys)

# Match the HTML chart scale (14.667 / 18.667 CSS pixels = 11 / 14 points).
# Explicit child sizes prevent ggplot's relative defaults shrinking ticks/captions.
report_chart_theme <- function() {
  if (.Platform$OS.type == "windows")
    grDevices::windowsFonts(Arial = grDevices::windowsFont("Arial"))
  text <- ggplot2::element_text(family = "Arial", size = 11)
  ggplot2::theme(text = text, axis.text = text, axis.title = text,
    legend.text = text, legend.title = text, strip.text = text,
    plot.subtitle = text, plot.caption = text,
    plot.title = ggplot2::element_text(family = "Arial", size = 14))
}

# Wrap using the output device's actual text metrics, including long footnotes.
report_wrap_text <- function(text, width, size = 11) {
  if (is.null(text) || !is.character(text)) return(text)
  measure <- function(x) grid::convertWidth(grid::grobWidth(grid::textGrob(x,
    gp = grid::gpar(fontfamily = "Arial", fontsize = size))), "in", valueOnly = TRUE)
  vapply(text, function(label) paste(vapply(strsplit(label, "\n", fixed = TRUE)[[1]], function(part) {
    lines <- ""
    for (word in strsplit(part, "\\s+")[[1]]) {
      i <- length(lines)
      next_line <- trimws(paste(lines[i], word))
      if (nzchar(lines[i]) && measure(next_line) > width) lines <- c(lines, word)
      else lines[i] <- next_line
    }
    paste(lines, collapse = "\n")
  }, character(1)), collapse = "\n"), character(1), USE.NAMES = FALSE)
}

# One accessible colour key per chart grid; individual charts retain hover data.
# Tab-top keys belong above the chart/card headings, never inside plot margins.
report_colour_key <- function(colours, class = NULL, lines = FALSE) {
  htmltools::div(class = paste(c("demo-ribbon-key", class), collapse = " "),
    lapply(seq_along(colours), function(i) htmltools::span(class = "demo-ribbon-key-item",
      htmltools::span(class = "demo-ribbon-swatch", `aria-hidden` = "true",
        style = paste0("background:", colours[i], ";",
          if (lines) "width:32px;height:5px;border:0;" else "")), names(colours)[i])))
}

booking_delivery_palette <- function() c(
  "Same trust" = "#006747", "Different cluster trust" = "#AE2573",
  "Outside cluster" = "#ED8B00", "No recorded delivery" = "#2C2825",
  "Unknown location" = "#959595")

# Native titles and legends stay inside the white chart canvas, including exports.
report_native_line_chart <- function(chart, title = NULL, legend_name = NULL, compact = FALSE) {
  built <- plotly::plotly_build(chart)
  if (!is.null(legend_name) && length(built$x$data) == 1L) {
    built$x$data[[1]]$name <- legend_name
    built$x$data[[1]]$showlegend <- TRUE
  }
  layout <- built$x$layout
  layout$showlegend <- TRUE
  layout$paper_bgcolor <- layout$plot_bgcolor <- "white"
  layout$legend <- utils::modifyList(if (is.null(layout$legend)) list() else layout$legend, list(
    orientation = "h", x = 0, xanchor = "left", y = -0.25, yanchor = "top",
    font = list(size = if (compact) 11 else 13), itemwidth = if (compact) 30 else 50))
  if (!is.null(title)) layout$title <- list(text = title, x = 0.03, xanchor = "left",
    y = 0.98, yanchor = "top", font = list(size = if (compact) 14 else 18))
  if (!is.null(title)) layout$margin$t <- max(c(layout$margin$t, if (compact) 50 else 70))
  if (compact) {
    axis_title <- if (is.list(layout$yaxis$title)) layout$yaxis$title$text else layout$yaxis$title
    if (identical(axis_title, "Projected antenatal bookings")) axis_title <- "Projected<br>bookings"
    if (!is.null(axis_title)) layout$yaxis$title <- list(text = sub(
      "^% change from modelled ", "% change from<br>modelled ", axis_title))
  }
  layout$meta <- utils::modifyList(if (is.null(layout$meta)) list() else layout$meta, list(
    native_line_legend = TRUE, compact_line_legend = compact,
    source_key_below = any(vapply(layout$annotations, function(a)
      identical(a$text, "MSDS / modelled") && identical(a$y, 0) && a$yshift < 0, logical(1)))))
  built$x$layout <- layout
  built
}

# Other standalone comparisons retain their existing top HTML key by default.
report_line_chart <- function(chart, legend_position = c("top", "bottom"), title = NULL) {
  legend_position <- match.arg(legend_position)
  if (legend_position == "bottom") return(report_native_line_chart(chart, title))
  built <- plotly::plotly_build(chart)
  traces <- Filter(function(trace) !identical(trace$showlegend, FALSE) &&
    length(trace$name) == 1L && length(trace$line$color) == 1L, built$x$data)
  colours <- stats::setNames(vapply(traces, function(x) x$line$color, character(1)),
    vapply(traces, function(x) x$name, character(1)))
  colours <- colours[!duplicated(names(colours))]
  built$x$layout$showlegend <- FALSE
  built$x$layout$margin$b <- 65
  key <- report_colour_key(colours, "organisation-comparison-key", lines = TRUE)
  htmltools::tagList(key, built)
}

# Presentation-only alternative for the provider total charts and their grids.
# Keep the original recorded/modelled traces (including gaps and hover notes).
report_msds_only <- function(x, height_scale = 1) {
  if (inherits(x, "plotly")) {
    built <- plotly::plotly_build(x)
    built$x$data <- Filter(function(trace) !identical(trace$meta$Series, "ONS base"), built$x$data)
    layout <- built$x$layout
    layout$annotations <- Filter(function(note)
      !isTRUE(note$text %in% c("MSDS / modelled", "ONS base")), layout$annotations)
    # The source key is drawn in pixels in paper coordinates. The historical
    # shading and divider use the year axis and must remain in both versions.
    layout$shapes <- Filter(function(shape) !(identical(shape$xref, "paper") &&
      identical(shape$yref, "paper") && identical(shape$xsizemode, "pixel") &&
      identical(shape$ysizemode, "pixel")), layout$shapes)
    if (is.list(layout$yaxis$title))
      layout$yaxis$title$text <- sub(" / ONS live-birth base$", "", layout$yaxis$title$text)
    else layout$yaxis$title <- sub(" / ONS live-birth base$", "", layout$yaxis$title)
    layout$meta$source_key_below <- FALSE
    layout$margin$b <- 80
    if (height_scale != 1) {
      height <- if (is.null(layout$height)) built$height else layout$height
      built$height <- layout$height <- round(height * height_scale)
    }
    built$x$layout <- layout
    # Duplicated panels need their own DOM IDs when both versions are rendered.
    built$elementId <- NULL
    return(built)
  }
  if (inherits(x, "shiny.tag")) x$children <- lapply(x$children, report_msds_only, height_scale = height_scale)
  else if (is.list(x)) for (i in seq_along(x)) x[i] <- list(report_msds_only(x[[i]], height_scale))
  x
}

# Static figures default to a key before the title, with an optional key below the axes.
report_plot_grob <- function(plot, legend_position = c("top", "bottom")) {
  legend_position <- match.arg(legend_position)
  position <- if (identical(plot$theme$legend.position, "none")) "none" else legend_position
  plot <- plot + report_chart_theme() + ggplot2::theme(legend.position = position, legend.location = "plot",
    legend.justification = "left", legend.box.just = "left",
    plot.title.position = "plot", plot.caption.position = "plot",
    plot.caption = ggplot2::element_text(hjust = 0, colour = "#5f5b58"))
  available <- max(2, grDevices::dev.size("in")[1] - 0.5)
  plot <- plot + ggplot2::labs(
    title = report_wrap_text(plot$labels$title, available, 14),
    subtitle = report_wrap_text(plot$labels$subtitle, available),
    caption = report_wrap_text(plot$labels$caption, available))
  plot$layers <- lapply(plot$layers, function(layer) {
    if (inherits(layer$geom, "GeomText") || inherits(layer$geom, "GeomLabel")) {
      params <- layer$aes_params
      params$family <- "Arial"
      params$size <- 11 / ggplot2::.pt
      return(ggplot2::ggproto(NULL, layer, aes_params = params))
    }
    layer
  })
  grob <- ggplot2::ggplotGrob(plot)
  positions <- match(c("title", "subtitle", "guide-box-top"), grob$layout$name)
  if (position == "top" && all(!is.na(positions))) {
    rows <- grob$layout$t[positions]
    if (length(unique(rows)) == 3L) {
      heights <- grob$heights[rows]
      grob$heights[rows] <- heights[c(3, 1, 2)]
      grob$layout$t[positions] <- grob$layout$b[positions] <- rows[c(2, 3, 1)]
    }
  }
  grob
}

report_static_plot <- function(plot, legend_position = "top") {
  grid::grid.newpage()
  grid::grid.draw(report_plot_grob(plot, legend_position))
  invisible(NULL)
}

# Ordered demographic colours: retain the same meaning across every view.
imd_quintile_labels <- function(values, prefix = "Quintile", unknown = "Unknown") {
  values <- as.character(values)
  labels <- paste(prefix, values)
  labels[is.na(values) | !nzchar(trimws(values))] <- unknown
  labels[values %in% "1"] <- paste(prefix, "1 (most deprived)")
  labels[values %in% "5"] <- paste(prefix, "5 (least deprived)")
  labels
}

imd_quintile_colours <- function(values) {
  key <- stats::setNames(c("#08306B", "#2171B5", "#4292C6", "#6BAED6", "#C6DBEF"), 1:5)
  colours <- unname(key[as.character(values)])
  colours[is.na(colours)] <- "#959595"
  colours
}

age_band_colours <- function(values) {
  key <- stats::setNames(c("#08306B", "#08519C", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", "#C6DBEF"),
    c("Under 20", "20-24", "25-29", "30-34", "35-39", "40-44", "45+"))
  colours <- unname(key[as.character(values)])
  colours[is.na(colours)] <- "#959595"
  colours
}

# Detailed older-age reviews use a finer set of ordered ages than the main bands.
age_series_colours <- function(values) {
  labels <- unique(as.character(values))
  if (all(labels %in% c("Under 20", "20-24", "25-29", "30-34", "35-39", "40-44", "45+")))
    return(stats::setNames(age_band_colours(labels), labels))
  lower <- suppressWarnings(as.numeric(sub("^([0-9]+).*$", "\\1", labels)))
  lower[grepl("^Under", labels)] <- 0
  ordered <- labels[order(lower, labels, na.last = TRUE)]
  stats::setNames(grDevices::colorRampPalette(c("#08306B", "#C6DBEF"), space = "Lab")(length(ordered)), ordered)
}

residence_ribbon_colours <- function(values) {
  ifelse(values %in% "QUA", "#F9BD07", ifelse(values %in% "QHL", "#5881C1", "#959595"))
}

source_line_key_grob <- function() {
  grid::grobTree(
    grid::rectGrob(gp = grid::gpar(fill = "white", col = "#777777", lwd = .8)),
    grid::segmentsGrob(x0 = .06, x1 = .30, y0 = .70, y1 = .70,
      gp = grid::gpar(col = "black", lwd = 3, lty = "solid")),
    grid::segmentsGrob(x0 = .06, x1 = .30, y0 = .30, y1 = .30,
      gp = grid::gpar(col = "black", lwd = 3, lty = "44")),
    grid::textGrob(c("MSDS / modelled", "ONS"), x = .35, y = c(.70, .30), just = "left",
      gp = grid::gpar(col = "black", fontsize = 11, fontfamily = "Arial")))
}

plotly_source_line_key <- function(position = c("top", "bottom")) {
  position <- match.arg(position)
  # Fixed-pixel key in the reserved margin. Bottom labels use y=0 and pixel
  # shifts so the responsive chart code does not collapse them onto one line.
  anchor <- if (position == "bottom") 0 else 1
  offset <- if (position == "bottom") -180 else 0
  line <- function(y, dash) list(type = "line", xref = "paper", yref = "paper",
    xsizemode = "pixel", ysizemode = "pixel", xanchor = 1, yanchor = anchor,
    x0 = -235, x1 = -175, y0 = y + offset, y1 = y + offset,
    line = list(color = "black", width = 3.5, dash = dash))
  label <- function(y, text) list(x = 1, y = anchor, xref = "paper", yref = "paper",
    xshift = -165, yshift = y + offset, xanchor = "left", yanchor = "middle",
    text = text, showarrow = FALSE, font = list(size = 14, color = "black"))
  list(shapes = list(
    list(type = "rect", xref = "paper", yref = "paper", xsizemode = "pixel", ysizemode = "pixel",
      xanchor = 1, yanchor = anchor, x0 = -250, x1 = 0, y0 = 50 + offset, y1 = 115 + offset,
      fillcolor = "white", line = list(color = "#777777", width = 1)),
    line(95, "solid"), line(70, "dash")),
    annotations = list(label(95, "MSDS / modelled"), label(70, "ONS base")))
}
