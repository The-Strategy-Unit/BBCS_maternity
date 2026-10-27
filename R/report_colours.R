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

# Standalone organisation comparisons use the same tab-top keys as chart grids.
# Keep the black source-style inset inside the chart; it encodes a different field.
report_line_chart <- function(chart) {
  built <- plotly::plotly_build(chart)
  traces <- Filter(function(trace) !identical(trace$showlegend, FALSE) &&
    length(trace$name) == 1L && length(trace$line$color) == 1L, built$x$data)
  colours <- stats::setNames(vapply(traces, function(x) x$line$color, character(1)),
    vapply(traces, function(x) x$name, character(1)))
  colours <- colours[!duplicated(names(colours))]
  built$x$layout$showlegend <- FALSE
  built$x$layout$margin$b <- 65
  htmltools::tagList(report_colour_key(colours, "organisation-comparison-key", lines = TRUE), built)
}

# Static figures also put their key before their title, not between title and axes.
report_plot_grob <- function(plot) {
  position <- if (identical(plot$theme$legend.position, "none")) "none" else "top"
  plot <- plot + ggplot2::theme(legend.position = position, legend.location = "plot",
    legend.justification = "left", legend.box.just = "left",
    legend.text = ggplot2::element_text(size = 12))
  grob <- ggplot2::ggplotGrob(plot)
  positions <- match(c("title", "subtitle", "guide-box-top"), grob$layout$name)
  if (all(!is.na(positions))) {
    rows <- grob$layout$t[positions]
    if (length(unique(rows)) == 3L) {
      heights <- grob$heights[rows]
      grob$heights[rows] <- heights[c(3, 1, 2)]
      grob$layout$t[positions] <- grob$layout$b[positions] <- rows[c(2, 3, 1)]
    }
  }
  grob
}

report_static_plot <- function(plot) {
  grid::grid.newpage()
  grid::grid.draw(report_plot_grob(plot))
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
      gp = grid::gpar(col = "black", fontsize = 12, fontfamily = "sans")))
}

plotly_source_line_key <- function() {
  # Fixed-pixel key in reserved top margin: never covers the data area.
  line <- function(y, dash) list(type = "line", xref = "paper", yref = "paper",
    xsizemode = "pixel", ysizemode = "pixel", xanchor = 1, yanchor = 1,
    x0 = -235, x1 = -175, y0 = y, y1 = y, line = list(color = "black", width = 3.5, dash = dash))
  label <- function(y, text) list(x = 1, y = 1, xref = "paper", yref = "paper",
    xshift = -165, yshift = y, xanchor = "left", yanchor = "middle",
    text = text, showarrow = FALSE, font = list(size = 14, color = "black"))
  list(shapes = list(
    list(type = "rect", xref = "paper", yref = "paper", xsizemode = "pixel", ysizemode = "pixel",
      xanchor = 1, yanchor = 1, x0 = -250, x1 = 0, y0 = 50, y1 = 115,
      fillcolor = "white", line = list(color = "#777777", width = 1)),
    line(95, "solid"), line(70, "dash")),
    annotations = list(label(95, "MSDS / modelled"), label(70, "ONS base")))
}
