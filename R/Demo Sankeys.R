# DEMOGRAPHIC SANKEYS ===================================================
# Run after Maternity analysis.R; Run analysis.R does this automatically.
# The master is an aggregate table: no mother or pregnancy IDs are retained.
source("R/demographics.R")
demo_period <- "2023/24\u20132025/26"

demo_provider_labels <- function() {
  full_flow_trust_labels()
}

# Same/different original trust codes; exclude missing events/providers and home births
cohort_trust_comparison <- function(episodes) {
  booking <- episodes$Booking_provider_supplied | episodes$Booking_date_supplied
  valid <- grepl("^R[A-Z0-9]{2}$", episodes$Booking_Trust) &
    grepl("^R[A-Z0-9]{2}$", episodes$Trust)
  dplyr::case_when(
    !booking & !episodes$Birth_recorded ~ "Neither booking nor delivery recorded",
    !booking ~ "No recorded booking",
    !episodes$Birth_recorded ~ "No recorded delivery",
    episodes$Home %in% 1 ~ "Home birth",
    !valid ~ "Missing or invalid trust code",
    episodes$Booking_Trust == episodes$Trust ~ "Same trust",
    TRUE ~ "Different trusts")
}

# 1. PREPARE THE MASTER TABLE -------------------------------------------
build_demo_sankeys <- function(cohort, demographics, episodes = prepare_report_cohort(cohort)) {
  lookup <- demographics |>
    dplyr::transmute(person_id_mother, ethnicity_broad, ethnicity_description,
      imd_quintile = as.integer(ceiling(imd_decile / 2))) |>
    dplyr::distinct()

  # The supplied three-year cohort is the population. Count each pregnancy row
  # once, without another event-year filter or duplication across chart views.
  # Join on the original mother ID. Missing IDs never match each other.
  # many-to-one prevents conflicting duplicate mother records multiplying flows.
  records <- episodes |>
    classify_patient_flow_stages() |>
    dplyr::mutate(
      Trust_comparison = cohort_trust_comparison(episodes),
      person_id_mother = demographic_mother_id(cohort$person_id_mother[Row]),
      Pathway = dplyr::case_when(
        Birth_recorded & Has_booking ~ "Delivery with recorded booking",
        Birth_recorded ~ "Delivery without recorded booking",
        Has_booking ~ "Booking without recorded delivery",
        TRUE ~ "Neither event recorded"),
      Outcome = classify_pathway_outcome(data.frame(birth_recorded = Birth_recorded,
        home_birth = Home, birth_code = Trust))) |>
    dplyr::left_join(lookup, by = "person_id_mother", na_matches = "never",
      relationship = "many-to-one")
  records |>
    dplyr::count(Booking, Delivery, Booking_ICB, Delivery_ICB, Pathway, Outcome, Trust_comparison,
      ethnicity_broad, ethnicity_description, imd_quintile, Age_band,
      name = "N", .drop = FALSE)
}

# 2. FORM CHARTS FROM THE MASTER ONLY -----------------------------------
demo_sankey_chart <- function(master, compact = FALSE) {
  if (!nrow(master)) return(htmltools::div("No supplied cohort pregnancies."))
  links <- master |>
    dplyr::group_by(source = Booking, target = Delivery) |>
    dplyr::summarise(n = sum(N), .groups = "drop")
  source_order <- sankey_category_order(trust_source_order, names(demo_provider_labels()))
  target_order <- sankey_category_order(trust_target_order, names(demo_provider_labels()))
  chart <- make_sankey(list(links = links), demo_provider_labels(),
    " (booking)", " (delivery)", source_order, target_order)
  if (!compact || !nrow(links)) return(chart)

  # Short node labels fit three panels across. Hover retains the full names.
  short <- c(RNA = "Dudley", RL4 = "Wolverhampton", RQ3 = "Birmingham W&C",
    RXK = "Sandwell & W Birmingham", RBK = "Walsall", RRK = "UHB",
    Other = "Other trusts", Unknown = "Unknown provider", Home = "Home birth",
    `No booking` = "No recorded booking", `No delivery` = "No recorded delivery")
  nodes <- c(source_order[source_order %in% links$source],
    target_order[target_order %in% links$target])
  extra <- setdiff(nodes, names(short))
  short <- c(short, stats::setNames(extra, extra))
  chart <- plotly::plotly_build(chart)
  chart$x$data[[1]]$node$label <- unname(short[nodes])
  chart$x$data[[1]]$node$pad <- 12
  trace <- chart$x$data[[1]]
  chart$x$data[[1]]$node$y <- sankey_node_y(trace$node$x, trace$link$source,
    trace$link$target, trace$link$value, 430 - 36, 12)
  chart$x$layout$height <- 430
  chart$x$layout$font$size <- 10
  chart$height <- 430
  chart
}

demo_category_values <- function(master, field) {
  values <- unique(as.character(master[[field]]))
  preferred <- switch(field,
    imd_quintile = as.character(1:5),
    ethnicity_broad = unname(demographic_ethnicity_groups),
    ethnicity_profile = c("White", "Asian", "Black", "All other"),
    Age_band = report_age_bands,
    character())
  sankey_category_order(preferred, values)
}

demo_category_label <- function(value, field) {
  label <- if (is.na(value)) "Unknown (NA)" else if (value == "") "Blank" else value
  if (field == "imd_quintile" && !is.na(value) && nzchar(value))
    label <- paste("IMD quintile", label)
  label
}

demo_sankey_grid <- function(master, field) {
  values <- demo_category_values(master, field)
  if (!length(values)) return(htmltools::div("No supplied cohort pregnancies."))
  panels <- lapply(values, function(value) {
    selected <- if (is.na(value)) is.na(master[[field]]) else
      !is.na(master[[field]]) & as.character(master[[field]]) == value
    rows <- master[selected, , drop = FALSE]
    label <- demo_category_label(value, field)
    htmltools::div(class = "demo-sankey-panel",
      htmltools::tags$h4(label),
      htmltools::tags$p(paste0(scales::comma(dc_count(sum(rows$N))), " pregnancies")),
      htmltools::tags$p(class = "demo-sankey-stages", "Booking \u2192 Delivery"),
      demo_sankey_chart(rows, compact = TRUE))
  })
  htmltools::div(class = "demo-sankey-grid", panels)
}

# 2b. FLOWS BY DEMOGRAPHIC AND ORGANISATION -----------------------------
demo_category_colours <- function(values, field) {
  if (field == "imd_quintile") return(imd_quintile_colours(values))
  if (field == "Age_band") return(age_band_colours(values))
  levels <- switch(field, Age_band = report_age_bands, imd_quintile = as.character(1:5),
    ethnicity_broad = unname(demographic_ethnicity_groups),
    ethnicity_profile = c("White", "Asian", "Black", "All other"),
    stop("Unsupported ribbon demographic: ", field))
  palette <- c("#5881C1", "#ED8B00", "#006747", "#330072", "#AE2573", "#F9BD07",
    "#00A499", "#8A1538")
  key <- stats::setNames(palette[seq_along(levels)], levels)
  if (field == "ethnicity_broad") key["Not stated"] <- "#2C2825"
  colours <- unname(key[as.character(values)])
  missing <- is.na(values) | values %in% c("Unknown", "", "NULL")
  colours[missing] <- "#959595"
  extra <- is.na(colours)
  colours[extra] <- extended_identity_colour(paste(field, values[extra]))
  colours
}

demo_ribbon_key <- function(master, field) {
  categories <- demo_category_values(master, field)
  colours <- demo_category_colours(categories, field)
  htmltools::div(class = "demo-ribbon-key",
    htmltools::tagList(lapply(seq_along(categories), function(i) {
      htmltools::span(class = "demo-ribbon-key-item",
        htmltools::span(class = "demo-ribbon-swatch", style = paste0("background: ", colours[i], ";"),
          `aria-hidden` = "true"), demo_category_label(categories[i], field))
    })))
}

# Pregnancies booked or delivered at the organisation; counted once per panel
demo_ribbon_org_rows <- function(master, group, code) {
  booking <- if (group == "Trust") master$Booking else master$Booking_ICB
  delivery <- if (group == "Trust") master$Delivery else master$Delivery_ICB
  codes <- if (group != "Trust" && code == "Cluster") cluster_icbs else code
  master[booking %in% codes | delivery %in% codes, , drop = FALSE]
}

demo_ribbon_org_codes <- function(master, group) {
  preferred <- if (group == "Trust") setdiff(trust_source_order, c("Other", "Unknown")) else cluster_icbs
  available <- if (group == "Trust") c(master$Booking, master$Delivery) else c(master$Booking_ICB, master$Delivery_ICB)
  # Organisation panels; exclude status-only endpoints
  available <- setdiff(unique(available), c("Home", "No booking", "No delivery", "Unknown"))
  codes <- sankey_category_order(preferred, union(preferred, available))
  if (group == "Trust") codes else c(codes, "Cluster")
}

demo_ribbon_sankey <- function(master, field, group = "Trust", compact = FALSE) {
  master <- master[!is.na(master$N) & master$N > 0, , drop = FALSE]
  if (!nrow(master)) return(htmltools::div("No supplied cohort pregnancies for this organisation."))
  booking_field <- if (group == "Trust") "Booking" else "Booking_ICB"
  delivery_field <- if (group == "Trust") "Delivery" else "Delivery_ICB"
  labels <- if (group == "Trust") demo_provider_labels() else
    c(flow_icb_labels, Home = "Home birth", `No booking` = "No recorded booking", `No delivery` = "No recorded delivery")
  preferred <- if (group == "Trust") trust_source_order else icb_source_order
  order <- sankey_category_order(preferred, names(labels))
  categories <- demo_category_values(master, field)
  # Counts by booking, delivery and demographic; disclosure control in make_sankey
  links <- master |>
    dplyr::group_by(source = .data[[booking_field]], target = .data[[delivery_field]],
      Category = as.character(.data[[field]])) |>
    dplyr::summarise(n = sum(N), .groups = "drop") |>
    dplyr::arrange(match(source, order), match(target, order), match(Category, categories))
  chart <- make_sankey(list(links = links), labels, " (booking)", " (delivery)", order, order)
  chart <- plotly::plotly_build(chart)
  trace <- chart$x$data[[1]]
  category_labels <- vapply(links$Category, demo_category_label, character(1), field = field)
  trace$meta <- list(demographic_colour_field = field)
  trace$node$color <- rep("#959595", length(trace$node$label))
  trace$link$label <- unname(category_labels)
  trace$link$color <- grDevices::adjustcolor(demo_category_colours(links$Category, field), alpha.f = 0.65)
  trace$link$customdata <- paste0(category_labels, "<br>",
    gsub("Records:", "Pregnancies:", trace$link$customdata, fixed = TRUE))
  trace$node$customdata <- paste0(trace$node$label, "<br>Pregnancies: ", trace$node$customdata,
    " of panel pregnancies")
  trace$node$hovertemplate <- "%{customdata}<extra></extra>"
  if (compact) {
    sources <- order[order %in% links$source]
    targets <- order[order %in% links$target]
    short <- c(RNA = "Dudley", RL4 = "Wolverhampton", RQ3 = "Birmingham W&C",
      RXK = "Sandwell & W Birmingham", RBK = "Walsall", RRK = "UHB")
    node_names <- unname(labels[c(sources, targets)])
    known <- match(c(sources, targets), names(short))
    node_names[!is.na(known)] <- unname(short[known[!is.na(known)]])
    node_names <- sub(" ICB$", "", node_names)
    trace$node$label <- vapply(node_names, function(label) paste(strwrap(label, 22), collapse = "<br>"), character(1))
    height <- max(430, 28 * max(length(sources), length(targets)) + 36)
    trace$node$pad <- 12
    trace$node$y <- sankey_node_y(trace$node$x, trace$link$source, trace$link$target,
      trace$link$value, height - 36, 12)
    chart$x$layout$height <- chart$height <- height
    chart$x$layout$font$size <- 10
  }
  chart$x$data[[1]] <- trace
  dc_widget(chart)
}

demo_ribbon_grid <- function(master, field, group) {
  codes <- demo_ribbon_org_codes(master, group)
  panels <- lapply(codes, function(code) {
    rows <- demo_ribbon_org_rows(master, group, code)
    label <- if (code == "Cluster") "Combined cluster" else demo_org_label(code, group)
    htmltools::div(class = "demo-sankey-panel",
      htmltools::tags$h4(label),
      htmltools::tags$p(paste0(scales::comma(dc_count(sum(rows$N))), " pregnancies booked or delivered here")),
      htmltools::tags$p(class = "demo-sankey-stages", "Booking \u2192 Delivery"),
      demo_ribbon_sankey(rows, field, group, compact = TRUE))
  })
  htmltools::tagList(demo_ribbon_key(master, field), htmltools::div(class = "demo-sankey-grid", panels))
}

demo_ribbon_views <- function(master, field) {
  list(overall = htmltools::tagList(demo_ribbon_key(master, field), demo_ribbon_sankey(master, field)),
    Trust = demo_ribbon_grid(master, field, "Trust"),
    ICB_Cluster = demo_ribbon_grid(master, field, "ICB_Cluster"))
}

# 3. ORGANISATION GROUPS AND RECORDED DELIVERY OUTCOMES -----------------
demo_org_codes <- function(master, mode, group) {
  if (group == "Trust") unique(c(setdiff(names(trust_labels), "Other"), master[[mode]])) else
    unique(c(cluster_icbs, master[[paste0(mode, "_ICB")]], "Cluster"))
}

demo_org_rows <- function(master, mode, group, code) {
  if (group != "Trust" && code == "Cluster") return(master)
  field <- if (group == "Trust") mode else paste0(mode, "_ICB")
  master[master[[field]] %in% code, , drop = FALSE]
}

demo_org_label <- function(code, group) {
  labels <- if (group == "Trust") demo_provider_labels() else
    c(icb_labels, Unknown = "Unknown ICB", `No booking` = "No recorded booking",
      `No delivery` = "No recorded delivery", Home = "Home birth", Cluster = "Whole BBCS cohort")
  label <- unname(labels[code])
  if (is.na(label)) code else label
}

demo_pathway_summary <- function(master, group, codes = NULL) {
  if (is.null(codes)) codes <- demo_org_codes(master, "Booking", group)
  dplyr::bind_rows(lapply(codes, function(code) {
    rows <- demo_org_rows(master, "Booking", group, code)
    organisation <- demo_org_label(code, group)
    data.frame(Code = code, Organisation = organisation, Starts = sum(rows$N),
      Within = sum(rows$N[rows$Outcome == "Within cluster"]),
      Outside = sum(rows$N[rows$Outcome == "Outside cluster"]),
      No_delivery = sum(rows$N[rows$Outcome == "No recorded delivery"]),
      Unknown = sum(rows$N[rows$Outcome == "Unknown location"]))
  }))
}

demo_outcome_counts <- function(rows, field) {
  categories <- demo_category_values(rows, field)
  outcomes <- c("Within cluster", "Outside cluster", "No recorded delivery", "Unknown location")
  counts <- rows |>
    dplyr::group_by(Category = as.character(.data[[field]]), Outcome) |>
    dplyr::summarise(N = sum(N), .groups = "drop")
  totals <- counts |>
    dplyr::group_by(Category) |>
    dplyr::summarise(Category_total = sum(N), .groups = "drop")
  counts |>
    dplyr::left_join(totals, by = "Category", na_matches = "na") |>
    dplyr::mutate(Organisation_total = sum(rows$N)) |>
    dc_cells(c("N", "Category_total", "Organisation_total"),
      list(Category_share = c("N", "Category_total"),
        Organisation_share = c("N", "Organisation_total"))) |>
    dplyr::mutate(Source = match(Category, categories) - 1L,
      Target = length(categories) + match(Outcome, outcomes) - 1L) |>
    dplyr::arrange(Source, Target)
}

demo_outcome_sankey <- function(rows, field, proportions = FALSE) {
  rows <- rows[!is.na(rows$N) & rows$N > 0, , drop = FALSE]
  if (!nrow(rows) || sum(rows$N) == 0) return(htmltools::div("No pregnancies in this group."))
  categories <- demo_category_values(rows, field)
  category_labels <- vapply(categories, demo_category_label, character(1), field = field)
  outcomes <- c("Within cluster", "Outside cluster", "No recorded delivery", "Unknown location")
  links <- demo_outcome_counts(rows, field)
  category_totals <- links$Category_total[match(seq_along(categories) - 1L, links$Source)]
  outcome_totals <- dc_count(vapply(outcomes, function(outcome) sum(rows$N[rows$Outcome == outcome]), numeric(1)))
  total <- dc_count(sum(rows$N))
  labels <- c(category_labels, outcome_labels)
  totals <- c(category_totals, outcome_totals)
  percentages <- scales::percent(dc_divide(totals, total), accuracy = 0.1)
  node_labels <- paste0(vapply(labels, function(label) paste(strwrap(label, 25), collapse = "<br>"), character(1)),
    "<br>", percentages)
  links <- links[links$N > 0 & !is.na(links$N), , drop = FALSE]
  values <- links$N
  metadata <- NULL
  if (proportions) {
    values <- 100 * links$N / ave(links$N, links$Source, FUN = sum)
    node_labels <- c(paste0(vapply(category_labels, function(label)
      paste(strwrap(label, 25), collapse = "<br>"), character(1)), "<br>100%"),
      vapply(outcome_labels, function(label) paste(strwrap(label, 25), collapse = "<br>"), character(1)))
    metadata <- list(msds_sankey_scale = "normalised_categories",
      display_counts = links$N, display_denominators = links$Category_total)
  }
  chart_height <- max(400, 55 * length(categories))
  node_x <- c(rep(0.01, length(categories)), rep(0.99, 4))
  plotly::plot_ly(type = "sankey", arrangement = "fixed", orientation = "h",
    meta = metadata,
    height = chart_height,
    node = list(label = node_labels, pad = 22, thickness = 18,
      color = c(rep("#5881C1", length(categories)), outcome_colours),
      x = node_x,
      y = sankey_node_y(node_x, links$Source, links$Target, values, chart_height - 32, 22),
      customdata = paste0(labels, "<br>Pregnancies: ", scales::comma(totals),
        "<br>", percentages, " of organisation pregnancies"),
      hovertemplate = "%{customdata}<extra></extra>"),
    link = list(source = links$Source, target = links$Target, value = values,
      color = grDevices::adjustcolor(outcome_colours[match(links$Outcome, outcomes)], alpha.f = 0.4),
      customdata = paste0(category_labels[links$Source + 1L], " \u2192 ",
        outcome_labels[match(links$Outcome, outcomes)],
        "<br>Pregnancies: ", scales::comma(links$N),
        "<br>", scales::percent(links$Category_share, accuracy = 0.1), " of this demographic category",
        "<br>", scales::percent(links$Organisation_share, accuracy = 0.1), " of organisation pregnancies"),
      hovertemplate = "%{customdata}<extra></extra>")) |>
    plotly::layout(font = list(family = "Arial", size = 12),
      margin = list(l = 12, r = 12, t = 12, b = 20), paper_bgcolor = "white", plot_bgcolor = "white") |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE) |>
    dc_widget()
}

demo_pathway_grid <- function(master, group, field = NULL, proportions = FALSE) {
  summary <- demo_pathway_summary(master, group)
  panels <- lapply(seq_len(nrow(summary)), function(i) {
    rows <- demo_org_rows(master, "Booking", group, summary$Code[i])
    htmltools::div(class = "demo-sankey-panel",
      htmltools::tags$h5(summary$Organisation[i]),
      htmltools::tags$p(paste0(scales::comma(dc_count(summary$Starts[i])), " pregnancies")),
      if (is.null(field)) make_pathway_sankey(summary[i, , drop = FALSE],
        count_label = "Pregnancies", period = demo_period) else demo_outcome_sankey(rows, field, proportions))
  })
  htmltools::tagList(pathway_legend,
    htmltools::div(class = if (is.null(field)) "demo-sankey-grid" else "demo-outcome-grid", panels))
}

demo_pathway_tabs <- function(master, group) {
  versions <- function(field) list(counts = demo_pathway_grid(master, group, field),
    proportions = demo_pathway_grid(master, group, field, proportions = TRUE))
  list(overall = demo_pathway_grid(master, group),
    ethnicity = versions("ethnicity_broad"),
    ethnicity_description = versions("ethnicity_description"),
    imd = versions("imd_quintile"),
    age = versions("Age_band"))
}

# 4. POOLED DEMOGRAPHIC BAR CHARTS -------------------------------------
demo_pathway_colours <- c(
  "Delivery with recorded booking" = "#006747",
  "Delivery without recorded booking" = "#5881C1",
  "Booking without recorded delivery" = "#ED8B00",
  "Neither event recorded" = "#959595")

demo_overview_bar <- function(rows, field) {
  if (!nrow(rows) || sum(rows$N) <= 0) return(htmltools::div("No pregnancies in this group."))
  # All demographic categories, including unknowns, share the panel's denominator.
  # Control the raw organisation total independently of the displayed cells.
  organisation_total <- dc_count(sum(rows$N))
  categories <- demo_category_values(rows, field)
  labels <- vapply(categories, demo_category_label, character(1), field = field)
  short_labels <- labels
  if (field == "imd_quintile") short_labels <- sub("^IMD ", "", short_labels)
  short_labels <- sub("Unknown (NA)", "Unknown", short_labels, fixed = TRUE)
  short_labels <- sub("Asian or Asian British", "Asian / Asian British", short_labels, fixed = TRUE)
  short_labels <- sub("Black or Black British", "Black / Black British", short_labels, fixed = TRUE)
  short_labels <- sub("Other ethnic groups - ", "Other: ", short_labels, fixed = TRUE)
  short_labels <- sub("Any other", "any other", short_labels, fixed = TRUE)
  tick_labels <- vapply(short_labels, function(label) paste(strwrap(label, width = 22), collapse = "<br>"), character(1))
  label_lines <- lengths(strsplit(tick_labels, "<br>", fixed = TRUE))
  left_margin <- switch(field, Age_band = 64, imd_quintile = 82, 148)
  counts <- rows |>
    dplyr::group_by(Category = as.character(.data[[field]]), Pathway) |>
    dplyr::summarise(N = sum(N), .groups = "drop") |>
    dplyr::mutate(Category = factor(labels[match(Category, categories)], levels = labels),
      Pathway = factor(Pathway, levels = names(demo_pathway_colours)),
      N = dc_count(N),
      Share = dc_divide(N, organisation_total),
      Hover = paste0(Category, "<br>", Pathway, "<br>",
        scales::percent(Share, accuracy = 0.1), " of panel total",
        "<br>Pregnancies: ", scales::comma(N)))
  plotly::plot_ly(counts, x = ~Share, y = ~Category, color = ~Pathway,
    colors = demo_pathway_colours, type = "bar", orientation = "h",
    text = ~scales::percent(Share, accuracy = 0.1), textposition = "inside", insidetextanchor = "middle",
    textfont = list(size = 13, color = "white"), constraintext = "inside",
    customdata = ~Hover, hovertemplate = "%{customdata}<extra></extra>",
    height = max(320, sum(pmax(34, label_lines * 17 + 10)) + 70)) |>
    plotly::layout(barmode = "stack", showlegend = FALSE,
      meta = list(demographic_overview_field = field), uniformtext = list(minsize = 12, mode = "hide"),
      xaxis = list(title = "% of panel total", range = c(0, 1),
        tickformat = ".0%", dtick = 0.25),
      yaxis = list(title = "", categoryorder = "array", categoryarray = rev(labels),
        tickmode = "array", tickvals = labels,
        ticktext = tick_labels, automargin = FALSE),
      margin = list(l = left_margin, r = 12, t = 12, b = 50),
      font = list(family = "Arial", size = 13)) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

demo_overview_grid <- function(master, mode, group, field) {
  codes <- demo_org_codes(master, mode, group)
  legend <- htmltools::div(class = "demo-ribbon-key", style = "margin-bottom:16px;",
    lapply(names(demo_pathway_colours), function(label) htmltools::span(
      htmltools::span(style = paste0("display:inline-block; width:12px; height:12px; margin-right:6px; background:",
        demo_pathway_colours[[label]]), `aria-hidden` = "true"), label)))
  htmltools::tagList(legend, htmltools::div(class = "demo-sankey-grid",
    lapply(codes, function(code) {
      rows <- demo_org_rows(master, mode, group, code)
      htmltools::div(class = "demo-sankey-panel",
        htmltools::tags$h5(demo_org_label(code, group)),
        htmltools::tags$p(paste0(scales::comma(dc_count(sum(rows$N))), " pregnancies")),
        demo_overview_bar(rows, field))
    })))
}

demo_overview_tabs <- function(master, mode) {
  if (mode == "Booking") master <- cohort_comparison_ethnicity(master)
  stats::setNames(lapply(c("Trust", "ICB_Cluster"), function(group) {
    fields <- c(age = "Age_band", imd = "imd_quintile", ethnicity = "ethnicity_broad",
      ethnicity_description = "ethnicity_description")
    if (mode == "Booking") fields <- c(fields[1:3], ethnicity_four = "ethnicity_profile")
    lapply(fields, function(field) demo_overview_grid(master, mode, group, field))
  }), c("Trust", "ICB_Cluster"))
}

# 5. WHOLE-COHORT POPULATION PROFILE -----------------------------------
cohort_comparison_ethnicity <- function(master) {
  # Collapse only sections 5.2/5.3, before aggregation and disclosure control.
  master$ethnicity_profile <- dplyr::case_when(
    master$ethnicity_broad %in% "White" ~ "White",
    master$ethnicity_broad %in% "Asian or Asian British" ~ "Asian",
    master$ethnicity_broad %in% "Black or Black British" ~ "Black",
    TRUE ~ "All other")
  master
}

cohort_profile_counts <- function(master, field, categories = NULL) {
  preferred <- switch(field, Age_band = report_age_bands, imd_quintile = as.character(1:5),
    ethnicity_broad = unname(demographic_ethnicity_groups),
    ethnicity_profile = c("White", "Asian", "Black", "All other"),
    stop("Unsupported cohort profile field: ", field))
  category <- as.character(master[[field]])
  # Combine missing values; retain Not stated and Two conflicting
  category[is.na(category) | trimws(category) %in% c("", "Unknown", "NULL")] <- NA_character_
  if (is.null(categories)) categories <- sankey_category_order(preferred, unique(c(preferred, category)))
  n <- vapply(categories, function(value) sum(master$N[
    if (is.na(value)) is.na(category) else !is.na(category) & category == value]), numeric(1))
  # Disclosure control after aggregation; whole-group denominator including unknowns
  data <- dc_cells(data.frame(Category = categories, N = unname(n), Total = sum(master$N)),
    c("N", "Total"), list(Share = c("N", "Total")))
  data$Label <- ifelse(is.na(categories), "Unknown", categories)
  if (field == "imd_quintile") {
    data$Label[!is.na(categories)] <- paste("Quintile", categories[!is.na(categories)])
    data$Label[categories %in% "1"] <- "Quintile 1 (most deprived)"
    data$Label[categories %in% "5"] <- "Quintile 5 (least deprived)"
  }
  data$Colour <- if (field %in% c("Age_band", "ethnicity_broad"))
    rep("#005EB8", length(categories)) else demo_category_colours(categories, field)
  data
}

cohort_profile_chart <- function(data, field) {
  if (!nrow(data) || all(data$Total == 0)) return(htmltools::div("No supplied cohort pregnancies."))
  percentages <- scales::percent(data$Share, accuracy = 0.1)
  hover <- paste0(data$Label, "<br>Pregnancies: ", scales::comma(data$N),
    "<br>", percentages, " of all cohort pregnancies")
  count_axis <- list(title = "Pregnancies", range = c(0, max(data$N) * 1.2),
    rangemode = "tozero", tickformat = ",.0f")
  # Short axis wording leaves room for bars in the three-column overview.
  # Full demographic labels remain available in hover.
  axis_labels <- data$Label
  if (field == "ethnicity_broad") {
    short <- c("Asian or Asian British" = "Asian / Asian British",
      "Black or Black British" = "Black / Black British",
      "Other ethnic groups - Chinese" = "Other: Chinese",
      "Other ethnic groups - Any other" = "Other: any other")
    matched <- axis_labels %in% names(short)
    axis_labels[matched] <- unname(short[axis_labels[matched]])
  }
  category_axis <- list(title = "", type = "category", categoryorder = "array",
    categoryarray = rev(data$Label),
    tickmode = "array", tickvals = data$Label,
    ticktext = vapply(axis_labels, function(label)
      if (grepl("<br>", label, fixed = TRUE)) label else
        paste(strwrap(label, width = if (field == "ethnicity_broad") 24 else 19), collapse = "<br>"), character(1)),
    automargin = TRUE)
  plotly::plot_ly(type = "bar", orientation = "h", x = data$N, y = data$Label,
    marker = list(color = data$Colour), text = percentages, textposition = "outside",
    textfont = list(size = 16),
    cliponaxis = FALSE, customdata = hover, hovertemplate = "%{customdata}<extra></extra>",
    meta = list(cohort_profile_field = field),
    height = max(400, 34 * nrow(data) + 90)) |>
    plotly::layout(showlegend = FALSE, bargap = .35,
      xaxis = count_axis, yaxis = category_axis,
      margin = list(l = if (field == "Age_band") 85 else 150, r = 65, t = 20, b = 65),
      font = list(family = "Arial", size = 15), hoverlabel = list(font = list(size = 15)),
      paper_bgcolor = "white", plot_bgcolor = "white") |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE) |>
    dc_widget()
}

build_cohort_profile <- function(master) {
  fields <- c(age = "Age_band", imd = "imd_quintile", ethnicity = "ethnicity_broad")
  lapply(fields, function(field) {
    data <- cohort_profile_counts(master, field)
    list(data = data, chart = cohort_profile_chart(data, field))
  })
}

# Paired booking/delivery profiles by trust
cohort_trust_profile_codes <- function(master) {
  available <- unique(c(trust_source_order, master$Booking, master$Delivery))
  available <- setdiff(available, c("Home", "No booking", "No delivery"))
  sankey_category_order(trust_source_order, available)
}

cohort_profile_org_rows <- function(master, stage, group, code) {
  column <- if (group == "Trust") stage else paste0(stage, "_ICB")
  codes <- if (group == "ICB_Cluster" && code == "Cluster") cluster_icbs else code
  master[master[[column]] %in% codes, , drop = FALSE]
}

cohort_profile_org_label <- function(code, group) {
  if (code == "Cluster") "Combined BBCS cluster" else demo_org_label(code, group)
}

cohort_trust_profile_counts <- function(master, field, code, categories, group = "Trust") {
  dplyr::bind_rows(lapply(c("Booking", "Delivery"), function(stage) {
    rows <- cohort_profile_org_rows(master, stage, group, code)
    data <- cohort_profile_counts(rows, field, categories)
    data$Stage <- stage
    data
  }))
}

cohort_trust_profile_chart <- function(data, field, code, count_max, group = "Trust") {
  horizontal <- field %in% c("ethnicity_broad", "ethnicity_profile")
  labels <- data$Label[data$Stage == "Booking"]
  colours <- c(Booking = "#005EB8", Delivery = "#ED8B00")
  chart <- plotly::plot_ly(height = if (horizontal) max(240, 24 * length(labels) + 60) else 220)
  for (stage in names(colours)) {
    rows <- data[data$Stage == stage, ]
    share <- ifelse(is.na(rows$Share), "Percentage unavailable (no pregnancies at this stage)",
      paste0(scales::percent(rows$Share, accuracy = .1), " of this panel's ", tolower(stage), " pregnancies"))
    hover <- paste0(rows$Label, "<br>", stage, "<br>Pregnancies: ", scales::comma(rows$N), "<br>", share)
    chart <- plotly::add_trace(chart, type = "bar", name = stage,
      orientation = if (horizontal) "h" else "v",
      x = if (horizontal) rows$N else rows$Label,
      y = if (horizontal) rows$Label else rows$N,
      marker = list(color = colours[[stage]]), offsetgroup = stage, legendgroup = stage,
      customdata = hover, hovertemplate = "%{customdata}<extra></extra>",
      meta = list(cohort_trust_profile_field = field, trust = code, stage = stage, organisation_group = group))
  }
  count_axis <- list(title = "Pregnancies", range = c(0, max(5, count_max) * 1.1),
    rangemode = "tozero", tickformat = ",.0f")
  category_axis <- list(title = "", type = "category", categoryorder = "array",
    categoryarray = if (horizontal) rev(labels) else labels,
    tickmode = "array", tickvals = labels,
    ticktext = vapply(labels, function(label)
      paste(strwrap(label, width = if (horizontal) 26 else 12), collapse = "<br>"), character(1)),
    automargin = TRUE)
  chart |>
    plotly::layout(barmode = "group", bargap = .3,
      meta = list(compact_cohort_profile = TRUE),
      legend = list(orientation = "h", x = 0, y = 1.04, yanchor = "bottom", traceorder = "normal",
        font = list(size = 15)),
      xaxis = if (horizontal) count_axis else category_axis,
      yaxis = if (horizontal) category_axis else count_axis,
      margin = list(l = if (horizontal) 120 else 70, r = 20, t = 38,
        b = if (horizontal) 55 else if (field == "imd_quintile") 82 else 65),
      font = list(family = "Arial", size = 15), hoverlabel = list(font = list(size = 15)),
      paper_bgcolor = "white", plot_bgcolor = "white") |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE) |>
    dc_widget()
}

build_cohort_trust_profiles <- function(master, group = "Trust") {
  master <- cohort_comparison_ethnicity(master)
  codes <- if (group == "Trust") cohort_trust_profile_codes(master) else {
    available <- unique(c(cluster_icbs, master$Booking_ICB, master$Delivery_ICB))
    available <- setdiff(available, c("Home", "No booking", "No delivery", NA_character_))
    c(sankey_category_order(icb_source_order, available), "Cluster")
  }
  fields <- c(age = "Age_band", imd = "imd_quintile", ethnicity = "ethnicity_profile")
  lapply(fields, function(field) {
    categories <- cohort_profile_counts(master, field)$Category
    counts <- stats::setNames(lapply(codes, function(code)
      cohort_trust_profile_counts(master, field, code, categories, group)), codes)
    count_max <- max(vapply(counts, function(data) max(data$N), numeric(1)))
    panels <- lapply(codes, function(code) {
      data <- counts[[code]]
      chart <- cohort_trust_profile_chart(data, field, code, count_max, group)
      total <- vapply(c("Booking", "Delivery"), function(stage)
        unique(data$Total[data$Stage == stage]), numeric(1))
      htmltools::div(class = "demo-sankey-panel",
        htmltools::tags$h4(cohort_profile_org_label(code, group)),
        htmltools::tags$p(paste0("Booking: ", scales::comma(total[1]),
          " | Delivery: ", scales::comma(total[2]), " pregnancies")), chart)
    })
    list(data = counts, grid = htmltools::div(class = "demo-outcome-grid", panels))
  })
}

cohort_match_counts <- function(master, field, categories = NULL) {
  if (is.null(categories)) categories <- cohort_profile_counts(master, field)$Category
  data <- dplyr::bind_rows(lapply(c("Same trust", "Different trusts"), function(group) {
    rows <- master[master$Trust_comparison %in% group, , drop = FALSE]
    data <- cohort_profile_counts(rows, field, categories)
    data$Group <- group
    data
  }))
  # Normalise the two protected cells, not an independently rounded total:
  # non-empty category bars then sum to 100% without exposing raw counts.
  data |>
    dplyr::group_by(Category) |>
    dplyr::mutate(Category_total = sum(N), Share = dc_divide(N, Category_total)) |>
    dplyr::ungroup()
}

cohort_match_chart <- function(data, field, proportions = FALSE) {
  horizontal <- field %in% c("ethnicity_broad", "ethnicity_profile")
  labels <- data$Label[data$Group == "Same trust"]
  colours <- c("Same trust" = "#006747", "Different trusts" = "#AE2573")
  chart <- plotly::plot_ly(height = if (horizontal) max(330, 36 * length(labels) + 130) else 330)
  for (group in names(colours)) {
    rows <- data[data$Group == group, ]
    percent <- ifelse(is.na(rows$Share), "Percentage unavailable (no comparable pregnancies in this category)",
      paste0(scales::percent(rows$Share, accuracy = .1), " of comparable pregnancies in this category"))
    hover <- paste0(rows$Label, "<br>", group, "<br>Pregnancies: ", scales::comma(rows$N), "<br>", percent)
    value <- if (proportions) rows$Share else rows$N
    chart <- plotly::add_trace(chart, type = "bar", name = group,
      orientation = if (horizontal) "h" else "v",
      x = if (horizontal) value else rows$Label, y = if (horizontal) rows$Label else value,
      marker = list(color = colours[[group]]),
      offsetgroup = if (proportions) "category" else group, legendgroup = group,
      text = if (proportions) ifelse(is.na(rows$Share) | rows$N == 0, "",
        scales::percent(rows$Share, accuracy = .1)) else NULL,
      textposition = if (proportions) "inside" else "none",
      insidetextanchor = "middle", textfont = list(color = "white", size = 15),
      constraintext = "none",
      customdata = hover, hovertemplate = "%{customdata}<extra></extra>",
      meta = list(cohort_match_field = field, group = group, measure = if (proportions) "share" else "count"))
  }
  maximum <- if (proportions) max(c(0, data$Share), na.rm = TRUE) else max(data$N)
  value_axis <- list(title = if (proportions) "% within category" else "Pregnancies",
    range = if (proportions) c(0, 1) else c(0, max(5, maximum) * 1.12),
    rangemode = "tozero", tickformat = if (proportions) ".0%" else ",.0f")
  category_axis <- list(title = "", type = "category", categoryorder = "array",
    categoryarray = if (horizontal) rev(labels) else labels,
    tickmode = "array", tickvals = labels,
    ticktext = vapply(labels, function(label)
      paste(strwrap(label, width = if (horizontal) 26 else 12), collapse = "<br>"), character(1)),
    automargin = TRUE)
  chart |>
    plotly::layout(barmode = if (proportions) "stack" else "group", bargap = .3,
      meta = list(compact_cohort_profile = TRUE),
      title = list(text = if (proportions) "Within-category percentages" else "Pregnancy counts", x = 0,
        font = list(size = 18)),
      legend = list(orientation = "h", x = 0, y = 1.05, yanchor = "bottom", traceorder = "normal",
        font = list(size = 15)),
      xaxis = if (horizontal) value_axis else category_axis,
      yaxis = if (horizontal) category_axis else value_axis,
      margin = list(l = if (horizontal) 120 else 75, r = 20, t = 80,
        b = if (horizontal) 60 else if (field == "imd_quintile") 85 else 65),
      font = list(family = "Arial", size = 15), hoverlabel = list(font = list(size = 15)),
      paper_bgcolor = "white", plot_bgcolor = "white") |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE) |>
    dc_widget()
}

cohort_match_panels <- function(data, field, title = "Whole BBCS cohort") {
  htmltools::div(class = "demo-sankey-panel cohort-match-card",
    htmltools::tags$h4(title),
    htmltools::div(class = "demo-outcome-grid",
      htmltools::div(class = "cohort-match-chart", cohort_match_chart(data, field)),
      htmltools::div(class = "cohort-match-chart", cohort_match_chart(data, field, TRUE))))
}

cohort_match_org_profiles <- function(master, field, group, categories) {
  eligible <- master[master$Trust_comparison %in% c("Same trust", "Different trusts"), , drop = FALSE]
  codes <- demo_org_codes(eligible, "Booking", group)
  codes <- setdiff(codes, c("Unknown", "No booking", "No delivery", "Home"))
  counts <- stats::setNames(lapply(codes, function(code)
    cohort_match_counts(cohort_profile_org_rows(eligible, "Booking", group, code), field, categories)), codes)
  panels <- lapply(codes, function(code)
    cohort_match_panels(counts[[code]], field, cohort_profile_org_label(code, group)))
  list(data = counts, charts = htmltools::div(class = "cohort-match-cards", panels))
}

build_cohort_match_profiles <- function(master) {
  master <- cohort_comparison_ethnicity(master)
  groups <- c("Same trust", "Different trusts", "Neither booking nor delivery recorded",
    "No recorded booking", "No recorded delivery", "Home birth", "Missing or invalid trust code")
  stopifnot(all(master$Trust_comparison %in% groups))
  summary <- dc_cells(data.frame(Group = groups,
    N = vapply(groups, function(group) sum(master$N[master$Trust_comparison %in% group]), numeric(1)),
    Total = sum(master$N)), c("N", "Total"), list(Share = c("N", "Total")))
  coverage <- summary |>
    dplyr::select(Group, N, Share) |>
    gt::gt() |>
    gt::cols_label(Group = "Trust comparison / reason not comparable", N = "Pregnancies", Share = "% of whole cohort") |>
    gt::fmt_number(columns = N, decimals = 0) |>
    gt::fmt_percent(columns = Share, decimals = 1) |>
    gt::sub_missing(missing_text = "Unavailable") |>
    gt::tab_source_note("Reasons are mutually exclusive.")
  fields <- c(age = "Age_band", imd = "imd_quintile", ethnicity = "ethnicity_profile")
  profiles <- lapply(fields, function(field) {
    data <- cohort_match_counts(master, field)
    categories <- data$Category[data$Group == "Same trust"]
    list(data = data, charts = cohort_match_panels(data, field),
      Trust = cohort_match_org_profiles(master, field, "Trust", categories),
      ICB_Cluster = cohort_match_org_profiles(master, field, "ICB_Cluster", categories))
  })
  list(summary = summary, coverage = coverage, profiles = profiles)
}

# 6. OBJECTS FOR RSTUDIO AND THE REPORT ---------------------------------
# Clean once per mother before the pregnancy join; keep raw inputs for inspection.
demographics_cleaned <- prepare_demographics(demographics)
`Demo Sankeys` <- build_demo_sankeys(final_cohort_2326, demographics_cleaned, report_episodes)
demo_sankey_charts <- list(
  version = "demographic-profile-v15",
  cohort_total = dc_count(sum(`Demo Sankeys`$N)),
  cohort_profile = build_cohort_profile(`Demo Sankeys`),
  cohort_trust_profiles = build_cohort_trust_profiles(`Demo Sankeys`),
  cohort_icb_profiles = build_cohort_trust_profiles(`Demo Sankeys`, "ICB_Cluster"),
  cohort_match_profiles = build_cohort_match_profiles(`Demo Sankeys`),
  overview_delivery = demo_overview_tabs(`Demo Sankeys`, "Delivery"),
  overview_booking = demo_overview_tabs(`Demo Sankeys`, "Booking"),
  overall = demo_sankey_chart(`Demo Sankeys`),
  ethnicity = demo_sankey_grid(`Demo Sankeys`, "ethnicity_broad"),
  imd = demo_sankey_grid(`Demo Sankeys`, "imd_quintile"),
  age = demo_sankey_grid(`Demo Sankeys`, "Age_band"),
  demographic_ribbons = list(
    ethnicity = demo_ribbon_views(`Demo Sankeys`, "ethnicity_broad"),
    age = demo_ribbon_views(`Demo Sankeys`, "Age_band"),
    imd = demo_ribbon_views(`Demo Sankeys`, "imd_quintile")),
  booking_icb = demo_pathway_tabs(`Demo Sankeys`, "ICB_Cluster"),
  booking_trust = demo_pathway_tabs(`Demo Sankeys`, "Trust"))
save_report_rds(demo_sankey_charts, ".demo_sankeys.rds")
