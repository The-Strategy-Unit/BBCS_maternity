# DEMOGRAPHIC SANKEYS ===================================================
# Run after Maternity analysis.R; Run analysis.R does this automatically.
# The master is an aggregate table: no mother or pregnancy IDs are retained.
source("R/demographics.R", encoding = "UTF-8")
demo_period <- "2023/24\u20132025/26"

# Display order and colours shared by all ethnicity charts.
report_ethnicity_colours <- c("Asian or Asian British" = "#006747",
  "Black or Black British" = "#330072", Mixed = "#ED8B00", Other = "#AE2573",
  White = "#5881C1", Unknown = "#959595")

report_ethnicity_group <- function(value) {
  dplyr::case_when(
    value %in% c("Other ethnic groups - Chinese", "Other ethnic groups - Any other") ~ "Other",
    value %in% names(report_ethnicity_colours) ~ as.character(value),
    .default = "Unknown")
}

demo_provider_labels <- function() {
  full_flow_trust_labels()
}

# Fixed organisation panels; outside organisations can still be flow endpoints.
demo_bbcs_org_codes <- function(group, include_other = FALSE) {
  if (group == "Trust") c(intersect(setdiff(names(trust_labels), "Other"), cluster_trusts),
    if (include_other) "Other") else
    c(cluster_icbs, "Cluster")
}

# Compare the booking trust with the actual trust or Home destination.
# A recorded home birth is Different even if its responsible provider is the
# booking trust. A home destination does not need a delivery-provider code.
cohort_trust_comparison <- function(episodes) {
  booking <- episodes$Booking_provider_supplied | episodes$Booking_date_supplied
  valid <- grepl("^R[A-Z0-9]{2}$", episodes$Booking_Trust) &
    (episodes$Home %in% 1 | grepl("^R[A-Z0-9]{2}$", episodes$Trust))
  dplyr::case_when(
    !booking & !episodes$Birth_recorded ~ "Neither booking nor delivery recorded",
    !booking ~ "No recorded booking",
    !episodes$Birth_recorded ~ "No recorded delivery",
    !valid ~ "Missing or invalid trust code",
    episodes$Home %in% 1 ~ "Different trusts",
    episodes$Booking_Trust == episodes$Trust ~ "Same trust",
    TRUE ~ "Different trusts")
}

# 1. PREPARE THE MASTER TABLE -------------------------------------------
build_demo_sankeys <- function(cohort, demographics, episodes = prepare_report_cohort(cohort)) {
  episodes <- bbcs_cohort_episodes(episodes, cluster_trusts, cluster_las)
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
        home_birth = Home, birth_code = Trust)),
      Delivery_pattern = classify_booking_delivery_outcome(data.frame(birth_recorded = Birth_recorded,
        home_birth = Home, birth_code = Trust, booking_code = Booking_Trust))) |>
    dplyr::left_join(lookup, by = "person_id_mother", na_matches = "never",
      relationship = "many-to-one")
  records |>
    dplyr::mutate(ethnicity_broad = report_ethnicity_group(ethnicity_broad)) |>
    dplyr::count(Booking, Delivery, Booking_ICB, Delivery_ICB, Pathway, Outcome, Trust_comparison, Delivery_pattern,
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
  labels <- demo_provider_labels()
  if (!compact) return(make_sankey(list(links = links), labels,
    " (booking)", " (delivery)", source_order, target_order))

  # Short node labels fit three panels across. Hover retains the full names.
  short <- c(RNA = "Dudley", RL4 = "Wolverhampton", RQ3 = "Birmingham W&C",
    RXK = "Sandwell & W Birmingham", RBK = "Walsall", RRK = "UHB",
    Other = "Other trusts", Unknown = "Unknown provider", Home = "Home birth",
    `No booking` = "No recorded booking", `No delivery` = "No recorded delivery")
  extra <- setdiff(names(labels), names(short))
  short <- c(short, stats::setNames(sub(" NHS( Foundation)? Trust$", "", unname(labels[extra])), extra))
  panel_labels <- stats::setNames(gsub("\n", "<br>", stringr::str_wrap(short, width = 22)), names(short))
  chart <- plotly::plotly_build(make_sankey(list(links = links), labels, " (booking)", " (delivery)",
    source_order, target_order, panel_labels = panel_labels))
  chart$x$data[[1]]$node$pad <- 12
  trace <- chart$x$data[[1]]
  chart$x$data[[1]]$node$y <- sankey_node_y(trace$node$x, trace$link$source,
    trace$link$target, trace$link$value, 430 - 36, 12)
  chart$x$layout$height <- 430
  chart$x$layout$font$size <- 10
  chart$height <- 430
  chart
}

demo_category_order <- function(field, preferred, values) {
  if (field == "ethnicity_broad") return(names(report_ethnicity_colours))
  sankey_category_order(preferred, values)
}

demo_category_values <- function(master, field) {
  values <- unique(as.character(master[[field]]))
  preferred <- switch(field,
    imd_quintile = as.character(1:5),
    Age_band = report_age_bands,
    character())
  demo_category_order(field, preferred, values)
}

demo_category_label <- function(value, field) {
  label <- if (is.na(value)) "Unknown (NA)" else if (value == "") "Blank" else value
  if (field == "imd_quintile")
    label <- imd_quintile_labels(value, prefix = "IMD quintile", unknown = label)
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
  if (field == "ethnicity_broad") return(unname(report_ethnicity_colours[values]))
  stop("Unsupported ribbon demographic: ", field)
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
  cohort_profile_org_rows(master, c("Booking", "Delivery"), group, code)
}

demo_ribbon_org_codes <- function(master, group) {
  demo_bbcs_org_codes(group)
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
  panel_labels <- labels
  if (compact) {
    short <- c(RNA = "Dudley", RL4 = "Wolverhampton", RQ3 = "Birmingham W&C",
      RXK = "Sandwell & W Birmingham", RBK = "Walsall", RRK = "UHB")
    known <- intersect(names(short), names(labels))
    panel_labels[known] <- short[known]
    panel_labels[] <- vapply(sub(" ICB$", "", panel_labels), function(label)
      paste(strwrap(label, 22), collapse = "<br>"), character(1))
  }
  chart <- make_sankey(list(links = links), labels, " (booking)", " (delivery)", order, order,
    panel_labels = panel_labels)
  chart <- plotly::plotly_build(chart)
  trace <- chart$x$data[[1]]
  category_labels <- vapply(links$Category, demo_category_label, character(1), field = field)
  trace$meta <- list(demographic_colour_field = field)
  trace$node$color <- rep("#959595", length(trace$node$label))
  trace$link$label <- unname(category_labels)
  trace$link$color <- grDevices::adjustcolor(demo_category_colours(links$Category, field), alpha.f = 0.65)
  trace$link$customdata <- paste0(category_labels, "<br>", trace$link$customdata)
  if (compact) {
    sources <- order[order %in% links$source]
    targets <- order[order %in% links$target]
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
      htmltools::tags$p(paste0(scales::comma(dc_count(sum(rows$N))), if (code == "Cluster") " BBCS cohort pregnancies" else " pregnancies booked or delivered here")),
      htmltools::tags$p(class = "demo-sankey-stages", "Booking \u2192 Delivery"),
      demo_ribbon_sankey(rows, field, group, compact = TRUE))
  })
  htmltools::tagList(demo_ribbon_key(master, field), htmltools::div(class = "demo-sankey-grid", panels))
}

demo_ribbon_views <- function(master, field) {
  list(overall = htmltools::tagList(chart_total(sum(master$N), "BBCS cohort pregnancies"),
    demo_ribbon_key(master, field), demo_ribbon_sankey(master, field)),
    Trust = demo_ribbon_grid(master, field, "Trust"),
    ICB_Cluster = demo_ribbon_grid(master, field, "ICB_Cluster"))
}

# 3. ORGANISATION GROUPS AND RECORDED DELIVERY OUTCOMES -----------------
demo_org_codes <- function(master, mode, group, whole_cohort = FALSE) {
  codes <- demo_bbcs_org_codes(group)
  if (whole_cohort && group != "Trust") codes[codes == "Cluster"] <- "Cohort"
  codes
}

demo_org_rows <- function(master, mode, group, code) {
  if (group != "Trust" && code == "Cohort") return(master)
  field <- if (group == "Trust") mode else paste0(mode, "_ICB")
  if (group != "Trust" && code == "Cluster") return(cohort_profile_org_rows(master, mode, group, code))
  master[master[[field]] %in% code, , drop = FALSE]
}

demo_org_label <- function(code, group) {
  labels <- if (group == "Trust") demo_provider_labels() else
    c(icb_labels, Unknown = "Unknown ICB", `No booking` = "No recorded booking",
      `No delivery` = "No recorded delivery", Home = "Home birth",
      Cluster = "Combined BBCS cluster", Cohort = "Whole BBCS cohort")
  label <- unname(labels[code])
  if (!is.na(label)) return(label)
  if (group == "Trust") report_trust_name(code, trust_labels) else "ICB name unavailable"
}

# In section 4.3 only the named trust and ICB panels are booking cohorts.
demo_pathway_rows <- function(master, group, code) {
  if (group == "ICB_Cluster" && code == "Cluster") return(master)
  cohort_profile_org_rows(master, "Booking", group, code)
}

demo_pathway_summary <- function(master, group, codes = NULL) {
  if (is.null(codes)) codes <- demo_bbcs_org_codes(group)
  dplyr::bind_rows(lapply(codes, function(code) {
    rows <- demo_pathway_rows(master, group, code)
    organisation <- cohort_profile_org_label(code, group)
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
  category_totals[is.na(category_totals)] <- 0
  outcome_totals <- dc_count(vapply(outcomes, function(outcome) sum(rows$N[rows$Outcome == outcome]), numeric(1)))
  total <- dc_count(sum(rows$N))
  labels <- c(category_labels, outcome_labels)
  totals <- c(category_totals, outcome_totals)
  percentages <- scales::percent(dc_divide(totals, total), accuracy = 0.1)
  node_labels <- paste0(vapply(labels, function(label) paste(strwrap(label, 25), collapse = "<br>"), character(1)),
    "<br>", percentages)
  links <- links[links$N > 0 & !is.na(links$N), , drop = FALSE]
  values <- links$N
  category_share <- links$Category_share
  metadata <- NULL
  if (proportions) {
    denominators <- ave(links$N, links$Source, FUN = sum)
    values <- 100 * links$N / denominators
    # Tooltip and ribbon must use the same protected within-category denominator.
    category_share <- values / 100
    node_labels <- c(paste0(vapply(category_labels, function(label)
      paste(strwrap(label, 25), collapse = "<br>"), character(1)), ifelse(category_totals > 0, "<br>100%", "")),
      tail(node_labels, length(outcome_labels)))
    metadata <- list(msds_sankey_scale = "normalised_categories",
      display_counts = links$N, display_denominators = denominators)
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
        "<br>", percentages, " of pregnancies in this panel"),
      hovertemplate = "%{customdata}<extra></extra>"),
    link = list(source = links$Source, target = links$Target, value = values,
      color = grDevices::adjustcolor(outcome_colours[match(links$Outcome, outcomes)], alpha.f = 0.4),
      customdata = paste0(category_labels[links$Source + 1L], " \u2192 ",
        outcome_labels[match(links$Outcome, outcomes)],
        "<br>Pregnancies: ", scales::comma(links$N),
        "<br>", scales::percent(category_share, accuracy = 0.1), " of this demographic category",
        "<br>", scales::percent(links$Organisation_share, accuracy = 0.1), " of pregnancies in this panel"),
      hovertemplate = "%{customdata}<extra></extra>")) |>
    plotly::layout(font = list(family = "Arial", size = 12),
      margin = list(l = 12, r = 12, t = 12, b = 20), paper_bgcolor = "white", plot_bgcolor = "white") |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE) |>
    dc_widget()
}

demo_pathway_grid <- function(master, group, field = NULL, proportions = FALSE) {
  summary <- demo_pathway_summary(master, group)
  panels <- lapply(seq_len(nrow(summary)), function(i) {
    rows <- demo_pathway_rows(master, group, summary$Code[i])
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
  categories <- demo_category_values(rows, field)
  labels <- vapply(categories, demo_category_label, character(1), field = field)
  short_labels <- labels
  if (field == "imd_quintile") short_labels <- sub("^IMD ", "", short_labels)
  short_labels <- sub("Unknown (NA)", "Unknown", short_labels, fixed = TRUE)
  tick_labels <- vapply(short_labels, function(label) paste(strwrap(label, width = 22), collapse = "<br>"), character(1))
  label_lines <- lengths(strsplit(tick_labels, "<br>", fixed = TRUE))
  left_margin <- switch(field, Age_band = 64, imd_quintile = 150, 148)
  counts <- rows |>
    dplyr::group_by(Category = as.character(.data[[field]]), Pathway) |>
    dplyr::summarise(N = sum(N), .groups = "drop") |>
    dplyr::mutate(Category = factor(labels[match(Category, categories)], levels = labels),
      Pathway = factor(Pathway, levels = names(demo_pathway_colours)),
      N = dc_count(N)) |>
    # Normalise protected status counts within each demographic category so
    # every non-empty bar totals 100%; unknown categories stay separate.
    dplyr::group_by(Category) |>
    dplyr::mutate(Category_total = sum(N),
      Share = dplyr::coalesce(dc_divide(N, Category_total), 0)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Hover = paste0(Category, "<br>", Pathway, "<br>",
        scales::percent(Share, accuracy = 0.1), " within this demographic group",
        "<br>Pregnancies: ", scales::comma(N)))
  chart <- plotly::plot_ly(counts, x = ~Share, y = ~Category, color = ~Pathway,
    colors = demo_pathway_colours, type = "bar", orientation = "h",
    text = ~scales::percent(Share, accuracy = 0.1), textposition = "inside", insidetextanchor = "middle",
    textfont = list(size = 13, color = "white"), textangle = 0, constraintext = "inside",
    customdata = ~Hover, hovertemplate = "%{customdata}<extra></extra>",
    height = max(320, sum(pmax(34, label_lines * 17 + 10)) + 70)) |>
    plotly::layout(barmode = "stack", showlegend = FALSE,
      meta = list(demographic_overview_field = field), uniformtext = list(minsize = 12, mode = "hide"),
      xaxis = list(title = "% within demographic group", range = c(0, 1),
        tickformat = ".0%", dtick = 0.25),
      yaxis = list(title = "", categoryorder = "array", categoryarray = rev(labels),
        tickmode = "array", tickvals = labels,
        ticktext = tick_labels, automargin = FALSE),
      margin = list(l = left_margin, r = 12, t = 12, b = 50),
      font = list(family = "Arial", size = 13)) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
  chart <- plotly::plotly_build(chart)
  for (i in seq_along(chart$x$data)) {
    light_segment <- chart$x$data[[i]]$name %in%
      c("Booking without recorded delivery", "Neither event recorded")
    chart$x$data[[i]]$textfont$color <- if (light_segment) "#111111" else "white"
  }
  chart
}

demo_overview_grid <- function(master, mode, group, field) {
  # 4.1.2 explicitly includes a whole-cohort recording-status summary.
  codes <- demo_org_codes(master, mode, group, whole_cohort = TRUE)
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

demo_overview_tabs <- function(master, mode, delivery_profiles = NULL) {
  # Apply the same booking cohort and outcome definitions as section 5.4.
  # Selecting recorded deliveries first would remove booking-only pregnancies.
  if (mode == "Delivery") {
    if (is.null(delivery_profiles)) delivery_profiles <- build_delivery_pattern_profiles(master)
    return(lapply(delivery_profiles, function(fields) lapply(fields, function(field) field$grid)))
  }
  stats::setNames(lapply(c("Trust", "ICB_Cluster"), function(group) {
    lapply(demo_profile_fields(), function(field) demo_overview_grid(master, mode, group, field))
  }), c("Trust", "ICB_Cluster"))
}

# 5. WHOLE-COHORT POPULATION PROFILE -----------------------------------
demo_profile_fields <- function() {
  c(age = "Age_band", imd = "imd_quintile", ethnicity = "ethnicity_broad")
}

cohort_profile_counts <- function(master, field, categories = NULL) {
  preferred <- switch(field, Age_band = report_age_bands, imd_quintile = as.character(1:5),
    Age_group = levels(master$Age_group),
    ethnicity_broad = names(report_ethnicity_colours),
    stop("Unsupported cohort profile field: ", field))
  category <- as.character(master[[field]])
  if (field != "ethnicity_broad")
    category[is.na(category) | trimws(category) %in% c("", "Unknown", "NULL")] <- NA_character_
  if (is.null(categories)) categories <- demo_category_order(field, preferred, c(preferred, category))
  n <- vapply(categories, function(value) sum(master$N[
    if (is.na(value)) is.na(category) else !is.na(category) & category == value]), numeric(1))
  # Disclosure control after aggregation; whole-group denominator including unknowns
  data <- dc_cells(data.frame(Category = categories, N = unname(n), Total = sum(master$N)),
    c("N", "Total"), list(Share = c("N", "Total")))
  data$Label <- ifelse(is.na(categories), "Unknown", categories)
  if (field == "imd_quintile") data$Label <- imd_quintile_labels(categories)
  data$Colour <- if (field %in% c("Age_band", "Age_group", "ethnicity_broad"))
    rep("#005EB8", length(categories)) else demo_category_colours(categories, field)
  data
}

cohort_profile_chart <- function(data, field, view = "percent") {
  count_axis <- view != "percent"
  if (!nrow(data) || all(data$Total == 0)) return(htmltools::div("No supplied cohort pregnancies."))
  percentages <- scales::percent(data$Share, accuracy = 0.1)
  hover <- paste0(data$Label, "<br>Pregnancies: ", scales::comma(data$N),
    "<br>", percentages, " of all cohort pregnancies")
  value_axis <- if (count_axis) list(title = "Pregnancies", range = c(0, max(data$N) * 1.2),
    rangemode = "tozero", tickformat = ",.0f") else
    list(title = "% of cohort pregnancies", range = c(0, 1), tickformat = ".0%", dtick = .25)
  category_axis <- list(title = "", type = "category", categoryorder = "array",
    categoryarray = rev(data$Label),
    tickmode = "array", tickvals = data$Label,
    ticktext = vapply(data$Label, function(label)
      if (grepl("<br>", label, fixed = TRUE)) label else
        paste(strwrap(label, width = if (field == "ethnicity_broad") 24 else 19), collapse = "<br>"), character(1)),
    automargin = TRUE)
  plotly::plot_ly(type = "bar", orientation = "h", x = if (count_axis) data$N else data$Share, y = data$Label,
    marker = list(color = data$Colour), text = if (view == "counts") scales::comma(data$N, accuracy = 1) else percentages,
    textposition = "outside",
    textfont = list(size = 16),
    cliponaxis = FALSE, customdata = hover, hovertemplate = "%{customdata}<extra></extra>",
    meta = list(cohort_profile_field = field),
    height = max(400, 34 * nrow(data) + 90)) |>
    plotly::layout(showlegend = FALSE, bargap = .35,
      xaxis = value_axis, yaxis = category_axis,
      margin = list(l = if (field == "Age_band") 85 else 150, r = 65, t = 20, b = 65),
      font = list(family = "Arial", size = 15), hoverlabel = list(font = list(size = 15)),
      paper_bgcolor = "white", plot_bgcolor = "white") |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE) |>
    dc_widget()
}

build_cohort_profile <- function(master) {
  lapply(demo_profile_fields(), function(field) {
    data <- cohort_profile_counts(master, field)
    list(data = data, chart = cohort_profile_chart(data, field),
      count_chart = cohort_profile_chart(data, field, "counts"),
      mixed_chart = cohort_profile_chart(data, field, "mixed"))
  })
}

# Paired booking/delivery profiles by trust
cohort_trust_profile_codes <- function(master) {
  demo_bbcs_org_codes("Trust", include_other = TRUE)
}

cohort_profile_org_rows <- function(master, stage, group, code) {
  if (group == "ICB_Cluster" && code == "Cluster") {
    if (length(stage) > 1L) return(master)
    absent <- if (stage == "Booking") "No booking" else "No delivery"
    return(master[master[[stage]] != absent, , drop = FALSE])
  }
  at_stage <- function(event) {
    provider <- master[[if (group == "Trust") event else paste0(event, "_ICB")]]
    if (group == "Trust" && code == "Other") {
      known <- provider %in% "Other" | grepl("^R[A-Z0-9]{2}$", provider)
      return(known & !provider %in% demo_bbcs_org_codes("Trust"))
    }
    provider %in% code
  }
  master[Reduce(`|`, lapply(stage, at_stage)), , drop = FALSE]
}

cohort_profile_org_label <- function(code, group) {
  if (code == "Cluster") "Combined BBCS cluster" else demo_org_label(code, group)
}

# Section 5.1 compares whole pregnancy profiles, counting either event once per organisation.
cohort_org_grid <- function(data, colours, group, inset = FALSE) {
  tags <- htmltools::tags
  organisations <- unique(data$age[c("Organisation", "Total")])
  fields <- c(age = "Age band", ethnicity = "Ethnicity", imd = "IMD quintile")
  if (inset) fields <- fields[c("age", "imd", "ethnicity")]
  legend <- lapply(names(fields), function(field) {
    tags$div(tags$strong(fields[[field]]), if (!inset) report_colour_key(colours[[field]]))
  })
  # A common vertical scale makes bar heights comparable across all trust clusters.
  peak <- max(.1, unlist(lapply(data, function(rows) rows$Share)), na.rm = TRUE)
  highest <- ceiling(peak * 10) / 10
  cards <- lapply(seq_len(nrow(organisations)), function(i) {
    organisation <- organisations$Organisation[i]
    clusters <- lapply(names(fields), function(field) {
      rows <- data[[field]][data[[field]]$Organisation == organisation, ]
      bars <- lapply(seq_len(nrow(rows)), function(j) {
        share <- rows$Share[j]
        category <- rows$Label[j]
        label <- if (is.finite(share)) scales::percent(share, accuracy = 1) else "\u2014"
        detail <- paste0(organisation, " | ", category, " | ",
          if (is.finite(share)) scales::percent(share, accuracy = .1) else "Percentage unavailable", " | ",
          scales::comma(rows$N[j]), " of ", scales::comma(rows$Total[j]), " pregnancies")
        tags$div(class = "trust-mini-column", title = detail, `aria-label` = detail,
          tags$div(class = "trust-mini-bar", style = paste0("background-color:",
            colours[[field]][[category]], ";--share:",
            if (is.finite(share)) sprintf("%.4f%%", 100 * share / highest) else "0%"),
            tags$span(class = "trust-mini-value", label)))
      })
      tags$div(class = "trust-mini-cluster", role = "group", `aria-label` = fields[[field]],
        tags$div(class = "trust-mini-field", fields[[field]]),
        tags$div(class = "trust-mini-bars", bars),
        if (inset && i == 1L) report_colour_key(colours[[field]], class = "trust-inset-key"))
    })
    tags$section(class = "trust-mini-card", `aria-label` = organisation,
      tags$div(class = "trust-mini-heading",
        tags$h5(title = organisation, sub(" NHS( Foundation)? Trust$", "", organisation)),
        tags$small(paste(scales::comma(organisations$Total[i]), "pregnancies"))), clusters)
  })
  tags$div(class = if (inset) "trust-mini-view trust-inset-plot" else "trust-mini-view",
    tags$div(class = "trust-mini-keys", tags$strong(class = "trust-mini-stub", if (group == "Trust") "Trust" else "ICB / cluster"), legend),
    tags$div(class = "trust-mini-cards", cards))
}

build_cohort_org_comparison <- function(master, group) {
  codes <- demo_bbcs_org_codes(group, include_other = TRUE)
  labels <- vapply(codes, cohort_profile_org_label, character(1), group = group)
  orgs <- lapply(codes, function(code)
    cohort_profile_org_rows(master, c("Booking", "Delivery"), group, code))
  fields <- demo_profile_fields()
  data <- lapply(fields, function(field) {
    categories <- cohort_profile_counts(master, field)$Category
    dplyr::bind_rows(lapply(seq_along(orgs), function(i) {
      rows <- cohort_profile_counts(orgs[[i]], field, categories)
      rows$Organisation <- unname(labels[i])
      rows
    }))
  })
  n_categories <- max(vapply(data, function(rows) length(unique(rows$Label)), integer(1)))
  height <- max(470, length(codes) * (17 * n_categories + 9) + 70)
  keys <- Map(function(rows, field) {
    categories <- unique(rows[c("Category", "Label")])
    stats::setNames(demo_category_colours(categories$Category, field), categories$Label)
  }, data, fields)
  ticks <- sub(" NHS( Foundation)? Trust$", "", labels)
  tick_labels <- paste0(gsub("\n", "<br>", stringr::str_wrap(ticks, 16)), "<br>",
    scales::comma(dc_count(vapply(orgs, function(rows) sum(rows$N), numeric(1)))), " pregnancies")
  percent_max <- max(1, vapply(data, function(rows)
    max(tapply(rows$Share, rows$Organisation, sum, na.rm = TRUE)), numeric(1)))
  count_max <- max(5, vapply(data, function(rows) max(rows$N), numeric(1)))
  make_chart <- function(rows_by_org, colours, view = "grouped") {
    stacked <- view %in% c("stacked", "stacked_outside")
    counts <- view == "counts"
    chart <- plotly::plot_ly(height = if (stacked) length(codes) * 75 + 85 else height)
    # Stacks follow the colour key left to right. Grouped bars draw bottom to top.
    categories <- if (stacked) names(colours) else rev(names(colours))
    for (category in categories) {
      rows <- rows_by_org[rows_by_org$Label == category, ]
      share <- ifelse(is.na(rows$Share), "Percentage unavailable",
        paste0(scales::percent(rows$Share, accuracy = .1), " of this organisation's pregnancies"))
      chart <- plotly::add_trace(chart, type = "bar", orientation = "h", name = category,
        x = if (counts) rows$N else rows$Share, y = rows$Organisation, marker = list(color = unname(colours[category])),
        text = if (counts) scales::comma(rows$N, accuracy = 1) else if (!stacked) scales::percent(rows$Share, accuracy = .1),
        textposition = if (stacked) "none" else "outside", textangle = 0,
        textfont = list(color = "#2C2825"), constraintext = "none", cliponaxis = FALSE,
        offsetgroup = if (!stacked) category, customdata = paste0(rows$Organisation, "<br>", category,
          "<br>Pregnancies: ", scales::comma(rows$N, accuracy = 1), "<br>", share),
        hovertemplate = "%{customdata}<extra></extra>")
    }
    chart |> plotly::layout(barmode = if (stacked) "stack" else "group",
      bargap = if (stacked) .62 else .12, showlegend = FALSE,
      annotations = if (stacked) stacked_share_annotations(rows_by_org$Organisation, rows_by_org$Share,
        outside = view == "stacked_outside"),
      meta = list(compact_cohort_profile = TRUE, organisation_count_labels = TRUE),
      xaxis = if (counts) list(title = "Pregnancies", range = c(0, count_max * 1.22), tickformat = ",.0f")
        else list(title = "% of pregnancies", range = c(0, percent_max), tickformat = ".0%", dtick = .25),
      yaxis = list(title = "", type = "category", categoryorder = "array", categoryarray = rev(unname(labels)),
        tickmode = "array", tickvals = unname(labels),
        ticktext = tick_labels, automargin = TRUE),
      margin = list(l = 55, r = if (stacked) 15 else 30, t = 8, b = 55), font = list(family = "Arial")) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE) |> dc_widget()
  }
  list(charts = Map(make_chart, data, keys),
    percent_charts = Map(make_chart, data, keys, MoreArgs = list(view = "stacked")),
    outside_charts = Map(make_chart, data, keys, MoreArgs = list(view = "stacked_outside")),
    count_charts = Map(make_chart, data, keys, MoreArgs = list(view = "counts")),
    compact_grid = cohort_org_grid(data, keys, group),
    inset_grid = cohort_org_grid(data, keys, group, inset = TRUE),
    keys = lapply(keys, report_colour_key, class = "cohort-org-key"))
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
  horizontal <- field == "ethnicity_broad"
  labels <- data$Label[data$Stage == "Booking"]
  colours <- c(Booking = "#005EB8", Delivery = "#ED8B00")
  chart <- plotly::plot_ly(height = if (horizontal) max(270, 36 * length(labels) + 95) else 250)
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
      text = ifelse(rows$N > 0, scales::comma(rows$N, accuracy = 1), ""),
      textposition = "outside", textangle = 0, cliponaxis = FALSE,
      textfont = list(size = 13, color = "#2C2825"), constraintext = "none",
      customdata = hover, hovertemplate = "%{customdata}<extra></extra>",
      meta = list(cohort_trust_profile_field = field, trust = code, stage = stage, organisation_group = group))
  }
  count_axis <- list(title = "Pregnancies", range = c(0, max(5, count_max) * 1.18),
    rangemode = "tozero", tickformat = ",.0f")
  category_axis <- list(title = "", type = "category", categoryorder = "array",
    categoryarray = if (horizontal) rev(labels) else labels,
    tickmode = "array", tickvals = labels,
    ticktext = vapply(labels, function(label)
      paste(strwrap(label, width = if (horizontal) 26 else 12), collapse = "<br>"), character(1)),
    automargin = TRUE)
  chart |>
    plotly::layout(barmode = "group", bargap = .3, showlegend = FALSE,
      meta = list(compact_cohort_profile = TRUE),
      legend = list(orientation = "h", x = 0, y = 1.04, yanchor = "bottom", traceorder = "normal",
        font = list(size = 15)),
      xaxis = if (horizontal) count_axis else category_axis,
      yaxis = if (horizontal) category_axis else count_axis,
      margin = list(l = if (horizontal) 120 else 70, r = if (horizontal) 65 else 20, t = 38,
        b = if (horizontal) 55 else if (field == "imd_quintile") 82 else 65),
      font = list(family = "Arial", size = 15), hoverlabel = list(font = list(size = 15)),
      paper_bgcolor = "white", plot_bgcolor = "white") |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE) |>
    dc_widget()
}

build_cohort_trust_profiles <- function(master, group = "Trust") {
  codes <- if (group == "Trust") cohort_trust_profile_codes(master) else c(cluster_icbs, "Cluster")
  lapply(demo_profile_fields(), function(field) {
    categories <- cohort_profile_counts(master, field)$Category
    counts <- stats::setNames(lapply(codes, function(code)
      cohort_trust_profile_counts(master, field, code, categories, group)), codes)
    panels <- lapply(codes, function(code) {
      data <- counts[[code]]
      chart <- cohort_trust_profile_chart(data, field, code, max(data$N), group)
      total <- vapply(c("Booking", "Delivery"), function(stage)
        unique(data$Total[data$Stage == stage]), numeric(1))
      htmltools::div(class = "demo-sankey-panel",
        htmltools::tags$h4(cohort_profile_org_label(code, group)),
        htmltools::tags$p(paste0("Booking: ", scales::comma(total[1]),
          " | Delivery: ", scales::comma(total[2]), " pregnancies")), chart)
    })
    list(data = counts, grid = htmltools::tagList(
      report_colour_key(c(Booking = "#005EB8", Delivery = "#ED8B00"), "cohort-stage-key"),
      htmltools::div(class = "demo-outcome-grid", panels)))
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

cohort_match_group_label <- function(group) {
  ifelse(group == "Different trusts", "Different trust or home birth", group)
}

cohort_match_chart <- function(data, field, proportions = FALSE) {
  horizontal <- field == "ethnicity_broad"
  labels <- data$Label[data$Group == "Same trust"]
  colours <- c("Same trust" = "#006747", "Different trusts" = "#AE2573")
  # Keep percentage labels readable even when one status occupies a thin sliver.
  small <- if (proportions) data[!is.na(data$Share) & data$N > 0 & data$Share < .08, ] else data[0, ]
  small_labels <- lapply(seq_len(nrow(small)), function(i) {
    label <- list(text = scales::percent(small$Share[i], accuracy = 1), showarrow = FALSE,
      font = list(size = 15, color = unname(colours[small$Group[i]])))
    if (horizontal) c(label, list(x = 1, xref = "paper", xshift = 8, xanchor = "left",
      y = small$Label[i], yref = "y")) else
      c(label, list(x = small$Label[i], xref = "x", y = 1, yref = "paper",
        yshift = 6, yanchor = "bottom"))
  })
  chart <- plotly::plot_ly(height = if (horizontal) max(260, 31 * length(labels) + 90) else 260)
  for (group in names(colours)) {
    rows <- data[data$Group == group, ]
    percent <- ifelse(is.na(rows$Share), "Percentage unavailable (no comparable pregnancies in this category)",
      paste0(scales::percent(rows$Share, accuracy = .1), " of comparable pregnancies in this category"))
    hover <- paste0(rows$Label, "<br>", cohort_match_group_label(group), "<br>Pregnancies: ", scales::comma(rows$N), "<br>", percent)
    value <- if (proportions) rows$Share else rows$N
    chart <- plotly::add_trace(chart, type = "bar", name = group,
      orientation = if (horizontal) "h" else "v",
      x = if (horizontal) value else rows$Label, y = if (horizontal) rows$Label else value,
      marker = list(color = colours[[group]]),
      offsetgroup = if (proportions) "category" else group, legendgroup = group,
      text = if (proportions) ifelse(is.na(rows$Share) | rows$N == 0 | rows$Share < .08, "",
        scales::percent(rows$Share, accuracy = 1)) else ifelse(rows$N > 0, scales::comma(rows$N, accuracy = 1), ""),
      textposition = if (proportions) "inside" else "outside", cliponaxis = FALSE,
      insidetextanchor = "middle",
      textfont = list(color = if (proportions) "white" else "#2C2825", size = if (proportions) 15 else 13), textangle = 0,
      constraintext = "none",
      customdata = hover, hovertemplate = "%{customdata}<extra></extra>",
      meta = list(cohort_match_field = field, group = group, measure = if (proportions) "share" else "count"))
  }
  maximum <- if (proportions) max(c(0, data$Share), na.rm = TRUE) else max(data$N)
  value_axis <- list(title = if (proportions) "% within category" else "Pregnancies",
    range = if (proportions) c(0, 1) else c(0, max(5, maximum) * 1.22),
    rangemode = "tozero", tickformat = if (proportions) ".0%" else ",.0f")
  category_axis <- list(title = "", type = "category", categoryorder = "array",
    categoryarray = if (horizontal) rev(labels) else labels,
    tickmode = "array", tickvals = labels,
    ticktext = vapply(labels, function(label)
      paste(strwrap(label, width = if (horizontal) 26 else 12), collapse = "<br>"), character(1)),
    automargin = TRUE)
  chart |>
    plotly::layout(barmode = if (proportions) "stack" else "group", bargap = .3, showlegend = FALSE,
      annotations = small_labels,
      meta = list(compact_cohort_profile = TRUE),
      title = list(text = if (proportions) "Within-category percentages" else "Pregnancy counts", x = 0,
        font = list(size = 18)),
      legend = list(orientation = "h", x = 0, y = 1.05, yanchor = "bottom", traceorder = "normal",
        font = list(size = 15)),
      xaxis = if (horizontal) value_axis else category_axis,
      yaxis = if (horizontal) category_axis else value_axis,
      margin = list(l = if (horizontal) 120 else 75,
        r = if (horizontal && (!proportions || nrow(small))) 80 else 20,
        t = if (!horizontal && nrow(small)) 58 else 40,
        b = if (horizontal) 50 else if (field == "imd_quintile") 75 else 55),
      font = list(family = "Arial", size = 15), hoverlabel = list(font = list(size = 15)),
      paper_bgcolor = "white", plot_bgcolor = "white") |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE) |>
    dc_widget()
}

cohort_match_panels <- function(data, field, title, total) {
  htmltools::div(class = "demo-sankey-panel cohort-match-card",
    htmltools::tags$h4(title), chart_total(total, "comparable pregnancies"),
    htmltools::div(class = "demo-outcome-grid",
      htmltools::div(class = "cohort-match-chart", cohort_match_chart(data, field)),
      htmltools::div(class = "cohort-match-chart", cohort_match_chart(data, field, TRUE))))
}

cohort_match_org_rows <- function(master, group, code) {
  cohort_profile_org_rows(master, c("Booking", "Delivery"), group, code)
}

cohort_match_org_profiles <- function(master, field, group, categories) {
  eligible <- master[master$Trust_comparison %in% c("Same trust", "Different trusts"), , drop = FALSE]
  codes <- demo_bbcs_org_codes(group, include_other = TRUE)
  codes <- setdiff(codes, c("Unknown", "No booking", "No delivery", "Home"))
  selected <- stats::setNames(lapply(codes, function(code) cohort_match_org_rows(eligible, group, code)), codes)
  totals <- vapply(selected, function(rows) sum(rows$N), numeric(1))
  counts <- lapply(selected, cohort_match_counts, field = field, categories = categories)
  panels <- lapply(codes, function(code)
    cohort_match_panels(counts[[code]], field, cohort_profile_org_label(code, group), totals[[code]]))
  key <- report_colour_key(c("Same trust" = "#006747", "Different trust or home birth" = "#AE2573"), "cohort-match-key")
  stacked <- lapply(codes, function(code) htmltools::div(class = "demo-sankey-panel",
    htmltools::tags$h4(cohort_profile_org_label(code, group)),
    chart_total(totals[[code]], "comparable pregnancies"),
    cohort_match_chart(counts[[code]], field, proportions = TRUE)))
  list(data = counts, charts = htmltools::tagList(key, htmltools::div(class = "cohort-match-cards", panels)),
    stacked_grid = htmltools::tagList(key, htmltools::div(class = "cohort-match-stacked-grid", stacked)))
}

cohort_match_age_group <- function(band) {
  groups <- c("Under 20" = "<20", "20-24" = "20-29", "25-29" = "20-29",
    "30-34" = "30-39", "35-39" = "30-39", "40-44" = "40+", "45+" = "40+")
  factor(unname(groups[as.character(band)]), levels = unique(unname(groups)))
}

cohort_match_heatmap <- function(rows, title) {
  rows$Quintile <- factor(rows$imd_quintile, levels = 1:5)
  rows$Comparison <- factor(rows$Trust_comparison, levels = c("Same trust", "Different trusts"))
  # Aggregate original pregnancy counts in each joint cell before applying disclosure control.
  counts <- stats::xtabs(N ~ Age_group + Quintile + Comparison, rows, na.action = stats::na.omit)
  different <- dc_count(counts[, , "Different trusts"])
  total <- different + dc_count(counts[, , "Same trust"])
  share <- dc_divide(different, total)
  pink <- grDevices::colorRampPalette(c("#FFF3F8", "#AE2573"))(101)
  cell <- function(i, j) {
    value <- share[i, j]
    if (is.na(value)) return(htmltools::tags$td(class = "no-comparisons",
      title = "No comparable pregnancies", "\u2014"))
    htmltools::tags$td(style = paste0("background-color:", pink[1 + round(100 * value)],
      ";color:", if (value >= .82) "white" else "black"),
      title = paste0(different[i, j], " of ", total[i, j], " comparable pregnancies"),
      scales::percent(value, accuracy = 1))
  }
  body <- lapply(seq_len(nrow(share)), function(i) htmltools::tags$tr(
    htmltools::tags$th(scope = "row", rownames(share)[i]),
    lapply(seq_len(ncol(share)), function(j) cell(i, j))))
  htmltools::div(class = "demo-sankey-panel",
    htmltools::tags$h4(title),
    chart_total(sum(counts), "comparable pregnancies with known age and IMD"),
    htmltools::tags$table(class = "cohort-match-heatmap", `aria-label` = paste(title, "percentage at a different trust or home"),
      htmltools::tags$thead(htmltools::tags$tr(htmltools::tags$th(scope = "col", "Age"),
        lapply(colnames(share), function(q) htmltools::tags$th(scope = "col", paste0("Q", q))))),
      htmltools::tags$tbody(body)))
}

cohort_match_heatmap_grid <- function(master, group) {
  codes <- demo_bbcs_org_codes(group, include_other = TRUE)
  htmltools::tagList(
    htmltools::tags$p("Rows show age at pregnancy and columns show IMD quintile, from Q1 (most deprived) to Q5 (least deprived). Each cell shows the percentage of comparable pregnancies delivered at a different trust or at home. Pregnancies with unknown age or IMD are excluded from these maps."),
    htmltools::div(class = "cohort-match-heatmap-key", "0%", htmltools::span(`aria-hidden` = "true"),
      "100%", htmltools::tags$small("\u2014  No comparable pregnancies")),
    htmltools::div(class = "cohort-match-stacked-grid", lapply(codes, function(code)
      cohort_match_heatmap(cohort_match_org_rows(master, group, code), cohort_profile_org_label(code, group)))))
}

build_cohort_match_profiles <- function(master) {
  master$Age_group <- cohort_match_age_group(master$Age_band)
  groups <- c("Same trust", "Different trusts", "Neither booking nor delivery recorded",
    "No recorded booking", "No recorded delivery", "Missing or invalid trust code")
  stopifnot(all(master$Trust_comparison %in% groups))
  summary <- dc_cells(data.frame(Group = groups,
    N = vapply(groups, function(group) sum(master$N[master$Trust_comparison %in% group]), numeric(1)),
    Total = sum(master$N)), c("N", "Total"), list(Share = c("N", "Total")))
  coverage <- summary |>
    dplyr::select(Group, N, Share) |>
    gt::gt() |>
    gt::cols_label(Group = "Comparison / reason not comparable", N = "Pregnancies", Share = "% of whole cohort") |>
    gt::text_transform(fn = cohort_match_group_label, locations = gt::cells_body(columns = Group)) |>
    gt::fmt_number(columns = N, decimals = 0) |>
    gt::fmt_percent(columns = Share, decimals = 1) |>
    gt::sub_missing(missing_text = "Unavailable") |>
    gt::tab_source_note("Reasons are mutually exclusive.")
  profiles <- lapply(c(demo_profile_fields(), age_four = "Age_group"), function(field) {
    data <- cohort_match_counts(master, field)
    categories <- data$Category[data$Group == "Same trust"]
    list(data = data,
      Trust = cohort_match_org_profiles(master, field, "Trust", categories),
      ICB_Cluster = cohort_match_org_profiles(master, field, "ICB_Cluster", categories))
  })
  heatmaps <- list(Trust = cohort_match_heatmap_grid(master, "Trust"),
    ICB_Cluster = cohort_match_heatmap_grid(master, "ICB_Cluster"))
  list(summary = summary, coverage = coverage, profiles = profiles, heatmaps = heatmaps)
}

# 6. DELIVERY PATTERNS WITHIN DEMOGRAPHIC GROUPS -----------------------
delivery_pattern_key <- function() {
  data.frame(Outcome = c("Same trust", "Different cluster trust", "Outside cluster",
    "No recorded delivery", "Unknown location", "Booking provider unknown"),
    Label = c("Same trust as booking", "Different trust within cluster",
      "Outside cluster or home birth", "No recorded delivery", "Delivery location unknown",
      "BBCS delivery, booking trust unknown"),
    Colour = unname(booking_delivery_palette()))
}

delivery_pattern_counts <- function(rows, field, categories = NULL) {
  if (is.null(categories)) categories <- cohort_profile_counts(rows, field)$Category
  data <- dplyr::bind_rows(lapply(delivery_pattern_key()$Outcome, function(outcome) {
    counts <- cohort_profile_counts(rows[rows$Delivery_pattern %in% outcome, ], field, categories)
    counts$Outcome <- outcome
    counts
  }))
  # Aggregate before disclosure control; normalise the protected cells only.
  data |> dplyr::group_by(Category) |>
    dplyr::mutate(Category_total = sum(N), Share = dc_divide(N, Category_total)) |>
    dplyr::ungroup() |> dplyr::select(Category, Label, Outcome, N, Category_total, Share)
}

delivery_pattern_chart <- function(data, field, code, group) {
  key <- delivery_pattern_key()
  labels <- unique(data$Label)
  ticks <- vapply(labels, function(x) paste(strwrap(x, 19), collapse = "<br>"), character(1))
  label_lines <- lengths(strsplit(ticks, "<br>", fixed = TRUE))
  chart <- plotly::plot_ly(height = max(285, sum(pmax(35, label_lines * 17 + 8)) + 65))
  for (i in seq_len(nrow(key))) {
    rows <- data[data$Outcome == key$Outcome[i], ]
    chart <- plotly::add_trace(chart, type = "bar", orientation = "h",
      x = rows$Share, y = rows$Label, name = key$Label[i],
      marker = list(color = key$Colour[i]),
      text = ifelse(rows$N > 0 & is.finite(rows$Share), scales::percent(rows$Share, accuracy = .1), ""),
      textposition = "inside", insidetextanchor = "middle", textangle = 0,
      textfont = list(size = 13, color = if (key$Outcome[i] %in% c("Outside cluster", "Unknown location")) "#111111" else "white"),
      constraintext = "inside",
      customdata = paste0(rows$Label, "<br>", key$Label[i], "<br>",
        ifelse(is.finite(rows$Share), scales::percent(rows$Share, accuracy = .1), "No pregnancies"),
        " within this demographic group<br>Pregnancies: ", scales::comma(rows$N)),
      hovertemplate = "%{customdata}<extra></extra>",
      meta = list(delivery_pattern_field = field, organisation = code, organisation_group = group))
  }
  chart |> plotly::layout(barmode = "stack", showlegend = FALSE,
    meta = list(delivery_pattern_field = field),
    uniformtext = list(minsize = 12, mode = "hide"),
    xaxis = list(title = "% within demographic group", range = c(0, 1), dtick = .25, tickformat = ".0%"),
    yaxis = list(title = "", categoryorder = "array", categoryarray = rev(labels),
      tickmode = "array", tickvals = labels, ticktext = ticks, automargin = FALSE),
    margin = list(l = if (field == "ethnicity_broad") 135 else if (field == "imd_quintile") 150 else 70,
      r = 12, t = 12, b = 55), font = list(family = "Arial", size = 14)) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE) |> dc_widget()
}

build_delivery_pattern_profiles <- function(master, include_other = FALSE) {
  # Booking providers define panels. Projection residence exclusions do not apply.
  master <- master[master$Booking != "No booking", , drop = FALSE]
  key <- delivery_pattern_key()
  fields <- demo_profile_fields()
  stats::setNames(lapply(c("Trust", "ICB_Cluster"), function(group) {
    codes <- demo_bbcs_org_codes(group, include_other = include_other)
    lapply(fields, function(field) {
      categories <- cohort_profile_counts(master, field)$Category
      selected <- stats::setNames(lapply(codes, function(code) cohort_profile_org_rows(master, "Booking", group, code)), codes)
      counts <- lapply(selected, delivery_pattern_counts, field = field, categories = categories)
      panels <- lapply(codes, function(code) {
        htmltools::div(class = "demo-sankey-panel",
          htmltools::h4(cohort_profile_org_label(code, group)),
          chart_total(sum(selected[[code]]$N), "pregnancies with a recorded booking"),
          delivery_pattern_chart(counts[[code]], field, code, group))
      })
      list(data = counts, grid = htmltools::tagList(
        report_colour_key(stats::setNames(key$Colour, key$Label), "delivery-pattern-key"),
        htmltools::div(class = "demo-sankey-grid delivery-pattern-grid", panels),
        htmltools::p(class = "cohort-overview-note",
          "Each non-empty demographic category totals 100%. Hover for percentages and controlled counts, including small segments. Empty categories have no bar.",
          if (field == "imd_quintile") " IMD quintile 1 (most deprived); quintile 5 (least deprived). Colour represents delivery outcome.")))
    })
  }), c("Trust", "ICB_Cluster"))
}

# 7. OBJECTS FOR RSTUDIO AND THE REPORT ---------------------------------
# Clean once per mother before the pregnancy join; keep raw inputs for inspection.
demographics_cleaned <- prepare_demographics(demographics)
`Demo Sankeys` <- build_demo_sankeys(final_cohort_2326, demographics_cleaned, report_episodes)
# Section 5 also includes a pooled Other trusts card.
demo_delivery_profiles <- build_delivery_pattern_profiles(get("Demo Sankeys"))
demo_sankey_charts <- list(
  version = "demographic-profile-v50",
  cohort_total = dc_count(sum(`Demo Sankeys`$N)),
  booking_totals = with(`Demo Sankeys`, dc_count(c(
    anywhere = sum(N[Booking != "No booking"]),
    bbcs_provider = sum(N[Booking_ICB %in% cluster_icbs])))),
  cohort_profile = build_cohort_profile(`Demo Sankeys`),
  cohort_org_comparisons = list(Trust = build_cohort_org_comparison(`Demo Sankeys`, "Trust"),
    ICB_Cluster = build_cohort_org_comparison(`Demo Sankeys`, "ICB_Cluster")),
  cohort_trust_profiles = build_cohort_trust_profiles(`Demo Sankeys`),
  cohort_icb_profiles = build_cohort_trust_profiles(`Demo Sankeys`, "ICB_Cluster"),
  cohort_match_profiles = build_cohort_match_profiles(`Demo Sankeys`),
  delivery_pattern_profiles = build_delivery_pattern_profiles(get("Demo Sankeys"), include_other = TRUE),
  overview_delivery = demo_overview_tabs(get("Demo Sankeys"), "Delivery", demo_delivery_profiles),
  overview_booking = demo_overview_tabs(`Demo Sankeys`, "Booking"),
  overall = htmltools::tagList(chart_total(sum(`Demo Sankeys`$N), "BBCS cohort pregnancies"),
    demo_sankey_chart(`Demo Sankeys`)),
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
