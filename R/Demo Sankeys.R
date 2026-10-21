# DEMOGRAPHIC SANKEYS ===================================================
# Run after Maternity analysis.R; Run analysis.R does this automatically.
# The master is an aggregate table: no mother or pregnancy IDs are retained.
source("R/demographics.R")
demo_period <- "2023/24\u20132025/26"

demo_provider_labels <- function() {
  extra <- setdiff(cluster_trusts, names(flow_trust_labels))
  c(flow_trust_labels, stats::setNames(extra, extra),
    `No booking` = "No recorded booking", `No delivery` = "No recorded delivery",
    Home = "Home birth")
}

demo_icb <- function(provider, stage) {
  icb <- unname(provider_icb[provider])
  icb[is.na(icb)] <- ifelse(grepl("^R[A-Z0-9]{2}$", provider[is.na(icb)]), "Other", "Unknown")
  special <- stage %in% c("No booking", "No delivery", "Home")
  icb[special] <- stage[special]
  icb
}

# 1. PREPARE THE MASTER TABLE -------------------------------------------
build_demo_sankeys <- function(cohort, demographics, episodes = prepare_report_cohort(cohort)) {
  lookup <- demographics |>
    dplyr::select(person_id_mother, ethnicity_broad, ethnicity_description, imd_decile) |>
    dplyr::distinct()

  # The supplied three-year cohort is the population. Count each pregnancy row
  # once, without another event-year filter or duplication across chart views.
  # Join on the original mother ID. Missing IDs never match each other.
  # many-to-one prevents conflicting duplicate mother records multiplying flows.
  records <- episodes |>
    dplyr::mutate(
      person_id_mother = demographic_mother_id(cohort$person_id_mother[Row]),
      Has_booking = Booking_provider_supplied | Booking_date_supplied,
      Pathway = dplyr::case_when(
        Birth_recorded & Has_booking ~ "Delivery with recorded booking",
        Birth_recorded ~ "Delivery without recorded booking",
        Has_booking ~ "Booking without recorded delivery",
        TRUE ~ "Neither event recorded"),
      Booking = ifelse(Has_booking, group_flow_code(Booking_Trust,
        union(setdiff(names(trust_labels), "Other"), cluster_trusts)), "No booking"),
      Delivery = dplyr::case_when(
        !Birth_recorded ~ "No delivery", Home %in% 1 ~ "Home",
        TRUE ~ group_flow_code(Trust, union(setdiff(names(trust_labels), "Other"), cluster_trusts))),
      Booking_ICB = demo_icb(Booking_Trust, Booking),
      Delivery_ICB = demo_icb(Trust, Delivery),
      Outcome = classify_pathway_outcome(data.frame(birth_recorded = Birth_recorded,
        home_birth = Home, birth_code = Trust))) |>
    dplyr::left_join(lookup, by = "person_id_mother", na_matches = "never",
      relationship = "many-to-one")
  records |>
    dplyr::count(Booking, Delivery, Booking_ICB, Delivery_ICB, Pathway, Outcome,
      ethnicity_broad, ethnicity_description, imd_decile, Age_band,
      name = "N", .drop = FALSE)
}

# 2. FORM CHARTS FROM THE MASTER ONLY -----------------------------------
demo_sankey_chart <- function(master, compact = FALSE) {
  if (!nrow(master)) return(htmltools::div("No supplied cohort pregnancies."))
  links <- master |>
    dplyr::group_by(source = Booking, target = Delivery) |>
    dplyr::summarise(n = sum(N), .groups = "drop")
  source_order <- unique(c(trust_source_order, names(demo_provider_labels())))
  target_order <- unique(c(trust_target_order, names(demo_provider_labels())))
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
  chart$x$layout$height <- 430
  chart$x$layout$font$size <- 10
  chart$height <- 430
  chart
}

demo_category_values <- function(master, field) {
  values <- unique(as.character(master[[field]]))
  preferred <- switch(field,
    imd_decile = as.character(1:10),
    ethnicity_broad = unname(demographic_ethnicity_groups),
    Age_band = report_age_bands,
    character())
  c(preferred[preferred %in% values],
    sort(setdiff(values[!is.na(values)], preferred)),
    if (anyNA(values)) NA_character_)
}

demo_category_label <- function(value, field) {
  label <- if (is.na(value)) "Unknown (NA)" else if (value == "") "Blank" else value
  if (field == "imd_decile" && !is.na(value) && nzchar(value))
    label <- paste("IMD decile", label)
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
  plotly::plot_ly(type = "sankey", arrangement = "snap", orientation = "h",
    meta = metadata,
    height = max(400, 55 * length(categories)),
    node = list(label = node_labels, pad = 22, thickness = 18,
      color = c(rep("#5D7790", length(categories)), outcome_colours),
      x = c(rep(0.01, length(categories)), rep(0.99, 4)),
      y = c(seq(0.02, 0.98, length.out = length(categories)), seq(0.08, 0.92, length.out = 4)),
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
    imd = versions("imd_decile"),
    age = versions("Age_band"))
}

# 4. POOLED DEMOGRAPHIC BAR CHARTS -------------------------------------
demo_pathway_colours <- c(
  "Delivery with recorded booking" = "#00856A",
  "Delivery without recorded booking" = "#005EB8",
  "Booking without recorded delivery" = "#D55E00",
  "Neither event recorded" = "#727272")

demo_overview_bar <- function(rows, field) {
  if (!nrow(rows)) return(htmltools::div("No pregnancies in this group."))
  categories <- demo_category_values(rows, field)
  labels <- vapply(categories, demo_category_label, character(1), field = field)
  counts <- rows |>
    dplyr::group_by(Category = as.character(.data[[field]]), Pathway) |>
    dplyr::summarise(N = sum(N), .groups = "drop") |>
    dplyr::mutate(Category = factor(labels[match(Category, categories)], levels = labels),
      Pathway = factor(Pathway, levels = names(demo_pathway_colours)),
      N = dc_count(N),
      Hover = paste0(Category, "<br>", Pathway, "<br>Pregnancies: ", scales::comma(N)))
  plotly::plot_ly(counts, x = ~N, y = ~Category, color = ~Pathway,
    colors = demo_pathway_colours, type = "bar", orientation = "h",
    text = ~Hover, hovertemplate = "%{text}<extra></extra>",
    height = max(320, 32 * length(categories) + 70)) |>
    plotly::layout(barmode = "stack", showlegend = FALSE,
      xaxis = list(title = "Pregnancies", rangemode = "tozero"),
      yaxis = list(title = "", categoryorder = "array", categoryarray = rev(labels),
        tickmode = "array", tickvals = labels,
        ticktext = vapply(labels, function(label) paste(strwrap(label, width = 24), collapse = "<br>"), character(1)),
        automargin = TRUE),
      margin = list(l = 100, r = 12, t = 12, b = 50),
      font = list(family = "Arial", size = 11)) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

demo_overview_grid <- function(master, mode, group, field) {
  codes <- demo_org_codes(master, mode, group)
  legend <- htmltools::div(style = "display:flex; flex-wrap:wrap; gap:12px; margin-bottom:16px;",
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
  stats::setNames(lapply(c("Trust", "ICB_Cluster"), function(group) {
    fields <- c(age = "Age_band", imd = "imd_decile", ethnicity = "ethnicity_broad",
      ethnicity_description = "ethnicity_description")
    lapply(fields, function(field) demo_overview_grid(master, mode, group, field))
  }), c("Trust", "ICB_Cluster"))
}

# 5. OBJECTS FOR RSTUDIO AND THE REPORT ---------------------------------
# Clean once per mother before the pregnancy join; keep raw inputs for inspection.
demographics_cleaned <- prepare_demographics(demographics)
`Demo Sankeys` <- build_demo_sankeys(final_cohort_2326, demographics_cleaned, report_episodes)
demo_sankey_charts <- list(
  version = "demographic-outcomes-v2",
  cohort_total = dc_count(sum(`Demo Sankeys`$N)),
  overview_delivery = demo_overview_tabs(`Demo Sankeys`, "Delivery"),
  overview_booking = demo_overview_tabs(`Demo Sankeys`, "Booking"),
  overall = demo_sankey_chart(`Demo Sankeys`),
  ethnicity = demo_sankey_grid(`Demo Sankeys`, "ethnicity_broad"),
  imd = demo_sankey_grid(`Demo Sankeys`, "imd_decile"),
  age = demo_sankey_grid(`Demo Sankeys`, "Age_band"),
  booking_icb = demo_pathway_tabs(`Demo Sankeys`, "ICB_Cluster"),
  booking_trust = demo_pathway_tabs(`Demo Sankeys`, "Trust"))
save_report_rds(demo_sankey_charts, ".demo_sankeys.rds")
