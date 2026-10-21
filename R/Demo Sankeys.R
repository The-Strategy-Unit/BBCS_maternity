# DEMOGRAPHIC SANKEYS ===================================================
# Run after Maternity analysis.R; Run analysis.R does this automatically.
# The master is an aggregate table: no mother or pregnancy IDs are retained.

# 1. PREPARE THE MASTER TABLE -------------------------------------------
build_demo_sankeys <- function(cohort, demographics, episodes = prepare_report_cohort(cohort)) {
  lookup <- demographics |>
    dplyr::select(person_id_mother, ethnicity_broad, ethnicity_description, imd_decile) |>
    dplyr::distinct()

  # Keep the two event populations separate: delivery FY for 4.1, booking FY
  # for 4.2. A pregnancy can belong to both views; never sum across View.
  # Join on the original mother ID. Missing IDs never match each other.
  # many-to-one prevents conflicting duplicate mother records multiplying flows.
  records <- episodes |>
    dplyr::filter((Delivery_year %in% 2025L & Birth_recorded & Home %in% 0) |
      (Booking_year %in% 2025L & Booking_Trust %in% cluster_trusts)) |>
    dplyr::mutate(
      person_id_mother = cohort$person_id_mother[Row],
      Outcome = classify_pathway_outcome(data.frame(birth_recorded = Birth_recorded,
        home_birth = Home, birth_code = Trust))) |>
    dplyr::left_join(lookup, by = "person_id_mother", na_matches = "never",
      relationship = "many-to-one")
  dplyr::bind_rows(
    records |>
      dplyr::filter(Delivery_year == 2025L, Birth_recorded, Home == 0) |>
      dplyr::transmute(View = "Delivery flows", FY = Year,
        Booking = group_flow_code(Booking_Trust, setdiff(names(trust_labels), "Other")),
        Delivery = group_flow_code(Trust, setdiff(names(trust_labels), "Other")),
        Outcome = NA_character_, ethnicity_broad, ethnicity_description, imd_decile, Age_band),
    records |>
      dplyr::filter(Booking_year == 2025L, Booking_Trust %in% cluster_trusts) |>
      dplyr::transmute(View = "Booking outcomes", FY = "2025/26",
        Booking = Booking_Trust, Delivery = NA_character_, Outcome,
        ethnicity_broad, ethnicity_description, imd_decile, Age_band)) |>
    dplyr::count(View, FY, Booking, Delivery, Outcome, ethnicity_broad, ethnicity_description, imd_decile, Age_band,
      name = "N", .drop = FALSE)
}

# 2. FORM CHARTS FROM THE MASTER ONLY -----------------------------------
demo_sankey_chart <- function(master, compact = FALSE) {
  master <- dplyr::filter(master, View == "Delivery flows")
  links <- master |>
    dplyr::group_by(source = Booking, target = Delivery) |>
    dplyr::summarise(n = sum(N), .groups = "drop")
  chart <- make_sankey(list(links = links), flow_trust_labels,
    " (booking)", " (delivery)", trust_source_order, trust_target_order)
  if (!compact || !nrow(links)) return(chart)

  # Short node labels fit three panels across. Hover retains the full names.
  short <- c(RNA = "Dudley", RL4 = "Wolverhampton", RQ3 = "Birmingham W&C",
    RXK = "Sandwell & W Birmingham", RBK = "Walsall", RRK = "UHB",
    Other = "Other trusts", Unknown = "Unknown provider")
  nodes <- c(trust_source_order[trust_source_order %in% links$source],
    trust_target_order[trust_target_order %in% links$target])
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
  master <- dplyr::filter(master, View == "Delivery flows")
  values <- demo_category_values(master, field)
  if (!length(values)) return(htmltools::div("No recorded non-home deliveries in 2025/26."))
  panels <- lapply(values, function(value) {
    selected <- if (is.na(value)) is.na(master[[field]]) else
      !is.na(master[[field]]) & as.character(master[[field]]) == value
    rows <- master[selected, , drop = FALSE]
    label <- demo_category_label(value, field)
    htmltools::div(class = "demo-sankey-panel",
      htmltools::tags$h4(label),
      htmltools::tags$p(paste0(scales::comma(dc_count(sum(rows$N))), " deliveries")),
      htmltools::tags$p(class = "demo-sankey-stages", "Booking trust \u2192 Delivery trust"),
      demo_sankey_chart(rows, compact = TRUE))
  })
  htmltools::div(class = "demo-sankey-grid", panels)
}

# 3. ANTENATAL BOOKING OUTCOMES (SECTION 2.2.2 e/f) ----------------------
demo_pathway_summary <- function(master, group, codes = NULL) {
  master <- dplyr::filter(master, View == "Booking outcomes")
  if (is.null(codes)) codes <- if (group == "Trust")
    unique(c(setdiff(names(trust_labels), "Other"), master$Booking)) else
    c(cluster_icbs, "Cluster")
  dplyr::bind_rows(lapply(codes, function(code) {
    keep <- if (group == "Trust") master$Booking == code else
      if (code == "Cluster") rep(TRUE, nrow(master)) else
        unname(provider_icb[master$Booking]) %in% code
    rows <- master[keep, , drop = FALSE]
    organisation <- if (group == "Trust") unname(trust_labels[code]) else
      if (code == "Cluster") cluster_label else unname(icb_labels[code])
    if (is.na(organisation)) organisation <- code
    data.frame(Code = code, Organisation = organisation, Starts = sum(rows$N),
      Within = sum(rows$N[rows$Outcome == "Within cluster"]),
      Outside = sum(rows$N[rows$Outcome == "Outside cluster"]),
      No_delivery = sum(rows$N[rows$Outcome == "No recorded delivery"]),
      Unknown = sum(rows$N[rows$Outcome == "Unknown location"]))
  }))
}

demo_pathway_grid <- function(master, group, field = NULL) {
  master <- dplyr::filter(master, View == "Booking outcomes")
  organisations <- demo_pathway_summary(master, group)$Code
  panels <- function(rows) {
    summary <- demo_pathway_summary(rows, group, organisations)
    htmltools::div(class = "demo-sankey-grid",
      lapply(seq_len(nrow(summary)), function(i) {
        htmltools::div(class = "demo-sankey-panel",
          htmltools::tags$h5(summary$Organisation[i]),
          make_pathway_sankey(summary[i, , drop = FALSE]))
      }))
  }
  if (is.null(field)) return(htmltools::tagList(pathway_legend, panels(master)))
  categories <- demo_category_values(master, field)
  if (!length(categories)) return(htmltools::div("No cluster antenatal bookings in 2025/26."))
  htmltools::tagList(pathway_legend, lapply(categories, function(value) {
    selected <- if (is.na(value)) is.na(master[[field]]) else
      !is.na(master[[field]]) & as.character(master[[field]]) == value
    htmltools::div(class = "demo-pathway-category",
      htmltools::tags$h4(demo_category_label(value, field)),
      panels(master[selected, , drop = FALSE]))
  }))
}

demo_pathway_tabs <- function(master, group) {
  list(overall = demo_pathway_grid(master, group),
    ethnicity = demo_pathway_grid(master, group, "ethnicity_broad"),
    ethnicity_description = demo_pathway_grid(master, group, "ethnicity_description"),
    imd = demo_pathway_grid(master, group, "imd_decile"),
    age = demo_pathway_grid(master, group, "Age_band"))
}

# 4. OBJECTS FOR RSTUDIO AND THE REPORT ---------------------------------
# Keep supplied demographic categories, including pipe-separated values.
# NA is labelled only for display; no other ethnicity/IMD fields are substituted.
`Demo Sankeys` <- build_demo_sankeys(final_cohort_2326, demographics, report_episodes)
demo_sankey_charts <- list(
  overall = demo_sankey_chart(`Demo Sankeys`),
  ethnicity = demo_sankey_grid(`Demo Sankeys`, "ethnicity_broad"),
  imd = demo_sankey_grid(`Demo Sankeys`, "imd_decile"),
  age = demo_sankey_grid(`Demo Sankeys`, "Age_band"),
  booking_icb = demo_pathway_tabs(`Demo Sankeys`, "ICB_Cluster"),
  booking_trust = demo_pathway_tabs(`Demo Sankeys`, "Trust"))
save_report_rds(demo_sankey_charts, ".demo_sankeys.rds")
