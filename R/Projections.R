# Section 9: First draft projections.
# Run after additional_analysis.R. The QMD displays the saved charts and tables.
# Run R/QA.R separately when you want to check the data and totals.

# Choices used by this section.
projection_baseline <- "2025/26"
projection_reference_year <- 2026L
projection_end_year <- NULL # NULL uses all years in the ONS file.
projection_history_years <- c("2023/24", "2024/25", "2025/26")
projection_age_bands <- c("<20", "20-24", "25-29", "30-34", "35-39", "40+")
projection_age_colours <- c("#005EB8", "#00856A", "#D55E00", "#8E5EA2", "#B07300", "#727272")
projection_minimum_age <- 10
projection_maximum_age <- 60
projection_other_feeders <- 10L
projection_chart_height <- 480
projection_history_chart_height <- 300
projection_history_colours <- c("MSDS observed" = "#005EB8", "ONS-based model" = "#D55E00")

# Remove the previous output first, so a failed run cannot leave old report charts.
unlink(".projections.rds")

# Check the inputs are available before starting. Counting checks belong in QA.R.
required_objects <- c("final_cohort_2326", "total_births_by_LA_2326",
  "trust_labels", "icb_labels", "provider_icb", "cluster_icbs", "cluster_trusts")
absent <- required_objects[!vapply(required_objects,
  function(name) exists(name, inherits = TRUE), logical(1))]
if (length(absent)) stop("Missing objects: ", paste(absent, collapse = ", "),
  ". Run R/Run analysis.R first.", call. = FALSE)
required_columns <- list(
  final_cohort_2326 = c("person_id_mother", "uniq_preg_id", "birth_yr", "birth_flag",
    "la_of_mother", "age_at_birth_mother", "birth_provider", "home_birth"),
  total_births_by_LA_2326 = c("birth_yr", "la_of_mother", "age_at_birth_mother", "count"))
for (name in names(required_columns)) {
  missing_columns <- setdiff(required_columns[[name]], names(get(name)))
  if (length(missing_columns)) stop(name, " is missing: ",
    paste(missing_columns, collapse = ", "), call. = FALSE)
}
source("R/ons_data.R")

# Local lookup files travel with the project; codes match the data and names label the report.
projection_la_lookup <- readr::read_csv("data/lookups/la_names.csv",
  col_types = readr::cols(LA = readr::col_character(), LA_name = readr::col_character()))
projection_trust_lookup <- readr::read_csv("data/lookups/trust_names.csv",
  col_types = readr::cols(Trust = readr::col_character(), Trust_name = readr::col_character())) |>
  dplyr::mutate(Trust_name = stringr::str_replace_all(stringr::str_to_title(Trust_name),
                                                    "\\bNhs\\b", "NHS"))
projection_catchment_las <- readr::read_csv("config/local_authorities.csv",
  show_col_types = FALSE)$area_code

build_first_draft_projections <- function(baseline_fy = projection_baseline,
  ons_reference_year = projection_reference_year, projection_end = projection_end_year,
  prepare_outputs = TRUE) {
  age_bands <- projection_age_bands
  age_colours <- projection_age_colours
  band_age <- function(age) {
    dplyr::case_when(
      is.na(age) | age < projection_minimum_age | age > projection_maximum_age ~ "Unknown",
      age < 20 ~ "<20", age < 25 ~ "20-24", age < 30 ~ "25-29",
      age < 35 ~ "30-34", age < 40 ~ "35-39", TRUE ~ "40+")
  }
  clean_code <- function(x) toupper(trimws(as.character(x)))
  relative_change <- function(value, reference) {
    reference <- rep_len(reference, length(value))
    result <- rep(NA_real_, length(value))
    valid <- is.finite(value) & is.finite(reference) & reference > 0
    result[valid] <- value[valid] / reference[valid] - 1
    result
  }
  base_table <- function(data, title, subtitle = NULL, rowname = NULL) {
    gt::gt(data, rowname_col = rowname) |>
      gt::tab_header(title = title, subtitle = subtitle) |>
      gt::sub_missing(missing_text = "n.a.") |>
      gt::tab_options(table.font.names = c("Open Sans", "Arial", "sans-serif"),
                      table.font.size = gt::px(12), container.width = "100%",
                      container.overflow.x = TRUE)
  }
  # Count actual deliveries. Keep one set of identical details per pregnancy.
  birth_records <- final_cohort_2326 |>
    dplyr::filter(trimws(as.character(birth_yr)) == baseline_fy,
                  as.character(birth_flag) == "1") |>
    dplyr::transmute(Person = trimws(as.character(person_id_mother)),
      Pregnancy = trimws(as.character(uniq_preg_id)), LA = clean_code(la_of_mother),
      Age = as.numeric(age_at_birth_mother), Trust = clean_code(birth_provider),
      Home = as.numeric(as.character(home_birth)))
  episodes <- dplyr::distinct(birth_records)
  episodes <- episodes |>
    dplyr::mutate(Age_band = band_age(Age))
  # Location classification: a flagged home birth is never a delivery at a trust.
  target_episodes <- episodes |>
    dplyr::filter(Trust %in% cluster_trusts, Home == 0)
  unusable <- is.na(target_episodes$LA) | target_episodes$LA == "" | target_episodes$LA == "L99999999" |
    target_episodes$Age_band == "Unknown"
  usable_targets <- target_episodes[!unusable, , drop = FALSE]

  # Fix the organisation list across both the forward projection and retrospective check.
  recorded_trusts <- final_cohort_2326 |>
    dplyr::filter(trimws(as.character(birth_yr)) %in% projection_history_years,
                  as.character(birth_flag) == "1", as.character(home_birth) == "0") |>
    dplyr::pull(birth_provider) |>
    clean_code()
  trust_codes <- unique(c(names(trust_labels)[names(trust_labels) != "Other"],
                          sort(intersect(recorded_trusts, cluster_trusts))))
  trust_names <- projection_trust_lookup$Trust_name[match(trust_codes, projection_trust_lookup$Trust)]
  named_trusts <- trust_codes %in% names(trust_labels)
  trust_names[named_trusts] <- unname(trust_labels[trust_codes[named_trusts]])
  trust_names[is.na(trust_names)] <- paste0("Trust ", trust_codes[is.na(trust_names)])
  organisations <- dplyr::bind_rows(
    data.frame(Key = paste0("trust-", tolower(trust_codes)), Level = "Trust",
               Code = trust_codes, Organisation = trust_names),
    data.frame(Key = paste0("icb-", tolower(cluster_icbs)), Level = "ICB",
               Code = cluster_icbs, Organisation = unname(icb_labels[cluster_icbs])),
    data.frame(Key = "cluster", Level = "Cluster", Code = "Cluster", Organisation = "Combined cluster"))

  # Work out feeder shares: deliveries here divided by all pregnancies in that LA/age band.
  raw_totals <- total_births_by_LA_2326 |>
    dplyr::filter(trimws(as.character(birth_yr)) == baseline_fy) |>
    dplyr::transmute(LA = clean_code(la_of_mother), Age = as.numeric(age_at_birth_mother),
                     Denominator = as.numeric(count))
  resident_denominators <- raw_totals |>
    dplyr::mutate(Age_band = band_age(Age)) |>
    dplyr::group_by(LA, Age_band) |>
    dplyr::summarise(Denominator = sum(Denominator), .groups = "drop")

  feeder_las <- sort(unique(usable_targets$LA))
  # ONS already uses 2023 LA geography. Only the later Barnsley/Sheffield codes need a proxy.
  ons_births <- read_snpp_births()
  geography_changes <- dplyr::tribble(
    ~LA, ~LA_name, ~ONS_source_codes, ~Treatment,
    "E08000038", "Barnsley", "E08000016", "Previous boundary growth proxy",
    "E08000039", "Sheffield", "E08000019", "Previous boundary growth proxy") |>
    dplyr::filter(LA %in% feeder_las, !LA %in% ons_births$area_code)
  # These previous-area growth rates are applied to the actual current-area baseline.
  # https://www.ons.gov.uk/explore-local-statistics/areas/E08000038-barnsley
  # https://www.ons.gov.uk/explore-local-statistics/areas/E08000039-sheffield
  proxy_births <- dplyr::bind_rows(lapply(seq_len(nrow(geography_changes)), function(i) {
    ons_births |>
      dplyr::filter(area_code == geography_changes$ONS_source_codes[i]) |>
      dplyr::mutate(area_code = geography_changes$LA[i], area_name = geography_changes$LA_name[i])
  }))
  ons_births <- dplyr::bind_rows(ons_births, proxy_births)
  ons_la_names <- ons_births |>
    dplyr::distinct(area_code, area_name) |>
    dplyr::rename(LA = area_code, LA_name = area_name)
  la_names <- projection_la_lookup
  feeder_names <- data.frame(LA = feeder_las) |>
    dplyr::left_join(la_names, by = "LA") |>
    dplyr::mutate(LA_name = dplyr::coalesce(LA_name, LA))

  select_org <- function(data, organisation) {
    if (organisation$Level[[1]] == "Trust") {
      data[data$Trust == organisation$Code[[1]], , drop = FALSE]
    } else if (organisation$Level[[1]] == "ICB") {
      data[unname(provider_icb[data$Trust]) == organisation$Code[[1]], , drop = FALSE]
    } else data
  }
  observed_totals <- dplyr::bind_rows(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, , drop = FALSE]
    data.frame(Key = org$Key, Observed_baseline = nrow(select_org(target_episodes, org)))
  }))
  # Keep unmatched deliveries in the forecast and describe their treatment in the report.
  target_status <- target_episodes |>
    dplyr::mutate(LA_usable = !is.na(LA) & LA != "" & LA != "L99999999",
      Age_usable = Age_band != "Unknown", ONS_covered = LA %in% ons_la_names$LA,
      Reason = dplyr::case_when(
        !LA_usable & !Age_usable ~ "Missing/unusable LA and age",
        !LA_usable ~ "Missing/unusable resident LA",
        !Age_usable ~ "Missing/unusable maternal age",
        !ONS_covered ~ "No England ONS series for resident LA", TRUE ~ ""))
  coverage <- dplyr::bind_rows(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, , drop = FALSE]
    data <- select_org(target_status, org)
    data.frame(Key = org$Key, Level = org$Level, Organisation = org$Organisation,
      Observed_baseline = nrow(data), ONS_growth = sum(data$Reason == ""),
      Unmatched = sum(data$Reason != ""))
  }))
  coverage_details <- dplyr::bind_rows(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, , drop = FALSE]
    select_org(target_status, org) |>
      dplyr::filter(Reason != "") |>
      dplyr::mutate(LA = dplyr::if_else(is.na(LA) | LA == "", "Unknown LA", LA)) |>
      dplyr::count(LA, Age_band, Reason, name = "N") |>
      dplyr::mutate(Key = org$Key, Level = org$Level, Organisation = org$Organisation) |>
      dplyr::left_join(la_names, by = "LA") |>
      dplyr::mutate(LA_name = dplyr::if_else(LA %in% c("Unknown LA", "L99999999"),
        "Unknown LA", dplyr::coalesce(LA_name, LA)))
  }))
  feeder_shares <- dplyr::bind_rows(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, , drop = FALSE]
    counts <- select_org(usable_targets, org) |>
      dplyr::count(LA, Age_band, name = "Numerator")
    # Retain all age bands for each contributing LA, including zero flows.
    org_las <- unique(counts$LA)
    tidyr::expand_grid(LA = org_las, Age_band = age_bands) |>
      dplyr::left_join(counts, by = c("LA", "Age_band")) |>
      dplyr::mutate(Numerator = dplyr::coalesce(Numerator, 0L)) |>
      dplyr::left_join(resident_denominators, by = c("LA", "Age_band")) |>
      dplyr::mutate(Key = org$Key, Level = org$Level, Organisation = org$Organisation,
        Share = dplyr::if_else(Denominator > 0, Numerator / Denominator, NA_real_)) |>
      dplyr::left_join(feeder_names, by = "LA")
  }))

  # Keep home births and other destinations in the LA denominator.
  destination_accounting <- resident_denominators |>
    dplyr::filter(LA %in% feeder_las, Age_band %in% age_bands) |>
    dplyr::left_join(feeder_shares |>
      dplyr::filter(Level == "Cluster") |>
      dplyr::select(LA, Age_band, Cluster_trusts = Numerator), by = c("LA", "Age_band")) |>
    dplyr::left_join(episodes |>
      dplyr::filter(LA %in% feeder_las, Home == 1, Age_band %in% age_bands) |>
      dplyr::count(LA, Age_band, name = "Recorded_home_births"), by = c("LA", "Age_band")) |>
    dplyr::mutate(Cluster_trusts = dplyr::coalesce(Cluster_trusts, 0L),
      Recorded_home_births = dplyr::coalesce(Recorded_home_births, 0L),
      Other_or_unrepresented = Denominator - Cluster_trusts - Recorded_home_births) |>
    dplyr::left_join(feeder_names, by = "LA")

  # Apply Method 1 ONS growth. Convert births to maternities before taking growth ratios.
  # Combine 40-44 and 45+ before calculating the modifier for the 40+ band.
  modifiers <- read_national_multiplicity() |>
    dplyr::mutate(age_band = ifelse(as.character(age_band) %in% c("40-44", "45+"),
                                   "40+", as.character(age_band))) |>
    births_to_maternities_modifiers()
  covered_las <- intersect(feeder_las, ons_la_names$LA)
  ons_maternity_projections <- project_maternities(dplyr::filter(ons_births, area_code %in% covered_las), modifiers,
    local_authorities = unique(ons_la_names$LA_name[ons_la_names$LA %in% covered_las])) |>
    dplyr::transmute(LA = area_code, Age_band = as.character(age_band), Year = year,
                     ONS_maternities = projected_maternities) |>
    dplyr::filter(Year >= ons_reference_year)
  if (!is.null(projection_end)) ons_maternity_projections <- dplyr::filter(ons_maternity_projections, Year <= projection_end)
  projection_years <- sort(unique(ons_births$year[ons_births$year >= ons_reference_year]))
  if (!is.null(projection_end)) projection_years <- projection_years[projection_years <= projection_end]
  if (!length(projection_years) || min(projection_years) != ons_reference_year) stop(
    "ONS maternity series lacks the ", ons_reference_year, " reference year.", call. = FALSE)
  ons_maternity_projections <- ons_maternity_projections |>
    dplyr::group_by(LA, Age_band) |>
    dplyr::mutate(ONS_reference = ONS_maternities[Year == ons_reference_year],
      Growth_factor = ifelse(ONS_reference > 0, ONS_maternities / ONS_reference, NA_real_),
      ONS_change = Growth_factor - 1) |>
    dplyr::ungroup()

  # Apply that growth to the actual baseline flow from each LA and age band.
  # Rows without usable LA/age keep their actual counts and have no allocation share.
  unallocated_deliveries <- coverage_details |>
    dplyr::filter(Reason != "No England ONS series for resident LA") |>
    dplyr::transmute(LA, LA_name, Age_band, Key, Level, Organisation, Numerator = N,
                     Denominator = NA_real_, Share = NA_real_)
  baseline_feeder_counts <- dplyr::bind_rows(feeder_shares, unallocated_deliveries) |>
    dplyr::mutate(Held_constant = !LA %in% covered_las | Age_band == "Unknown",
      Treatment = ifelse(Held_constant, "Baseline count held constant", "Method 1 ONS growth"))
  feeder_projections <- tidyr::crossing(baseline_feeder_counts, Year = projection_years) |>
    dplyr::left_join(ons_maternity_projections, by = c("LA", "Age_band", "Year"), relationship = "many-to-one") |>
    dplyr::mutate(Growth_factor = ifelse(Held_constant, 1, Growth_factor),
      ONS_change = ifelse(Held_constant, NA_real_, ONS_change),
      Projected = ifelse(Year == ons_reference_year, Numerator,
        ifelse(Numerator == 0, 0, Numerator * Growth_factor)),
      Year_label = ifelse(Year == ons_reference_year, baseline_fy, paste0("Mid-", Year)))

  # Add trust, ICB and combined-cluster totals across all feeder LAs and ages.
  age_projections <- feeder_projections |>
    dplyr::group_by(Key, Age_band, Year, Year_label) |>
    dplyr::summarise(Projected = sum(Projected), .groups = "drop")
  maternity_projections <- tidyr::expand_grid(Key = organisations$Key, Year = projection_years) |>
    dplyr::left_join(age_projections |>
      dplyr::group_by(Key, Year) |>
      dplyr::summarise(Projected = sum(Projected), .groups = "drop"), by = c("Key", "Year")) |>
    dplyr::mutate(Projected = ifelse(!Key %in% feeder_projections$Key, 0, Projected),
      Year_label = ifelse(Year == ons_reference_year, baseline_fy, paste0("Mid-", Year))) |>
    dplyr::left_join(observed_totals, by = "Key") |>
    dplyr::mutate(Change = Projected - Observed_baseline,
      Percent_change = relative_change(Projected, Observed_baseline)) |>
    dplyr::left_join(organisations |> dplyr::select(Key, Level, Organisation), by = "Key")
  projection_note <- paste(c(
    if (any(target_status$Reason != "")) paste(sum(target_status$Reason != ""),
      "baseline deliveries have no matched LA/age ONS series and remain constant; see projection coverage."),
    if (nrow(geography_changes))
      "Barnsley/Sheffield use previous-boundary ONS growth as a proxy."), collapse = " ")
  result <- list(organisations = organisations, feeder_shares = feeder_shares,
    observed_totals = observed_totals, resident_denominators = resident_denominators, destination_accounting = destination_accounting,
    baseline_fy = baseline_fy, ons_reference_year = ons_reference_year,
    projection_years = projection_years, age_bands = age_bands,
    coverage = coverage, coverage_details = coverage_details, geography = geography_changes,
    modifiers = modifiers, ons_maternity_projections = ons_maternity_projections, feeder_projections = feeder_projections,
    age_projections = age_projections, maternity_projections = maternity_projections, note = projection_note)
  if (!prepare_outputs) return(result)

  # Make charts and tables for sections 9.1 to 9.4.
  coverage_table <- coverage |>
    dplyr::select(Level, Organisation, Observed_baseline, ONS_growth, Unmatched) |>
    gt::gt(groupname_col = "Level", rowname_col = "Organisation") |>
    gt::tab_header(title = paste(baseline_fy, "projection coverage")) |>
    gt::cols_label(Observed_baseline = "All observed deliveries", ONS_growth = "Matched LA/age ONS growth",
                   Unmatched = "No matched LA/age growth") |>
    gt::fmt_number(columns = c("Observed_baseline", "ONS_growth", "Unmatched"), decimals = 0) |>
    gt::tab_options(container.width = "100%", container.overflow.x = TRUE)
  coverage_details_table <- coverage_details |>
    dplyr::select(Level, Organisation, LA_name, LA, Age_band, Reason, N) |>
    base_table(paste(baseline_fy, "records without matched ONS growth")) |>
    gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age_band = "Age band",
                   Reason = "Reason", N = "Observed deliveries") |>
    gt::fmt_number(columns = "N", decimals = 0)
  geography_table <- geography_changes |>
    base_table("Feeder codes matched to the ONS geography") |>
    gt::cols_label(LA = "MSDS LA code", LA_name = "Resident LA", Treatment = "ONS treatment",
                   ONS_source_codes = "ONS source area codes")
  # Section 9.1: keep cluster LAs and the ten largest additional feeders visible.
  # Rank once using actual baseline deliveries; group the remaining feeders only in the chart.
  # Tables keep every LA code. Unknown LA/age counts stay visible without an invented share.
  mapping_outputs <- stats::setNames(lapply(organisations$Key, function(key) {
    org_shares <- baseline_feeder_counts[baseline_feeder_counts$Key == key, , drop = FALSE]
    if (!nrow(org_shares)) return(list(chart = htmltools::div(paste("No recorded deliveries in", baseline_fy)),
      table = base_table(data.frame(Note = paste("No recorded deliveries in", baseline_fy)), "LA allocation")))
    la_volume <- org_shares |>
      dplyr::filter(!is.na(LA), LA != "", !LA %in% c("Unknown LA", "L99999999")) |>
      dplyr::group_by(LA, LA_name) |>
      dplyr::summarise(N = sum(Numerator), .groups = "drop") |>
      dplyr::filter(N > 0) |>
      dplyr::arrange(dplyr::desc(N), LA_name, LA)
    additional_las <- la_volume |>
      dplyr::filter(!LA %in% projection_catchment_las) |>
      dplyr::slice_head(n = projection_other_feeders) |>
      dplyr::pull(LA)
    visible_las <- la_volume$LA[la_volume$LA %in% c(projection_catchment_las, additional_las)]
    chart_bands <- c(age_bands, if (any(org_shares$Age_band == "Unknown" & org_shares$Numerator > 0)) "Unknown")
    chart_age_colours <- c(age_colours, if ("Unknown" %in% chart_bands) "#222222")
    org_shares <- org_shares |>
      dplyr::mutate(Age_band = factor(Age_band, levels = c(age_bands, "Unknown"))) |>
      dplyr::arrange(match(LA, c(la_volume$LA, "Unknown LA", "L99999999")), Age_band)
    flow <- org_shares |>
      dplyr::filter(Age_band %in% chart_bands) |>
      dplyr::mutate(Source_LA = dplyr::case_when(
        is.na(LA) | LA == "" | LA %in% c("Unknown LA", "L99999999") ~ "unknown-la",
        LA %in% visible_las ~ LA, TRUE ~ "other-feeder-las")) |>
      dplyr::group_by(Source_LA, Age_band) |>
      dplyr::summarise(N = sum(Numerator),
        Denominator = if (all(is.finite(Denominator) & Denominator >= 0)) sum(Denominator) else NA_real_,
        .groups = "drop") |>
      dplyr::filter(N > 0) |>
      dplyr::mutate(Share = ifelse(Denominator > 0, N / Denominator, NA_real_))
    source_order <- c(visible_las,
      if ("other-feeder-las" %in% flow$Source_LA) "other-feeder-las",
      if ("unknown-la" %in% flow$Source_LA) "unknown-la")
    source_labels <- c(la_volume$LA_name[match(visible_las, la_volume$LA)],
      if ("other-feeder-las" %in% source_order) "All other feeder LAs",
      if ("unknown-la" %in% source_order) "Unknown LA")
    flow <- dplyr::arrange(flow, match(Source_LA, source_order), Age_band)
    if (!nrow(flow)) {
      chart <- htmltools::div(paste("No usable LA/age delivery flows in", baseline_fy))
    } else {
      source_counts <- vapply(source_order, function(la) sum(flow$N[flow$Source_LA == la]), numeric(1))
      age_counts <- vapply(chart_bands, function(age) sum(flow$N[flow$Age_band == age]), numeric(1))
      org <- organisations[match(key, organisations$Key), , drop = FALSE]
      org_name <- org$Organisation[[1]]
      chart_height <- max(420, length(source_order) * 34 + 120)
      # Fixed positions retain the largest LA at the top without overlapping unequal nodes.
      node_y <- function(counts) {
        gap <- 14 / (chart_height - 50)
        (cumsum(counts) - counts / 2) / sum(counts) * (1 - gap * (length(counts) - 1L)) +
          gap * (seq_along(counts) - 1L)
      }
      source_id <- match(flow$Source_LA, source_order) - 1L
      age_id <- length(source_order) + match(flow$Age_band, chart_bands) - 1L
      used_ages <- which(age_counts > 0)
      target_id <- length(source_order) + length(chart_bands)
      origin_text <- source_labels[match(flow$Source_LA, source_order)]
      flow_hover <- paste0(origin_text, "<br>Age: ", flow$Age_band,
        "<br>Deliveries here: ", scales::comma(flow$N, accuracy = 1),
        "<br>All LA/age pregnancies: ", ifelse(is.na(flow$Denominator), "n.a.",
          scales::comma(flow$Denominator, accuracy = 1)),
        "<br>LA/age share sent here: ", ifelse(is.na(flow$Share), "n.a.",
          scales::percent(flow$Share, accuracy = 0.1)))
      age_hover <- paste0("Age: ", chart_bands[used_ages], "<br>", org_name,
        "<br>Deliveries here: ", scales::comma(age_counts[used_ages], accuracy = 1),
        "<br>Share of charted deliveries: ",
          scales::percent(age_counts[used_ages] / sum(source_counts), accuracy = 0.1))
      node_labels <- c(source_labels, chart_bands, org_name)
      chart <- plotly::plot_ly(type = "sankey", height = chart_height,
        arrangement = "fixed", orientation = "h",
        node = list(label = node_labels,
          x = c(rep(0.02, length(source_order)), rep(0.55, length(chart_bands)), 0.98),
          y = c(node_y(source_counts), node_y(age_counts), 0.5),
          color = c(rep("#005EB8", length(source_order)), chart_age_colours, "#005EB8"),
          pad = 14, thickness = 16,
          customdata = paste0(c(source_labels, chart_bands, org_name), "<br>Deliveries here: ",
            scales::comma(c(source_counts, age_counts, sum(source_counts)), accuracy = 1)),
          hovertemplate = "%{customdata}<extra></extra>"),
        link = list(source = c(source_id, length(source_order) + used_ages - 1L),
          target = c(age_id, rep(target_id, length(used_ages))),
          value = c(flow$N, age_counts[used_ages]),
          color = grDevices::adjustcolor(c(chart_age_colours[match(flow$Age_band, chart_bands)],
            chart_age_colours[used_ages]), alpha.f = 0.45),
          customdata = c(flow_hover, age_hover), hovertemplate = "%{customdata}<extra></extra>")) |>
        plotly::layout(margin = list(l = 12, r = 12, t = 25, b = 25),
          font = list(family = "Arial", size = 12)) |>
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    }
    table <- org_shares |>
      dplyr::select(LA_name, LA, Age_band, Numerator, Denominator, Share) |>
      base_table(paste(baseline_fy, "LA and age-band allocation")) |>
      gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age_band = "Age band",
        Numerator = "Deliveries here", Denominator = "All LA/age pregnancies", Share = "% sent here") |>
      gt::fmt_number(columns = c("Numerator", "Denominator"), decimals = 0) |>
      gt::fmt_percent(columns = "Share", decimals = 1)
    list(chart = htmltools::div(style = "max-height: 650px; overflow-y: auto;", chart),
      table = table, flow = flow, visible_las = visible_las)
  }), organisations$Key)
  accounting_table <- destination_accounting |>
    dplyr::select(LA_name, LA, Age_band, Denominator, Cluster_trusts,
                  Recorded_home_births, Other_or_unrepresented) |>
    base_table("All destinations remain in the LA denominator") |>
    gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age_band = "Age band",
      Denominator = "All LA pregnancies", Cluster_trusts = "At cluster trusts",
      Recorded_home_births = "Recorded home births",
      Other_or_unrepresented = "Other / unrepresented destinations") |>
    gt::fmt_number(columns = c("Denominator", "Cluster_trusts", "Recorded_home_births",
                               "Other_or_unrepresented"), decimals = 0)
  style_line_chart <- function(chart, y_title, percent = FALSE, ons_only = FALSE) {
    tick_years <- sort(unique(c(ons_reference_year,
      projection_years[projection_years %% 5 == 0], max(projection_years))))
    plotly::layout(chart,
      xaxis = list(title = "Observed financial year / ONS year ending mid-year",
        tickmode = "array", tickvals = tick_years,
        ticktext = ifelse(tick_years == ons_reference_year & !ons_only, baseline_fy, paste0("Mid-", tick_years))),
      yaxis = list(title = y_title, tickformat = if (percent) ".0%" else ",.0f",
                   rangemode = "tozero", zeroline = TRUE),
      legend = list(orientation = "h", x = 0, y = -0.25),
      margin = list(l = 85, r = 20, t = 20, b = 125), font = list(family = "Arial")) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }
  age_line_chart <- function(data, value, title, percent = FALSE) {
    chart <- plotly::plot_ly(height = projection_chart_height)
    chart_bands <- c(age_bands, if ("Unknown" %in% data$Age_band) "Unknown")
    chart_colours <- c(age_colours, "#222222")
    for (i in seq_along(chart_bands)) {
      line <- data |> dplyr::filter(Age_band == chart_bands[i]) |> dplyr::arrange(Year)
      if (!nrow(line)) next
      values <- line[[value]]
      hover <- paste0(ifelse(line$Year == ons_reference_year & !percent, baseline_fy, paste0("Mid-", line$Year)),
        "<br>Age: ", chart_bands[i], "<br>", title, ": ",
        if (percent) scales::percent(values, accuracy = 0.1) else scales::comma(values, accuracy = 1))
      chart <- plotly::add_trace(chart, x = line$Year, y = values, text = hover,
        type = "scatter", mode = "lines+markers", name = chart_bands[i],
        line = list(color = chart_colours[i], width = 2), marker = list(color = chart_colours[i], size = 4),
        hovertemplate = "%{text}<extra></extra>", connectgaps = FALSE)
    }
    style_line_chart(chart, title, percent, ons_only = percent)
  }
  forecast_outputs <- stats::setNames(lapply(organisations$Key, function(key) {
    org_shares <- feeder_shares[feeder_shares$Key == key, , drop = FALSE]
    org_flows <- feeder_projections[feeder_projections$Key == key, , drop = FALSE]
    org_ages <- age_projections[age_projections$Key == key, , drop = FALSE]
    org_totals <- maternity_projections[maternity_projections$Key == key, , drop = FALSE]
    org_totals <- dplyr::arrange(org_totals, Year)
    org_feeders <- unique(org_shares$LA)
    # Section 9.2: ONS age-band growth and the incoming maternities it produces.
    ons_age_growth <- ons_maternity_projections |>
      dplyr::filter(LA %in% org_feeders) |>
      dplyr::group_by(Age_band, Year) |>
      dplyr::summarise(ONS_maternities = sum(ONS_maternities), .groups = "drop") |>
      dplyr::group_by(Age_band) |>
      dplyr::mutate(ONS_change = relative_change(ONS_maternities,
                         ONS_maternities[Year == ons_reference_year])) |>
      dplyr::ungroup()
    growth_chart <- age_line_chart(ons_age_growth, "ONS_change", "% change in feeder-LA maternities", TRUE)
    incoming_chart <- age_line_chart(org_ages, "Projected", "Incoming maternities")
    # Tables retain every forecast year; their columns scroll horizontally.
    growth_table <- if (nrow(org_flows)) org_flows |>
      dplyr::mutate(ONS_year_label = paste0("Mid-", Year)) |>
      dplyr::select(LA_name, LA, Age_band, Numerator, Denominator, Share, Treatment, ONS_year_label, ONS_change) |>
      tidyr::pivot_wider(names_from = ONS_year_label, values_from = ONS_change) |>
      dplyr::arrange(LA_name, match(Age_band, age_bands)) |>
      base_table("ONS change by feeder LA and age", paste("Relative to the ONS", ons_reference_year, "reference")) |>
      gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age_band = "Age band",
        Numerator = "Observed deliveries here", Denominator = "All LA/age pregnancies", Share = "% sent here",
        Treatment = "Projection treatment") |>
      gt::fmt_number(columns = c("Numerator", "Denominator"), decimals = 0) |>
      gt::fmt_percent(columns = c("Share", paste0("Mid-", projection_years)), decimals = 1) else
      base_table(data.frame(Note = paste("No recorded feeder-LA flows in", baseline_fy)), "ONS change")
    incoming_table <- if (nrow(org_flows)) org_flows |>
      dplyr::select(LA_name, LA, Age_band, Treatment, Year_label, Projected) |>
      tidyr::pivot_wider(names_from = Year_label, values_from = Projected) |>
      dplyr::arrange(LA_name, match(Age_band, age_bands)) |>
      base_table("Incoming maternities by feeder LA and age") |>
      gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age_band = "Age band",
                     Treatment = "Projection treatment") |>
      gt::fmt_number(columns = c(baseline_fy, paste0("Mid-", projection_years[-1])), decimals = 0) else
      base_table(data.frame(Note = paste("No recorded feeder-LA flows in", baseline_fy)), "Incoming maternities")
    if (!nrow(org_flows)) {
      growth_chart <- htmltools::div(paste("No recorded feeder-LA flows in", baseline_fy))
      incoming_chart <- htmltools::div(paste("No recorded feeder-LA flows in", baseline_fy))
    }
    if (nrow(org_flows) && !nrow(ons_age_growth)) {
      growth_chart <- htmltools::div("No matched ONS age-specific series; baseline deliveries are held constant.")
    }
    # Sections 9.3 and 9.4: total counts and percentage change from the actual baseline.
    total_chart <- plotly::plot_ly(org_totals, height = projection_chart_height, x = ~Year, y = ~Projected,
      type = "scatter", mode = "lines+markers", line = list(color = "#005EB8", width = 2),
      text = ~paste0(Year_label, "<br>Maternities: ", scales::comma(Projected, accuracy = 1)),
      hovertemplate = "%{text}<extra></extra>", showlegend = FALSE) |>
      style_line_chart("Total maternities")
    change_chart <- plotly::plot_ly(org_totals, height = projection_chart_height, x = ~Year, y = ~Percent_change,
      type = "scatter", mode = "lines+markers", line = list(color = "#005EB8", width = 2),
      text = ~paste0(Year_label, "<br>Change from ", baseline_fy, ": ",
        ifelse(is.na(Percent_change), "n.a.", scales::percent(Percent_change, accuracy = 0.1))),
      hovertemplate = "%{text}<extra></extra>", showlegend = FALSE) |>
      style_line_chart(paste("% change from observed", baseline_fy), TRUE)
    if (org_totals$Observed_baseline[1] == 0) change_chart <- plotly::layout(change_chart,
      annotations = list(list(text = paste("No observed deliveries in", baseline_fy, "; relative change unavailable."),
        x = 0, y = 1.05, xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "left")),
      margin = list(t = 45))
    total_table <- org_totals |>
      dplyr::select(Year_label, Projected) |>
      base_table("Total maternity projections") |>
      gt::cols_label(Year_label = "Year", Projected = "Maternities") |>
      gt::fmt_number(columns = "Projected", decimals = 0)
    change_table <- org_totals |>
      dplyr::select(Year_label, Observed_baseline, Projected, Change, Percent_change) |>
      base_table(paste("Change from observed", baseline_fy)) |>
      gt::cols_label(Year_label = "Year", Observed_baseline = paste("Observed", baseline_fy),
        Projected = "Maternities", Change = "Count change", Percent_change = "% change") |>
      gt::fmt_number(columns = c("Observed_baseline", "Projected", "Change"), decimals = 0) |>
      gt::fmt_percent(columns = "Percent_change", decimals = 1)
    list(growth_chart = growth_chart, incoming_chart = incoming_chart,
      growth_table = growth_table, incoming_table = incoming_table,
      total_chart = total_chart, total_table = total_table,
      change_chart = change_chart, change_table = change_table)
  }), organisations$Key)

  all_totals_table <- maternity_projections |>
    dplyr::select(Level, Organisation, Year_label, Projected) |>
    tidyr::pivot_wider(names_from = Year_label, values_from = Projected) |>
    gt::gt(groupname_col = "Level", rowname_col = "Organisation") |>
    gt::tab_header(title = "All organisation maternity projections") |>
    gt::fmt_number(columns = c(baseline_fy, paste0("Mid-", projection_years[-1])), decimals = 0) |>
    gt::tab_options(container.width = "100%", container.overflow.x = TRUE,
                    table.font.names = c("Open Sans", "Arial", "sans-serif"))
  all_changes_table <- maternity_projections |>
    dplyr::select(Level, Organisation, Year_label, Percent_change) |>
    tidyr::pivot_wider(names_from = Year_label, values_from = Percent_change) |>
    gt::gt(groupname_col = "Level", rowname_col = "Organisation") |>
    gt::tab_header(title = paste("All organisation changes from observed", baseline_fy)) |>
    gt::fmt_percent(columns = c(baseline_fy, paste0("Mid-", projection_years[-1])), decimals = 1) |>
    gt::sub_missing(missing_text = "n.a.") |>
    gt::tab_options(container.width = "100%", container.overflow.x = TRUE,
                    table.font.names = c("Open Sans", "Arial", "sans-serif"))
  result$mapping_outputs <- mapping_outputs
  result$accounting_table <- accounting_table
  result$coverage_table <- coverage_table
  result$coverage_details_table <- coverage_details_table
  result$geography_table <- geography_table
  result$forecast_outputs <- forecast_outputs
  result$all_totals_table <- all_totals_table
  result$all_changes_table <- all_changes_table
  result
}

build_projection_backtest <- function() {
  # Section 9.5: compare with actual history.
  # Start again from 2023/24, using only that year's actual counts and feeder shares.
  historical <- build_first_draft_projections(baseline_fy = "2023/24",
    ons_reference_year = 2024L, projection_end = 2026L, prepare_outputs = FALSE)
  organisations <- historical$organisations
  years <- projection_history_years
  # Count the actual deliveries separately for each financial year.
  observed <- final_cohort_2326 |>
    dplyr::filter(trimws(as.character(birth_yr)) %in% years, as.character(birth_flag) == "1") |>
    dplyr::transmute(Person = trimws(as.character(person_id_mother)),
      Pregnancy = trimws(as.character(uniq_preg_id)), Year_label = trimws(as.character(birth_yr)),
      LA = toupper(trimws(as.character(la_of_mother))), Age = as.numeric(age_at_birth_mother),
      Trust = toupper(trimws(as.character(birth_provider))), Home = as.numeric(as.character(home_birth))) |>
    dplyr::distinct()
  observed <- observed |> dplyr::filter(Trust %in% cluster_trusts, Home == 0)
  observed_totals <- dplyr::bind_rows(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, , drop = FALSE]
    keep <- if (org$Level == "Trust") observed$Trust == org$Code else
      if (org$Level == "ICB") unname(provider_icb[observed$Trust]) == org$Code else rep(TRUE, nrow(observed))
    data <- observed[keep, , drop = FALSE]
    data.frame(Key = org$Key, Year_label = years,
      Observed = vapply(years, function(year) sum(data$Year_label == year), integer(1)))
  }))
  # Put observed and modelled counts side by side and calculate the difference.
  comparison <- historical$maternity_projections |>
    dplyr::transmute(Key, Level, Organisation, Year,
      Year_label = paste0(Year - 1L, "/", substr(Year, 3, 4)), Modelled = Projected) |>
    dplyr::left_join(observed_totals, by = c("Key", "Year_label")) |>
    dplyr::mutate(Difference = Modelled - Observed,
      Percent_difference = ifelse(Observed > 0, Difference / Observed, NA_real_)) |>
    dplyr::arrange(match(Key, organisations$Key), Year)
  # Make compact comparison charts for the six named trusts, ICBs and combined cluster.
  trust_organisations <- organisations |>
    dplyr::filter(Level == "Trust", Code %in% names(trust_labels)[names(trust_labels) != "Other"])
  icb_cluster_organisations <- organisations |>
    dplyr::filter(Level %in% c("ICB", "Cluster"))
  chart_organisations <- dplyr::bind_rows(trust_organisations, icb_cluster_organisations)
  outputs <- stats::setNames(lapply(chart_organisations$Key, function(key) {
    data <- comparison[comparison$Key == key, , drop = FALSE]
    chart <- plotly::plot_ly(height = projection_history_chart_height) |>
      plotly::add_trace(x = data$Year, y = data$Observed, type = "scatter", mode = "lines+markers",
        name = "MSDS observed", line = list(color = projection_history_colours[["MSDS observed"]], width = 2),
        marker = list(color = projection_history_colours[["MSDS observed"]], size = 5),
        text = paste0(data$Year_label, "<br>Observed: ", scales::comma(data$Observed, accuracy = 1)),
        hovertemplate = "%{text}<extra></extra>") |>
      plotly::add_trace(x = data$Year, y = data$Modelled, type = "scatter", mode = "lines+markers",
        name = "ONS-based model", line = list(color = projection_history_colours[["ONS-based model"]], width = 2),
        marker = list(color = projection_history_colours[["ONS-based model"]], size = 5),
        text = paste0(data$Year_label, "<br>Modelled: ", scales::comma(data$Modelled, accuracy = 1),
          "<br>Difference from observed: ", ifelse(is.na(data$Percent_difference), "n.a.",
                                                   scales::percent(data$Percent_difference, accuracy = 0.1))),
        hovertemplate = "%{text}<extra></extra>") |>
      plotly::layout(xaxis = list(title = "Financial year",
        tickmode = "array", tickvals = 2024:2026, ticktext = years),
        yaxis = list(title = "Maternities", tickformat = ",.0f", rangemode = "tozero"),
        showlegend = FALSE,
        margin = list(l = 65, r = 12, t = 15, b = 55), font = list(family = "Arial", size = 11)) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
    list(chart = chart)
  }), chart_organisations$Key)

  # Put one shared colour key above each grid, as in sections 6e and 6f.
  comparison_key <- htmltools::div(
    style = "display: flex; flex-wrap: wrap; gap: 12px 24px; margin-bottom: 20px; font-size: 14px;",
    htmltools::tagList(lapply(names(projection_history_colours), function(series) {
      htmltools::span(style = "display: inline-flex; align-items: center; gap: 6px;",
        htmltools::span(style = paste0("display: inline-block; width: 18px; height: 3px; background: ",
          projection_history_colours[[series]], ";"), `aria-hidden` = "true"), series)
    })))
  comparison_grid <- function(rows) {
    htmltools::tagList(comparison_key,
      htmltools::div(style = "display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 24px;",
        htmltools::tagList(lapply(seq_len(nrow(rows)), function(i) {
          htmltools::div(style = "min-width: 0;",
            htmltools::h3(style = "font-size: 14px; margin: 0 0 8px;", rows$Organisation[i]),
            outputs[[rows$Key[i]]]$chart)
        }))))
  }
  trust_grid <- comparison_grid(trust_organisations)
  icb_cluster_grid <- comparison_grid(icb_cluster_organisations)

  # Keep all comparison values together: one organisation per row and metrics under each year.
  metrics <- c("Observed", "Modelled", "Difference", "Percent_difference")
  wide <- comparison |>
    dplyr::select(Level, Organisation, Year_label, dplyr::all_of(metrics)) |>
    tidyr::pivot_wider(names_from = Year_label, values_from = dplyr::all_of(metrics), names_sep = "_")
  column_order <- unlist(lapply(years, function(year) paste0(metrics, "_", year)), use.names = FALSE)
  wide <- dplyr::select(wide, Level, Organisation, dplyr::all_of(column_order))
  percent_columns <- paste0("Percent_difference_", years)
  count_columns <- setdiff(column_order, percent_columns)
  labels <- stats::setNames(rep(c("MSDS observed", "ONS-based model", "Model − observed", "% difference"),
                                  length(years)), column_order)
  all_table <- gt::gt(wide, groupname_col = "Level", rowname_col = "Organisation") |>
    gt::tab_header(title = "Observed versus modelled maternities: all organisations") |>
    gt::cols_label(.list = as.list(labels)) |>
    gt::fmt_number(columns = dplyr::all_of(count_columns), decimals = 0) |>
    gt::fmt_percent(columns = dplyr::all_of(percent_columns), decimals = 1) |>
    gt::sub_missing(missing_text = "n.a.") |>
    gt::tab_source_note(source_note = "% difference = (modelled − observed) / observed × 100; positive means overprediction. n.a. = zero observed count.") |>
    gt::tab_options(table.font.names = c("Open Sans", "Arial", "sans-serif"),
                    container.width = "100%", container.overflow.x = TRUE,
                    table.width = gt::px(1500), table.font.size = gt::px(12))
  for (year in years) all_table <- gt::tab_spanner(all_table, label = year,
    columns = paste0(metrics, "_", year), id = paste0("backtest-", year), gather = FALSE)
  coverage_table <- historical$coverage |>
    dplyr::select(Level, Organisation, Observed_baseline, ONS_growth, Unmatched) |>
    gt::gt(groupname_col = "Level", rowname_col = "Organisation") |>
    gt::tab_header(title = "2023/24 projection coverage") |>
    gt::cols_label(Observed_baseline = "All observed deliveries", ONS_growth = "Matched LA/age ONS growth",
      Unmatched = "No matched LA/age growth") |>
    gt::fmt_number(columns = c("Observed_baseline", "ONS_growth", "Unmatched"), decimals = 0) |>
    gt::tab_options(container.width = "100%", container.overflow.x = TRUE)
  coverage_details_table <- historical$coverage_details |>
    dplyr::select(Level, Organisation, LA_name, LA, Age_band, Reason, N) |>
    gt::gt() |>
    gt::tab_header(title = "2023/24 records without matched ONS growth") |>
    gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age_band = "Age band",
      Reason = "Reason", N = "Observed deliveries") |>
    gt::fmt_number(columns = "N", decimals = 0)
  list(note = historical$note, organisations = organisations, baseline = historical,
       data = comparison, outputs = outputs, trust_grid = trust_grid,
       icb_cluster_grid = icb_cluster_grid, all_table = all_table,
       coverage = historical$coverage, coverage_details = historical$coverage_details,
       coverage_table = coverage_table, coverage_details_table = coverage_details_table)
}

# Prepare section 9 and save it only after both projections and the comparison finish.
first_draft_projections <- build_first_draft_projections()
projection_backtest <- build_projection_backtest()
first_draft_projections$backtest <- projection_backtest
saveRDS(first_draft_projections, ".projections.rds")
message("Section 9 prepared: Method 1 projections and the 2023/24 historical comparison.")
