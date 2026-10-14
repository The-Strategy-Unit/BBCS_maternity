# Run the counting checks when you want to QA a new dataset or changed analysis.
# Run automatically by Run analysis.R after preparation.
# This script does not make charts, change inputs or overwrite report outputs.

# 1. Get the inputs and start a readable list of results.
required_inputs <- c("final_cohort_2326", "total_births_by_LA_2326")
missing_inputs <- required_inputs[!vapply(required_inputs,
  function(name) exists(name, inherits = TRUE), logical(1))]
if (length(missing_inputs)) stop("Load the Azure data first. Missing: ",
                               paste(missing_inputs, collapse = ", "), call. = FALSE)
source("R/ons_data.R")
source("R/report_cohort.R")
qa_years <- c("2023/24", "2024/25", "2025/26")
qa_results <- data.frame(Check = character(), Result = character(), Details = character())
qa_details <- list()

# Add one line to the results; problems do not hide the remaining checks.
qa_check <- function(description, passed, details = "") {
  qa_results <<- rbind(qa_results, data.frame(Check = description,
    Result = if (isTRUE(passed)) "OK" else "Check", Details = details))
}
numbers_match <- function(actual, expected, tolerance = 1e-8) {
  length(actual) == length(expected) && all(is.finite(actual)) && all(is.finite(expected)) &&
    all(abs(actual - expected) <= tolerance)
}

# Missing model values must agree explicitly; never equate them with zero.
projection_numbers_match <- function(actual, expected, tolerance = 1e-8) {
  actual <- as.numeric(actual); expected <- as.numeric(expected)
  length(actual) == length(expected) && identical(is.na(actual), is.na(expected)) &&
    numbers_match(actual[!is.na(actual)], expected[!is.na(expected)], tolerance)
}

# 2. Check the source pregnancy records and LA totals.
cohort_records <- final_cohort_2326
prepared_records <- prepare_report_cohort(cohort_records)
missing_ids <- is.na(cohort_records$person_id_mother) |
  trimws(as.character(cohort_records$person_id_mother)) == "" |
  is.na(cohort_records$uniq_preg_id) | trimws(as.character(cohort_records$uniq_preg_id)) == ""
qa_check("Every cohort record has mother and pregnancy identifiers", !any(missing_ids),
         paste(sum(missing_ids), "records have missing identifiers"))
identified_records <- cohort_records[!missing_ids, , drop = FALSE]
repeated_pregnancies <- duplicated(identified_records[c("person_id_mother", "uniq_preg_id")])
qa_check("One consistent cohort record per mother and pregnancy", !any(repeated_pregnancies),
         paste(sum(repeated_pregnancies), "additional rows repeat an identified pregnancy; no rows are removed by this check"))
recorded_deliveries <- cohort_records[prepared_records$Birth_recorded &
  prepared_records$Year %in% qa_years, , drop = FALSE]
qa_check("Recorded births have a valid delivery financial year",
  !any(prepared_records$Birth_recorded & is.na(prepared_records$Year)),
  paste(sum(prepared_records$Birth_recorded & is.na(prepared_records$Year)), "recorded births cannot be assigned to a delivery year"))
qa_check("Supplied birth flags are 0 or 1", all(prepared_records$Birth_flag %in% c(0, 1)),
  paste(sum(!prepared_records$Birth_flag %in% c(0, 1)), "missing/invalid flags; not counted as recorded deliveries"))
missing_delivery_years <- setdiff(qa_years, as.character(recorded_deliveries$birth_yr))
qa_check("Recorded deliveries cover all three financial years", !length(missing_delivery_years),
         paste("Missing:", paste(missing_delivery_years, collapse = ", ")))
home_flags <- report_number(recorded_deliveries$home_birth)
qa_check("Recorded deliveries have home flags of 0 or 1", all(home_flags %in% c(0, 1)),
         paste(sum(!home_flags %in% c(0, 1)), "records have missing or invalid home flags"))
resident_records <- total_births_by_LA_2326 |>
  dplyr::mutate(LA = report_la_code(la_of_mother), Age = report_age(age_at_birth_mother)) |>
  dplyr::filter(LA %in% prepared_records$LA, !LA %in% read_area_config()$area_code,
    as.character(birth_yr) %in% qa_years)
if (file.exists(".projections.rds")) {
  used_residents <- readRDS(".projections.rds")$allocation_tables$residents
  resident_records <- dplyr::semi_join(resident_records, used_residents, by = c("LA", "Age"))
}
resident_counts <- report_number(resident_records$count)
valid_resident_counts <- is.finite(resident_counts) & resident_counts >= 0 &
  resident_counts == floor(resident_counts)
qa_check("Resident LA counts are nonnegative whole numbers", all(valid_resident_counts),
         paste(sum(!valid_resident_counts), "invalid counts"))
repeated_resident_cells <- duplicated(resident_records[
  c("birth_yr", "la_of_mother", "age_at_birth_mother")])
qa_check("Resident totals have one row per LA, age and year", !any(repeated_resident_cells),
         paste(sum(repeated_resident_cells), "repeated LA/age/year rows"))

# 3. Check the public ONS data covers the selected areas and years.
area_settings <- read_area_config()
age_settings <- read_analysis_parameters()
ons_births <- read_snpp_births()
female_population <- read_female_population_single_age()
historic_births <- read_historic_live_births()
qa_check("Configured LA codes and names are unique",
  !anyDuplicated(area_settings$area_code) && !anyDuplicated(area_settings$area_name))
qa_check("ONS birth cells are unique and have valid counts",
  !anyDuplicated(ons_births[c("area_code", "age_group", "year")]) &&
    all(is.finite(ons_births$projected_births) & ons_births$projected_births >= 0))
expected_population <- tidyr::expand_grid(area_code = area_settings$area_code,
  year = sort(unique(ons_births$year)),
  age = seq(age_settings[["minimum_maternal_age"]], age_settings[["maximum_maternal_age"]]))
missing_population <- dplyr::anti_join(expected_population, female_population,
                                     by = c("area_code", "year", "age"))
qa_details$missing_female_population <- missing_population
qa_check("Female population covers every selected LA, age and year",
  !nrow(missing_population) &&
    !anyDuplicated(female_population[c("area_code", "year", "age")]) &&
    all(is.finite(female_population$female_population) & female_population$female_population > 0),
  paste(nrow(missing_population), "missing cells"))
expected_observed <- tidyr::expand_grid(year = 2023:2025,
                                      area_name = area_settings$area_name)
observed_maternities <- read_local_authority_maternities(area_settings$area_name) |>
  dplyr::rename(area_name = local_authority)
observed_rates <- read_local_authority_birth_rates(area_settings$area_name)
qa_check("Published maternities cover every selected LA and year",
  !nrow(dplyr::anti_join(expected_observed, observed_maternities, by = c("year", "area_name"))))
qa_check("Published fertility rates cover every selected LA and year",
  !nrow(dplyr::anti_join(expected_observed, observed_rates, by = c("year", "area_name"))))
missing_historic_las <- setdiff(area_settings$area_code, unique(historic_births$area_code))
qa_check("Historic births include every selected LA", !length(missing_historic_las),
         paste(missing_historic_las, collapse = ", "))
historic_totals <- historic_births |>
  dplyr::filter(age_band == "Total") |>
  dplyr::select(area_code, year, Published = live_births)
historic_age_totals <- historic_births |>
  dplyr::filter(age_band != "Total") |>
  dplyr::group_by(area_code, year) |>
  dplyr::summarise(Age_total = sum(live_births), .groups = "drop") |>
  dplyr::left_join(historic_totals, by = c("area_code", "year"))
qa_details$historic_birth_totals <- historic_age_totals
qa_check("Historic age-band births add up to published totals",
  nrow(historic_age_totals) > 0 && !anyNA(historic_age_totals$Published) &&
    numbers_match(historic_age_totals$Age_total, historic_age_totals$Published))

# 4. Check MSDS exploration views, using the saved calculations rather than remaking charts.
if (file.exists(".patient_flows.rds")) {
  patient_flows <- readRDS(".patient_flows.rds")
  counted_episodes <- patient_flows$activity_episodes
  qa_check("MSDS exploration views retain every supplied pregnancy row",
    identical(counted_episodes$Row, prepared_records$Row))
  qa_check("Patient-flow episodes cover all delivery years",
    all(2023:2025 %in% counted_episodes$delivery_year))
  booking_trusts <- patient_flows$delivery_over_time$organisations |>
    dplyr::filter(Level == "Trust") |>
    dplyr::pull(Code)
  starts <- counted_episodes[counted_episodes$booking_code %in% booking_trusts, , drop = FALSE]
  qa_check("Cluster starts cover all booking years", all(2023:2025 %in% starts$booking_year))
  home_birth_starts <- starts[starts$home_birth %in% 1L & starts$birth_recorded, , drop = FALSE]
  qa_check("Recorded home births count outside cluster trust delivery locations",
    all(classify_pathway_outcome(home_birth_starts) == "Outside cluster"))
  hospital_counts <- vapply(2023:2025, function(year) sum(
    counted_episodes$birth_recorded & counted_episodes$delivery_year %in% year & counted_episodes$home_birth %in% 0L &
      counted_episodes$birth_code %in% cluster_trusts), integer(1))
  qa_check("Section 2.2.2 trust delivery totals use recorded non-home births",
    numbers_match(patient_flows$cluster_activity$delivery_trend$Total, hospital_counts))
  latest_pathways <- patient_flows$cluster_activity$pathways
  latest_cluster <- latest_pathways[latest_pathways$Level == "Cluster", , drop = FALSE]
  pathway_columns <- c("Starts", "Within", "Outside", "No_delivery", "Unknown")
  qa_check("Section 2.2.2 combined-cluster starts and outcomes equal the sum of its two ICBs",
    nrow(latest_cluster) == 1L &&
      sum(latest_pathways$Level == "ICB") == 2L &&
      numbers_match(unlist(latest_cluster[1, pathway_columns], use.names = FALSE),
        colSums(latest_pathways[latest_pathways$Level == "ICB", pathway_columns, drop = FALSE])) &&
      numbers_match(latest_cluster$Starts, rowSums(latest_cluster[outcome_columns])))
  for (flow_name in c("icb_flow", "trust_flow")) {
    flow <- patient_flows[[flow_name]]$links
    qa_check(paste(flow_name, "has positive complete flow counts"), nrow(flow) > 0 &&
      all(is.finite(flow$n) & flow$n > 0 & !is.na(flow$source) & !is.na(flow$target)))
  }
  three_stage <- patient_flows$residence_booking_birth_flow
  qa_check("Section 2.2.4 has residence-to-booking-to-delivery flows available", !is.null(three_stage),
    "Run R/Run analysis.R again if these outputs are missing")
  if (!is.null(three_stage)) {
    expected_flow_total <- sum(patient_flows$trust_flow$links$n)
    qa_check("Section 2.2.4's three-stage paths and both legs count the same deliveries as the trust flow",
      numbers_match(c(sum(three_stage$paths$n), sum(three_stage$residence_booking$n),
        sum(three_stage$booking_birth$n)), rep(expected_flow_total, 3)))
    qa_check("Section 2.2.4's three-stage paths have unique complete groups and positive counts",
      !anyDuplicated(three_stage$paths[c("residence", "booking", "birth")]) &&
        !anyNA(three_stage$paths[c("residence", "booking", "birth")]) &&
        all(is.finite(three_stage$paths$n) & three_stage$paths$n > 0))
    booking_conservation <- dplyr::full_join(
      three_stage$residence_booking |>
        dplyr::group_by(Booking = target) |>
        dplyr::summarise(Incoming = sum(n), .groups = "drop"),
      three_stage$booking_birth |>
        dplyr::group_by(Booking = source) |>
        dplyr::summarise(Outgoing = sum(n), .groups = "drop"), by = "Booking")
    qa_details$residence_booking_birth_conservation <- booking_conservation
    qa_check("Section 2.2.4's three-stage flow conserves deliveries at every booking organisation",
      numbers_match(booking_conservation$Incoming, booking_conservation$Outgoing))
    booking_birth_check <- dplyr::full_join(
      patient_flows$trust_flow$links |> dplyr::rename(Trust_flow = n),
      three_stage$booking_birth |> dplyr::rename(Three_stage = n), by = c("source", "target"))
    qa_details$residence_booking_birth_reconciliation <- booking_birth_check
    qa_check("Section 2.2.4's three-stage booking-to-delivery counts match the existing trust flow",
      numbers_match(booking_birth_check$Trust_flow, booking_birth_check$Three_stage))
  }
  outcome_counts <- patient_flows$delivery_over_time$data |>
    dplyr::group_by(Key, Year_start) |>
    dplyr::summarise(Outcomes = sum(Numerator), Starts = dplyr::first(Denominator),
                    .groups = "drop")
  qa_details$delivery_outcome_counts <- outcome_counts
  qa_check("The four delivery outcomes add up to all starts",
           numbers_match(outcome_counts$Outcomes, outcome_counts$Starts))
  outcome_levels <- patient_flows$delivery_over_time$data |>
    dplyr::group_by(Level, Year_start, Outcome) |>
    dplyr::summarise(N = sum(Numerator), Starts = sum(Denominator), .groups = "drop") |>
    tidyr::pivot_wider(names_from = Level, values_from = c(N, Starts))
  qa_details$delivery_outcome_levels <- outcome_levels
  qa_check("Section 2.2.6 trust and ICB numerators and denominators add up to the combined cluster",
    numbers_match(outcome_levels$N_Trust, outcome_levels$N_Cluster) &&
      numbers_match(outcome_levels$N_ICB, outcome_levels$N_Cluster) &&
      numbers_match(outcome_levels$Starts_Trust, outcome_levels$Starts_Cluster) &&
      numbers_match(outcome_levels$Starts_ICB, outcome_levels$Starts_Cluster))
  change_table <- patient_flows$delivery_over_time$change_data
  qa_check("Section 2.2.6 change table has one organisation per row and all outcome/year columns",
    nrow(change_table) == nrow(patient_flows$delivery_over_time$organisations) &&
      ncol(change_table) == 2 + 4 * length(qa_years))

  booking_outcomes <- patient_flows$booking_delivery_over_time
  qa_check("Section 2.2.7 has the new booking-to-delivery outputs available",
    !is.null(booking_outcomes), "Run R/Run analysis.R again if these outputs are missing")
  if (!is.null(booking_outcomes)) {
    expected_outcomes <- c("Delivery at same trust as booking",
      "Delivery at different trust within cluster",
      "Delivery outside cluster or home birth", "No recorded delivery",
      "Delivery location unknown")
    booking_counts <- booking_outcomes$data |>
      dplyr::group_by(Key, Year_start) |>
      dplyr::summarise(Outcomes = sum(Numerator), Starts = dplyr::first(Denominator),
        Categories = dplyr::n_distinct(Outcome), Denominators = dplyr::n_distinct(Denominator),
        .groups = "drop")
    qa_details$booking_delivery_outcome_counts <- booking_counts
    qa_check("Section 2.2.7's five outcomes partition all starts with one common denominator",
      setequal(unique(booking_outcomes$data$Outcome), expected_outcomes) &&
        all(booking_counts$Categories == 5L & booking_counts$Denominators == 1L) &&
        numbers_match(booking_counts$Outcomes, booking_counts$Starts))

    # The two within-cluster destinations sum to the section 2.2.6 count.
    collapsed <- booking_outcomes$data |>
      dplyr::mutate(Outcome = dplyr::case_when(
        Outcome %in% expected_outcomes[1:2] ~ "Delivery at cluster trust",
        Outcome == expected_outcomes[3] ~ "Other trust or home birth",
        TRUE ~ Outcome)) |>
      dplyr::group_by(Key, Year_start, Outcome) |>
      dplyr::summarise(Section8 = sum(Numerator), Section8_starts = dplyr::first(Denominator),
        .groups = "drop")
    outcome_reconciliation <- dplyr::full_join(
      patient_flows$delivery_over_time$data |>
        dplyr::select(Key, Year_start, Outcome, Section7 = Numerator, Section7_starts = Denominator),
      collapsed, by = c("Key", "Year_start", "Outcome"))
    qa_details$booking_delivery_outcome_reconciliation <- outcome_reconciliation
    qa_check("The booking-outcome views have the same starts and outcomes after combining the within-cluster split",
      numbers_match(outcome_reconciliation$Section7, outcome_reconciliation$Section8) &&
        numbers_match(outcome_reconciliation$Section7_starts, outcome_reconciliation$Section8_starts))

    booking_levels <- booking_outcomes$data |>
      dplyr::group_by(Level, Year_start, Outcome) |>
      dplyr::summarise(N = sum(Numerator), Starts = sum(Denominator), .groups = "drop") |>
      tidyr::pivot_wider(names_from = Level, values_from = c(N, Starts))
    qa_details$booking_delivery_outcome_levels <- booking_levels
    qa_check("Section 2.2.7 trust and ICB numerators and denominators add up to the combined cluster",
      numbers_match(booking_levels$N_Trust, booking_levels$N_Cluster) &&
        numbers_match(booking_levels$N_ICB, booking_levels$N_Cluster) &&
        numbers_match(booking_levels$Starts_Trust, booking_levels$Starts_Cluster) &&
        numbers_match(booking_levels$Starts_ICB, booking_levels$Starts_Cluster))
    qa_check("Section 2.2.7 change table has one trust, ICB or cluster per row and all five outcomes for each year",
      nrow(booking_outcomes$change_data) == nrow(booking_outcomes$organisations) &&
        ncol(booking_outcomes$change_data) == 2 + 5 * length(qa_years))
  }
  booking_icb_outcomes <- patient_flows$booking_icb_delivery_over_time
  qa_check("Section 2.2.7 has booking-ICB-to-delivery-ICB outcomes available",
    !is.null(booking_icb_outcomes), "Run R/Run analysis.R again if these outputs are missing")
  if (!is.null(booking_icb_outcomes)) {
    icb_outcome_labels <- c("Delivery ICB same as booking ICB",
      "Delivery ICB different to booking ICB but within cluster",
      "Delivery outside cluster or home birth", "No recorded delivery", "Delivery location unknown")
    icb_counts <- booking_icb_outcomes$data |>
      dplyr::group_by(Key, Year_start) |>
      dplyr::summarise(N = sum(Numerator), Starts = dplyr::first(Denominator),
        Categories = dplyr::n_distinct(Outcome), Denominators = dplyr::n_distinct(Denominator),
        .groups = "drop")
    qa_details$booking_icb_delivery_counts <- icb_counts
    qa_check("Section 2.2.7's ICB comparison partitions the same starts into five outcomes",
      setequal(unique(booking_icb_outcomes$data$Outcome), icb_outcome_labels) &&
        setequal(booking_icb_outcomes$organisations$Level, c("ICB", "Cluster")) &&
        all(icb_counts$Categories == 5L & icb_counts$Denominators == 1L) &&
        numbers_match(icb_counts$N, icb_counts$Starts))
    icb_collapsed <- booking_icb_outcomes$data |>
      dplyr::mutate(Outcome = dplyr::case_when(
        Outcome %in% icb_outcome_labels[1:2] ~ "Delivery at cluster trust",
        Outcome == icb_outcome_labels[3] ~ "Other trust or home birth", TRUE ~ Outcome)) |>
      dplyr::group_by(Key, Year_start, Outcome) |>
      dplyr::summarise(ICB_comparison = sum(Numerator), ICB_starts = dplyr::first(Denominator),
        .groups = "drop")
    icb_reconciliation <- dplyr::full_join(
      patient_flows$delivery_over_time$data |>
        dplyr::filter(Level %in% c("ICB", "Cluster")) |>
        dplyr::select(Key, Year_start, Outcome, Section7 = Numerator, Section7_starts = Denominator),
      icb_collapsed, by = c("Key", "Year_start", "Outcome"))
    qa_details$booking_icb_delivery_reconciliation <- icb_reconciliation
    qa_check("Section 2.2.7's ICB comparison reconciles to section 2.2.6 after combining both cluster destinations",
      numbers_match(icb_reconciliation$ICB_comparison, icb_reconciliation$Section7) &&
        numbers_match(icb_reconciliation$ICB_starts, icb_reconciliation$Section7_starts))
    icb_levels <- booking_icb_outcomes$data |>
      dplyr::group_by(Level, Year_start, Outcome) |>
      dplyr::summarise(N = sum(Numerator), Starts = sum(Denominator), .groups = "drop") |>
      tidyr::pivot_wider(names_from = Level, values_from = c(N, Starts))
    qa_check("Section 2.2.7's ICB comparison adds up to the combined cluster",
      numbers_match(icb_levels$N_ICB, icb_levels$N_Cluster) &&
        numbers_match(icb_levels$Starts_ICB, icb_levels$Starts_Cluster))
    icb_changes <- booking_icb_outcomes$data |>
      dplyr::group_by(Key, Outcome) |>
      dplyr::mutate(Base_share = Share[Year_start == 2023L],
        Expected_change = ifelse(is.finite(Base_share) & Base_share > 0,
          Share / Base_share - 1, NA_real_)) |>
      dplyr::ungroup()
    usable_icb_changes <- is.finite(icb_changes$Expected_change)
    qa_check("Section 2.2.7's ICB changes are calculated from the grouped shares",
      numbers_match(icb_changes$Base_change[usable_icb_changes],
        icb_changes$Expected_change[usable_icb_changes]) &&
        all(is.na(icb_changes$Base_change[!usable_icb_changes])))
    qa_check("Section 2.2.7's ICB change table has all five outcomes for each year",
      nrow(booking_icb_outcomes$change_data) == nrow(booking_icb_outcomes$organisations) &&
        ncol(booking_icb_outcomes$change_data) == 2 + 5 * length(qa_years))
  }
} else {
  qa_check("MSDS exploration views have saved outputs available", FALSE, "Run R/Run analysis.R first")
}

# 5. Check section 2.2.3's destination flows and hospital counts.
if (file.exists(".delivery_geography.rds")) {
  delivery_mapping <- readRDS(".delivery_geography.rds")
  delivery_records <- delivery_mapping$episodes
  qa_check("Section 2.2.3 retains every recorded delivery in the three years",
    identical(delivery_records$Row, prepared_records$Row[prepared_records$Birth_recorded &
      prepared_records$Year %in% qa_years]))
  qa_check("Section 2.2.3 includes all required years", all(qa_years %in% delivery_records$Year))
  destination_total <- nrow(delivery_mapping$matrix_episodes)
  qa_check("Section 2.2.3 matrix counts add up to its recorded deliveries",
    numbers_match(sum(delivery_mapping$matrix_data$N), destination_total))
  qa_check("Section 2.2.3 Sankey counts match the destination matrix",
    numbers_match(sum(delivery_mapping$sankey_links$N), destination_total))
  qa_check("Section 2.2.3 grouped matrix retains all destination counts",
    numbers_match(sum(delivery_mapping$grouped_matrix$N), destination_total))
  qa_check("Section 2.2.3.1 destination grid shows no more than ten additional organisations",
    length(delivery_mapping$visible_other_trusts) <= delivery_mapping$sankey_focus_n)
  # Check the separately grouped charts against their original delivery counts.
  la_links <- delivery_mapping$sankey_by_la_links
  destination_links <- delivery_mapping$sankey_by_destination_links
  qa_check("Section 2.2.3.1 LA charts retain all recorded deliveries",
    numbers_match(sum(vapply(la_links, function(links) sum(links$N), numeric(1))), destination_total))
  qa_check("Section 2.2.3.1 destination charts retain all recorded deliveries",
    numbers_match(sum(vapply(destination_links, function(links) sum(links$N), numeric(1))), destination_total))
  chart_counts <- delivery_mapping$matrix_data |>
    dplyr::mutate(Origin_focus = dplyr::case_when(
      grepl("^Unknown|^L999", LA) ~ "Unknown LA",
      LA %in% names(la_links) ~ LA, TRUE ~ "All other feeder LAs"),
      Destination_focus = ifelse(Destination %in% names(destination_links), Destination, "All other trusts"))
  main_trust_codes <- setdiff(names(trust_labels), "Other")
  cluster_la_codes <- readr::read_csv("config/local_authorities.csv", show_col_types = FALSE)$area_code
  for (origin in names(la_links)) {
    links <- la_links[[origin]]
    original <- chart_counts[chart_counts$Origin_focus == origin, , drop = FALSE]
    qa_check(paste("Section 2.2.3.1 LA chart keeps its deliveries and home births:", origin),
      numbers_match(c(sum(links$N), sum(links$N[links$Destination_group == "Home"])),
        c(sum(original$N), sum(original$N[original$Destination == "Home"]))))
    extra_trusts <- setdiff(unique(links$Destination_group),
      c(main_trust_codes, "Home", "Unknown provider", "Unknown location", "All other trusts"))
    qa_check(paste("Section 2.2.3.1 LA chart has up to five other trusts, ranked by volume:", origin),
      length(extra_trusts) <= delivery_mapping$sankey_top_n && all(diff(links$N) <= 0))
  }
  for (destination in names(destination_links)) {
    links <- destination_links[[destination]]
    original <- chart_counts[chart_counts$Destination_focus == destination, , drop = FALSE]
    qa_check(paste("Section 2.2.3.1 destination chart keeps its recorded deliveries:", destination),
      numbers_match(sum(links$N), sum(original$N)))
    extra_las <- setdiff(unique(links$Origin),
      c(cluster_la_codes, "Unknown LA", "All other feeder LAs"))
    qa_check(paste("Section 2.2.3.1 destination chart has up to five other LAs, ranked by volume:", destination),
      length(extra_las) <= delivery_mapping$sankey_top_n && all(diff(links$N) <= 0))
  }
  qa_details$unmatched_la_names <- delivery_records |>
    dplyr::filter(LA_name == "Unmatched LA") |> dplyr::distinct(LA)
  qa_details$unmatched_trust_names <- delivery_mapping$matrix_data |>
    dplyr::filter(Delivery_destination == "Unmatched delivery provider") |>
    dplyr::distinct(Destination)
  qa_check("Section 2.2.3 resident LA codes have names in the local lookup",
    !nrow(qa_details$unmatched_la_names))
  qa_check("Section 2.2.3 delivery provider codes have names in the local lookup",
    !nrow(qa_details$unmatched_trust_names))
  for (key in names(delivery_mapping$outputs)) {
    output <- delivery_mapping$outputs[[key]]
    if (is.null(output$trend_groups)) next # No activity at this organisation.
    original <- delivery_mapping$la_trends |>
      dplyr::filter(Key == key) |> dplyr::group_by(Year) |>
      dplyr::summarise(N = sum(N), .groups = "drop") |> dplyr::arrange(Year)
    charted <- output$trend_groups |> dplyr::group_by(Year) |>
      dplyr::summarise(N = sum(N), .groups = "drop") |> dplyr::arrange(Year)
    qa_check(paste("Section 2.2.3 LA chart groups retain every count:", key),
      identical(original$Year, charted$Year) && numbers_match(original$N, charted$N))
    if (!is.null(output$age_mix)) {
      age_totals <- output$age_mix |> dplyr::group_by(LA) |>
        dplyr::summarise(Share = sum(Age_share), .groups = "drop")
      qa_check(paste("Section 2.2.3 age mix adds up to 100% within each LA group:", key),
        numbers_match(age_totals$Share, rep(1, nrow(age_totals))))
    }
  }
  hospital_levels <- delivery_mapping$hospital_mapping |>
    dplyr::group_by(Level, Year) |>
    dplyr::summarise(N = sum(N), .groups = "drop") |>
    tidyr::pivot_wider(names_from = Level, values_from = N)
  qa_details$hospital_delivery_levels <- hospital_levels
  qa_check("Section 2.2.3 trust and ICB deliveries add up to the combined cluster",
    numbers_match(hospital_levels$Trust, hospital_levels$Cluster) &&
      numbers_match(hospital_levels$ICB, hospital_levels$Cluster))
} else {
  qa_check("Section 2.2.3 has saved outputs available", FALSE, "Run R/Run analysis.R first")
}

# 6. Check the master and both independent destination allocations.
if (file.exists(".projections.rds")) {
  projections <- readRDS(".projections.rds")
  qa_check("Projection outputs use the current four-modifier method",
    identical(projections$calculation_version, "ons-shared-age-calculations-v19"))
  flows <- projections$sankey_comparison
  qa_check("Delivery Sankey recorded flows match the observed cluster total",
    !is.null(flows) && numbers_match(sum(flows$flows$Observed),
      projections$observed_totals$Observed_baseline[projections$observed_totals$Key == "cluster"]))
  qa_check("Delivery Sankey final-year flows match available cluster allocations",
    !is.null(flows) && numbers_match(sum(flows$flows$Projected),
      projections$maternity_projections$Available_projected[projections$maternity_projections$Key == "cluster" &
        projections$maternity_projections$Year == flows$final_year], 1e-7))
  for (base in list(projections, projections$backtest$baseline)) {
    m <- base$master
    label <- paste("Reference", base$baseline_fy)
    la_matching <- base$la_source_matching
    la_mismatches <- la_matching[la_matching$Issue != "", ]
    qa_details[[paste(label, "LA source matching")]] <- la_matching
    qa_check(paste(label, "required LAs match resident counts and ONS births"),
      nrow(la_matching) > 0 && !nrow(la_mismatches),
      if (nrow(la_mismatches)) paste(nrow(la_mismatches), "contributing LAs lack a source match")
      else paste(nrow(la_matching), "LA codes match"))
    qa_check(paste(label, "uses the matching ONS year"),
      all(m$ONS_reference_year == as.integer(substr(base$baseline_fy, 1, 4))))
    qa_check(paste(label, "both allocations use the same adjusted maternity total"),
      projection_numbers_match(m$Delivery_projection_base, m$Adjusted_maternities) &&
      projection_numbers_match(m$Booking_projection_base, m$Adjusted_maternities))
    expected_denominator <- ifelse(m$Allocation_available %in% TRUE,
      m$MSDS_resident_count * m$Bookings_uplift_modifier, NA_real_)
    qa_check(paste(label, "both allocations use resident births multiplied by the booking uplift"),
      projection_numbers_match(m$Delivery_denominator, expected_denominator) &&
      projection_numbers_match(m$Booking_denominator, expected_denominator))
    qa_check(paste(label, "unique LA/Age (SY)/source-age/year master rows"),
      !anyDuplicated(m[c("LA", "ONS_age", "Age", "Year")]))
    national <- base$ons_modifications
    qa_check(paste(label, "ONS factors use pooled counts before division"),
      numbers_match(national$Stillbirth_ratio, national$Pooled_stillbirths / national$Pooled_live_births) &&
      numbers_match(national$Multiplicity_factor, national$Pooled_maternities / national$Pooled_total_births) &&
      numbers_match(national$National_conversion_factor, national$Pooled_maternities / national$Pooled_live_births))
    qa_check(paste(label, "stillbirth adjustment increases or preserves births (L)"),
      all(m$Adjusted_births_ls >= m$Exact_age_live_births) &&
      numbers_match(m$Adjusted_births_ls, m$Exact_age_live_births * (1 + m$Stillbirth_ratio)))
    expected_adjusted <- m$Adjusted_births_ls * m$Multiplicity_factor * m$Bookings_uplift_modifier
    expected_adjusted[m$Exact_age_live_births == 0] <- 0
    qa_check(paste(label, "all four modifiers feed the adjusted maternity total"),
      projection_numbers_match(m$Adjusted_maternities, expected_adjusted))
    missing_uplift <- unique(m[!m$Projection_available, c("LA", "Age", "ONS_age")])
    qa_details[[paste(label, "missing uplift evidence")]] <- missing_uplift
    qa_check(paste(label, "positive projections have same-age uplift evidence"),
      !nrow(missing_uplift), paste(nrow(missing_uplift), "LA/age groups lack an estimate"))
    calibration <- base$allocation_tables$calibration
    qa_details[[paste(label, "booking denominator reconciliation")]] <- calibration
    qa_check(paste(label, "resident totals cover recorded cohort deliveries"),
      all(is.finite(calibration$Resident_birth_gap) & calibration$Resident_birth_gap >= -1e-8),
      paste(sum(!is.finite(calibration$Resident_birth_gap) | calibration$Resident_birth_gap < -1e-8),
        "LA/age groups have missing or smaller resident totals"))
    recorded_cells <- calibration[is.na(calibration$MSDS_resident_count) |
      calibration$MSDS_resident_count > 0 | calibration$Cohort_pregnancies > 0, ]
    qa_check(paste(label, "calibration denominators support nonnegative allocations"),
      all(recorded_cells$Allocation_available), paste(sum(!recorded_cells$Allocation_available), "LA/age cells need review"))
    valid_calibration <- calibration[calibration$Allocation_available, ]
    qa_check(paste(label, "booking residual plus supplied pregnancies equals shared denominator"),
      numbers_match(valid_calibration$Booking_Denominator_Factor + valid_calibration$Cohort_pregnancies,
        valid_calibration$Shared_maternity_denominator))
    qa_check(paste(label, "recorded births plus no-delivery allowance equals shared denominator"),
      numbers_match(valid_calibration$MSDS_resident_count + valid_calibration$Estimated_no_recorded_delivery,
        valid_calibration$Shared_maternity_denominator))
    outside <- calibration[!calibration$LA %in% base$allocation_tables$cluster_resident_las, ]
    qa_check(paste(label, "outside LAs use the same-age cluster uplift"),
      projection_numbers_match(outside$Bookings_uplift_modifier, outside$Cluster_bookings_uplift))
    split_check <- m |> dplyr::group_by(Source_cell_id) |>
      dplyr::summarise(Source = dplyr::first(ONS_live_births), Split = sum(Exact_age_live_births), .groups = "drop")
    qa_check(paste(label, "age split preserves each ONS births (L) total"),
      numbers_match(split_check$Source, split_check$Split, 1e-7))
    for (type in c("Delivery", "Booking")) {
      volume <- m[[paste0(type, "_projection_base")]]
      proportions <- grep(paste0("^", type, "_proportion_"), names(m), value = TRUE)
      counts <- sub("_proportion_", "_count_", proportions, fixed = TRUE)
      qa_check(paste(label, type, "proportions are valid and sum to one"),
        all(as.matrix(m[proportions]) >= 0 & as.matrix(m[proportions]) <= 1) &&
        numbers_match(rowSums(m[proportions]), rep(1, nrow(m))))
      calculated <- ifelse(as.matrix(m[proportions]) == 0, 0, as.matrix(m[proportions]) * volume)
      qa_check(paste(label, type, "counts follow their own proportions"),
        projection_numbers_match(as.matrix(m[counts]), calculated))
      qa_check(paste(label, type, "counts conserve their starting volume"),
        projection_numbers_match(rowSums(m[counts]), volume, 1e-7))
      qa_check(paste(label, type, "proportions stay fixed over future years"),
        nrow(unique(m[c("LA", "Age", proportions)])) == nrow(unique(m[c("LA", "Age")])))
    }
    qa_check(paste(label, "available counts are finite/nonnegative; unavailable counts remain missing"),
      all(is.finite(m$Adjusted_maternities[m$Projection_available]) & m$Adjusted_maternities[m$Projection_available] >= 0) &&
      all(is.na(m$Adjusted_maternities[!m$Projection_available])))
    master_totals <- base$delivery_rows |>
      dplyr::filter(Delivery_destination %in% cluster_trusts) |>
      dplyr::group_by(Year) |> dplyr::summarise(N = sum(Projected_maternities), .groups = "drop")
    presented <- base$maternity_projections |> dplyr::filter(Level == "Cluster") |> dplyr::arrange(Year)
    master_totals <- data.frame(Year = presented$Year) |>
      dplyr::left_join(master_totals, by = "Year") |> dplyr::mutate(N = dplyr::coalesce(N, 0))
    qa_check(paste(label, "available delivery allocations come from the master"),
      numbers_match(presented$Available_projected, master_totals$N))
    qa_details[[paste(label, "provider coverage gaps")]] <- base$provider_projection_gaps
    provider_gaps <- base$provider_projection_gaps
    qa_check(paste(label, "provider flows within the model age range have matching projections"),
      !nrow(provider_gaps),
      if (nrow(provider_gaps)) {
        reasons <- provider_gaps |> dplyr::filter(Level == "Cluster") |>
          dplyr::group_by(Allocation, Reason) |>
          dplyr::summarise(Rows = sum(Cohort_rows), .groups = "drop")
        paste(paste(reasons$Allocation, reasons$Rows, reasons$Reason, sep = ": "), collapse = "; ")
      } else "No unmatched provider flows")
    for (shown in list(base$maternity_projections, base$booking_total_projections)) {
      usable <- shown$Coverage_status != "No usable estimate"
      qa_check(paste(label, if (identical(shown, base$maternity_projections)) "delivery" else "booking",
        "partial estimates remain visible and unsupported estimates remain missing"),
        all(is.na(shown$Projected[!usable])) && all(is.na(shown$Percent_change[!usable])) &&
          numbers_match(shown$Projected[usable], shown$Available_projected[usable]) &&
          all(shown$Coverage_status[!shown$Projection_complete & usable] == "Partial"))
    }
    qa_check(paste(label, "provider coverage partitions recorded activity without losing rows"),
      all(base$provider_coverage$Matched_rows + base$provider_coverage$Unmatched_rows ==
        base$provider_coverage$Cohort_rows))
    delivery_comparison <- base$maternity_projections
    expected_change <- ifelse(delivery_comparison$Observed_matched_baseline > 0,
      delivery_comparison$Projected / delivery_comparison$Observed_matched_baseline - 1, NA_real_)
    qa_check(paste(label, "delivery changes use matching observed LA/age coverage"),
      projection_numbers_match(delivery_comparison$Percent_change, expected_change))
    totals <- base$maternity_projections |> dplyr::group_by(Level, Year) |>
      dplyr::summarise(N = sum(Available_projected), .groups = "drop") |>
      tidyr::pivot_wider(names_from = Level, values_from = N)
    qa_check(paste(label, "delivery trust and ICB totals reconcile to cluster"),
      numbers_match(totals$Trust, totals$Cluster, 1e-7) && numbers_match(totals$ICB, totals$Cluster, 1e-7))
    booking_sums <- lapply(base$booking_totals, function(x) x |>
      dplyr::group_by(Year) |> dplyr::summarise(N = sum(Projected), .groups = "drop"))
    qa_check(paste(label, "booking trust and ICB totals reconcile to cluster"),
      numbers_match(booking_sums$Trust$N, booking_sums$Cluster$N, 1e-7) &&
      numbers_match(booking_sums$ICB$N, booking_sums$Cluster$N, 1e-7))
    booking_display <- base$booking_total_projections |> dplyr::group_by(Level, Year) |>
      dplyr::summarise(N = sum(Available_projected), .groups = "drop") |>
      tidyr::pivot_wider(names_from = Level, values_from = N)
    qa_check(paste(label, "displayed booking trust rows reconcile to ICB and cluster"),
      numbers_match(booking_display$Trust, booking_display$Cluster, 1e-7) &&
      numbers_match(booking_display$ICB, booking_display$Cluster, 1e-7))
    booking_master <- base$booking_rows |>
      dplyr::filter(Booking_destination %in% cluster_trusts) |>
      dplyr::group_by(Year) |> dplyr::summarise(N = sum(Projected_bookings), .groups = "drop")
    booking_report <- base$booking_total_projections |> dplyr::filter(Level == "Cluster") |> dplyr::arrange(Year)
    booking_bands <- base$booking_age_projections |> dplyr::filter(Key == "cluster") |>
      dplyr::group_by(Year) |> dplyr::summarise(N = sum(Available_projected), .groups = "drop")
    booking_master <- data.frame(Year = booking_report$Year) |>
      dplyr::left_join(booking_master, by = "Year") |> dplyr::mutate(N = dplyr::coalesce(N, 0))
    booking_bands <- data.frame(Year = booking_report$Year) |>
      dplyr::left_join(booking_bands, by = "Year") |> dplyr::mutate(N = dplyr::coalesce(N, 0))
    qa_check(paste(label, "booking chart totals and age bands come from the master"),
      numbers_match(booking_master$N, booking_report$Available_projected) && numbers_match(booking_master$N, booking_bands$N))
    # Check each ICB separately: a correct grand total can conceal switched ICB rows.
    for (type in c("Delivery", "Booking")) {
      displayed <- if (type == "Delivery") base$maternity_projections else base$booking_total_projections
      trust_to_icb <- displayed |> dplyr::filter(Level == "Trust") |>
        dplyr::left_join(base$organisations |> dplyr::select(Key, Code), by = "Key", suffix = c("", "_lookup")) |>
        dplyr::mutate(ICB_key = paste0("icb-", tolower(unname(provider_icb[Code])))) |>
        dplyr::group_by(ICB_key, Year) |>
        dplyr::summarise(Trust_sum = sum(Available_projected), .groups = "drop")
      icb_check <- displayed |> dplyr::filter(Level == "ICB") |>
        dplyr::left_join(trust_to_icb, by = c("Key" = "ICB_key", "Year"))
      qa_check(paste(label, type, "each ICB equals its own trusts"),
        numbers_match(icb_check$Available_projected, icb_check$Trust_sum, 1e-7))
    }
    # Descriptive source totals include records without an ONS match.
    pooled_cluster <- base$allocation_tables$delivery_observed |>
      dplyr::filter(Destination %in% cluster_trusts) |>
      dplyr::group_by(LA, Age) |> dplyr::summarise(Expected = sum(Count), .groups = "drop")
    feeder_check <- pooled_cluster |> dplyr::left_join(
      base$feeder_shares |> dplyr::filter(Level == "Cluster"), by = c("LA", "Age"))
    qa_check(paste(label, "descriptive feeder numerators retain pooled MSDS counts"),
      numbers_match(feeder_check$Expected, feeder_check$Numerator))
    feeder_denominators <- base$feeder_shares |>
      dplyr::left_join(calibration |> dplyr::select(LA, Age, Shared_maternity_denominator),
        by = c("LA", "Age"))
    qa_check(paste(label, "descriptive feeder shares use the shared denominator"),
      projection_numbers_match(feeder_denominators$Denominator,
        feeder_denominators$Shared_maternity_denominator))
    projected_denominators <- base$feeder_projections |>
      dplyr::left_join(calibration |> dplyr::select(LA, Age, Shared_maternity_denominator),
        by = c("LA", "Age"))
    qa_check(paste(label, "projected feeder rows use their allocation denominator"),
      projection_numbers_match(projected_denominators$Denominator,
        projected_denominators$Shared_maternity_denominator) &&
      projection_numbers_match(projected_denominators$Share,
        projected_denominators$Numerator / projected_denominators$Shared_maternity_denominator))
    source_growth <- m |> dplyr::distinct(LA, ONS_age, Year, ONS_live_births) |>
      dplyr::group_by(LA, ONS_age) |>
      dplyr::mutate(Reference = ONS_live_births[Year == base$ons_reference_year],
        Expected = dplyr::if_else(Reference > 0, ONS_live_births / Reference - 1, NA_real_)) |>
      dplyr::ungroup() |>
      dplyr::left_join(base$ons_maternity_projections, by = c("LA", "ONS_age", "Year"))
    qa_check(paste(label, "detailed ONS growth uses source births once per LA and source age"),
      projection_numbers_match(source_growth$ONS_live_births, source_growth$ONS_source_births) &&
      projection_numbers_match(source_growth$Expected, source_growth$ONS_change))
    gap_view <- base$booking_gap_projections |>
      dplyr::left_join(m |> dplyr::select(LA, Age, ONS_age, Year, Exact_age_live_births,
        Projection_available, Allocation_available, Booking_count_Booking_Denominator_Factor),
        by = c("LA", "Age", "ONS_age", "Year"))
    available_gap <- gap_view$Exact_age_live_births == 0 |
      (gap_view$Projection_available & gap_view$Allocation_available)
    qa_check(paste(label, "booking gap estimates preserve unavailable allocations"),
      all(is.na(gap_view$Booking_Denominator_Factor[!available_gap])) &&
      projection_numbers_match(gap_view$Booking_Denominator_Factor[available_gap],
        gap_view$Booking_count_Booking_Denominator_Factor[available_gap]))
    # Resident estimates in section 2 use the master values before allocation.
    resident_view <- readRDS(".ons_exploration.rds")
    resident_comparison <- m |>
      dplyr::filter(LA %in% read_area_config()$area_code) |>
      dplyr::group_by(LA, Year) |>
      dplyr::summarise(Expected = sum(Adjusted_maternities), .groups = "drop") |>
      dplyr::left_join(resident_view$total_maternity_projections,
        by = c("LA" = "area_code", "Year" = "year"))
    qa_check(paste(label, "resident chart totals equal the shared master before allocation"),
      projection_numbers_match(resident_comparison$Expected, resident_comparison$projected_maternities))
    resident_bands <- resident_view$age_band_projections |>
      dplyr::group_by(area_code, year) |>
      dplyr::summarise(Bands = sum(projected_maternities), .groups = "drop") |>
      dplyr::left_join(resident_view$total_maternity_projections, by = c("area_code", "year"))
    qa_check(paste(label, "resident age bands including unsplit counts conserve totals"),
      projection_numbers_match(resident_bands$Bands, resident_bands$projected_maternities))
    qa_check(paste(label, "resident display retains 2023-2047"),
      all(2023:2047 %in% resident_view$total_maternity_projections$year))
    qa_details[[paste(label, "unmatched LA codes")]] <- attr(m, "unmatched_las")
    qa_details[[paste(label, "unallocated contributions")]] <- base$delivery_rows |>
      dplyr::filter(!Allocation_available, Projected_maternities > 0)
  }
} else {
  qa_check("Section 3 has saved outputs available", FALSE, "Run Run analysis.R first")
}

# Reconcile sections that claim to describe the same delivery activity.
# Booking-year pathway starts deliberately use a different denominator.
if (all(file.exists(c(".patient_flows.rds", ".delivery_geography.rds", ".projections.rds")))) {
  section9_totals <- delivery_mapping$hospital_mapping |>
    dplyr::group_by(Key, Year) |>
    dplyr::summarise(Section9 = sum(N), .groups = "drop")
  reconciliation <- projections$backtest$data |>
    dplyr::transmute(Key, Organisation, Year = Year_label, Section10_observed = Observed) |>
    dplyr::left_join(section9_totals, by = c("Key", "Year")) |>
    dplyr::mutate(Section9 = dplyr::coalesce(Section9, 0L))
  qa_details$delivery_reconciliation <- reconciliation
  qa_check("Geography and projection views count the same deliveries for every organisation/year",
    numbers_match(reconciliation$Section9, reconciliation$Section10_observed))
  cluster_reconciliation <- reconciliation |> dplyr::filter(Key == "cluster") |>
    dplyr::left_join(patient_flows$cluster_activity$delivery_trend |>
      dplyr::select(Year, Section6 = Total), by = "Year")
  qa_details$cluster_reconciliation <- cluster_reconciliation
  qa_check("Activity, geography and projection views have identical annual cluster delivery totals",
    numbers_match(cluster_reconciliation$Section6, cluster_reconciliation$Section9))
  for (year in qa_years) {
    flows <- patient_flows$by_year[[year]]
    expected <- sum(prepared_records$Year %in% year & prepared_records$Birth_recorded &
      prepared_records$Home %in% 0)
    qa_check(paste("Section 2.2.5 uses the same recorded non-home definition:", year),
      numbers_match(c(sum(flows$trust_flow$links$n), sum(flows$icb_flow$links$n)), rep(expected, 2)))
    cluster_links <- flows$icb_flow$links
    cluster_n <- sum(cluster_links$n[cluster_links$target %in% cluster_icbs])
    qa_check(paste("Patient-flow and activity views agree on deliveries at cluster providers:", year),
      numbers_match(cluster_n, cluster_reconciliation$Section6[cluster_reconciliation$Year == year]))
  }
  qa_check("The latest-year and annual patient flows use identical latest-year flows",
    identical(patient_flows$icb_flow, patient_flows$by_year[["2025/26"]]$icb_flow) &&
    identical(patient_flows$trust_flow, patient_flows$by_year[["2025/26"]]$trust_flow))

}

# 7. Show only items needing attention; keep the complete results for inspection.
qa_issues <- qa_results[qa_results$Result == "Check", , drop = FALSE]
qa_issues$Check <- sub("^Reference [0-9]{4}/[0-9]{2} ", "", qa_issues$Check)
qa_issues <- unique(qa_issues)
message("QA: ", sum(qa_results$Result == "OK"), " passed",
  if (nrow(qa_issues)) paste0("; ", nrow(qa_issues), " to review") else "", ".")
for (i in seq_len(nrow(qa_issues))) {
  message("- ", qa_issues$Check[i],
    if (nzchar(qa_issues$Details[i])) paste0(": ", qa_issues$Details[i]) else "")
}
if (nrow(qa_issues)) message("Full details are saved in qa_details; repeated projection years are not listed here.")
