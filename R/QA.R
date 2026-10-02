# Run the counting checks when you want to QA a new dataset or changed analysis.
# Run after Run analysis.R, in the same secure R session.
# This script does not make charts, change inputs or overwrite report outputs.

# 1. Get the inputs and start a readable list of results.
required_inputs <- c("final_cohort_2326", "total_births_by_LA_2326",
                     "icb_matrix_data", "trust_matrix_data")
missing_inputs <- required_inputs[!vapply(required_inputs,
  function(name) exists(name, inherits = TRUE), logical(1))]
if (length(missing_inputs)) stop("Load the Azure data first. Missing: ",
                               paste(missing_inputs, collapse = ", "), call. = FALSE)
source("R/ons_data.R")
source("R/report_cohort.R")
qa_years <- c("2023/24", "2024/25", "2025/26")
qa_age_bands <- report_age_bands
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
home_flags <- suppressWarnings(as.numeric(as.character(recorded_deliveries$home_birth)))
qa_check("Recorded deliveries have home flags of 0 or 1", all(home_flags %in% c(0, 1)),
         paste(sum(!home_flags %in% c(0, 1)), "records have missing or invalid home flags"))
resident_counts <- suppressWarnings(as.numeric(as.character(total_births_by_LA_2326$count)))
valid_resident_counts <- is.finite(resident_counts) & resident_counts >= 0 &
  resident_counts == floor(resident_counts)
qa_check("Resident LA counts are nonnegative whole numbers", all(valid_resident_counts),
         paste(sum(!valid_resident_counts), "invalid counts"))
repeated_resident_cells <- duplicated(total_births_by_LA_2326[
  c("birth_yr", "la_of_mother", "age_at_birth_mother")])
qa_check("Resident totals have one row per LA, age and year", !any(repeated_resident_cells),
         paste(sum(repeated_resident_cells), "repeated LA/age/year rows"))
for (matrix_name in c("icb_matrix_data", "trust_matrix_data")) {
  matrix_data <- get(matrix_name)
  matrix_counts <- suppressWarnings(as.numeric(as.character(matrix_data$n)))
  qa_check(paste(matrix_name, "has valid counts"),
    nrow(matrix_data) > 0 && length(matrix_counts) == nrow(matrix_data) &&
      all(is.finite(matrix_counts) & matrix_counts >= 0), "Uses the n column")
}

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

# 4. Check sections 4-8, using the saved calculations rather than remaking charts.
if (file.exists(".patient_flows.rds")) {
  patient_flows <- readRDS(".patient_flows.rds")
  counted_episodes <- patient_flows$activity_episodes
  qa_check("Sections 4-8 retain every supplied pregnancy row",
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
  qa_check("Section 6 trust delivery totals use recorded non-home births",
    numbers_match(patient_flows$cluster_activity$delivery_trend$Total, hospital_counts))
  latest_pathways <- patient_flows$cluster_activity$pathways
  latest_cluster <- latest_pathways[latest_pathways$Level == "Cluster", , drop = FALSE]
  pathway_columns <- c("Starts", "Within", "Outside", "No_delivery", "Unknown")
  qa_check("Section 6 combined-cluster starts and outcomes equal the sum of its two ICBs",
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
  qa_check("Section 4 has residence-to-booking-to-delivery flows available", !is.null(three_stage),
    "Run R/Run analysis.R again if these outputs are missing")
  if (!is.null(three_stage)) {
    expected_flow_total <- sum(patient_flows$trust_flow$links$n)
    qa_check("Section 4's three-stage paths and both legs count the same deliveries as the trust flow",
      numbers_match(c(sum(three_stage$paths$n), sum(three_stage$residence_booking$n),
        sum(three_stage$booking_birth$n)), rep(expected_flow_total, 3)))
    qa_check("Section 4's three-stage paths have unique complete groups and positive counts",
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
    qa_check("Section 4's three-stage flow conserves deliveries at every booking organisation",
      numbers_match(booking_conservation$Incoming, booking_conservation$Outgoing))
    booking_birth_check <- dplyr::full_join(
      patient_flows$trust_flow$links |> dplyr::rename(Trust_flow = n),
      three_stage$booking_birth |> dplyr::rename(Three_stage = n), by = c("source", "target"))
    qa_details$residence_booking_birth_reconciliation <- booking_birth_check
    qa_check("Section 4's three-stage booking-to-delivery counts match the existing trust flow",
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
  qa_check("Section 7 trust and ICB numerators and denominators add up to the combined cluster",
    numbers_match(outcome_levels$N_Trust, outcome_levels$N_Cluster) &&
      numbers_match(outcome_levels$N_ICB, outcome_levels$N_Cluster) &&
      numbers_match(outcome_levels$Starts_Trust, outcome_levels$Starts_Cluster) &&
      numbers_match(outcome_levels$Starts_ICB, outcome_levels$Starts_Cluster))
  change_table <- patient_flows$delivery_over_time$change_data
  qa_check("Section 7 change table has one organisation per row and all outcome/year columns",
    nrow(change_table) == nrow(patient_flows$delivery_over_time$organisations) &&
      ncol(change_table) == 2 + 4 * length(qa_years))

  booking_outcomes <- patient_flows$booking_delivery_over_time
  qa_check("Section 8 has the new booking-to-delivery outputs available",
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
    qa_check("Section 8's five outcomes partition all starts with one common denominator",
      setequal(unique(booking_outcomes$data$Outcome), expected_outcomes) &&
        all(booking_counts$Categories == 5L & booking_counts$Denominators == 1L) &&
        numbers_match(booking_counts$Outcomes, booking_counts$Starts))

    # Collapsing the two within-cluster destinations must reproduce section 7 exactly.
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
    qa_check("Sections 7 and 8 have the same starts and outcomes after combining the within-cluster split",
      numbers_match(outcome_reconciliation$Section7, outcome_reconciliation$Section8) &&
        numbers_match(outcome_reconciliation$Section7_starts, outcome_reconciliation$Section8_starts))

    booking_levels <- booking_outcomes$data |>
      dplyr::group_by(Level, Year_start, Outcome) |>
      dplyr::summarise(N = sum(Numerator), Starts = sum(Denominator), .groups = "drop") |>
      tidyr::pivot_wider(names_from = Level, values_from = c(N, Starts))
    qa_details$booking_delivery_outcome_levels <- booking_levels
    qa_check("Section 8 trust and ICB numerators and denominators add up to the combined cluster",
      numbers_match(booking_levels$N_Trust, booking_levels$N_Cluster) &&
        numbers_match(booking_levels$N_ICB, booking_levels$N_Cluster) &&
        numbers_match(booking_levels$Starts_Trust, booking_levels$Starts_Cluster) &&
        numbers_match(booking_levels$Starts_ICB, booking_levels$Starts_Cluster))
    qa_check("Section 8 change table has one trust, ICB or cluster per row and all five outcomes for each year",
      nrow(booking_outcomes$change_data) == nrow(booking_outcomes$organisations) &&
        ncol(booking_outcomes$change_data) == 2 + 5 * length(qa_years))
  }
  booking_icb_outcomes <- patient_flows$booking_icb_delivery_over_time
  qa_check("Section 8 has booking-ICB-to-delivery-ICB outcomes available",
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
    qa_check("Section 8's ICB comparison partitions the same starts into five outcomes",
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
    qa_check("Section 8's ICB comparison reconciles to section 7 after combining both cluster destinations",
      numbers_match(icb_reconciliation$ICB_comparison, icb_reconciliation$Section7) &&
        numbers_match(icb_reconciliation$ICB_starts, icb_reconciliation$Section7_starts))
    icb_levels <- booking_icb_outcomes$data |>
      dplyr::group_by(Level, Year_start, Outcome) |>
      dplyr::summarise(N = sum(Numerator), Starts = sum(Denominator), .groups = "drop") |>
      tidyr::pivot_wider(names_from = Level, values_from = c(N, Starts))
    qa_check("Section 8's ICB comparison adds up to the combined cluster",
      numbers_match(icb_levels$N_ICB, icb_levels$N_Cluster) &&
        numbers_match(icb_levels$Starts_ICB, icb_levels$Starts_Cluster))
    icb_changes <- booking_icb_outcomes$data |>
      dplyr::group_by(Key, Outcome) |>
      dplyr::mutate(Base_share = Share[Year_start == 2023L],
        Expected_change = ifelse(is.finite(Base_share) & Base_share > 0,
          Share / Base_share - 1, NA_real_)) |>
      dplyr::ungroup()
    usable_icb_changes <- is.finite(icb_changes$Expected_change)
    qa_check("Section 8's ICB changes are calculated from the grouped shares",
      numbers_match(icb_changes$Base_change[usable_icb_changes],
        icb_changes$Expected_change[usable_icb_changes]) &&
        all(is.na(icb_changes$Base_change[!usable_icb_changes])))
    qa_check("Section 8's ICB change table has all five outcomes for each year",
      nrow(booking_icb_outcomes$change_data) == nrow(booking_icb_outcomes$organisations) &&
        ncol(booking_icb_outcomes$change_data) == 2 + 5 * length(qa_years))
  }
} else {
  qa_check("Sections 4-8 have saved outputs available", FALSE, "Run R/Run analysis.R first")
}

# 5. Check section 9's destination flows and hospital counts.
if (file.exists(".delivery_geography.rds")) {
  delivery_mapping <- readRDS(".delivery_geography.rds")
  delivery_records <- delivery_mapping$episodes
  qa_check("Section 9 retains every recorded delivery in the three years",
    identical(delivery_records$Row, prepared_records$Row[prepared_records$Birth_recorded &
      prepared_records$Year %in% qa_years]))
  qa_check("Section 9 includes all required years", all(qa_years %in% delivery_records$Year))
  destination_total <- nrow(delivery_mapping$matrix_episodes)
  qa_check("Section 9 matrix counts add up to its recorded deliveries",
    numbers_match(sum(delivery_mapping$matrix_data$N), destination_total))
  qa_check("Section 9 Sankey counts match the destination matrix",
    numbers_match(sum(delivery_mapping$sankey_links$N), destination_total))
  qa_check("Section 9 grouped matrix retains all destination counts",
    numbers_match(sum(delivery_mapping$grouped_matrix$N), destination_total))
  qa_check("Section 9.1 destination grid shows no more than ten additional organisations",
    length(delivery_mapping$visible_other_trusts) <= delivery_mapping$sankey_focus_n)
  # Check the separately grouped charts against their original delivery counts.
  la_links <- delivery_mapping$sankey_by_la_links
  destination_links <- delivery_mapping$sankey_by_destination_links
  qa_check("Section 9.1 LA charts retain all recorded deliveries",
    numbers_match(sum(vapply(la_links, function(links) sum(links$N), numeric(1))), destination_total))
  qa_check("Section 9.1 destination charts retain all recorded deliveries",
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
    qa_check(paste("Section 9.1 LA chart keeps its deliveries and home births:", origin),
      numbers_match(c(sum(links$N), sum(links$N[links$Destination_group == "Home"])),
        c(sum(original$N), sum(original$N[original$Destination == "Home"]))))
    extra_trusts <- setdiff(unique(links$Destination_group),
      c(main_trust_codes, "Home", "Unknown provider", "Unknown location", "All other trusts"))
    qa_check(paste("Section 9.1 LA chart has up to five other trusts, ranked by volume:", origin),
      length(extra_trusts) <= delivery_mapping$sankey_top_n && all(diff(links$N) <= 0))
  }
  for (destination in names(destination_links)) {
    links <- destination_links[[destination]]
    original <- chart_counts[chart_counts$Destination_focus == destination, , drop = FALSE]
    qa_check(paste("Section 9.1 destination chart keeps its recorded deliveries:", destination),
      numbers_match(sum(links$N), sum(original$N)))
    extra_las <- setdiff(unique(links$Origin),
      c(cluster_la_codes, "Unknown LA", "All other feeder LAs"))
    qa_check(paste("Section 9.1 destination chart has up to five other LAs, ranked by volume:", destination),
      length(extra_las) <= delivery_mapping$sankey_top_n && all(diff(links$N) <= 0))
  }
  qa_details$unmatched_la_names <- delivery_records |>
    dplyr::filter(LA_name == "Unmatched LA") |> dplyr::distinct(LA)
  qa_details$unmatched_trust_names <- delivery_mapping$matrix_data |>
    dplyr::filter(Delivery_destination == "Unmatched delivery provider") |>
    dplyr::distinct(Destination)
  qa_check("Section 9 resident LA codes have names in the local lookup",
    !nrow(qa_details$unmatched_la_names))
  qa_check("Section 9 delivery provider codes have names in the local lookup",
    !nrow(qa_details$unmatched_trust_names))
  for (key in names(delivery_mapping$outputs)) {
    output <- delivery_mapping$outputs[[key]]
    if (is.null(output$trend_groups)) next # No activity at this organisation.
    original <- delivery_mapping$la_trends |>
      dplyr::filter(Key == key) |> dplyr::group_by(Year) |>
      dplyr::summarise(N = sum(N), .groups = "drop") |> dplyr::arrange(Year)
    charted <- output$trend_groups |> dplyr::group_by(Year) |>
      dplyr::summarise(N = sum(N), .groups = "drop") |> dplyr::arrange(Year)
    qa_check(paste("Section 9 LA chart groups retain every count:", key),
      identical(original$Year, charted$Year) && numbers_match(original$N, charted$N))
    if (!is.null(output$age_mix)) {
      age_totals <- output$age_mix |> dplyr::group_by(LA) |>
        dplyr::summarise(Share = sum(Age_share), .groups = "drop")
      qa_check(paste("Section 9 age mix adds up to 100% within each LA group:", key),
        numbers_match(age_totals$Share, rep(1, nrow(age_totals))))
    }
  }
  hospital_levels <- delivery_mapping$hospital_mapping |>
    dplyr::group_by(Level, Year) |>
    dplyr::summarise(N = sum(N), .groups = "drop") |>
    tidyr::pivot_wider(names_from = Level, values_from = N)
  qa_details$hospital_delivery_levels <- hospital_levels
  qa_check("Section 9 trust and ICB deliveries add up to the combined cluster",
    numbers_match(hospital_levels$Trust, hospital_levels$Cluster) &&
      numbers_match(hospital_levels$ICB, hospital_levels$Cluster))
} else {
  qa_check("Section 9 has saved outputs available", FALSE, "Run R/Run analysis.R first")
}

# 6. Check section 10's shares, growth and totals, including the historical comparison.
if (file.exists(".projections.rds")) {
  projections <- readRDS(".projections.rds")
  catchment_codes <- readr::read_csv("config/local_authorities.csv", show_col_types = FALSE)$area_code
  for (key in names(projections$mapping_outputs)) {
    mapping <- projections$mapping_outputs[[key]]
    if (is.null(mapping$flow)) next
    baseline_count <- projections$observed_totals$Observed_baseline[projections$observed_totals$Key == key]
    qa_check(paste("Section 10.1 grouped feeder counts retain the actual baseline:", key),
      numbers_match(sum(mapping$flow$N), baseline_count))
    qa_check(paste("Section 10.1 shows no more than ten additional feeder LAs:", key),
      length(setdiff(mapping$visible_las, catchment_codes)) <= 10L)
  }
  projection_trusts <- projections$organisations |>
    dplyr::filter(Level == "Trust") |>
    dplyr::pull(Code)
  cluster_hospital_records <- recorded_deliveries |>
    dplyr::filter(toupper(trimws(as.character(birth_provider))) %in% projection_trusts,
                  as.character(home_birth) == "0")
  qa_check("Cluster-trust hospital deliveries cover all three comparison years",
    all(qa_years %in% as.character(cluster_hospital_records$birth_yr)))
  # Check both the 2025/26 projection base and the separate 2023/24 comparison base.
  for (base in list(projections, projections$backtest$baseline)) {
    base_label <- base$baseline_fy
    qa_check(paste(base_label, "has recorded cluster-trust deliveries"),
      sum(base$observed_totals$Observed_baseline) > 0)
    qa_check(paste(base_label, "coverage shows both exclusions and held-constant records"),
      numbers_match(base$coverage$Recorded_births, base$coverage$Home_births +
        base$coverage$Unknown_home_flag + base$coverage$Observed_baseline) &&
      numbers_match(base$coverage$Observed_baseline, base$coverage$ONS_growth + base$coverage$Unmatched))
    occupied_shares <- base$feeder_shares |> dplyr::filter(Numerator > 0)
    valid_denominators <- is.finite(occupied_shares$Denominator) &
      occupied_shares$Denominator > 0 & occupied_shares$Numerator <= occupied_shares$Denominator
    qa_details[[paste0(base_label, " invalid denominators")]] <- occupied_shares[!valid_denominators, ]
    qa_check(paste(base_label, "feeder counts fit within resident LA totals"),
      all(valid_denominators), paste(sum(!valid_denominators), "invalid LA/age denominators"))
    qa_check(paste(base_label, "feeder shares are between 0% and 100%"),
      all(is.finite(occupied_shares$Share) & occupied_shares$Share >= 0 & occupied_shares$Share <= 1))
    feeder_levels <- base$feeder_shares |>
      dplyr::group_by(Level) |>
      dplyr::summarise(N = sum(Numerator), .groups = "drop") |>
      tidyr::pivot_wider(names_from = Level, values_from = N)
    qa_check(paste(base_label, "trust and ICB feeder counts add up to the combined cluster"),
      numbers_match(feeder_levels$Trust, feeder_levels$Cluster) &&
        numbers_match(feeder_levels$ICB, feeder_levels$Cluster))
    qa_check(paste(base_label, "other destinations have nonnegative counts"),
      all(base$destination_accounting$Other_or_unrepresented >= 0))
    expected_ons <- tidyr::expand_grid(LA = unique(base$ons_maternity_projections$LA),
      Age_band = base$age_bands, Year = base$projection_years)
    missing_ons <- dplyr::anti_join(expected_ons, base$ons_maternity_projections,
                                   by = c("LA", "Age_band", "Year"))
    qa_details[[paste0(base_label, " missing ONS cells")]] <- missing_ons
    qa_check(paste(base_label, "ONS covers every feeder LA, age band and year"),
      !nrow(missing_ons) &&
        !anyDuplicated(base$ons_maternity_projections[c("LA", "Age_band", "Year")]) &&
        all(is.finite(base$ons_maternity_projections$ONS_maternities) &
              base$ons_maternity_projections$ONS_maternities >= 0),
      paste(nrow(missing_ons), "missing cells"))
    english_las_without_ons <- setdiff(
      unique(occupied_shares$LA[startsWith(occupied_shares$LA, "E")]),
      unique(base$ons_maternity_projections$LA))
    qa_check(paste(base_label, "English feeder LA codes match ONS"),
      !length(english_las_without_ons), paste(english_las_without_ons, collapse = ", "))
    active_flows <- base$feeder_projections |>
      dplyr::filter(!Held_constant, Numerator > 0)
    qa_check(paste(base_label, "occupied feeder cells have usable ONS growth"),
      all(is.finite(active_flows$Growth_factor)))
    qa_check(paste(base_label, "incoming flows follow their age-specific ONS growth"),
      numbers_match(active_flows$Projected, active_flows$Numerator * active_flows$Growth_factor))
    chart_growth <- base$weighted_age_growth |> dplyr::filter(Baseline > 0)
    qa_check(paste(base_label, "age-growth chart agrees with incoming maternity counts"),
      numbers_match(chart_growth$Baseline * (1 + chart_growth$Growth_change), chart_growth$Projected))
    zero_base_growth <- base$weighted_age_growth |> dplyr::filter(Baseline == 0)
    qa_check(paste(base_label, "zero age-band baselines have no percentage change"),
      all(is.na(zero_base_growth$Growth_change)))
    held_flows <- base$feeder_projections |> dplyr::filter(Held_constant)
    qa_check(paste(base_label, "unmatched records retain their actual baseline counts"),
      numbers_match(held_flows$Projected, held_flows$Numerator))
    baseline_counts <- base$maternity_projections |>
      dplyr::filter(Year == base$ons_reference_year)
    qa_check(paste(base_label, "projected starting counts equal observed counts"),
      numbers_match(baseline_counts$Projected, baseline_counts$Observed_baseline))
    projection_levels <- base$maternity_projections |>
      dplyr::group_by(Level, Year) |>
      dplyr::summarise(N = sum(Projected), .groups = "drop") |>
      tidyr::pivot_wider(names_from = Level, values_from = N)
    qa_details[[paste0(base_label, " projected totals")]] <- projection_levels
    qa_check(paste(base_label, "trust and ICB projections add up to the combined cluster"),
      numbers_match(projection_levels$Trust, projection_levels$Cluster, 1e-7) &&
        numbers_match(projection_levels$ICB, projection_levels$Cluster, 1e-7))
  }
  historical_comparison <- projections$backtest$data
  qa_check("Historical comparison has observed counts for each organisation and year",
    !anyNA(historical_comparison$Observed) &&
      all(qa_years %in% historical_comparison$Year_label))
  historical_base <- historical_comparison |>
    dplyr::filter(Year_label == "2023/24")
  qa_check("Historical model starts exactly at actual 2023/24 counts",
    numbers_match(historical_base$Difference, rep(0, nrow(historical_base))))
} else {
  qa_check("Section 10 has saved outputs available", FALSE, "Run R/Run analysis.R first")
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
  qa_check("Sections 9 and 10 count the same deliveries for every organisation/year",
    numbers_match(reconciliation$Section9, reconciliation$Section10_observed))
  cluster_reconciliation <- reconciliation |> dplyr::filter(Key == "cluster") |>
    dplyr::left_join(patient_flows$cluster_activity$delivery_trend |>
      dplyr::select(Year, Section6 = Total), by = "Year")
  qa_details$cluster_reconciliation <- cluster_reconciliation
  qa_check("Sections 6, 9 and 10 have identical annual cluster delivery totals",
    numbers_match(cluster_reconciliation$Section6, cluster_reconciliation$Section9))
  for (year in qa_years) {
    flows <- patient_flows$by_year[[year]]
    expected <- sum(prepared_records$Year %in% year & prepared_records$Birth_recorded &
      prepared_records$Home %in% 0)
    qa_check(paste("Section 5 uses the same recorded non-home definition:", year),
      numbers_match(c(sum(flows$trust_flow$links$n), sum(flows$icb_flow$links$n)), rep(expected, 2)))
    cluster_links <- flows$icb_flow$links
    cluster_n <- sum(cluster_links$n[cluster_links$target %in% cluster_icbs])
    qa_check(paste("Sections 5 and 6 agree on deliveries at cluster providers:", year),
      numbers_match(cluster_n, cluster_reconciliation$Section6[cluster_reconciliation$Year == year]))
  }
  qa_check("Sections 4 and 5 use identical latest-year flows",
    identical(patient_flows$icb_flow, patient_flows$by_year[["2025/26"]]$icb_flow) &&
    identical(patient_flows$trust_flow, patient_flows$by_year[["2025/26"]]$trust_flow))
  for (base in list(projections, projections$backtest$baseline)) {
    historical_cells <- delivery_mapping$hospital_mapping |>
      dplyr::filter(Year == base$baseline_fy) |>
      dplyr::select(Key, LA, Age_band, Section9 = N)
    forecast_cells <- base$feeder_projections |>
      dplyr::filter(Year == base$ons_reference_year) |>
      dplyr::select(Key, LA, Age_band, Section10 = Projected)
    cells <- dplyr::full_join(historical_cells, forecast_cells, by = c("Key", "LA", "Age_band")) |>
      dplyr::mutate(Section9 = dplyr::coalesce(Section9, 0), Section10 = dplyr::coalesce(Section10, 0))
    qa_check(paste(base$baseline_fy, "sections 9 and 10 agree within every organisation/LA/age cell"),
      numbers_match(cells$Section9, cells$Section10))
    qa_details[[paste0(base$baseline_fy, " age-cell differences")]] <- cells |>
      dplyr::filter(Section9 != Section10)
  }
}

# 7. Check the imaginary example reaches its intended ten-year total.
if (exists("imaginary_example")) {
  example_totals <- imaginary_example$totals$Total
  qa_check("Imaginary example starts at 110 and follows the rounded ONS change",
    length(example_totals) > 0 && numbers_match(example_totals[1], 110) &&
      numbers_match(tail(example_totals, 1), 110 * (1 + imaginary_example$ten_year_change)))
  qa_check("Imaginary example uses two valid public ONS reference totals",
    nrow(imaginary_example$public_totals) == 2 &&
      all(imaginary_example$public_totals$year ==
            c(example_reference_year, example_reference_year + example_horizon)) &&
      all(is.finite(imaginary_example$public_totals$Maternities) &
            imaginary_example$public_totals$Maternities > 0))
}

# 8. Print the results. Detailed count tables remain in qa_details for inspection.
print(qa_results, row.names = FALSE)
message(sum(qa_results$Result == "Check"), " checks need attention. See qa_results and qa_details.")
