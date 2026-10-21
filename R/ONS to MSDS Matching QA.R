# Run from the project root, after loading Sarah's two parquet objects.
# source("R/ONS to MSDS Matching QA.R") or run one numbered section at a time.
# Internal, unrounded console counts. No report rebuild or files written.

library(dplyr)
library(tidyr)
source("R/report_cohort.R")
stopifnot(exists("final_cohort_2326"), exists("total_births_by_LA_2326"))
matching_show <- function(title, x) {
  cat("\n", title, "\n", sep = "")
  print(tibble::as_tibble(x), n = Inf, width = Inf)
}

# 1. Inputs and totals --------------------------------------------------
matching_cohort <- prepare_report_cohort(final_cohort_2326) |>
  select(-Person, -Pregnancy)
matching_residents <- prepare_la_records(total_births_by_LA_2326) |>
  transmute(LA_raw = as.character(la_of_mother), LA = report_la_code(la_of_mother),
    Birth_year = as.character(birth_yr), Due_year = as.character(edd_yr),
    Pregnancy_FY, Group_raw = as.character(group), Pathway_group, Has_delivery, Has_antenatal,
    Age_raw = Age_value, Count_raw = as.character(total_births_by_LA_2326$count), Count = count) |>
  mutate(Age = report_age(Age_raw), Age_band = report_age_band(Age_raw))
matching_ons <- read.csv("data/2022 SNPP Births persons.csv", check.names = FALSE) |>
  pivot_longer(matches("^20[0-9]{2}$"), names_to = "Year", values_to = "Births") |>
  transmute(LA = AREA_CODE, LA_name = AREA_NAME, ONS_age = as.character(AGE_GROUP),
    Year = as.integer(Year), Births = report_number(Births))
matching_names <- read.csv("data/lookups/la_names.csv")
matching_parameters <- read.csv("config/analysis_parameters.csv")
matching_ages <- seq(
  matching_parameters$value[matching_parameters$parameter == "minimum_maternal_age"],
  matching_parameters$value[matching_parameters$parameter == "maximum_maternal_age"])

matching_show("Cohort: pregnancy rows (no year or age exclusions)",
  matching_cohort |> count(Pregnancy_FY, Pregnancy_year_source, Birth_FY, Due_FY,
    Birth_recorded, name = "Pregnancies"))
matching_show("Resident extract: sum count, not number of aggregate rows",
  matching_residents |> group_by(Group_raw, Pathway_group, Pregnancy_FY, Birth_year, Due_year) |>
    summarise(Source_rows = n(), Total = sum(Count), Missing_counts = sum(is.na(Count)),
      Nonfinite_counts = sum(!is.finite(Count)), Negative_counts = sum(Count < 0, na.rm = TRUE),
      .groups = "drop"))
matching_show("Non-numeric, missing, non-finite or negative resident counts",
  matching_residents |> filter(!is.finite(Count) | Count < 0) |>
    count(LA, Birth_year, Due_year, Age_raw, Count_raw, name = "Source_rows"))
matching_show("ONS: projected live babies by calendar year; do not add All ages to age rows",
  matching_ons |> group_by(Year, All_ages_row = ONS_age == "All ages") |>
    summarise(Source_rows = n(), Births = sum(Births), Missing_counts = sum(is.na(Births)),
      .groups = "drop"))
cat("\nThese totals have different units, periods and populations; their difference is not a match failure.\n")

# 2. LA lists: both MSDS extracts, before and after geography mapping -----
matching_las <- bind_rows(
  tibble(Source = "Cohort", LA_raw = as.character(final_cohort_2326$la_of_mother),
    LA = matching_cohort$LA),
  matching_residents |> transmute(Source = "Resident extract", LA_raw, LA))
matching_la_inventory <- bind_rows(
  matching_las |> transmute(Source, Stage = "Raw supplied code", LA = LA_raw),
  matching_las |> transmute(Source, Stage = "2023 harmonised code", LA)) |>
  distinct(Source, Stage, LA)
matching_ons_las <- matching_ons |> distinct(LA)
matching_msds_only_las <- matching_la_inventory |>
  anti_join(matching_ons_las, by = "LA", na_matches = "never") |>
  left_join(matching_names, by = "LA")
matching_ons_only_las <- crossing(Source = unique(matching_las$Source),
    Stage = unique(matching_la_inventory$Stage), LA = matching_ons_las$LA) |>
  anti_join(matching_la_inventory, by = c("Source", "Stage", "LA"), na_matches = "never") |>
  left_join(matching_names, by = "LA")
matching_show("LAs in MSDS but not ONS", matching_msds_only_las)
matching_show("LAs in ONS but not MSDS (national ONS coverage can exceed this extract)",
  matching_ons_only_las)
matching_show("Supplied LA codes changed by cleaning / geography mapping",
  matching_las |> filter(is.na(LA_raw) | LA_raw != LA) |>
    count(Source, LA_raw, LA, name = "Source_rows") |>
    left_join(matching_names, by = "LA"))

# 3. Age bands and source age keys --------------------------------------
matching_show("Cohort age bands: birth age first; booking fallback only for source NA",
  matching_cohort |> count(Age_source, Age_band, In_model_range = Age %in% matching_ages,
    name = "Pregnancies"))
matching_show("Resident age bands: birth age first, booking age when birth age is NA",
  matching_residents |> group_by(Age_band, In_model_range = Age %in% matching_ages) |>
    summarise(Source_rows = n(), Count = sum(Count), Missing_counts = sum(is.na(Count)),
      .groups = "drop"))
matching_show("ONS source ages (15 means <=15; 44 means 44+)",
  matching_ons |> group_by(ONS_age) |>
    summarise(LAs = n_distinct(LA), Years = n_distinct(Year), .groups = "drop"))
matching_msds_ages <- bind_rows(
  matching_cohort |> transmute(Source = "Cohort", Age_raw = Age_value, Age),
  matching_residents |> transmute(Source = "Resident extract", Age_raw, Age)) |>
  distinct() |> mutate(Age_band = report_age_band(Age_raw),
    In_model_range = Age %in% matching_ages,
    ONS_age = if_else(In_model_range, as.character(report_ons_age(Age)), NA_character_))
matching_ons_ages <- matching_ons |> filter(ONS_age != "All ages") |> distinct(ONS_age)
matching_show("MSDS ages without an eligible ONS source key",
  matching_msds_ages |> anti_join(matching_ons_ages, by = "ONS_age", na_matches = "never"))
matching_show("ONS source keys absent from each MSDS extract",
  crossing(Source = unique(matching_msds_ages$Source), ONS_age = matching_ons_ages$ONS_age) |>
    anti_join(matching_msds_ages, by = c("Source", "ONS_age"), na_matches = "never"))

# 4. Provider coverage and reasons, using the current prepared model -----
# Run the usual analysis first with these same inputs for sections 4-5.
if (exists("first_draft_projections")) {
  matching_model <- first_draft_projections
  matching_master <- matching_model$master
  matching_calibration <- matching_model$allocation_tables$calibration
  matching_settings <- new.env()
  for (statement in parse("R/Maternity analysis.R")) {
    if (is.call(statement) && identical(statement[[1]], as.name("<-")) &&
        as.character(statement[[2]])[1] %in% c("cluster_icbs", "icb_provider_codes",
          "provider_icb", "cluster_trusts")) eval(statement, matching_settings)
  }
  matching_cells <- matching_master |> group_by(LA, Age) |>
    summarise(In_master = TRUE, Allocation_available = all(Allocation_available),
      Projection_available = all(Projection_available),
      Calibration_issue = first(Calibration_issue), .groups = "drop")
  matching_provider_rows <- bind_rows(
    matching_cohort |> mutate(Allocation = "Booking", Trust = projection_booking_destination(matching_cohort)),
    matching_cohort |> mutate(Allocation = "Delivery", Trust = projection_delivery_destination(matching_cohort)))
  matching_show("Provider scope: categories outside cluster trusts remain visible here",
    matching_provider_rows |> count(Allocation, Trust,
      In_cluster = Trust %in% matching_settings$cluster_trusts, name = "Pregnancies"))
  matching_provider_rows <- matching_provider_rows |>
    filter(Trust %in% matching_settings$cluster_trusts) |>
    left_join(matching_cells, by = c("LA", "Age")) |>
    mutate(ICB = unname(matching_settings$provider_icb[Trust]),
      Reason = case_when(
        LA %in% c("Unknown LA", "L99999999", "") ~ "Missing/unusable resident LA",
        is.na(Age) ~ "Missing/unusable maternal age",
        !Age %in% matching_model$allocation_tables$ages ~ "Outside projection age range",
        is.na(In_master) ~ "No matching ONS LA/age projection",
        !Allocation_available ~ Calibration_issue,
        !Projection_available ~ "No same-age uplift estimate", TRUE ~ ""),
      LA_in_ONS = LA %in% matching_ons_las$LA, Country = report_la_country(LA))
  matching_levels <- bind_rows(
    matching_provider_rows |> mutate(Level = "Trust", Code = Trust),
    matching_provider_rows |> mutate(Level = "ICB", Code = ICB),
    matching_provider_rows |> mutate(Level = "Cluster", Code = "Cluster")) |>
    left_join(matching_model$organisations |> select(Level, Code, Organisation), by = c("Level", "Code"))
  matching_coverage <- matching_levels |> group_by(Allocation, Level, Code, Organisation) |>
    summarise(Cohort_rows = n(), Matched_rows = sum(Reason == ""),
      Unmatched_rows = sum(Reason != ""), Unknown_reason_rows = sum(is.na(Reason)), .groups = "drop") |>
    mutate(Coverage = Matched_rows / Cohort_rows,
      Partition_difference = Cohort_rows - Matched_rows - Unmatched_rows)
  matching_show("Provider coverage: raw counts, including unknown and out-of-range ages", matching_coverage)
  matching_show("Difference from the prepared report counts (zero if same inputs / rules)",
    matching_coverage |> full_join(matching_model$provider_coverage |>
      select(Allocation, Level, Organisation, Cohort_rows, Matched_rows, Unmatched_rows),
      by = c("Allocation", "Level", "Organisation"), suffix = c("", "_report")) |>
      transmute(Allocation, Level, Organisation, Cohort_difference = Cohort_rows - Cohort_rows_report,
        Matched_difference = Matched_rows - Matched_rows_report,
        Unmatched_difference = Unmatched_rows - Unmatched_rows_report))
  matching_reasons <- matching_levels |> count(Allocation, Level, Code, Organisation, Reason,
    name = "Pregnancies")
  matching_show("Why each trust / ICB / cluster loses coverage", matching_reasons)
  matching_country_coverage <- matching_levels |>
    group_by(Allocation, Level, Code, Organisation, Country) |>
    summarise(Pregnancies = n(), No_ONS_LA = sum(!LA_in_ONS),
      Unmatched = sum(Reason != ""), .groups = "drop") |>
    group_by(Allocation, Level, Code, Organisation) |>
    mutate(Pct_organisation = 100 * Pregnancies / sum(Pregnancies),
      Pct_organisation_without_ONS_LA = 100 * No_ONS_LA / sum(Pregnancies),
      Pct_all_unmatched = 100 * Unmatched / sum(Unmatched)) |> ungroup()
  matching_show("Country contribution to trust / ICB / cluster coverage gaps", matching_country_coverage)
  matching_gaps <- matching_provider_rows |> filter(is.na(Reason) | Reason != "") |>
    count(Allocation, Trust, ICB, LA_source, LA, LA_in_ONS, Age_value, Age, Age_band,
      Age_source, Reason, name = "Pregnancies", sort = TRUE) |>
    left_join(matching_names, by = "LA")
  matching_show("Trace gaps to trust, resident LA, supplied age and age source", matching_gaps)
  matching_show("Roll-up check: counts should agree across levels; do not add the levels together",
    matching_coverage |> group_by(Allocation, Level) |>
      summarise(across(c(Cohort_rows, Matched_rows, Unmatched_rows), sum), .groups = "drop"))

  # 5. Raw resident counts alongside the model's processed denominators --
  matching_raw_cells <- matching_residents |>
    filter(Has_delivery, Pregnancy_FY %in% matching_model$allocation_tables$years) |>
    group_by(LA, Age) |>
    summarise(Raw_resident_rows = n(), Raw_resident_count = sum(Count),
      Missing_raw_counts = sum(is.na(Count)), .groups = "drop")
  matching_denominators <- matching_calibration |>
    full_join(matching_raw_cells, by = c("LA", "Age"))
  matching_show("Affected LA/age denominators: raw missing values stay missing",
    matching_denominators |> semi_join(matching_gaps, by = c("LA", "Age")) |>
      select(LA, Age, Raw_resident_rows, Raw_resident_count, Missing_raw_counts,
        MSDS_resident_count, Resident_antenatal_only, Cohort_recorded_births, Cohort_pregnancies,
        Shared_maternity_denominator, Resident_birth_gap, Booking_Denominator_Factor, Calibration_issue))
  matching_show("Source gaps/NA converted to zero in model resident counts",
    matching_denominators |> filter(is.na(Raw_resident_count), !is.na(MSDS_resident_count)) |>
      select(LA, Age, Raw_resident_rows, Raw_resident_count, Missing_raw_counts, MSDS_resident_count))
  cat("\nCoverage is a ratio of summed counts, not an average of trust percentages.\n",
    "ICBs use provider membership, not resident ICB. Booking and delivery populations differ.\n",
    "The report keeps available projected subtotals; unsupported estimates can be NA.\n",
    "Unknown ages flag each age band, but count once in provider totals.\n",
    "Known ages outside the model range appear in this coverage table but not projection-gap totals.\n",
    "Model processing replaces missing resident counts with zero; the raw checks above preserve them.\n", sep = "")
} else {
  cat("\nSource checks complete. For provider/model checks, run the usual analysis with these inputs,\n",
    "then rerun sections 4-5 (first_draft_projections is not loaded).\n", sep = "")
}
