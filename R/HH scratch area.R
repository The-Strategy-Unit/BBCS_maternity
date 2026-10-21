usethis::edit_r_environ()



# QUICK COMPARISON: ALL LAs AND COMBINED COHORT LAs ----------------------
# Run this section only. Reads Sarah's two loaded objects directly.
# Cohort LAs = Birmingham, Dudley, Sandwell, Solihull, Walsall, Wolverhampton.
# These are RESIDENT groups, not provider groups. All LAs includes missing LA.
# Total-file pregnancies = deliveries + antenatal-only, plus any unassigned rows.
# Its deliveries use birth_yr; antenatal-only uses estimated_yr (DUE year).
# It cannot give appointment-year bookings. Cohort bookings use appointment FY.
# Sum count in the total file; count rows in the cohort. No deduplication/rounding.
library(dplyr)

quick_comparison <- bind_rows(lapply(c("All LAs", "Cohort LAs"), function(scope) {
  totals <- total_births_by_LA_2326
  cohort <- final_cohort_2326
  if (scope == "Cohort LAs") {
    totals <- filter(totals, la_of_mother %in%
      c("E08000025", "E08000027", "E08000028", "E08000029", "E08000030", "E08000031"))
    cohort <- filter(cohort, la_of_mother %in%
      c("E08000025", "E08000027", "E08000028", "E08000029", "E08000030", "E08000031"))
  }
  total_counts <- totals |>
    mutate(FY = coalesce(birth_yr, estimated_yr)) |>
    group_by(FY, .drop = FALSE) |>
    summarise(Total_file_pregnancies = sum(count),
      Total_file_antenatal_only = sum(count[is.na(birth_yr) & !is.na(estimated_yr)]),
      Total_file_deliveries = sum(count[!is.na(birth_yr)]), .groups = "drop") |>
    tidyr::complete(FY = c("2023/24", "2024/25", "2025/26"),
      fill = list(Total_file_pregnancies = 0, Total_file_antenatal_only = 0,
        Total_file_deliveries = 0), explicit = FALSE)
  cohort_counts <- bind_rows(
    cohort |>
      mutate(appointment = as.Date(antenatal_app_date, format = "%Y-%m-%d"),
        start = as.integer(format(appointment, "%Y")) -
          as.integer(as.integer(format(appointment, "%m")) < 4)) |>
      transmute(FY = if_else(is.na(start), NA_character_,
        paste0(start, "/", substr(start + 1L, 3, 4))), Event = "Booking"),
    cohort |> filter(birth_flag %in% 1) |>
      transmute(FY = birth_yr, Event = "Delivery")
  ) |>
    group_by(FY, .drop = FALSE) |>
    summarise(Cohort_bookings = sum(Event == "Booking"),
      Cohort_deliveries = sum(Event == "Delivery"), .groups = "drop") |>
    tidyr::complete(FY = c("2023/24", "2024/25", "2025/26"),
      fill = list(Cohort_bookings = 0, Cohort_deliveries = 0), explicit = FALSE)
  full_join(total_counts, cohort_counts, by = "FY") |>
    mutate(Scope = scope, .before = 1)
}))

# Main comparison: six rows, with the two files side by side.
tidyr::expand_grid(Scope = c("All LAs", "Cohort LAs"),
  FY = c("2023/24", "2024/25", "2025/26")) |>
  left_join(quick_comparison, by = c("Scope", "FY")) |>
  print(n = Inf, width = Inf)

# Separate small table so missing/other years cannot disappear unnoticed.
# In the NA-year row, Cohort_bookings counts rows with no assignable appointment
# year, not confirmed bookings. NA counts are left as NA, never replaced by zero.
quick_comparison |>
  filter(!FY %in% c("2023/24", "2024/25", "2025/26")) |>
  arrange(Scope, FY) |>
  print(n = Inf, width = Inf)
# END QUICK COMPARISON -------------------------------------------------

# LATEST: DEMOGRAPHICS COLUMN NAMES AND TYPES ---------------------------
# Schema only: does not print patient records or change either source object.
data.frame(
  column = names(demographics),
  type = vapply(demographics, function(x) paste(class(x), collapse = "/"), character(1))
) |> print(row.names = FALSE)

# Cohort mother-ID type, to compare with the demographics join key.
class(final_cohort_2326$person_id_mother)
# END DEMOGRAPHICS SCHEMA ----------------------------------------------

# LATEST: RAW ANNUAL TOTALS AND THE 2025/26 BOOKING DROP -------------------
# Select and run individual chunks. Uses Sarah's already-loaded objects only.
# No source rows/values are changed, removed or deduplicated. No year filter.
# NA is retained; blanks and literal "null" remain distinct in supplied fields.
# Cohort totals count rows; LA totals sum count (an NA count leaves the sum NA).
# Cluster providers = Sarah's provider_flag == 1.
# Cluster residents = the six named LAs, independently of the provider.
# A missing date/year is not proof that an event did not occur.
library(dplyr)

# 1. DELIVERIES: supplied delivery FY and birth flag, including NA/other flags.
# Rows with birth_flag 1 are recorded deliveries; all other groups remain shown.
final_cohort_2326 |>
  group_by(birth_yr, birth_flag, .drop = FALSE) |>
  summarise(Whole_file_rows = n(),
    Cluster_delivery_provider_rows = sum(birth_provider_flag %in% 1),
    Cluster_LA_resident_rows = sum(la_of_mother %in%
      c("E08000025", "E08000027", "E08000028", "E08000029", "E08000030", "E08000031")),
    Missing_delivery_provider_flags = sum(is.na(birth_provider_flag)),
    Missing_LA_rows = sum(is.na(la_of_mother)), .groups = "drop") |>
  print(n = Inf, width = Inf)

# 2. BOOKINGS: derived appointment FY, including absent/unparsed dates.
# Sarah supplies ISO dates. The parse is a separate diagnostic column.
# Unparsed source values stay in the NA FY group; the raw values appear below.
# A usable appointment date identifies dated bookings. Unknown-date rows remain
# visible rather than being counted as confirmed bookings in an invented year.
final_cohort_2326 |>
  mutate(Diagnostic_date = as.Date(antenatal_app_date, format = "%Y-%m-%d"),
    Diagnostic_date_missing = is.na(antenatal_app_date),
    Diagnostic_date_unparsed = !is.na(antenatal_app_date) & is.na(Diagnostic_date),
    Diagnostic_start = as.integer(format(Diagnostic_date, "%Y")) -
      as.integer(as.integer(format(Diagnostic_date, "%m")) < 4),
    Diagnostic_booking_FY = if_else(is.na(Diagnostic_start), NA_character_,
      paste0(Diagnostic_start, "/", substr(Diagnostic_start + 1, 3, 4)))) |>
  group_by(Diagnostic_booking_FY, Diagnostic_date_missing,
    Diagnostic_date_unparsed, .drop = FALSE) |>
  summarise(Whole_file_rows = n(),
    Cluster_booking_provider_rows = sum(booking_provider_flag %in% 1),
    Cluster_LA_resident_rows = sum(la_of_mother %in%
      c("E08000025", "E08000027", "E08000028", "E08000029", "E08000030", "E08000031")),
    Missing_booking_provider_flags = sum(is.na(booking_provider_flag)),
    Missing_LA_rows = sum(is.na(la_of_mother)), .groups = "drop") |>
  print(n = Inf, width = Inf)

# 3. BOOKING FY VERSUS SUPPLIED DUE/DELIVERY FY (all combinations).
# Look for 2025/26 appointments with edd_yr or birth_yr = 2026/27.
# Their absence cannot prove upstream exclusion: records excluded before this
# extract cannot be recovered here. Compare with Sarah's current extraction SQL.
final_cohort_2326 |>
  mutate(Diagnostic_date = as.Date(antenatal_app_date, format = "%Y-%m-%d"),
    Diagnostic_start = as.integer(format(Diagnostic_date, "%Y")) -
      as.integer(as.integer(format(Diagnostic_date, "%m")) < 4),
    Diagnostic_booking_FY = if_else(is.na(Diagnostic_start), NA_character_,
      paste0(Diagnostic_start, "/", substr(Diagnostic_start + 1, 3, 4)))) |>
  group_by(Diagnostic_booking_FY, edd_yr, birth_yr, birth_flag, .drop = FALSE) |>
  summarise(Whole_file_rows = n(),
    Cluster_booking_provider_rows = sum(booking_provider_flag %in% 1),
    Cluster_delivery_provider_rows = sum(birth_provider_flag %in% 1),
    Cluster_LA_resident_rows = sum(la_of_mother %in%
      c("E08000025", "E08000027", "E08000028", "E08000029", "E08000030", "E08000031")),
    .groups = "drop") |>
  print(n = Inf, width = Inf)

# 4. LA EXTRACT: every supplied delivery/due-year pair, without coalescing them.
# Non-NA birth_yr = deliveries. NA birth_yr + non-NA estimated_yr =
# antenatal-only pregnancies by DUE FY, not appointment FY.
# Both years NA = unassigned, still printed. Provider totals cannot be obtained
# from this LA extract because it has no provider field.
total_births_by_LA_2326 |>
  group_by(birth_yr, estimated_yr, .drop = FALSE) |>
  summarise(Source_rows = n(), Whole_file_count = sum(count),
    Cluster_LA_resident_count = sum(count[la_of_mother %in%
      c("E08000025", "E08000027", "E08000028", "E08000029", "E08000030", "E08000031")]),
    Missing_LA_count = sum(count[is.na(la_of_mother)]),
    Missing_count_rows = sum(is.na(count)),
    Missing_birth_age_rows = sum(is.na(age_at_birth_mother)),
    Missing_booking_age_rows = sum(is.na(age_at_booking_mother)), .groups = "drop") |>
  print(n = Inf, width = Inf)

# 5. PROVIDER COVERAGE: supplied providers and delivery years, including NA.
final_cohort_2326 |>
  count(booking_provider, booking_provider_flag, name = "Rows", .drop = FALSE) |>
  print(n = Inf, width = Inf)
final_cohort_2326 |>
  count(birth_yr, birth_flag, birth_provider, birth_provider_flag,
    name = "Rows", .drop = FALSE) |>
  print(n = Inf, width = Inf)

# 6. ORIGINAL FLAGS AND DATES: every supplied combination, no parsing.
final_cohort_2326 |>
  count(resident_flag, booking_provider_flag, birth_provider_flag, birth_flag,
    name = "Rows", .drop = FALSE) |>
  print(n = Inf, width = Inf)
final_cohort_2326 |>
  count(antenatal_app_date, edd_yr, birth_yr, birth_date,
    name = "Rows", .drop = FALSE) |>
  print(n = Inf, width = Inf)

# Optional: schema only, to confirm demographic fields without printing IDs.
names(demographics)
# END LATEST RAW ANNUAL TOTALS ------------------------------------------
# ALL PREVIOUS INVESTIGATIONS RETAINED BELOW, IN THEIR ORIGINAL ORDER.

# UNFILTERED COUNTS DIRECTLY FROM SARAH'S TWO LOADED OBJECTS.
# Phase-1-Final: run R/Read parquet files from azure.R, then any chunk below.
# No rows removed, no source fields overwritten, no deduplication or null cleanup.
# NA, blank and literal "null" values stay distinct. Print every group.
# Rows = source records, not assumed bookings/deliveries when evidence is missing.
# Only chunk 2 derives a year; chunk 3 shows the original date values unchanged.
library(dplyr)

# 1. COHORT: supplied birth FY and flags, including every NA/unexpected flag.
# birth_flag: 1 = recorded delivery; booking/birth_provider_flag: 1 = cluster.
# Every source row contributes to exactly one group; sum Rows = nrow(source).
final_cohort_2326 |>
  count(birth_yr, birth_flag, booking_provider_flag, birth_provider_flag,
    name = "Rows", .drop = FALSE) |>
  print(n = Inf, width = Inf)

# 2. COHORT: booking FY derived from appointment date, with all rows retained.
# FY = NA includes absent/unparsed dates; the two explicit checks distinguish them.
# A row without a usable booking date is not labelled a confirmed booking.
# No year-range filter: earlier/later years and NA all appear.
final_cohort_2326 |>
  mutate(Diagnostic_booking_date = as.Date(antenatal_app_date, format = "%Y-%m-%d"),
    Diagnostic_missing_date = is.na(antenatal_app_date),
    Diagnostic_unparsed_date = !is.na(antenatal_app_date) & is.na(Diagnostic_booking_date),
    Diagnostic_FY_start = as.integer(format(Diagnostic_booking_date, "%Y")) -
      as.integer(as.integer(format(Diagnostic_booking_date, "%m")) < 4),
    Diagnostic_booking_FY = if_else(is.na(Diagnostic_FY_start), NA_character_,
      paste0(Diagnostic_FY_start, "/", substr(Diagnostic_FY_start + 1, 3, 4)))) |>
  count(Diagnostic_booking_FY, Diagnostic_missing_date, Diagnostic_unparsed_date,
    birth_yr, birth_flag, booking_provider_flag, name = "Rows", .drop = FALSE) |>
  print(n = Inf, width = Inf)

# 3. COHORT: original appointment dates and booking flags, without date parsing.
# This may print many rows. Includes NA, blanks and any unexpected source values.
final_cohort_2326 |>
  count(antenatal_app_date, booking_provider_flag, name = "Rows", .drop = FALSE) |>
  print(n = Inf, width = Inf)

# 4. LA FILE: BOTH supplied year fields together, including null combinations.
# birth_yr present = deliveries; birth_yr NA = the added antenatal bookings.
# estimated_yr is due FY. Do not combine these into a cleaned/coalesced year.
# Total_count includes every row; cluster/unknown-LA columns are extra subtotals.
# No na.rm: an NA count stays visible as an NA sum, with Missing_count_rows shown.
total_births_by_LA_2326 |>
  group_by(birth_yr, estimated_yr, .drop = FALSE) |>
  summarise(Source_rows = n(), Total_count = sum(count),
    Cluster_resident_count = sum(count[la_of_mother %in%
      c("E08000025", "E08000027", "E08000028", "E08000029", "E08000030", "E08000031")]),
    NA_LA_count = sum(count[is.na(la_of_mother)]),
    Missing_count_rows = sum(is.na(count)), .groups = "drop") |>
  print(n = Inf, width = Inf)

# 5. LA FILE: combined residence groups by year, never one row per LA.
# Group labels are diagnostic only; source values and missing counts are unchanged.
total_births_by_LA_2326 |>
  mutate(Residence_group = case_when(
    is.na(la_of_mother) ~ "Missing LA (NA)",
    la_of_mother == "" ~ "Blank LA",
    la_of_mother %in%
      c("E08000025", "E08000027", "E08000028", "E08000029", "E08000030", "E08000031") ~
        "Combined cluster residents",
    TRUE ~ "All other supplied LA values")) |>
  group_by(Residence_group, birth_yr, estimated_yr, .drop = FALSE) |>
  summarise(Source_rows = n(), Total_count = sum(count),
    Missing_count_rows = sum(is.na(count)), .groups = "drop") |>
  print(n = Inf, width = Inf)

# 6. Whole-file totals: compare these with the sums of the groups above.
# Each cohort table counts the same full set of source rows in a different way.
final_cohort_2326 |>
  summarise(Source_rows = n()) |>
  print(width = Inf)
total_births_by_LA_2326 |>
  summarise(Source_rows = n(), Total_count = sum(count),
    Missing_count_rows = sum(is.na(count))) |>
  print(width = Inf)

# END LATEST UNFILTERED DIAGNOSTICS.
# OLDER DETAILED CHECKS BELOW: these retain their original filters and processing.

library(dplyr)
source("R/report_cohort.R")

# Use exactly the report's birth-age / booking-age fallback rule.
prepared_age_check <- prepare_report_cohort(final_cohort_2326)
# Raw diagnostic counts behind the report's age-completeness table.
# Denominator distinguishes the whole cohort from records missing birth age.
summarise_maternal_age_completeness(final_cohort_2326) |>
  mutate(percent = if_else(Denominator > 0, 100 * N / Denominator, NA_real_))
age_check <- final_cohort_2326 |>
  mutate(age_numeric = prepared_age_check$Age,
    age_source = prepared_age_check$Age_source)

# Before/after missing-age counts and the number recovered from booking age.
age_check |>
  group_by(birth_yr, birth_flag) |>
  summarise(pregnancies = n(), birth_age_na = sum(is.na(age_at_birth_mother)),
    recovered_from_booking = sum(age_source == "Booking fallback"),
    still_unknown_or_invalid = sum(is.na(age_numeric)),
    percent_still_unknown = 100 * mean(is.na(age_numeric)), .groups = "drop")

age_check |>
  summarise(
    total_pregnancies = n(),
    pregnancies_without_age = sum(is.na(age_numeric)),
    mothers_without_age = n_distinct(
      person_id_mother[is.na(age_numeric)], na.rm = TRUE
    ),
    percent_without_age = round(100 * mean(is.na(age_numeric)), 2)
  )

age_check |>
  filter(
    is.na(age_numeric) |
      !between(age_numeric, 13, 54) |
      age_numeric != floor(age_numeric)
  ) |>
  count(age_at_birth_mother, age_at_booking_mother, age_numeric, age_source,
    name = "pregnancies", sort = TRUE)


# OPTIONAL SNIPPETS: select and run whichever bits are useful.
# Run Sarah's parquet reader first if the input objects are not loaded.
# These are raw internal diagnostics, not controlled publication outputs.
# Nothing here is sourced by the report runner or saves an output file.

# Missing/invalid ages by delivery status, year or provider ----------------
age_check |>
  mutate(age_issue = case_when(
    is.na(age_numeric) ~ "Missing/non-numeric",
    !is.finite(age_numeric) | age_numeric != floor(age_numeric) ~ "Invalid age",
    !between(age_numeric, 13, 54) ~ "Known age outside 13-54",
    TRUE ~ "Usable age 13-54")) |>
  count(birth_yr, birth_flag, booking_provider, age_issue, name = "pregnancies")

# Missing age among records with booking evidence, by delivery status.
age_check |>
  filter((!is.na(booking_provider) & nzchar(trimws(as.character(booking_provider)))) |
    (!is.na(antenatal_app_date) & nzchar(trimws(as.character(antenatal_app_date))))) |>
  group_by(birth_yr, birth_flag) |>
  summarise(pregnancies = n(), missing_age = sum(is.na(age_numeric)),
    percent_missing_age = 100 * mean(is.na(age_numeric)), .groups = "drop")

# Wider resident file: weight by count, not the number of aggregate rows.
total_births_by_LA_2326 |>
  mutate(age_numeric = suppressWarnings(as.numeric(as.character(age_at_birth_mother))),
    count_numeric = suppressWarnings(as.numeric(as.character(count))),
    age_missing = is.na(age_numeric)) |>
  group_by(birth_yr, age_missing) |>
  summarise(source_rows = n(), recorded_count = sum(count_numeric), .groups = "drop")

# Other available age/date fields; the report uses only the agreed two ages.
grep("age|dob|birth_date", names(final_cohort_2326), ignore.case = TRUE, value = TRUE)

# Geography coverage summaries: totals only, no individual-LA tables ------
source("R/report_cohort.R")
ons_codes <- unique(read.csv("data/2022 SNPP Births persons.csv")$AREA_CODE)

final_cohort_2326 |>
  mutate(LA_2023 = report_la_code(la_of_mother),
    Source_LA_missing = is.na(la_of_mother),
    code_changed = report_code(la_of_mother, "Unknown LA") != LA_2023,
    in_ONS = LA_2023 %in% ons_codes) |>
  count(birth_yr, Source_LA_missing, code_changed, in_ONS,
    name = "pregnancies", .drop = FALSE)

total_births_by_LA_2326 |>
  mutate(LA_2023 = report_la_code(la_of_mother),
    Source_LA_missing = is.na(la_of_mother),
    code_changed = report_code(la_of_mother, "Unknown LA") != LA_2023,
    in_ONS = LA_2023 %in% ons_codes) |>
  group_by(birth_yr, estimated_yr, Source_LA_missing, code_changed, in_ONS,
    .drop = FALSE) |>
  summarise(Source_rows = n(), Total_count = sum(count),
    Missing_count_rows = sum(is.na(count)), .groups = "drop")

# Number of supported code conversions present; no list of individual LAs.
report_applied_geography(final_cohort_2326$la_of_mother,
  total_births_by_LA_2326$la_of_mother) |>
  summarise(Source_codes_changed = n(), Target_codes = n_distinct(Target_code))

# Additional quick checks ------------------------------------------------
# Birth-age-only source check, before the booking fallback.
# Filter missing ages first: count(age_at_birth_mother = NA) would count all rows.
final_cohort_2326 |>
  filter(is.na(age_at_birth_mother)) |>
  summarise(pregnancies_without_age = n())

final_cohort_2326 |>
  count(age_at_birth_mother, name="n") |>
  mutate(percentage = round(n / sum(n)*100, 2)) |>
  arrange(age_at_birth_mother)  |>
  print(n=Inf)

file.exists("render.log")

# BOOKING DROP CHECK: 2023/24, 2024/25 AND 2025/26 --------------------------
# Run this setup, then whichever numbered checks are useful. Load the current
# parquet objects with R/Read parquet files from azure.R first; no model run needed.
# Raw internal counts only: do not copy these tables into the shared report.
# This section does not change the source objects, run the model or save files.
library(dplyr)
source("R/report_cohort.R")
hh_booking_fy <- function(date) {
  start <- as.integer(format(date, "%Y")) -
    as.integer(as.integer(format(date, "%m")) < 4L)
  ifelse(is.na(start), NA_character_, paste0(start, "/", substr(start + 1L, 3, 4)))
}
# Read just the organisation settings from the analysis, without executing it.
hh_booking_config <- new.env(parent = baseenv())
for (hh_statement in parse("R/Maternity analysis.R")) {
  if (is.call(hh_statement) && identical(hh_statement[[1]], as.name("<-")) &&
      as.character(hh_statement[[2]])[1] %in%
        c("cluster_icbs", "icb_provider_codes", "provider_icb", "cluster_trusts"))
    eval(hh_statement, envir = hh_booking_config)
}
hh_bookings <- prepare_report_cohort(final_cohort_2326) |>
  mutate(Source_birth_FY = as.character(final_cohort_2326$birth_yr),
    Booking_FY = hh_booking_fy(Booking_date),
    Booking_ICB = coalesce(unname(hh_booking_config$provider_icb[Booking_Trust]),
      "Outside lookup / unknown"),
    Booking_in_cluster = Booking_Trust %in% hh_booking_config$cluster_trusts)
stopifnot(nrow(hh_bookings) == nrow(final_cohort_2326))

# 1. Source rows BEFORE any report filtering, by supplied year and birth flag.
# These are pregnancy rows, not distinct women (a woman may have >1 pregnancy).
tibble::as_tibble(final_cohort_2326) |>
  count(birth_yr, birth_flag, name = "raw_pregnancy_rows") |>
  print(n = Inf)

# Missing dates/providers and failed appointment-date parsing.
# Delivery FY uses birth_yr only; birth_date is not needed for these totals.
# A provider without a usable booking date cannot enter a booking-FY chart.
hh_bookings |>
  group_by(Source_birth_FY) |>
  summarise(rows = n(), recorded_deliveries = sum(Birth_recorded),
    booking_date_absent = sum(!Booking_date_supplied),
    booking_date_unparsed = sum(Booking_date_supplied & is.na(Booking_date)),
    booking_provider_absent = sum(!Booking_provider_supplied),
    booking_provider_outside_lookup = sum(Booking_provider_supplied &
      !Booking_Trust %in% names(hh_booking_config$provider_icb)),
    cluster_booking_rows = sum(Booking_in_cluster),
    cluster_booking_rows_with_date = sum(Booking_in_cluster & !is.na(Booking_date)),
    .groups = "drop") |>
  print(width = Inf)
class(final_cohort_2326$antenatal_app_date)

# 2. Annual bookings by provider code, including codes outside our lookup.
# No age, LA, ONS match, birth flag or delivery-year filter is applied here.
hh_bookings |>
  count(Booking_Trust, Booking_ICB, Booking_FY, name = "raw_bookings") |>
  tidyr::pivot_wider(names_from = Booking_FY, values_from = raw_bookings,
    values_fill = 0) |>
  print(n = Inf, width = Inf)
hh_bookings |>
  count(Booking_ICB, Booking_FY, name = "raw_bookings") |>
  tidyr::pivot_wider(names_from = Booking_FY, values_from = raw_bookings,
    values_fill = 0) |>
  print(n = Inf, width = Inf)
# ICB here means the BOOKING PROVIDER'S ICB, as in the chart, not residence.

# 3. Annual cluster counts, including unknown and out-of-period years.
# Each event uses its own FY; the two columns are not a linked outcome comparison.
hh_bookings |> filter(Booking_in_cluster) |>
  count(FY = Booking_FY, name = "bookings") |>
  full_join(hh_bookings |> filter(Birth_recorded,
      Trust %in% hh_booking_config$cluster_trusts) |>
      count(FY = Source_birth_FY, name = "recorded_deliveries"), by = "FY") |>
  arrange(FY) |>
  print(n = Inf)

# 4. Is this a delivery-year cohort being used to count booking-year activity?
# Complete 2025/26 DELIVERIES do not prove complete 2025/26 BOOKINGS: later
# bookings may lead to 2026/27 deliveries. Check the extract's SQL selection
# with the analysts if those pregnancies are absent; do not assume a cause.
# Split birth_flag too: pregnancies without a recorded delivery may be included.
hh_bookings |>
  filter(Booking_in_cluster) |>
  count(Booking_FY, Source_birth_FY, Birth_flag, name = "bookings") |>
  print(n = Inf)
# Duplicate pregnancy keys could inflate earlier years; inspect, don't deduplicate.
hh_bookings |>
  filter(!is.na(Person), nzchar(Person), !is.na(Pregnancy), nzchar(Pregnancy)) |>
  group_by(Person, Pregnancy) |>
  mutate(repeated_key = n() > 1L) |>
  ungroup() |>
  group_by(Source_birth_FY) |>
  summarise(rows_with_repeated_key = sum(repeated_key), .groups = "drop") |>
  print(n = Inf)

# 5. OPTIONAL: reconcile these rows to the SAVED report's raw historical totals.
# Run only when .projections.rds and the loaded parquet are from the same run.
# Differences can also mean the saved report is stale. Comparison is before
# disclosure rounding; the displayed chart can differ slightly through rounding.
if (file.exists(".projections.rds")) {
  hh_booking_saved <- readRDS(".projections.rds")
  hh_booking_reconcile <- tibble::as_tibble(hh_booking_saved$total_comparison$Booking) |>
    filter(Series == "MSDS recorded")
  hh_booking_reconcile$Raw_recount <- vapply(seq_len(nrow(hh_booking_reconcile)),
    function(i) {
      line <- hh_booking_reconcile[i, ]
      org <- hh_booking_saved$organisations |>
        filter(Key == line$Key)
      stopifnot(nrow(org) == 1L)
      codes <- if (org$Level == "Trust") org$Code else if (org$Level == "ICB")
        names(hh_booking_config$provider_icb)[hh_booking_config$provider_icb == org$Code]
        else hh_booking_config$cluster_trusts
      sum(hh_bookings$Booking_Trust %in% codes & hh_bookings$Booking_FY %in% line$Period)
    }, numeric(1))
  hh_booking_reconcile |>
    transmute(Organisation, Period, Saved_raw_count = Value, Raw_recount,
      Difference = Saved_raw_count - Raw_recount) |>
    print(n = Inf)
} else message("No .projections.rds here; checks 1-4 do not need a rendered report.")
