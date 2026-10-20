#scratch area

# SARAH'S RAW PARQUET OUTPUTS: select this whole block through END RAW TOTALS.
# Run R/Read parquet files from azure.R first to initialise Azure/the read helper.
# These TWO FRESH READS are the only data inputs. Existing tables are not used.
# No report/model code, joins, deduplication, age exclusions or rounding.
# Internal diagnostics only. Count pregnancy rows in cohort; SUM count in LA file.
hh_source_cohort <- get_su_lakemart_parquet_file(
  "final_cohort_2326.parquet/part", az_files, az_container
) |> janitor::clean_names()
hh_source_la <- get_su_lakemart_parquet_file(
  "total_births_by_LA_2326.parquet/part", az_files, az_container
) |> janitor::clean_names()
stopifnot(all(c("antenatal_app_date", "birth_yr", "birth_flag", "la_of_mother",
  "booking_provider_flag", "birth_provider_flag") %in% names(hh_source_cohort)),
  all(c("birth_yr", "estimated_yr", "la_of_mother", "count") %in% names(hh_source_la)))
hh_source_las <- c("E08000025", "E08000027", "E08000028",
  "E08000029", "E08000030", "E08000031") # Birmingham, Dudley, Sandwell,
                                         # Solihull, Walsall, Wolverhampton.
hh_source_years <- c("2023/24", "2024/25", "2025/26")
hh_source_periods <- c(hh_source_years, "Other / missing FY", "ALL YEARS")
hh_source_text <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | tolower(x) %in% c("", "null", "na")] <- NA_character_
  x
}
hh_source_in_period <- function(year, period) {
  if (period == "ALL YEARS") return(rep(TRUE, length(year)))
  if (period == "Other / missing FY") return(!year %in% hh_source_years)
  year %in% period
}
# Only derive booking FY from the supplied date: April to March.
hh_source_booking_date <- hh_source_cohort$antenatal_app_date
hh_source_date_text <- hh_source_text(hh_source_booking_date)
if (inherits(hh_source_booking_date, "Date") || inherits(hh_source_booking_date, "POSIXt")) {
  hh_source_booking_date <- as.Date(hh_source_booking_date)
} else {
  hh_source_booking_date <- as.Date(hh_source_date_text, format = "%Y-%m-%d")
  hh_source_retry <- is.na(hh_source_booking_date) & !is.na(hh_source_date_text)
  hh_source_booking_date[hh_source_retry] <- as.Date(
    hh_source_date_text[hh_source_retry], format = "%d/%m/%Y")
}
hh_source_start <- as.integer(format(hh_source_booking_date, "%Y")) -
  as.integer(as.integer(format(hh_source_booking_date, "%m")) < 4L)
hh_source_booking_fy <- ifelse(is.na(hh_source_start), NA_character_,
  paste0(hh_source_start, "/", substr(hh_source_start + 1L, 3, 4)))
hh_source_birth_fy <- hh_source_text(hh_source_cohort$birth_yr)
hh_source_birth <- hh_source_cohort$birth_flag %in% 1
hh_source_resident <- hh_source_cohort$la_of_mother %in% hh_source_las

# 1. FINAL_COHORT: actual bookings by booking FY; deliveries by supplied birth FY.
# Cluster providers use Sarah's booking_provider_flag / birth_provider_flag,
# i.e. RNA, RXK, RBK, RL4, RRK, RQ3 in her saved extraction notebook.
# Resident totals are a separate view, matching the six-LA definition below.
hh_source_cohort_totals <- do.call(rbind, lapply(
  c("Whole file", "Cluster providers (source flags)", "Cluster LA residents"), function(scope) {
    booking_scope <- switch(scope, "Whole file" = rep(TRUE, nrow(hh_source_cohort)),
      "Cluster providers (source flags)" = hh_source_cohort$booking_provider_flag %in% 1,
      "Cluster LA residents" = hh_source_resident)
    delivery_scope <- switch(scope, "Whole file" = rep(TRUE, nrow(hh_source_cohort)),
      "Cluster providers (source flags)" = hh_source_cohort$birth_provider_flag %in% 1,
      "Cluster LA residents" = hh_source_resident)
    data.frame(Scope = scope, FY = hh_source_periods,
      Bookings = vapply(hh_source_periods, function(y) sum(booking_scope &
        !is.na(hh_source_booking_date) & hh_source_in_period(hh_source_booking_fy, y)), numeric(1)),
      Deliveries = vapply(hh_source_periods, function(y) sum(delivery_scope &
        hh_source_birth & hh_source_in_period(hh_source_birth_fy, y)), numeric(1)), row.names = NULL)
  }))
cat("\nFINAL_COHORT_2326: source pregnancy rows, no report filters\n")
print(hh_source_cohort_totals, row.names = FALSE)
cat("Source rows:", nrow(hh_source_cohort),
  "| Missing booking dates:", sum(is.na(hh_source_date_text)),
  "| Supplied booking dates not parsed:",
  sum(!is.na(hh_source_date_text) & is.na(hh_source_booking_date)),
  "| Recorded deliveries without birth FY:", sum(hh_source_birth & is.na(hh_source_birth_fy)), "\n")

# 2. LA FILE: use count, never nrow, for activity totals. No age filtering.
# The added bookings are rows with birth_yr missing; use estimated_yr for FY.
# Bookings_due_FY sums those antenatal records by DUE FY, as supplied.
# All_records = deliveries + these bookings. Delivered rows count only as deliveries.
# Cluster means residents of the six LAs; provider-based totals are unavailable.
hh_source_weights <- as.numeric(as.character(hh_source_la$count))
stopifnot(all(is.finite(hh_source_weights)), all(hh_source_weights >= 0))
hh_source_la_birth_fy <- hh_source_text(hh_source_la$birth_yr)
hh_source_la_due_fy <- hh_source_text(hh_source_la$estimated_yr)
hh_source_la_totals <- do.call(rbind, lapply(c("Whole file", "Cluster LA residents"), function(scope) {
  keep <- if (scope == "Whole file") rep(TRUE, nrow(hh_source_la)) else
    hh_source_la$la_of_mother %in% hh_source_las
  result <- data.frame(Scope = scope, FY = hh_source_periods,
    Deliveries = vapply(hh_source_periods, function(y) sum(hh_source_weights[keep &
      !is.na(hh_source_la_birth_fy) & hh_source_in_period(hh_source_la_birth_fy, y)]), numeric(1)),
    Bookings_due_FY = vapply(hh_source_periods, function(y) sum(hh_source_weights[keep &
      is.na(hh_source_la_birth_fy) & hh_source_in_period(hh_source_la_due_fy, y)]), numeric(1)),
    row.names = NULL)
  result$All_records <- result$Deliveries + result$Bookings_due_FY
  result
}))
cat("\nTOTAL_BIRTHS_BY_LA_2326: sum(count), no report filters\n")
print(hh_source_la_totals, row.names = FALSE)
cat("LA bookings use estimated due FY; cohort bookings above use appointment FY.\n",
  "Source rows:", nrow(hh_source_la), "| Total count:", sum(hh_source_weights),
  "| Count with neither birth nor due FY:", sum(hh_source_weights[
    is.na(hh_source_la_birth_fy) & is.na(hh_source_la_due_fy)]), "\n")
stopifnot(hh_source_la_totals$All_records[hh_source_la_totals$Scope == "Whole file" &
  hh_source_la_totals$FY == "ALL YEARS"] == sum(hh_source_weights))
# END RAW TOTALS. Existing detailed checks below are optional/separate.

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

# Raw and harmonised LA codes, plus remaining ONS gaps ---------------------
source("R/report_cohort.R")
ons_codes <- unique(read.csv("data/2022 SNPP Births persons.csv")$AREA_CODE)

final_cohort_2326 |>
  count(la_of_mother, name = "pregnancies") |>
  mutate(LA_2023 = report_la_code(la_of_mother),
    code_changed = report_code(la_of_mother, "Unknown LA") != LA_2023,
    in_ONS = LA_2023 %in% ons_codes) |>
  arrange(in_ONS, desc(code_changed), desc(pregnancies))

total_births_by_LA_2326 |>
  mutate(LA_2023 = report_la_code(la_of_mother)) |>
  filter(!LA_2023 %in% ons_codes) |>
  group_by(la_of_mother, LA_2023, birth_yr) |>
  summarise(recorded_count = sum(as.numeric(as.character(count))), .groups = "drop")

# Supported conversions present in either input (codes only).
report_applied_geography(final_cohort_2326$la_of_mother,
  total_births_by_LA_2326$la_of_mother)

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
    Birth_date_FY = hh_booking_fy(Delivery_date),
    Booking_month = format(Booking_date, "%Y-%m"),
    Birth_month = format(Delivery_date, "%Y-%m"),
    Booking_ICB = coalesce(unname(hh_booking_config$provider_icb[Booking_Trust]),
      "Outside lookup / unknown"),
    Booking_in_cluster = Booking_Trust %in% hh_booking_config$cluster_trusts,
    Days_booking_to_birth = as.numeric(Delivery_date - Booking_date))
stopifnot(nrow(hh_bookings) == nrow(final_cohort_2326))

# 1. Source rows BEFORE any report filtering, by supplied year and birth flag.
# These are pregnancy rows, not distinct women (a woman may have >1 pregnancy).
tibble::as_tibble(final_cohort_2326) |>
  count(birth_yr, birth_flag, name = "raw_pregnancy_rows") |>
  print(n = Inf)

# Missing dates/providers, failed date parsing and year disagreements.
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
    recorded_birth_date_missing = sum(Birth_recorded & is.na(Delivery_date)),
    birth_year_date_disagree = sum(Birth_recorded &
      !is.na(Birth_date_FY) & !is.na(Year) & Birth_date_FY != Year),
    booking_after_birth = sum(Birth_recorded & Days_booking_to_birth < 0, na.rm = TRUE),
    .groups = "drop") |>
  print(width = Inf)
class(final_cohort_2326$antenatal_app_date)
# Aggregate the SHAPE of unparsed dates, without printing individual dates/IDs.
hh_bookings |>
  filter(Booking_date_supplied & is.na(Booking_date)) |>
  mutate(date_format = gsub("[0-9]", "9",
    trimws(as.character(final_cohort_2326$antenatal_app_date[Row])))) |>
  count(Source_birth_FY, date_format, name = "rows", sort = TRUE) |>
  print(n = Inf)

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

# 3. Monthly cluster counts: does the drop emerge part-way through the year?
# Zero means no rows in THIS extract, not proof of no activity in the service.
hh_booking_months <- tibble::tibble(Month = format(seq(as.Date("2023-04-01"),
  as.Date("2026-03-01"), by = "month"), "%Y-%m"))
hh_booking_months |>
  left_join(hh_bookings |> filter(Booking_in_cluster) |>
      count(Month = Booking_month, name = "bookings"), by = "Month") |>
  left_join(hh_bookings |> filter(Birth_recorded,
      Trust %in% hh_booking_config$cluster_trusts) |>
      count(Month = Birth_month, name = "recorded_deliveries"), by = "Month") |>
  mutate(across(c(bookings, recorded_deliveries), ~ coalesce(.x, 0L))) |>
  print(n = Inf)
# Provider detail for the same monthly investigation (including unknown dates).
hh_bookings |>
  filter(Booking_in_cluster) |>
  count(Booking_Trust, Booking_ICB, Booking_month, name = "bookings") |>
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
