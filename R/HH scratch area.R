#scratch area 

# RAW BOOKING DROP: select and run through END RAW BOOKING DROP.
# Requires the helper/Azure connection from R/Read parquet files from azure.R.
# Re-read the source parquet, so this cannot reuse an altered analysis object.
# Internal diagnostic counts only. No report code, joins, deduplication or
# age, residence, delivery-status or provider exclusions.
hh_raw_bookings <- get_su_lakemart_parquet_file(
  "final_cohort_2326.parquet/part", az_files, az_container
) |> janitor::clean_names()
hh_raw_date <- hh_raw_bookings$antenatal_app_date
hh_raw_date_text <- trimws(as.character(hh_raw_date))
if (inherits(hh_raw_date, "Date") || inherits(hh_raw_date, "POSIXt")) {
  hh_raw_date <- as.Date(hh_raw_date)
} else {
  hh_raw_date <- as.Date(hh_raw_date_text, format = "%Y-%m-%d")
  hh_raw_retry <- is.na(hh_raw_date) & !is.na(hh_raw_date_text)
  hh_raw_date[hh_raw_retry] <- as.Date(hh_raw_date_text[hh_raw_retry], format = "%d/%m/%Y")
}
hh_raw_start <- as.integer(format(hh_raw_date, "%Y")) -
  as.integer(as.integer(format(hh_raw_date, "%m")) < 4L)
hh_raw_fy <- ifelse(is.na(hh_raw_start), "Missing/unparsed date",
  paste0(hh_raw_start, "/", substr(hh_raw_start + 1L, 3, 4)))
hh_raw_years <- c("2023/24", "2024/25", "2025/26")
hh_raw_fy[!hh_raw_fy %in% c(hh_raw_years, "Missing/unparsed date")] <- "Other FY"
hh_raw_provider <- as.character(hh_raw_bookings$booking_provider)
hh_raw_provider[is.na(hh_raw_provider) | trimws(hh_raw_provider) == ""] <- "(missing provider)"
hh_raw_counts <- table(Provider = hh_raw_provider,
  FY = factor(hh_raw_fy, levels = c(hh_raw_years, "Other FY", "Missing/unparsed date")))
hh_raw_counts <- rbind(hh_raw_counts, `ALL PROVIDERS` = colSums(hh_raw_counts))
hh_raw_summary <- data.frame(Provider = rownames(hh_raw_counts),
  as.matrix(hh_raw_counts), check.names = FALSE, row.names = NULL)
hh_raw_summary$Change_25_26_pct <- ifelse(hh_raw_counts[, "2024/25"] > 0,
  round(100 * (hh_raw_counts[, "2025/26"] / hh_raw_counts[, "2024/25"] - 1), 1), NA_real_)
print(hh_raw_summary, row.names = FALSE)
stopifnot(sum(hh_raw_counts["ALL PROVIDERS", ]) == nrow(hh_raw_bookings))
cat("Source rows:", nrow(hh_raw_bookings),
  "| Blank/NA booking dates:", sum(is.na(hh_raw_date_text) | hh_raw_date_text == ""),
  "| Supplied dates not parsed:",
  sum(!is.na(hh_raw_date_text) & hh_raw_date_text != "" & is.na(hh_raw_date)), "\n")
# Counts are pregnancy rows with a booking date, not distinct women.
# END RAW BOOKING DROP

# QUICK RAW PARQUET TOTALS: run this section after loading the parquet files.
# Select this section only; no analysis/report run or extra packages needed.
# Internal diagnostic counts, before report filters or publication rounding.
hh_quick_years <- c("2023/24", "2024/25", "2025/26")
hh_quick_text <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | tolower(x) %in% c("", "null", "na")] <- NA_character_
  x
}

# A. LA totals: SUM count, not rows. Births take precedence over estimated year.
# Antenatal_only means no recorded birth year; estimated_yr is DUE year.
# All_records is deliveries + antenatal-only records, NOT total dated bookings.
hh_quick_la <- total_births_by_LA_2326
hh_quick_la$birth_yr <- hh_quick_text(hh_quick_la$birth_yr)
hh_quick_la$estimated_yr <- hh_quick_text(hh_quick_la$estimated_yr)
hh_quick_la$count <- as.numeric(as.character(hh_quick_la$count))
stopifnot(all(is.finite(hh_quick_la$count)), all(hh_quick_la$count >= 0))
hh_quick_la_table <- data.frame(FY = hh_quick_years)
hh_quick_la_table$Deliveries <- vapply(hh_quick_years, function(y)
  sum(hh_quick_la$count[hh_quick_la$birth_yr %in% y]), numeric(1))
hh_quick_la_table$Antenatal_only_due <- vapply(hh_quick_years, function(y)
  sum(hh_quick_la$count[is.na(hh_quick_la$birth_yr) &
    hh_quick_la$estimated_yr %in% y]), numeric(1))
hh_quick_la_table$All_records <- with(hh_quick_la_table, Deliveries + Antenatal_only_due)
print(hh_quick_la_table, row.names = FALSE)
cat("LA count outside these years or with no usable year:",
  sum(hh_quick_la$count) - sum(hh_quick_la_table$All_records), "\n")

# B. Pregnancy file: actual bookings by booking FY; deliveries by birth_yr.
# Counts all supplied pregnancy rows/providers, with no age/LA/home-birth filter.
# Booked_with_delivery includes recorded deliveries in ANY year; it is a subset
# of Bookings. Deliveries_in_FY is separate annual activity, not that subset.
source("R/report_cohort.R") # Date parser only; does not prepare/filter the cohort.
hh_quick_cohort <- final_cohort_2326
hh_quick_date <- report_date(hh_quick_cohort$antenatal_app_date)
hh_quick_start <- as.integer(format(hh_quick_date, "%Y")) -
  as.integer(as.integer(format(hh_quick_date, "%m")) < 4L)
hh_quick_booking_fy <- ifelse(is.na(hh_quick_start), NA_character_,
  paste0(hh_quick_start, "/", substr(hh_quick_start + 1L, 3, 4)))
hh_quick_birth <- hh_quick_text(hh_quick_cohort$birth_flag) %in% "1"
hh_quick_cohort_table <- data.frame(FY = hh_quick_years)
hh_quick_cohort_table$Bookings <- vapply(hh_quick_years, function(y)
  sum(hh_quick_booking_fy %in% y), integer(1))
hh_quick_cohort_table$Deliveries_in_FY <- vapply(hh_quick_years, function(y)
  sum(hh_quick_birth & hh_quick_text(hh_quick_cohort$birth_yr) %in% y), integer(1))
hh_quick_cohort_table$Booked_with_delivery <- vapply(hh_quick_years, function(y)
  sum(hh_quick_booking_fy %in% y & hh_quick_birth), integer(1))
hh_quick_cohort_table$Booked_no_recorded_delivery <- with(hh_quick_cohort_table,
  Bookings - Booked_with_delivery)
print(hh_quick_cohort_table, row.names = FALSE)
cat("Pregnancy rows without a usable booking date:", sum(is.na(hh_quick_date)), "\n")
# END QUICK RAW PARQUET TOTALS. Existing detailed checks follow below.

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
