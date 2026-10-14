# Common counting and field definitions for the MSDS and projection views.
# The supplied cohort is already pregnancy-level: one input row contributes one.
# Do not deduplicate on different subsets of fields or drop rows with missing IDs.
# R/QA.R checks the supplied row-per-pregnancy assumption without choosing a record.

# Final reporting bands; apply after the individual-age model calculations.
report_age_bands <- c("Under 20", "20-24", "25-29", "30-34", "35-39", "40-44", "45+")

report_number <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  suppressWarnings(as.numeric(trimws(as.character(x))))
}

# Output aggregation only; never use this to fill missing source inputs.
sum_available_projection <- function(x) {
  if (!length(x)) return(0) # an empty structural category
  if (!any(is.finite(x))) return(NA_real_) # no usable estimate
  sum(x[is.finite(x)])
}

# Exact maternal age for calculations; invalid ages share one explicit missing cell.
report_age <- function(x) {
  age <- report_number(x)
  age[!is.finite(age) | age != floor(age)] <- NA_real_
  age
}

# ONS publishes combined boundary rows, not separate births below 15 or above 44.
# Retain MSDS Age and use this separate key only to select an ONS source boundary.
report_ons_age <- function(x) pmin(44, pmax(15, report_age(x)))

report_code <- function(x, missing = "Unknown") {
  code <- toupper(trimws(as.character(x)))
  code[is.na(code) | code == ""] <- missing
  code
}

# Local, versioned ONS crosswalk: names never determine a match. Read once
# per source() rather than repeatedly for each chart/age/year calculation.
report_la_crosswalk <- read.csv("data/lookups/la_to_2023.csv", stringsAsFactors = FALSE)
stopifnot(!anyDuplicated(report_la_crosswalk$Source_code),
  !anyNA(report_la_crosswalk$Target_code),
  !any(report_la_crosswalk$Target_code %in% report_la_crosswalk$Source_code))

# Harmonise MSDS residence only. No rows are replicated, dropped or weighted;
# source parquet objects and ONS inputs are never modified.
report_la_code <- function(x, missing = "Unknown LA") {
  code <- report_code(x, missing)
  row <- match(code, report_la_crosswalk$Source_code)
  mapped <- !is.na(row)
  code[mapped] <- report_la_crosswalk$Target_code[row[mapped]]
  code
}

report_applied_geography <- function(cohort_codes, resident_codes = character()) {
  used <- unique(report_code(c(cohort_codes, resident_codes)))
  report_la_crosswalk[report_la_crosswalk$Source_code %in% used,
    c("Source_code", "Source_name", "Target_code", "Target_name", "Treatment"), drop = FALSE]
}

report_age_band <- function(x) {
  age <- report_number(x)
  dplyr::case_when(
    !is.finite(age) | age != floor(age) ~ "Unknown",
    age < 20 ~ "Under 20", age < 25 ~ "20-24", age < 30 ~ "25-29",
    age < 35 ~ "30-34", age < 40 ~ "35-39", age < 45 ~ "40-44", TRUE ~ "45+")
}

report_date <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x))
  value <- trimws(as.character(x))
  date <- as.Date(value, format = "%Y-%m-%d")
  missing <- is.na(date) & !is.na(value)
  date[missing] <- as.Date(value[missing], format = "%d/%m/%Y")
  date
}

prepare_report_cohort <- function(cohort) {
  required <- c("person_id_mother", "uniq_preg_id", "birth_yr", "birth_flag",
    "birth_date", "la_of_mother", "age_at_birth_mother", "age_at_booking_mother", "birth_provider",
    "home_birth", "resident_icb", "booking_provider", "antenatal_app_date")
  absent <- setdiff(required, names(cohort))
  if (length(absent)) stop("final_cohort_2326 is missing: ",
    paste(absent, collapse = ", "), call. = FALSE)

  year <- trimws(as.character(cohort$birth_yr))
  start <- suppressWarnings(as.integer(substr(year, 1, 4)))
  valid_year <- !is.na(year) & grepl("^[0-9]{4}/[0-9]{2}$", year) &
    year == paste0(start, "/", substr(start + 1L, 3, 4))
  year[!valid_year] <- NA_character_
  start[!valid_year] <- NA_integer_
  booking_date <- report_date(cohort$antenatal_app_date)
  # Birth age takes precedence. Only a source NA permits the booking fallback;
  # a supplied but invalid birth age remains an explicit data-quality issue.
  birth_age <- report_age(cohort$age_at_birth_mother)
  booking_age <- report_age(cohort$age_at_booking_mother)
  use_booking_age <- is.na(cohort$age_at_birth_mother)
  age <- birth_age
  age[use_booking_age] <- booking_age[use_booking_age]

  tibble::tibble(
    Row = seq_len(nrow(cohort)),
    Person = trimws(as.character(cohort$person_id_mother)),
    Pregnancy = trimws(as.character(cohort$uniq_preg_id)),
    Year = year, Delivery_year = start, Delivery_date = report_date(cohort$birth_date),
    Birth_flag = report_number(cohort$birth_flag),
    Birth_recorded = Birth_flag %in% 1,
    LA_source = report_code(cohort$la_of_mother, "Unknown LA"),
    LA = report_la_code(cohort$la_of_mother),
    Age_at_birth = birth_age, Age_at_booking = booking_age,
    Age = age,
    Age_source = ifelse(is.na(age), "Unknown",
      ifelse(use_booking_age, "Booking fallback", "Birth")),
    Age_band = report_age_band(age),
    Trust = report_code(cohort$birth_provider, "Unknown provider"),
    Home = report_number(cohort$home_birth),
    Resident_ICB = report_code(cohort$resident_icb),
    Booking_Trust = report_code(cohort$booking_provider),
    Booking_provider_supplied = !is.na(cohort$booking_provider) &
      nzchar(trimws(as.character(cohort$booking_provider))),
    Booking_date_supplied = !is.na(cohort$antenatal_app_date) &
      nzchar(trimws(as.character(cohort$antenatal_app_date))),
    Booking_date = booking_date,
    Booking_year = as.integer(format(booking_date, "%Y")) -
      as.integer(as.integer(format(booking_date, "%m")) < 4L))
}

# Descriptive diagnostic over the whole supplied cohort, before age/year filters.
# Missing here means source NA, not an invalid or out-of-model-range age.
summarise_maternal_age_completeness <- function(cohort) {
  records <- prepare_report_cohort(cohort)
  missing_birth <- is.na(cohort$age_at_birth_mother)
  missing_booking <- is.na(cohort$age_at_booking_mother)
  n <- nrow(cohort)
  missing_n <- sum(missing_birth)
  tibble::tibble(
    Measure = c("All pregnancy records", "Age at birth missing",
      "Missing birth age: birth recorded", "Missing birth age: no birth recorded",
      "Missing birth age: birth status unknown/invalid",
      "Missing birth age: recovered from booking age", "Neither birth nor booking age recorded"),
    N = c(n, missing_n, sum(missing_birth & records$Birth_flag %in% 1),
      sum(missing_birth & records$Birth_flag %in% 0),
      sum(missing_birth & !records$Birth_flag %in% c(0, 1)),
      sum(records$Age_source == "Booking fallback"), sum(missing_birth & missing_booking)),
    Denominator = c(n, n, rep(missing_n, 4), n),
    Percentage_of = c(rep("Full cohort", 2), rep("Records missing birth age", 4), "Full cohort"))
}

# Diagnostic only: classify numerator AND denominator by booking age so that
# birthdays between booking and delivery do not move the comparison population.
summarise_booking_age_fallback <- function(cohort) {
  records <- prepare_report_cohort(cohort)
  band <- report_age_band(records$Age_at_booking)
  missing_birth <- is.na(cohort$age_at_birth_mother)
  tibble::tibble(Age_band = report_age_bands,
    Booking_age_known = vapply(report_age_bands, function(b) sum(band == b), integer(1)),
    Birth_age_missing = vapply(report_age_bands,
      function(b) sum(band == b & missing_birth), integer(1)))
}

# Keep the non-provider categories explicit in the projection allocations.
projection_delivery_destination <- function(records) {
  dplyr::case_when(
    !records$Birth_recorded ~ "No_recorded_delivery",
    records$Home %in% 1 ~ "Home",
    records$Home %in% 0 & grepl("^R[A-Z0-9]{2}$", records$Trust) ~ records$Trust,
    TRUE ~ "Unknown_location")
}

# Agreed recorded-evidence rule: both provider and date absent means no
# recorded booking. A supplied date remains booking evidence even when it
# cannot be parsed.
projection_booking_destination <- function(records) {
  dplyr::case_when(
    !records$Booking_provider_supplied & !records$Booking_date_supplied ~ "No_recorded_booking",
    grepl("^R[A-Z0-9]{2}$", records$Booking_Trust) ~ records$Booking_Trust,
    TRUE ~ "Booking_provider_unknown")
}

projection_destination_label <- function(destination) {
  labels <- c(Home = "Home birth", Unknown_location = "Delivery location unknown",
    No_recorded_delivery = "No recorded delivery", No_recorded_booking = "No recorded booking",
    Booking_provider_unknown = "Booking provider unknown",
    Booking_Denominator_Factor = "Booking Denominator Factor",
    Not_represented = "Not represented in linked cohort", Unallocated = "Unallocated")
  result <- unname(labels[destination])
  result[is.na(result)] <- destination[is.na(result)]
  result
}
