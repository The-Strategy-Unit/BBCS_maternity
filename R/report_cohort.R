# Common counting and field definitions for report sections 4-10.
# The supplied cohort is already pregnancy-level: one input row contributes one.
# Do not deduplicate on different subsets of fields or drop rows with missing IDs.
# R/QA.R checks the supplied row-per-pregnancy assumption without choosing a record.

report_age_bands <- c("<20", "20-24", "25-29", "30-34", "35-39", "40+")

report_number <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))

report_code <- function(x, missing = "Unknown") {
  code <- toupper(trimws(as.character(x)))
  code[is.na(code) | code == ""] <- missing
  code
}

report_age_band <- function(x) {
  age <- report_number(x)
  dplyr::case_when(
    !is.finite(age) | age < 10 | age > 60 | age != floor(age) ~ "Unknown",
    age < 20 ~ "<20", age < 25 ~ "20-24", age < 30 ~ "25-29",
    age < 35 ~ "30-34", age < 40 ~ "35-39", TRUE ~ "40+")
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
    "birth_date", "la_of_mother", "age_at_birth_mother", "birth_provider",
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

  tibble::tibble(
    Row = seq_len(nrow(cohort)),
    Person = trimws(as.character(cohort$person_id_mother)),
    Pregnancy = trimws(as.character(cohort$uniq_preg_id)),
    Year = year, Delivery_year = start, Delivery_date = report_date(cohort$birth_date),
    Birth_flag = report_number(cohort$birth_flag),
    Birth_recorded = Birth_flag %in% 1,
    LA = report_code(cohort$la_of_mother, "Unknown LA"),
    Age = report_number(cohort$age_at_birth_mother),
    Age_band = report_age_band(cohort$age_at_birth_mother),
    Trust = report_code(cohort$birth_provider, "Unknown provider"),
    Home = report_number(cohort$home_birth),
    Resident_ICB = report_code(cohort$resident_icb),
    Booking_Trust = report_code(cohort$booking_provider),
    Booking_date = booking_date,
    Booking_year = as.integer(format(booking_date, "%Y")) -
      as.integer(as.integer(format(booking_date, "%m")) < 4L))
}
