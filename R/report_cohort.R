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

# Numeric copy for arithmetic only. Supplied age labels are retained separately.
report_age <- function(x) report_number(x)

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

# Harmonise MSDS residence only. No rows are replicated, dropped or weighted;
# source parquet objects and ONS inputs are never modified.
report_la_code <- function(x, missing = "Unknown LA") {
  code <- report_code(x, missing)
  row <- match(code, report_la_crosswalk$Source_code)
  mapped <- !is.na(row)
  code[mapped] <- report_la_crosswalk$Target_code[row[mapped]]
  code
}

report_la_country <- function(x) {
  code <- report_code(x)
  country <- c(E = "England", W = "Wales", S = "Scotland", N = "Northern Ireland")
  result <- unname(country[substr(code, 1, 1)])
  result[!grepl("^[ENSW][0-9]{8}$", code)] <- "Unknown/other code"
  result
}

report_country_coverage <- function(episodes) {
  countries <- c("England", "Wales", "Scotland", "Northern Ireland", "Unknown/other code")
  records <- episodes |>
    dplyr::transmute(FY = dplyr::coalesce(Pregnancy_FY, "Unknown FY"),
      Country = report_la_country(LA),
      Booking = Booking_provider_supplied | Booking_date_supplied,
      Delivery = Birth_recorded)
  activities <- c("Whole cohort", "Bookings", "Deliveries")
  years <- sort(unique(c("2023/24", "2024/25", "2025/26", records$FY)))
  counts <- dplyr::bind_rows(
    dplyr::mutate(records, Activity = "Whole cohort"),
    dplyr::mutate(dplyr::filter(records, Booking), Activity = "Bookings"),
    dplyr::mutate(dplyr::filter(records, Delivery), Activity = "Deliveries")) |>
    dplyr::count(FY, Activity, Country, name = "N") |>
    tidyr::complete(FY = years, Activity = activities, Country = countries, fill = list(N = 0L)) |>
    dplyr::group_by(FY, Activity) |>
    dplyr::mutate(Denominator = sum(N)) |>
    dplyr::ungroup()
  shown <- dc_cells(counts, c("N", "Denominator"), list(Share = c("N", "Denominator")))
  tables <- stats::setNames(lapply(activities, function(activity) {
    n_columns <- paste0(years, "_N")
    pct_columns <- paste0(years, "_Share")
    columns <- as.vector(rbind(n_columns, pct_columns))
    table <- shown |>
      dplyr::filter(Activity == activity, Country %in% countries[1:3]) |>
      dplyr::select(Country, FY, N, Share) |>
      tidyr::pivot_wider(names_from = FY, values_from = c(N, Share), names_glue = "{FY}_{.value}") |>
      dplyr::arrange(match(Country, countries)) |>
      dplyr::select(Country, dplyr::all_of(columns)) |>
      gt::gt(rowname_col = "Country") |>
      gt::tab_header(title = paste(activity, "by country of resident LA")) |>
      gt::cols_label(.list = stats::setNames(rep(c("n", "%"), length(years)), columns)) |>
      gt::fmt_number(columns = dplyr::all_of(n_columns), decimals = 0) |>
      gt::fmt_percent(columns = dplyr::all_of(pct_columns), decimals = 1) |>
      gt::sub_missing(missing_text = "Unavailable") |>
      gt::tab_source_note("Pregnancies, not distinct mothers. Each FY percentage uses all countries, including Northern Ireland and unknown residence, in that activity's denominator. The three displayed countries need not sum to 100%.") |>
      gt::tab_options(table.width = gt::pct(100), container.overflow.x = TRUE)
    for (i in seq_along(years)) table <- gt::tab_spanner(table,
      label = years[i], columns = dplyr::all_of(c(n_columns[i], pct_columns[i])))
    table
  }), c("cohort", "booking", "delivery"))
  list(counts = counts, display = shown, tables = tables)
}

report_applied_geography <- function(cohort_codes, resident_codes = character()) {
  used <- unique(report_code(c(cohort_codes, resident_codes)))
  report_la_crosswalk[report_la_crosswalk$Source_code %in% used,
    c("Source_code", "Source_name", "Target_code", "Target_name", "Treatment"), drop = FALSE]
}

report_age_band <- function(x) {
  age <- report_age(x)
  dplyr::case_when(
    is.na(x) ~ "Unknown",
    !is.finite(age) | age < 0 | age != floor(age) ~ as.character(x),
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

report_financial_year <- function(x) {
  year <- trimws(as.character(x))
  start <- suppressWarnings(as.integer(substr(year, 1, 4)))
  valid <- !is.na(year) & grepl("^[0-9]{4}/[0-9]{2}$", year) &
    year == paste0(start, "/", substr(start + 1L, 3, 4))
  year[!valid] <- NA_character_
  year
}

report_fy_start <- function(x) suppressWarnings(as.integer(substr(report_financial_year(x), 1, 4)))

report_year_missing <- function(x) is.na(x) | trimws(as.character(x)) == ""

report_pregnancy_fy <- function(birth_year, due_year) {
  report_financial_year(ifelse(report_year_missing(birth_year),
    as.character(due_year), as.character(birth_year)))
}

report_resident_group <- function(x) {
  value <- gsub("[[:space:]_-]", "", tolower(as.character(x)))
  dplyr::case_when(
    value == "antenatalonly" ~ "antenatal only",
    value == "birthonly" ~ "birth only",
    value == "antenatal+delivery" ~ "antenatal+delivery",
    TRUE ~ NA_character_)
}

prepare_la_records <- function(residents) {
  required <- c("la_of_mother", "birth_yr", "edd_yr", "age_at_birth_mother",
    "age_at_booking_mother", "group", "count")
  absent <- setdiff(required, names(residents))
  if (length(absent)) stop("total_births_by_LA_2326 is missing: ",
    paste(absent, collapse = ", "), call. = FALSE)
  residents |>
    dplyr::mutate(count = report_number(count),
      Pregnancy_FY = report_pregnancy_fy(birth_yr, edd_yr),
      Pathway_group = report_resident_group(group),
      Has_delivery = Pathway_group %in% c("birth only", "antenatal+delivery"),
      Has_antenatal = Pathway_group %in% c("antenatal only", "antenatal+delivery"),
      Age_value = dplyr::if_else(is.na(age_at_birth_mother),
        as.character(age_at_booking_mother), as.character(age_at_birth_mother)),
      Age = report_age(Age_value))
}

# Group defines the pathway; dates only assign its shared FY. Keep NA counts.
# Output year/age fields are the assigned pregnancy keys for existing consumers.
prepare_la_deliveries <- function(residents, missing_count_zero = FALSE) {
  residents <- prepare_la_records(residents)
  if (missing_count_zero) residents$count <- dplyr::coalesce(residents$count, 0)
  residents |>
    dplyr::filter(Has_delivery) |>
    dplyr::mutate(birth_yr = Pregnancy_FY, age_at_birth_mother = Age) |>
    dplyr::group_by(la_of_mother, birth_yr, age_at_birth_mother, .drop = FALSE) |>
    dplyr::summarise(count = sum(count), .groups = "drop")
}

prepare_la_antenatal_only <- function(residents, missing_count_zero = FALSE) {
  residents <- prepare_la_records(residents)
  if (missing_count_zero) residents$count <- dplyr::coalesce(residents$count, 0)
  residents |>
    dplyr::filter(Pathway_group == "antenatal only") |>
    dplyr::mutate(edd_yr = Pregnancy_FY, age_at_booking_mother = Age) |>
    dplyr::group_by(la_of_mother, edd_yr, age_at_booking_mother, .drop = FALSE) |>
    dplyr::summarise(count = sum(count), .groups = "drop")
}

prepare_report_cohort <- function(cohort) {
  required <- c("person_id_mother", "uniq_preg_id", "birth_yr", "birth_flag",
    "birth_date", "la_of_mother", "age_at_birth_mother", "age_at_booking_mother", "birth_provider",
    "home_birth", "resident_icb", "booking_provider", "antenatal_app_date")
  absent <- setdiff(required, names(cohort))
  if (length(absent)) stop("final_cohort_2326 is missing: ",
    paste(absent, collapse = ", "), call. = FALSE)

  due_year <- if ("edd_yr" %in% names(cohort)) cohort$edd_yr else rep(NA_character_, nrow(cohort))
  # One pregnancy, one FY for both event views; appointment dates never assign it.
  year <- report_pregnancy_fy(cohort$birth_yr, due_year)
  start <- report_fy_start(year)
  booking_date <- report_date(cohort$antenatal_app_date)
  # Agreed maternal-age rule: birth age takes priority; only a source NA
  # permits booking age. Keep Age_source visible rather than relabelling the
  # fallback as an exact age at delivery.
  birth_age <- report_age(cohort$age_at_birth_mother)
  booking_age <- report_age(cohort$age_at_booking_mother)
  use_booking_age <- is.na(cohort$age_at_birth_mother)
  age_value <- as.character(cohort$age_at_birth_mother)
  age_value[use_booking_age] <- as.character(cohort$age_at_booking_mother[use_booking_age])
  age <- birth_age
  age[use_booking_age] <- booking_age[use_booking_age]

  tibble::tibble(
    Row = seq_len(nrow(cohort)),
    Person = trimws(as.character(cohort$person_id_mother)),
    Pregnancy = trimws(as.character(cohort$uniq_preg_id)),
    Year = year, Pregnancy_FY = year, Pregnancy_year = start,
    Birth_FY = report_financial_year(cohort$birth_yr), Due_FY = report_financial_year(due_year),
    Pregnancy_year_source = ifelse(!report_year_missing(cohort$birth_yr), "Birth year",
      ifelse(!report_year_missing(due_year), "EDD year", "Missing")),
    Delivery_year = start, Delivery_date = report_date(cohort$birth_date),
    Birth_flag = report_number(cohort$birth_flag),
    Birth_recorded = Birth_flag %in% 1,
    LA_source = report_code(cohort$la_of_mother, "Unknown LA"),
    LA = report_la_code(cohort$la_of_mother),
    Age_at_birth = birth_age, Age_at_booking = booking_age,
    Age = age, Age_value = age_value,
    Age_source = ifelse(is.na(age_value), "Unknown",
      ifelse(use_booking_age, "Booking fallback", "Birth")),
    Age_band = report_age_band(age_value),
    Trust = report_code(cohort$birth_provider, "Unknown provider"),
    Home = report_number(cohort$home_birth),
    Resident_ICB = report_code(cohort$resident_icb),
    Booking_Trust = report_code(cohort$booking_provider),
    Booking_provider_supplied = !is.na(cohort$booking_provider) &
      nzchar(trimws(as.character(cohort$booking_provider))),
    Booking_date_supplied = !is.na(cohort$antenatal_app_date) &
      nzchar(trimws(as.character(cohort$antenatal_app_date))),
    Booking_date = booking_date,
    Booking_year = start)
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
      "Missing birth age: booking age used", "Neither birth nor booking age recorded"),
    N = c(n, missing_n, sum(missing_birth & records$Birth_flag %in% 1),
      sum(missing_birth & records$Birth_flag %in% 0),
      sum(missing_birth & !records$Birth_flag %in% c(0, 1)),
      sum(missing_birth & !missing_booking), sum(missing_birth & missing_booking)),
    Denominator = c(n, n, rep(missing_n, 4), n),
    Percentage_of = c(rep("Full cohort", 2), rep("Records missing birth age", 4), "Full cohort"))
}

# Diagnostic only: classify numerator AND denominator by booking age so that
# birthdays between booking and delivery do not move the comparison population.
summarise_booking_age_fallback <- function(cohort) {
  band <- report_age_band(cohort$age_at_booking_mother)
  missing_birth <- is.na(cohort$age_at_birth_mother)
  bands <- unique(c(report_age_bands, "Unknown", band))
  tibble::tibble(Age_band = bands,
    Booking_age_known = vapply(bands, function(b) sum(band == b), integer(1)),
    Birth_age_missing = vapply(bands,
      function(b) sum(band == b & missing_birth), integer(1)))
}

# Descriptive report output, not a validation gate. Every row contributes to
# every field's denominator, including records with no year or provider.
# Original fields and the agreed age-selection rule remain unchanged.
summarise_record_completeness <- function(cohort) {
  records <- prepare_report_cohort(cohort)
  label_group <- function(x) {
    value <- as.character(x)
    value[is.na(value)] <- "Unknown (NA)"
    value[!is.na(value) & trimws(value) == ""] <- "Unknown (blank)"
    value
  }
  groups <- list(
    "Whole extract" = rep("All pregnancy rows", nrow(cohort)),
    "Supplied delivery year" = label_group(cohort$birth_yr),
    "Booking provider" = label_group(cohort$booking_provider),
    "Delivery provider" = label_group(cohort$birth_provider))
  fields <- c(age_at_birth_mother = "Age at birth", age_at_booking_mother = "Age at booking",
    birth_yr = "Supplied delivery year", antenatal_app_date = "Booking appointment date",
    edd_yr = "Supplied due year", birth_date = "Supplied birth date")
  status <- lapply(names(fields), function(field) {
    if (!field %in% names(cohort)) return(rep("Column absent", nrow(cohort)))
    value <- cohort[[field]]
    if (field %in% c("age_at_birth_mother", "age_at_booking_mother")) {
      value <- as.character(value)
      value[is.na(value)] <- "Missing (NA)"
      return(value)
    }
    text <- trimws(as.character(value))
    state <- rep("Supplied", length(value))
    state[is.na(value)] <- "Missing (NA)"
    state[!is.na(text) & text == ""] <- "Missing (blank)"
    state[!is.na(text) & toupper(text) %in% c("NULL", "NA", "N/A", "UNKNOWN", "NOT KNOWN")] <-
      "Missing marker"
    state
  })
  names(status) <- unname(fields)
  age_sources <- lapply(names(groups), function(scope) {
    tibble::tibble(Scope = scope, Group = groups[[scope]], Age_source = records$Age_source) |>
      dplyr::count(Scope, Group, Age_source, name = "N") |>
      dplyr::group_by(Scope, Group) |>
      dplyr::mutate(Total = sum(N)) |> dplyr::ungroup()
  })
  field_counts <- lapply(names(groups), function(scope) {
    dplyr::bind_rows(lapply(names(status), function(field) {
      tibble::tibble(Scope = scope, Group = groups[[scope]], Field = field,
        Status = status[[field]]) |>
        dplyr::count(Scope, Group, Field, Status, name = "N") |>
        dplyr::group_by(Scope, Group, Field) |>
        dplyr::mutate(Total = sum(N)) |> dplyr::ungroup()
    }))
  })
  list(age_sources = dplyr::bind_rows(age_sources),
    fields = dplyr::bind_rows(field_counts))
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
