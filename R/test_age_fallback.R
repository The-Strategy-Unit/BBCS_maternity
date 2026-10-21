# Synthetic age-fallback regression; no Azure data or production output files.
# Run from the project root: Rscript R/test_age_fallback.R
local({
  code <- parse("R/Maternity analysis.R")
  for (statement in code) {
    if (is.call(statement) && identical(statement[[1]], as.name("<-")) &&
        identical(statement[[2]], as.name("required_inputs"))) break
    eval(statement)
  }
  for (statement in code) if (is.call(statement) && identical(statement[[1]], as.name("<-")) &&
      is.call(statement[[3]]) && identical(statement[[3]][[1]], as.name("function"))) eval(statement)
  near <- function(x, y) stopifnot(isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance=1e-10)))
  make_cohort <- function(birth_age, booking_age) {
    n <- length(birth_age)
    data.frame(person_id_mother=seq_len(n), uniq_preg_id=seq_len(n),
      birth_yr="2025/26", birth_flag=1L, birth_date=as.Date("2025-12-01"),
      la_of_mother="E08000025", age_at_birth_mother=birth_age,
      age_at_booking_mother=booking_age, birth_provider="RQ3", home_birth=0L,
      resident_icb="QHL", booking_provider="RQ3", antenatal_app_date=as.Date("2025-06-01"))
  }
  cohort <- make_cohort(c(31, NA, NA, 20, NA, 44, NA, NA, 12, 30.5, Inf),
    c(30, 39, NA, 19, 19, 43, 44, 54, 13, 30, 29))
  original <- cohort
  prepared <- prepare_report_cohort(cohort)
  near(prepared$Age, c(31,39,NA,20,19,44,44,54,12,30.5,Inf))
  stopifnot(identical(cohort, original), nrow(prepared) == nrow(cohort),
    identical(prepared$Age_source, c("Birth","Booking fallback","Unknown","Birth",
      "Booking fallback","Birth","Booking fallback","Booking fallback","Birth","Birth","Birth")),
    identical(prepared$Age_band, c("30-34","35-39","Unknown","20-24","Under 20",
      "40-44","40-44","45+","Under 20","30.5","Inf")))
  typed <- make_cohort(factor(c("31", NA, NA)), factor(c("30", "39", NA)))
  near(prepare_report_cohort(typed)$Age, c(31,39,NA))
  # Supplied values remain visible labels; only NA triggers booking fallback.
  supplied <- make_cohort(c("-20", "green", NA), c("30", "28", "green"))
  stopifnot(identical(prepare_report_cohort(supplied)$Age_value, c("-20", "green", "green")),
    identical(prepare_report_cohort(supplied)$Age_band, c("-20", "green", "green")))
  typed$age_at_birth_mother <- c("31", NA, "invalid")
  typed$age_at_booking_mother <- c("30", "39", "28")
  near(prepare_report_cohort(typed)$Age, c(31,39,NA))
  missing_column <- cohort; missing_column$age_at_booking_mother <- NULL
  failure <- try(prepare_report_cohort(missing_column), silent=TRUE)
  stopifnot(inherits(failure, "try-error"), grepl("age_at_booking_mother", failure))

  # Recover known ages across every model age, including 13/15 and 44/54.
  # A deliberately different booking age on nonmissing births must be ignored.
  full <- make_cohort(rep(13:54, each=6), rep(13:54, each=6)-1)
  full$birth_flag <- rep(c(1L,1L,1L,1L,0L,0L), 42)
  recovered <- seq(2, nrow(full), by=2)
  full$age_at_booking_mother[recovered] <- full$age_at_birth_mother[recovered]
  fallback <- full
  fallback$age_at_birth_mother[recovered] <- NA_real_
  residents <- full |> dplyr::filter(birth_flag == 1) |>
    dplyr::count(la_of_mother,age_at_birth_mother,birth_yr,name="count") |>
    dplyr::mutate(estimated_yr = NA_character_, age_at_booking_mother = age_at_birth_mother)
  residents <- dplyr::bind_rows(residents,
    full |> dplyr::filter(birth_flag == 0) |>
      dplyr::transmute(la_of_mother, estimated_yr = birth_yr, birth_yr = NA_character_,
        age_at_booking_mother = age_at_birth_mother, age_at_birth_mother = NA_real_) |>
      dplyr::count(la_of_mother, estimated_yr, birth_yr,
        age_at_booking_mother, age_at_birth_mother, name = "count"))
  ons <- tidyr::expand_grid(area_code="E08000025",age_group=as.character(15:44),year=2023:2027) |>
    dplyr::mutate(area_name=area_code,projected_births=100)
  counts <- read_national_age_counts()
  baseline <- build_projection_master(full,residents,ons,counts,start_year=2023)
  actual <- build_projection_master(fallback,residents,ons,counts,start_year=2023)
  fields <- c("Age_weight","Private_NHS_factor","Bookings_uplift_modifier",
    "Adjusted_maternities", grep("^(Booking|Delivery)_count_",names(actual),value=TRUE))
  for (field in fields) near(actual[[field]],baseline[[field]])
  for (field in c("residents","calibration","booking","delivery"))
    stopifnot(isTRUE(all.equal(attr(actual,"allocation_tables")[[field]],
      attr(baseline,"allocation_tables")[[field]],check.attributes=FALSE)))
  # Historical counts use the same age, without changing all-age activity totals.
  observed <- function(x) prepare_report_cohort(x) |>
    dplyr::count(Year, Booking_year, Birth_recorded, Age, Age_band, Trust, Booking_Trust)
  stopifnot(identical(observed(full), observed(fallback)))
  # If both ages are missing the record remains visible but is not age-allocated.
  fallback$age_at_booking_mother[2] <- NA_real_
  unresolved <- prepare_report_cohort(fallback)
  stopifnot(sum(is.na(unresolved$Age)) == 1L, nrow(unresolved) == nrow(full))
  # Independent diagnostic counts: seven birth-age NAs, four without a recorded
  # birth, two unknown/invalid birth flags, and three with neither age recorded.
  diagnostic <- make_cohort(c(30,NA,NA,NA,NA,NA,30,NA,14,NA),
    c(29,29,NA,28,NA,27,NA,NA,13,25))
  diagnostic$birth_flag <- c(1,1,0,0,NA,2,1,0,1,0)
  diagnostic$birth_yr[3] <- NA_character_ # Must not exclude no-birth records.
  raw <- summarise_maternal_age_completeness(diagnostic)
  near(raw$N, c(10,7,1,4,2,4,3))
  near(raw$Denominator, c(10,10,7,7,7,7,10))
  # Control the completed counts, not the component records; derive percentages
  # from controlled numerator/denominator, including the small-count case.
  display <- maternal_age_completeness_table(raw)[['_data']]
  near(display$N, c(10,5,5,5,5,5,5))
  near(display$Percent, c(1,.5,1,1,1,1,.5))
  stopifnot(identical(names(display),c('Measure','N','Percent','Percentage_of')))
  bigger <- summarise_maternal_age_completeness(diagnostic[rep(seq_len(10),each=100), ])
  display <- maternal_age_completeness_table(bigger)[['_data']]
  near(display$N, c(1000,700,100,400,200,400,300))
  near(display$Percent, c(1,.7,1/7,4/7,2/7,4/7,.3))
  complete <- summarise_maternal_age_completeness(full)
  display <- maternal_age_completeness_table(complete)[['_data']]
  stopifnot(all(is.na(display$Percent[3:6])), all(display$N[2:7] == 0))
  empty <- summarise_maternal_age_completeness(full[FALSE, ])
  stopifnot(all(empty$N == 0), all(is.na(maternal_age_completeness_table(empty)[['_data']]$Percent)))
  # Compare each booking-age band on the same age basis. The first row is
  # age 20 at birth but 19 at booking, so it belongs in this diagnostic's under-20 band.
  bands <- make_cohort(c(20,NA,NA,26,NA,35,NA,NA,46,NA,NA,NA),
    c(19,19,24,25,29,34,39,44,45,50,NA,30.5))
  band_counts <- summarise_booking_age_fallback(bands)
  near(band_counts$Booking_age_known, c(2,1,2,1,1,1,2,1,1))
  near(band_counts$Birth_age_missing, c(1,1,1,0,1,1,1,1,1))
  stopifnot(identical(band_counts$Age_band, c(report_age_bands, "Unknown", "30.5")))
  band_outputs <- booking_age_fallback_outputs(band_counts)
  shown <- band_outputs$table[['_data']]
  near(shown$Booking_age_known, rep(5,9))
  near(shown$Birth_age_missing, c(5,5,5,0,5,5,5,5,5))
  near(shown$Share, c(1,1,1,0,1,1,1,1,1))
  near(plotly::plotly_build(band_outputs$chart)$x$data[[1]]$y, shown$Share)
  bigger_bands <- summarise_booking_age_fallback(bands[rep(seq_len(nrow(bands)),each=100), ])
  shown <- booking_age_fallback_outputs(bigger_bands)$table[['_data']]
  near(shown$Share, c(.5,1,.5,0,1,1,.5,1,1))
  empty_bands <- summarise_booking_age_fallback(bands[FALSE, ])
  empty_outputs <- booking_age_fallback_outputs(empty_bands)
  stopifnot(all(is.na(empty_outputs$table[['_data']]$Share)))

  # Delivery, appointment and due years remain independent; neither missing
  # dates nor month-only delivery dates acquire an invented day.
  dates <- make_cohort(c(30, NA, 34, NA), c(29, 28, 35, NA))
  dates$birth_yr <- c("2024/25", NA, "2025/26", NA)
  dates$birth_date <- c("2025-02", "2025-09", NA, NA)
  dates$edd_yr <- c("2024/25", "2025/26", "2025/26", "2026/27")
  dates$antenatal_app_date <- c("2025-03-31", "2025-04-01", NA, "2025-04")
  dates$birth_flag <- c(1, 0, 1, 0)
  dates$booking_provider <- c("RQ3", NA, "", "RQ3")
  before <- dates
  resolved <- prepare_report_cohort(dates)
  near(resolved$Delivery_year, c(2024, NA, 2025, NA))
  near(resolved$Booking_year, c(2024, 2025, NA, NA))
  stopifnot(identical(resolved$Birth_recorded, c(TRUE, FALSE, TRUE, FALSE)),
    all(is.na(resolved$Delivery_date)), identical(dates, before))
  completeness <- summarise_record_completeness(dates)
  totals <- completeness$fields |>
    dplyr::group_by(Scope, Field) |> dplyr::summarise(N = sum(N), .groups = "drop")
  stopifnot(all(totals$N == nrow(dates)),
    any(completeness$fields$Group == "Unknown (NA)"),
    any(completeness$fields$Group == "Unknown (blank)"))
  source_totals <- completeness$age_sources |>
    dplyr::group_by(Scope) |> dplyr::summarise(N = sum(N), .groups = "drop")
  stopifnot(all(source_totals$N == nrow(dates)))
  for (view in record_completeness_outputs(completeness)$by_scope) {
    stopifnot(nchar(gt::as_raw_html(view$age_table)) > 0,
      nchar(gt::as_raw_html(view$field_table)) > 0)
    plotly::plotly_build(view$chart)
  }
  cat("PASS: event-year separation, no invented dates, due year never establishes delivery, unknown years/providers and completeness chart/table counts.\n")
  cat("PASS: within-booking-age prevalence, birthdays across bands, missing/invalid booking ages, completed-count disclosure controls, zero/absent groups and chart/table agreement.\n")
  cat("PASS: age-completeness counts, birth-status separation, whole-cohort and missing-age denominators, disclosure-controlled percentages, no missing ages and empty cohort.\n")
  cat("PASS: birth-age precedence, booking fallback, factors/characters, unknown/invalid ages, required input column, all 13-54 model ages, calibration, private factors, both allocation paths and historical age totals.\n")
})
