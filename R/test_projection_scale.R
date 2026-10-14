# Independent scale regression: invented pregnancies only; no Azure or saved report inputs.
# Run from the project root: Rscript R/test_projection_scale.R
local({
  code <- parse("R/Maternity analysis.R")
  for (statement in code) {
    if (is.call(statement) && identical(statement[[1]], as.name("<-")) &&
        identical(statement[[2]], as.name("required_inputs"))) break
    eval(statement)
  }
  for (statement in code) {
    if (is.call(statement) && identical(statement[[1]], as.name("<-")) &&
        is.call(statement[[3]]) && identical(statement[[3]][[1]], as.name("function"))) eval(statement)
  }
  # Neutral Private-NHS evidence isolates these existing scale/allocation cases.
  # test_private_nhs.R exercises actual Table 4 rates through the full model.
  private_test_counts <- read_private_nhs_counts()
  private_test_counts$NHS_maternities <- private_test_counts$NHS_maternities + private_test_counts$Non_NHS_maternities
  private_test_counts$Non_NHS_maternities <- 0
  build_master_with_private <- build_projection_master
  build_projection_master <- function(...) build_master_with_private(..., private_nhs_counts = private_test_counts)
  near <- function(actual, expected) {
    stopifnot(isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = 1e-8)))
  }
  projection_la_lookup <- readr::read_csv("data/lookups/la_names.csv", show_col_types = FALSE)
  projection_trust_lookup <- readr::read_csv("data/lookups/trust_names.csv", show_col_types = FALSE)
  projection_catchment_las <- cluster_las

  # Every year: 600 deliveries, 720 bookings, spread over all ONS source ages,
  # two LAs, two trusts and two ICBs. National conversion is exactly one.
  cells <- tidyr::expand_grid(LA = c("E08000025", "E08000031"), Age = 15:44,
    FY = c("2023/24", "2024/25", "2025/26"), Pregnancy_in_cell = 1:12)
  trust <- ifelse(cells$LA == "E08000025", "RQ3", "RL4")
  final_cohort_2326 <- data.frame(person_id_mother = seq_len(nrow(cells)),
    uniq_preg_id = seq_len(nrow(cells)), birth_yr = cells$FY,
    birth_flag = as.integer(cells$Pregnancy_in_cell <= 10),
    birth_date = as.Date(paste0(substr(cells$FY, 1, 4), "-12-01")),
    la_of_mother = cells$LA, age_at_birth_mother = cells$Age, age_at_booking_mother = NA_real_,
    birth_provider = trust, home_birth = 0L, resident_icb = unname(provider_icb[trust]),
    booking_provider = trust,
    antenatal_app_date = as.Date(paste0(substr(cells$FY, 1, 4), "-06-01")))
  total_births_by_LA_2326 <- unique(cells[c("LA", "Age", "FY")]) |>
    dplyr::transmute(la_of_mother = LA, age_at_birth_mother = Age, birth_yr = FY, count = 10)
  ons <- tidyr::expand_grid(area_code = unique(cells$LA), age_group = as.character(15:44),
    year = 2023:2047) |>
    dplyr::mutate(area_name = area_code, projected_births = 10 * (1 + (year - 2025) / 100))
  counts <- read_national_age_counts()
  counts$live_births <- counts$total_births <- counts$maternities <- 100
  counts$stillbirths <- 0
  m <- build_projection_master(final_cohort_2326, total_births_by_LA_2326, ons, counts)
  result <- build_first_draft_projections(master = m)
  # The model has every age 13--54, including zero activity. Supplied ages
  # outside that range must not change boundary weights or suppress providers.
  stopifnot(identical(sort(unique(m$Age)), as.numeric(13:54)))
  zero_ages <- attr(m, "allocation_tables")$residents |>
    dplyr::filter(Age %in% c(13, 14, 45:54))
  stopifnot(nrow(zero_ages) == 24L, all(zero_ages$MSDS_resident_count == 0))
  original_cohort <- final_cohort_2326
  # Local delivery totals and age weights come from the cohort, regardless
  # of the counts supplied for those same LAs in the wider file.
  inflated_local_totals <- total_births_by_LA_2326
  inflated_local_totals$count <- inflated_local_totals$count * 10
  local_source <- build_projection_master(original_cohort, inflated_local_totals, ons, counts)
  near(local_source$Age_weight, m$Age_weight)
  near(local_source$Adjusted_maternities, m$Adjusted_maternities)
  near(local_source$Booking_count_RQ3, m$Booking_count_RQ3)
  near(local_source$Delivery_count_RQ3, m$Delivery_count_RQ3)
  # The national resident file must not expand the BBCS catchment or its QA.
  unrelated_residents <- dplyr::bind_rows(total_births_by_LA_2326,
    data.frame(la_of_mother = c("W06000002", "E07000028", "E07000001"),
      age_at_birth_mother = 36, birth_yr = "2025/26", count = 10000))
  unrelated_ons <- dplyr::bind_rows(ons, ons |> dplyr::filter(area_code == "E08000025") |>
    dplyr::mutate(area_code = "E07000001", area_name = "Unrelated area", projected_births = 10000))
  unrelated_cohort <- original_cohort[1, ]
  unrelated_cohort$la_of_mother <- "E07000001"
  unrelated_cohort$age_at_birth_mother <- 36
  unrelated_cohort$birth_provider <- unrelated_cohort$booking_provider <- "RZZ"
  scoped <- build_projection_master(dplyr::bind_rows(original_cohort, unrelated_cohort),
    unrelated_residents, unrelated_ons, counts)
  stopifnot(identical(scoped$LA, m$LA),
    identical(attr(scoped, "la_source_matching"), attr(m, "la_source_matching")))
  near(scoped$Adjusted_maternities, m$Adjusted_maternities)
  unrelated_cohort$birth_flag <- 0L
  unrelated_cohort$booking_provider <- "RQ3"
  booking_feeder <- build_projection_master(dplyr::bind_rows(original_cohort, unrelated_cohort),
    unrelated_residents, unrelated_ons, counts)
  stopifnot("E07000001" %in% booking_feeder$LA,
    any(booking_feeder$Booking_count_RQ3[booking_feeder$LA == "E07000001"] > 0, na.rm = TRUE))
  # Empty exact-age cells contribute zero to denominators; they are not missing totals.
  calibration <- attr(m, "allocation_tables")$calibration
  near(calibration$Shared_maternity_denominator[calibration$MSDS_resident_count == 0],
    rep(0, sum(calibration$MSDS_resident_count == 0)))
  outside_ages <- original_cohort[1:3, ]
  outside_ages$age_at_birth_mother <- c(12, 55, 61)
  near(prepare_report_cohort(outside_ages)$Age, c(12, 55, 61))
  final_cohort_2326 <- dplyr::bind_rows(original_cohort, outside_ages)
  outside_residents <- dplyr::bind_rows(total_births_by_LA_2326,
    data.frame(la_of_mother = "E08000025", age_at_birth_mother = c(12, 55, 61),
      birth_yr = "2023/24", count = 1000))
  bounded <- build_projection_master(final_cohort_2326, outside_residents, ons, counts)
  bounded_result <- build_first_draft_projections(master = bounded, prepare_outputs = FALSE)
  final_cohort_2326 <- original_cohort
  near(bounded$Age_weight, m$Age_weight)
  near(bounded$Adjusted_maternities, m$Adjusted_maternities)
  near(bounded_result$maternity_projections$Projected, result$maternity_projections$Projected)
  near(bounded_result$booking_total_projections$Projected, result$booking_total_projections$Projected)
  stopifnot(all(bounded$Age %in% 13:54), nrow(bounded_result$provider_projection_gaps) == 0L)
  # Both inclusive endpoints participate in the boundary splits.
  endpoints <- original_cohort[rep(1, 5), ]
  endpoints$age_at_birth_mother <- c(13, 13, 54, 54, 54)
  endpoint_cohort <- dplyr::bind_rows(original_cohort, endpoints)
  endpoint_residents <- endpoint_cohort |> dplyr::filter(birth_flag == 1) |>
    dplyr::count(la_of_mother, age_at_birth_mother, birth_yr, name = "count")
  endpoint_master <- build_projection_master(endpoint_cohort, endpoint_residents, ons, counts)
  endpoint_cells <- endpoint_master |>
    dplyr::filter(LA == "E08000025", Year == 2025, Age %in% c(13, 15, 44, 54)) |>
    dplyr::arrange(Age)
  near(endpoint_cells$Age_weight, c(2 / 32, 30 / 32, 30 / 33, 3 / 33))
  source_totals <- endpoint_master |> dplyr::group_by(LA, ONS_age, Year) |>
    dplyr::summarise(Split = sum(Exact_age_live_births), Source = dplyr::first(ONS_live_births), .groups = "drop")
  near(source_totals$Split, source_totals$Source)
  expected_total <- function(key, year, bookings) {
    n <- if (key == "cluster") 600 else if (key %in%
      c("trust-rq3", "trust-rl4", "icb-qhl", "icb-qua")) 300 else 0
    n * (1 + (year - 2025) / 100) * if (bookings) 1.2 else 1
  }
  for (bookings in c(FALSE, TRUE)) {
    shown <- if (bookings) result$booking_total_projections else result$maternity_projections
    near(shown$Projected, mapply(expected_total, shown$Key, shown$Year, MoreArgs = list(bookings = bookings)))
  }
  # Exercise the same serialization boundary as the report, without replacing its files.
  saved <- tempfile(fileext = ".rds")
  on.exit(unlink(saved), add = TRUE)
  saveRDS(result, saved)
  restored <- readRDS(saved)
  check_total_chart <- function(chart, bookings) {
    built <- plotly::plotly_build(chart)$x
    traces <- built$data
    stopifnot(length(traces) >= 3L)
    for (trace in traces) {
      key <- trace$meta$Key
      series <- trace$meta$Series
      year <- as.numeric(trace$x)
      expected <- expected_total(key, if (series == "MSDS recorded") rep(2025, length(year)) else year, bookings)
      if (series == "ONS base") expected <- expected / 1.2
      near(trace$y, dc_count(expected))
      if (series == "MSDS recorded") near(year, 2023:2025) else
        if (series == "Modelled projection") near(year, 2026:2047) else near(year, 2025:2047)
      stopifnot(trace$mode == "lines", is.null(trace$marker$symbol),
        trace$line$color == unname(organisation_line_colours(key)),
        trace$line$dash == if (series == "ONS base") "dash" else "solid")
    }
    stopifnot(sum(vapply(traces, function(x) isTRUE(x$showlegend), logical(1))) ==
      length(unique(vapply(traces, function(x) x$meta$Key, character(1)))),
      built$layout$shapes[[1]]$type == "rect",
      built$layout$shapes[[2]]$x0 == 2025.5)
  }
  for (chart in restored$booking_charts$total) check_total_chart(chart, TRUE)
  for (chart in restored$booking_total_charts) check_total_chart(chart, TRUE)
  for (chart in restored$total_charts) check_total_chart(chart, FALSE)
  for (output in restored$forecast_outputs) check_total_chart(output$total_chart, FALSE)
  full_history <- build_projection_master(final_cohort_2326, total_births_by_LA_2326, ons, counts, start_year = 2023L)
  with_history <- build_provider_total_comparison(m, prepare_report_cohort(final_cohort_2326),
    result$organisations, result$maternity_projections, "Delivery", history_master = full_history)
  historic_base <- with_history |> dplyr::filter(Series == "ONS base", Year < 2025)
  near(historic_base$Value, mapply(expected_total, historic_base$Key, historic_base$Year,
    MoreArgs = list(bookings = FALSE)) / 1.2)
  stopifnot(setequal(historic_base$Year, 2023:2024))
  # Historical bookings follow booking FY, independently of delivery year,
  # birth flag, age, geography, home-birth status and model coverage.
  booking_history <- original_cohort[rep(1, 11), ]
  booking_history$person_id_mother <- booking_history$uniq_preg_id <- 1:11
  booking_history$birth_yr <- "2025/26"
  booking_history$birth_yr[11] <- "2026/27"
  booking_history$birth_flag <- rep(c(0L, 1L), length.out = 11)
  booking_history$home_birth <- 1L
  booking_history$age_at_birth_mother <- NA_real_
  booking_history$la_of_mother <- NA_character_
  booking_history$booking_provider <- " rq3 "
  booking_history$antenatal_app_date <- c("2023-04-01", "31/03/2024",
    "2024-04-01", "31/03/2025", "2025-04-01", "31/03/2026",
    "2026-04-01", "bad date", "", NA, "2025-12-01")
  prepared_history <- prepare_report_cohort(booking_history)
  near(prepared_history$Booking_year,
    c(2023, 2023, 2024, 2024, 2025, 2025, 2026, NA, NA, NA, 2025))
  stopifnot(nrow(prepared_history) == nrow(booking_history))
  history_check <- build_provider_total_comparison(m, prepared_history,
    result$organisations, result$booking_total_projections, "Booking") |>
    dplyr::filter(Series == "MSDS recorded", Key == "trust-rq3")
  near(history_check$Value, c(2, 2, 3))
  # A complete delivery-year extract can still omit later booking cohorts.
  # Here all 36 delivery months exist; bookings precede deliveries by 180 days.
  # This demonstrates the possible selection effect, not evidence about Azure.
  delivery_window <- original_cohort[rep(1, 36), ]
  delivery_window$person_id_mother <- delivery_window$uniq_preg_id <- 1:36
  delivery_window$birth_date <- seq(as.Date("2023-04-15"), by = "month", length.out = 36)
  delivery_window$birth_yr <- rep(c("2023/24", "2024/25", "2025/26"), each = 12)
  delivery_window$booking_provider <- "RQ3"
  delivery_window$antenatal_app_date <- delivery_window$birth_date - 180
  window_check <- build_provider_total_comparison(m, prepare_report_cohort(delivery_window),
    result$organisations, result$booking_total_projections, "Booking") |>
    dplyr::filter(Series == "MSDS recorded", Key == "trust-rq3")
  near(window_check$Value, c(12, 12, 6))
  for (type in names(restored$total_comparison)) {
    totals <- restored$total_comparison[[type]] |>
      dplyr::group_by(Level, Series, Year) |>
      dplyr::summarise(Value = sum(Value), .groups = "drop") |>
      tidyr::pivot_wider(names_from = Level, values_from = Value)
    near(totals$Trust, totals$Cluster)
    near(totals$ICB, totals$Cluster)
  }
  for (key in names(restored$booking_charts$age)) {
    traces <- plotly::plotly_build(restored$booking_charts$age[[key]])$x$data
    for (trace in traces) {
      if (trace$name == "No allocated bookings") { near(trace$y, rep(0, length(trace$y))); next }
      n_ages <- sum(report_age_band(15:44) == trace$name)
      near(trace$y, dc_count(expected_total(key, as.numeric(trace$x), TRUE) * n_ages / 30))
    }
  }
  # A tenfold uplift must cancel in provider allocation when the same denominator
  # is used: changing non-provider no-delivery rows cannot inflate recorded providers.
  no_booking <- final_cohort_2326[final_cohort_2326$birth_flag == 0, ]
  no_booking$booking_provider <- NA_character_
  no_booking$antenatal_app_date <- as.Date(NA)
  enlarged <- dplyr::bind_rows(final_cohort_2326, no_booking[rep(seq_len(nrow(no_booking)), 54), ])
  enlarged$person_id_mother <- enlarged$uniq_preg_id <- seq_len(nrow(enlarged))
  high_uplift <- build_projection_master(enlarged, total_births_by_LA_2326, ons, counts)
  near(high_uplift$Bookings_uplift_modifier[high_uplift$Age == 25],
    rep(12, sum(high_uplift$Age == 25)))
  for (column in c("Booking_count_RQ3", "Booking_count_RL4", "Delivery_count_RQ3", "Delivery_count_RL4"))
    near(high_uplift[[column]], m[[column]])
  # Regression for the old, incorrect booking denominator: ten observed
  # bookings from an outside LA must not receive that LA's entire ONS volume.
  feeder <- final_cohort_2326[final_cohort_2326$la_of_mother == "E08000025" &
    final_cohort_2326$age_at_birth_mother == 25 & final_cohort_2326$birth_flag == 1, ]
  feeder$la_of_mother <- "E09999999"
  feeder_cohort <- dplyr::bind_rows(final_cohort_2326, feeder)
  feeder_cohort$person_id_mother <- feeder_cohort$uniq_preg_id <- seq_len(nrow(feeder_cohort))
  feeder_residents <- dplyr::bind_rows(total_births_by_LA_2326,
    data.frame(la_of_mother = "E09999999", age_at_birth_mother = 25,
      birth_yr = c("2023/24", "2024/25", "2025/26"), count = 100))
  feeder_ons <- ons[ons$area_code == "E08000025", ]
  feeder_ons$area_code <- feeder_ons$area_name <- "E09999999"
  feeder_ons$projected_births <- ifelse(feeder_ons$age_group == "25", 100, 0)
  feeder_master <- build_projection_master(feeder_cohort, feeder_residents,
    dplyr::bind_rows(ons, feeder_ons), counts)
  check <- feeder_master |> dplyr::filter(LA == "E09999999", Age == 25, Year == 2025)
  near(check$Booking_count_RQ3, 10)
  near(check$Delivery_count_RQ3, 10)
  near(check$Booking_denominator, 360)
  # The old method's 30 / 30 booking share would incorrectly allocate 100.
  near(check$Exact_age_live_births * (30 / 30), 100)
  # Unknown age and unmatched LA must not erase other valid provider allocations.
  gaps <- original_cohort[rep(1, 4), ]
  gaps$birth_yr <- "2025/26"
  gaps$age_at_birth_mother <- c(NA, 25, NA, NA)
  gaps$la_of_mother[2] <- "W06000002"
  gaps$birth_provider[3] <- gaps$booking_provider[3] <- "RXT"
  gaps$birth_flag[4] <- 0L
  final_cohort_2326 <- dplyr::bind_rows(original_cohort, gaps)
  partial_master <- build_projection_master(final_cohort_2326, total_births_by_LA_2326, ons, counts)
  partial <- build_first_draft_projections(master = partial_master)
  for (field in c("maternity_projections", "booking_total_projections")) {
    shown <- partial[[field]]
    affected <- shown[shown$Key == "trust-rq3", ]
    near(affected$Projected, result[[field]]$Projected[result[[field]]$Key == "trust-rq3"])
    stopifnot(all(affected$Coverage_status == "Partial"), all(!affected$Projection_complete),
      all(is.na(shown$Projected[shown$Key == "trust-rxt"])),
      all(shown$Coverage_status[shown$Key == "trust-rxt"] == "No usable estimate"),
      all(shown$Projected[shown$Key == "trust-rbk"] == 0))
  }
  rq3 <- partial$maternity_projections |> dplyr::filter(Key == "trust-rq3", Year == 2025)
  near(rq3$Observed_baseline, 302)
  near(rq3$Observed_matched_baseline, 300)
  near(rq3$Percent_change, 0)
  comparison <- partial$total_comparison$Delivery
  near(comparison$Value[comparison$Key == "trust-rq3" & comparison$Series == "MSDS recorded"], c(300,300,302))
  near(comparison$Value[comparison$Key == "trust-rq3" & comparison$Series == "Modelled projection"],
    result$maternity_projections$Projected[result$maternity_projections$Key == "trust-rq3"])
  traces <- plotly::plotly_build(partial$forecast_outputs[["trust-rq3"]]$total_chart)$x$data
  model_trace <- traces[[which(vapply(traces, function(x) x$meta$Series == "Modelled projection", logical(1)))]]
  stopifnot(all(is.finite(model_trace$y)), all(grepl("Partial", model_trace$text)))
  age_review <- build_msds_age_review(final_cohort_2326, partial, partial, trust_labels, cluster_trusts)
  check_age <- age_review$exact_comparison |> dplyr::filter(year == 2025, Age == 25)
  stopifnot(is.finite(check_age$Modelled[check_age$Key == "trust-rq3"]),
    is.na(check_age$Modelled[check_age$Key == "trust-rxt"]))
  near(sum_available_projection(c(NA, 12, 0)), 12)
  stopifnot(is.na(sum_available_projection(c(NA_real_, NA_real_))), sum_available_projection(numeric()) == 0)
  final_cohort_2326 <- original_cohort
  # Sparse destinations are zero, but absent calibration cannot create a rate.
  near(m$Delivery_count_RL4[m$LA == "E08000025"], rep(0, sum(m$LA == "E08000025")))
  empty_cell <- original_cohort[!(original_cohort$la_of_mother == "E08000025" &
    original_cohort$age_at_birth_mother == 25), ]
  empty_master <- build_projection_master(empty_cell, total_births_by_LA_2326, ons, counts)
  empty <- empty_master |> dplyr::filter(LA == "E08000025", Age == 25, Year == 2025)
  stopifnot(!empty$Allocation_available, is.na(empty$Delivery_denominator))
  near(empty$Delivery_count_Unallocated, empty$Adjusted_maternities)
  invalid_residents <- feeder_residents
  invalid_residents$count[invalid_residents$la_of_mother == "E09999999"] <- NA_real_
  stopifnot(inherits(try(build_allocation_tables(feeder_cohort, invalid_residents), silent = TRUE), "try-error"))
  stopifnot(all(c("Trust", "ICB_Cluster") %in% names(result$delivery_total_grids)),
    all(c("Trust", "ICB_Cluster") %in% names(result$delivery_change_grids)),
    all(c("Trust", "ICB_Cluster") %in% names(result$booking_total_charts)),
    all(c("Trust", "ICB_Cluster") %in% names(result$booking_change_charts)),
    "age_change" %in% names(result$booking_grids))
  if (Sys.getenv("BBCS_QA_PREVIEW") == "1") {
    htmltools::save_html(htmltools::tagList(
      htmltools::tags$style("body {font-family: Arial; max-width: 1500px; margin: 24px auto;} h1 {color:#005eb8;}"),
      htmltools::h1("Synthetic chart QA - invented counts"),
      htmltools::h2("Deliveries: combined trusts"), result$total_charts$Trust,
      htmltools::h2("Deliveries: separate ICBs and cluster"), result$delivery_total_grids$ICB_Cluster,
      htmltools::h2("Bookings: combined trusts"), result$booking_total_charts$Trust,
      htmltools::h2("Bookings: separate ICBs and cluster"), result$booking_grids$total$ICB_Cluster,
      htmltools::h2("Bookings: change by age"), result$booking_charts$age_change[["trust-rq3"]]),
      file = "qa_comparison_preview.html", libdir = "qa_comparison_preview_files")
  }
  message("PASS: ages 13--54, zero activity and boundary totals; annual scale, three-source total charts, saved/reloaded values, missing coverage, uplift cancellation and the outside-LA denominator regression (10 bookings, not 100).")
})
