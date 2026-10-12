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
    la_of_mother = cells$LA, age_at_birth_mother = cells$Age,
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
  outside_ages <- original_cohort[1:3, ]
  outside_ages$age_at_birth_mother <- c(12, 55, 60)
  final_cohort_2326 <- dplyr::bind_rows(original_cohort, outside_ages)
  outside_residents <- dplyr::bind_rows(total_births_by_LA_2326,
    data.frame(la_of_mother = "E08000025", age_at_birth_mother = c(12, 55, 60),
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
    traces <- plotly::plotly_build(chart)$x$data
    stopifnot(length(traces) >= 3L)
    for (trace in traces) {
      key <- trace$meta$Key
      series <- trace$meta$Series
      year <- as.numeric(trace$x)
      expected <- expected_total(key, if (series == "MSDS recorded") rep(2025, length(year)) else year, bookings)
      if (series == "ONS base") expected <- expected / 1.2
      near(trace$y, expected)
      if (series == "MSDS recorded") near(year, 2023:2025) else near(year, 2025:2047)
    }
  }
  for (chart in restored$booking_charts$total) check_total_chart(chart, TRUE)
  for (chart in restored$booking_total_charts) check_total_chart(chart, TRUE)
  for (chart in restored$total_charts) check_total_chart(chart, FALSE)
  for (output in restored$forecast_outputs) check_total_chart(output$total_chart, FALSE)
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
      near(trace$y, expected_total(key, as.numeric(trace$x), TRUE) * n_ages / 30)
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
  # Incomplete model coverage does not erase observed historical records.
  incomplete <- result$maternity_projections
  incomplete$Projected[incomplete$Key == "trust-rq3"] <- NA_real_
  incomplete$Projection_complete[incomplete$Key == "trust-rq3"] <- FALSE
  comparison <- build_provider_total_comparison(m, prepare_report_cohort(final_cohort_2326),
    result$organisations, incomplete, "Delivery")
  affected <- comparison[comparison$Key == "trust-rq3", ]
  stopifnot(all(is.na(affected$Value[affected$Series != "MSDS recorded"])))
  near(affected$Value[affected$Series == "MSDS recorded"], rep(300, 3))
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
