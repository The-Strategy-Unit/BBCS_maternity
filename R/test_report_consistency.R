# Run from the project root: Rscript R/test_report_consistency.R
# Invented pregnancies + public ONS data only. No Azure calls or saved-output changes.
local({
  test_environment <- environment()
  outputs <- list()
  source <- function(file, ...) base::source(file, local = test_environment, ...)
  unlink <- function(...) invisible(0L)
  saveRDS <- function(object, file, ...) {
    outputs[[basename(file)]] <<- object
    invisible(NULL)
  }
  readRDS <- function(file, ...) outputs[[basename(file)]]
  file.exists <- function(...) {
    paths <- unlist(list(...))
    ifelse(grepl("\\.rds$", paths), basename(paths) %in% names(outputs), base::file.exists(paths))
  }
  near <- function(x, y) stopifnot(isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = 1e-9)))
  source("R/report_cohort.R")

  # Each side of every current age boundary, including invalid and factor inputs.
  ages <- c(9, 10, 13, 15, 16, 19, 20, 24, 25, 29, 30, 34, 35, 39,
    40, 44, 45, 49, 50, 55, 60, 61, 19.5, Inf, NA)
  expected <- c("Unknown", rep("<20", 5), rep("20-24", 2), rep("25-29", 2),
    rep("30-34", 2), rep("35-39", 2), rep("40+", 7), rep("Unknown", 4))
  stopifnot(identical(report_age_band(ages), expected),
    identical(report_age_band(factor(ages, levels = rev(unique(ages)))), expected))

  trusts <- c("RBK", "RL4", "RNA", "RQ3", "RRK", "RXK")
  icbs <- c("QUA", "QUA", "QUA", "QHL", "QHL", "QUA")
  las <- c("E08000030", "E08000031", "E08000027", "E08000025", "E08000029", "E08000028")
  final_cohort_2326 <- tidyr::expand_grid(
    birth_yr = c("2023/24", "2024/25", "2025/26"), birth_provider = trusts,
    age_at_birth_mother = c(15, 20, 25, 30, 35, 44, 45, 55, NA_real_)) |>
    dplyr::mutate(person_id_mother = as.character(dplyr::row_number()),
      uniq_preg_id = person_id_mother, birth_flag = 1L, home_birth = 0L,
      la_of_mother = las[match(birth_provider, trusts)], resident_icb = icbs[match(birth_provider, trusts)],
      booking_provider = birth_provider, birth_date = as.Date(paste0(substr(birth_yr, 1, 4), "-12-01")),
      antenatal_app_date = as.Date(paste0(substr(birth_yr, 1, 4), "-06-01")))
  template <- final_cohort_2326 |>
    dplyr::filter(birth_yr == "2025/26", birth_provider == "RBK", age_at_birth_mother == 30)
  extra <- template[rep(1, 10), ]
  extra$person_id_mother <- extra$uniq_preg_id <- paste0("extra", seq_len(10))
  extra$home_birth[1:2] <- c(1, NA)
  extra$birth_provider[3] <- NA
  extra$birth_flag[4:5] <- c(0, NA) # A date and year alone must not count as a birth.
  extra$la_of_mother[6] <- NA      # Retained in baseline, held constant.
  extra$person_id_mother[7:8] <- extra$uniq_preg_id[7:8] <- NA # Both supplied rows remain.
  extra$birth_yr[9] <- "2026/27"  # Later birth still belongs to its 2025/26 booking cohort.
  extra$birth_date[9] <- as.Date("2026-05-01")
  extra$birth_yr[10] <- NA        # Recorded birth, but cannot enter delivery-year totals.
  final_cohort_2326 <- dplyr::bind_rows(final_cohort_2326, extra)
  total_births_by_LA_2326 <- final_cohort_2326 |>
    dplyr::filter(birth_flag == 1) |>
    dplyr::count(birth_yr, la_of_mother, age_at_birth_mother, name = "count")
  trust_matrix_data <- data.frame(booking_provider_grouped = "RBK", birth_provider_grouped = "RBK", n = 99999)
  icb_matrix_data <- data.frame(resident_icb_interest = "QUA", birth_icb_interest = "QUA", n = 99999)

  # Deliberately reverse factor levels: level numbers must never become ages/counts/flags.
  original_cohort <- final_cohort_2326
  original_totals <- total_births_by_LA_2326
  for (column in c("age_at_birth_mother", "home_birth", "birth_flag")) {
    final_cohort_2326[[column]] <- factor(final_cohort_2326[[column]],
      levels = rev(sort(unique(final_cohort_2326[[column]]))))
  }
  for (column in c("age_at_birth_mother", "count")) {
    total_births_by_LA_2326[[column]] <- factor(total_births_by_LA_2326[[column]],
      levels = rev(sort(unique(total_births_by_LA_2326[[column]]))))
  }
  for (script in c("R/additional_analysis.R", "R/Deliveries by LA and Age Bands.R", "R/Projections.R")) {
    source(script)
  }
  near(cluster_activity$delivery_trend$Total, c(54, 54, 57))
  # The third section 6.e chart combines both ICBs without counting starts twice.
  cluster_pathway <- cluster_activity$pathways |>
    dplyr::filter(Level == "Cluster")
  stopifnot(nrow(cluster_pathway) == 1L, cluster_pathway$Organisation == "Combined cluster")
  near(unlist(cluster_pathway[c("Starts", "Within", "Outside", "No_delivery", "Unknown")]),
    c(64, 59, 1, 2, 2))
  near(first_draft_projections$observed_totals$Observed_baseline[
    first_draft_projections$observed_totals$Key == "trust-rbk"], 12)
  rbk_coverage <- first_draft_projections$coverage |>
    dplyr::filter(Key == "trust-rbk")
  near(unlist(rbk_coverage[c("Recorded_births", "Home_births", "Unknown_home_flag",
    "Observed_baseline", "ONS_growth", "Unmatched")]), c(14, 1, 1, 12, 10, 2))
  outcomes <- classify_pathway_outcome(tail(activity_episodes, 10))
  stopifnot(identical(outcomes, c("Outside cluster", "Unknown location", "Unknown location",
    "No recorded delivery", "No recorded delivery", rep("Within cluster", 5))))
  stopifnot(nrow(activity_episodes) == nrow(final_cohort_2326),
    sum(by_year[["2025/26"]]$icb_flow$links$n) == 58,
    sum(by_year[["2025/26"]]$trust_flow$links$n) == 58,
    "Unknown location" %in% delivery_geography$matrix_data$Destination)
  # Empty year remains a valid, explicitly empty flow output.
  empty <- make_cohort_year_flows(2000)
  stopifnot(nrow(empty$trust$links) == 0,
    inherits(make_sankey(empty$trust, flow_trust_labels, "", "",
      trust_source_order, trust_target_order), "shiny.tag"))
  make_flow_table(empty$trust, flow_trust_labels, "Booking", "Delivery",
    trust_source_order, trust_target_order, "2000/01")
  unknown <- make_flows("invalid", "invalid", 1, trusts)$links
  stopifnot(unknown$source == "Unknown", unknown$target == "Unknown")

  # Section 8 compares individual trusts before ICB/cluster aggregation.
  # These separate inputs leave the existing forecast hand calculations unchanged.
  booking_codes <- c("Same organisation", "Different cluster organisation", "Outside cluster",
    "No recorded delivery", "Unknown location")
  booking_labels <- c("Delivery at same organisation as booking",
    "Delivery at different organisation within cluster", "Delivery outside cluster or home birth",
    "No recorded delivery", "Delivery location unknown")
  booking_short_labels <- c("Same organisation", "Different cluster organisation",
    "Other trust or home", "No recorded delivery", "Location unknown")
  booking_colours <- c("#00856A", "#005EB8", "#D55E00", "#727272", "#8E5EA2")
  booking_examples <- activity_episodes[rep(1, 9), ]
  booking_examples$booking_code <- "RBK"
  booking_examples$booking_year <- 2023L
  booking_examples$birth_code <- c("RBK", "RL4", "RQ3", "R1A", rep("RBK", 3), "Unknown", "RBK")
  booking_examples$birth_recorded <- c(rep(TRUE, 5), FALSE, rep(TRUE, 3))
  booking_examples$home_birth <- c(rep(0L, 4), 1L, 0L, NA_integer_, 0L, 0L)
  booking_examples$delivery_year[9] <- 2026L
  booking_examples$delivery_date[9] <- as.Date("2026-05-01")
  booking_examples$outcome <- classify_booking_delivery_outcome(booking_examples)
  stopifnot(identical(booking_examples$outcome,
    c(booking_codes[c(1, 2, 2, 3, 3, 4, 5, 5, 1)])))
  stopifnot(identical(classify_booking_delivery_outcome(booking_examples[0, ]), character()))
  # A second trust has a zero baseline share for different-cluster deliveries.
  additional_starts <- booking_examples[c(1, 8, 1, 1), ]
  additional_starts$booking_year <- c(2024L, 2024L, 2023L, 2024L)
  additional_starts$booking_code[3:4] <- "RL4"
  additional_starts$birth_code[3:4] <- c("RL4", "RBK")
  additional_starts$outcome <- classify_booking_delivery_outcome(additional_starts)
  booking_inputs <- dplyr::bind_rows(booking_examples, additional_starts)
  booking_test <- build_delivery_outcome_trends(booking_inputs, booking_codes, booking_labels,
    booking_colours, booking_short_labels, section_number = 8L)
  rbk_start <- booking_test$data |>
    dplyr::filter(Key == "trust-rbk", Year_start == 2023L)
  near(rbk_start$Numerator, c(2, 2, 2, 1, 2))
  near(rbk_start$Denominator, rep(9, 5))
  near(rbk_start$Share, c(2, 2, 2, 1, 2) / 9)
  rbk_next <- booking_test$data |>
    dplyr::filter(Key == "trust-rbk", Year_start == 2024L)
  near(rbk_next$Share, c(0.5, 0, 0, 0, 0.5))
  near(rbk_next$Base_change, c(1.25, -1, -1, -1, 1.25))
  rl4_zero_baseline <- booking_test$data |>
    dplyr::filter(Key == "trust-rl4", Outcome == booking_labels[2])
  near(rl4_zero_baseline$Share[1:2], c(0, 1))
  stopifnot(all(is.na(rl4_zero_baseline$Base_change)))
  empty_booking_year <- booking_test$data |> dplyr::filter(Year_start == 2025L)
  stopifnot(all(empty_booking_year$Denominator == 0), all(empty_booking_year$Numerator == 0),
    all(is.na(empty_booking_year$Share)), all(is.na(empty_booking_year$Base_change)),
    ncol(booking_test$change_data) == 2 + 5 * 3,
    nrow(booking_test$change_data) == nrow(booking_test$organisations))
  booking_partition <- booking_test$data |>
    dplyr::group_by(Key, Year_start) |>
    dplyr::summarise(N = sum(Numerator), Starts = dplyr::first(Denominator), .groups = "drop")
  near(booking_partition$N, booking_partition$Starts)
  booking_levels <- booking_test$data |>
    dplyr::group_by(Level, Year_start, Outcome) |>
    dplyr::summarise(N = sum(Numerator), Starts = sum(Denominator), .groups = "drop") |>
    tidyr::pivot_wider(names_from = Level, values_from = c(N, Starts))
  near(booking_levels$N_Trust, booking_levels$N_Cluster)
  near(booking_levels$N_ICB, booking_levels$N_Cluster)
  near(booking_levels$Starts_Trust, booking_levels$Starts_Cluster)
  near(booking_levels$Starts_ICB, booking_levels$Starts_Cluster)

  # Compare the focused five-category results with section 7 using the same starts.
  booking_inputs$outcome <- classify_pathway_outcome(booking_inputs)
  combined_test <- build_delivery_outcome_trends(booking_inputs,
    c("Within cluster", "Outside cluster", "No recorded delivery", "Unknown location"),
    outcome_labels, outcome_colours,
    c("At cluster trust", "Other trust or home", "No recorded delivery", "Location unknown"))
  booking_collapsed <- booking_test$data |>
    dplyr::mutate(Outcome = dplyr::case_when(
      Outcome %in% booking_labels[1:2] ~ outcome_labels[1],
      Outcome == booking_labels[3] ~ outcome_labels[2], TRUE ~ Outcome)) |>
    dplyr::group_by(Key, Year_start, Outcome) |>
    dplyr::summarise(Split_n = sum(Numerator), Split_starts = dplyr::first(Denominator), .groups = "drop")
  booking_reconciled <- dplyr::full_join(combined_test$data, booking_collapsed,
    by = c("Key", "Year_start", "Outcome"))
  near(booking_reconciled$Numerator, booking_reconciled$Split_n)
  near(booking_reconciled$Denominator, booking_reconciled$Split_starts)

  # A completely empty booking dataset keeps five categories and unavailable shares.
  booking_empty <- build_delivery_outcome_trends(booking_inputs[0, ], booking_codes, booking_labels,
    booking_colours, booking_short_labels, section_number = 8L)
  stopifnot(nrow(booking_empty$data) == nrow(booking_empty$organisations) * 3 * 5,
    all(booking_empty$data$Numerator == 0), all(booking_empty$data$Denominator == 0),
    all(is.na(booking_empty$data$Share)), all(is.na(booking_empty$data$Base_change)))

  # QA runs on in-memory synthetic outputs, never the user's saved RDS files.
  invisible(capture.output(source("R/QA.R")))
  expected_flags <- c("Every cohort record has mother and pregnancy identifiers",
    "Recorded births have a valid delivery financial year", "Supplied birth flags are 0 or 1",
    "Recorded deliveries have home flags of 0 or 1")
  unexpected <- qa_results[qa_results$Result == "Check" & !qa_results$Check %in% expected_flags, ]
  if (nrow(unexpected)) { print(unexpected); stop("Unexpected QA result in synthetic data") }

  # Numeric and factor inputs must give identical counts, shares and forecasts.
  factor_result <- first_draft_projections
  final_cohort_2326 <- original_cohort
  total_births_by_LA_2326 <- original_totals
  numeric_result <- build_first_draft_projections(prepare_outputs = FALSE)
  near(factor_result$maternity_projections$Projected, numeric_result$maternity_projections$Projected)
  stopifnot(identical(factor_result$feeder_shares, numeric_result$feeder_shares))

  # Hand calculation: 90 deliveries from an LA doubling, 10 from an LA halving.
  # Correct contribution-weighted forecast = 90*2 + 10*0.5 = 185 (85% growth).
  final_cohort_2326 <- template[rep(1, 100), ]
  final_cohort_2326$person_id_mother <- final_cohort_2326$uniq_preg_id <- as.character(1:100)
  final_cohort_2326$la_of_mother <- rep(c(las[1], las[4]), c(90, 10))
  total_births_by_LA_2326 <- final_cohort_2326 |>
    dplyr::count(birth_yr, la_of_mother, age_at_birth_mother, name = "count")
  read_snpp_births <- function(...) {
    tidyr::expand_grid(area_code = c(las[1], las[4]), age_group = as.character(c(15, 20, 25, 30, 35, 40)),
      year = 2026:2027) |>
      dplyr::mutate(area_name = ifelse(area_code == las[1], "Walsall", "Birmingham"),
        projected_births = ifelse(area_code == las[1], 100, 900) *
          ifelse(year == 2026, 1, ifelse(area_code == las[1], 2, 0.5)))
  }
  controlled <- build_first_draft_projections(prepare_outputs = FALSE)
  control <- controlled$weighted_age_growth |>
    dplyr::filter(Key == "trust-rbk", Age_band == "30-34", Year == 2027)
  near(c(control$Baseline, control$Projected, control$Growth_change), c(100, 185, 0.85))
  zero <- controlled$weighted_age_growth |> dplyr::filter(Baseline == 0)
  stopifnot(all(is.na(zero$Growth_change)), all(zero$Projected == 0))

  # Add ten unknown-LA deliveries in the same age band: they stay constant in both charts.
  held <- template[rep(1, 10), ]
  held$person_id_mother <- held$uniq_preg_id <- paste0("held", 1:10)
  held$la_of_mother <- NA
  final_cohort_2326 <- dplyr::bind_rows(final_cohort_2326, held)
  with_held <- build_first_draft_projections(prepare_outputs = FALSE)
  control <- with_held$weighted_age_growth |>
    dplyr::filter(Key == "trust-rbk", Age_band == "30-34", Year == 2027)
  near(c(control$Baseline, control$Projected, control$Growth_change), c(110, 195, 195/110 - 1))

  # A matched occupied cell with no positive ONS reference must stop, not save NA totals.
  good_reader <- read_snpp_births
  read_snpp_births <- function(...) good_reader() |>
    dplyr::mutate(projected_births = ifelse(year == 2026 & age_group == "30", 0, projected_births))
  failure <- tryCatch(build_first_draft_projections(prepare_outputs = FALSE), error = identity)
  stopifnot(inherits(failure, "error"), grepl("Missing/invalid ONS growth", conditionMessage(failure)))
  cat("PASS: age boundaries; factors; inclusion/outcomes; five booking-to-delivery outcomes; cross-section reconciliation; weighted growth; held counts; invalid growth.\n")
})
