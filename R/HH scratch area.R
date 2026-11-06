# Run after loading the MSDS objects; every check prints in the console.
# Exact internal counts only. No files are written and source objects are unchanged.
suppressPackageStartupMessages(library(dplyr))
source("R/report_cohort.R")

# Print all rows and columns for either a data frame or tibble.
hh_print <- function(x, ...) {
  if (inherits(x, "data.frame")) print(tibble::as_tibble(x), n = Inf, width = Inf)
  else print(x)
  invisible(x)
}

# Print the check name and its result, including when this file is sourced.
hh_show <- function(label, value) {
  cat("\n", label, "\n", sep = "")
  tryCatch({
    result <- withVisible(force(value))
    if (result$visible) {
      if (inherits(result$value, "data.frame"))
        print(tibble::as_tibble(result$value), n = Inf, width = Inf)
      else print(result$value)
    }
    invisible(result$value)
  }, error = function(e) {
    cat("CHECK COULD NOT RUN: ", conditionMessage(e), "\n", sep = "")
    invisible(NULL)
  })
}

# Check how many cohort pregnancies the residence rule removes from projections.
# Run from the top through END RESIDENCE CHECK; no model run is needed.
hh_residence_exclusion_impact <- function(cohort) {
  episodes <- prepare_report_cohort(cohort)
  scope <- projection_la_scope(cohort$la_of_mother)
  excluded <- !scope$Included
  booking <- episodes$Booking_provider_supplied | episodes$Booking_date_supplied
  delivery <- episodes$Birth_recorded
  percent <- function(n, total) ifelse(total > 0, round(100 * n / total, 1), NA_real_)
  activities <- list("All pregnancy records" = rep(TRUE, nrow(episodes)),
    "With a recorded booking" = booking, "With a recorded delivery" = delivery)
  summary <- dplyr::bind_rows(lapply(names(activities), function(label) {
    selected <- activities[[label]]
    before <- sum(selected)
    removed <- sum(selected & excluded)
    data.frame(Activity = label, Before_rule = before, Excluded = removed,
      After_rule = before - removed, Excluded_pct = percent(removed, before))
  }))
  reasons <- c("Outside England", "Unknown or invalid resident LA",
    "English code without an ONS LA match")
  by_reason <- dplyr::bind_rows(lapply(reasons, function(reason) {
    selected <- scope$Reason == reason
    data.frame(Reason = reason, Pregnancies = sum(selected),
      Pct_of_cohort = percent(sum(selected), nrow(episodes)),
      Bookings = sum(selected & booking), Deliveries = sum(selected & delivery))
  }))
  by_year <- data.frame(FY = dplyr::coalesce(episodes$Pregnancy_FY, "Unknown FY"),
    Excluded_record = excluded, Booking = booking, Delivery = delivery) |>
    dplyr::group_by(FY) |>
    dplyr::summarise(Before_rule = dplyr::n(), Excluded = sum(Excluded_record),
      After_rule = Before_rule - Excluded,
      Excluded_bookings = sum(Booking & Excluded_record),
      Excluded_deliveries = sum(Delivery & Excluded_record), .groups = "drop")
  cat("\nIMPACT OF RESIDENCE EXCLUSIONS ONLY: BBCS COHORT\n",
    "Current loaded cohort; LA codes reconciled before matching to the supplied ONS projection.\n",
    "Only the residence rule changes below: no age, year or calibration exclusions are applied.\n", sep = "")
  hh_print(summary)
  cat("\nEXCLUDED PREGNANCIES BY REASON\n")
  hh_print(by_reason)
  cat("\nEXCLUSIONS BY ASSIGNED PREGNANCY FINANCIAL YEAR\n")
  hh_print(by_year)
  cat("\n", sum(excluded), " of ", nrow(episodes), " pregnancy records excluded",
    if (nrow(episodes)) paste0(" (", formatC(percent(sum(excluded), nrow(episodes)),
      format = "f", digits = 1), "%)") else " (percentage not applicable: empty cohort)", ".\n",
    "Each cohort row counts as one pregnancy, not one mother; exclusion reasons do not overlap.\n",
    "Bookings and deliveries can belong to the same pregnancy: do not add them.\n",
    "Descriptive sections retain these records; the reductions apply to projection inputs/comparisons.\n",
    "These are input-count differences, not changes in future projected totals. Individual panels also apply their own filters.\n",
    "Exact internal counts; no files written, input objects changed or model recalculated.\n", sep = "")
  invisible(list(summary = summary, by_reason = by_reason, by_year = by_year))
}
hh_show("Check the impact of the residence exclusions", {
  if (!exists("final_cohort_2326")) stop("Load final_cohort_2326 first, then rerun this check.")
  hh_residence_exclusion_impact(final_cohort_2326)
})
# END RESIDENCE CHECK --------------------------------------------------

# CALIBRATION CHECK: IS THIS ACTUALLY A PROBLEM? ------------------------
# Select and run this block after running the analysis on your current data.
# It reads the failures already recorded by the model; it does not rerun or
# change any calculations. Only affected cohort/provider records are counted.
hh_calibration_problem_check <- function(projections) {
  gaps <- projections$provider_projection_gaps
  if (is.null(gaps) || !all(c("Level", "Reason", "Allocation", "LA", "Age", "Cohort_rows") %in% names(gaps)))
    stop("Run the current analysis first; its saved projection-gap results are needed.")
  checks <- list(
    "Cohort births exceed resident births" =
      "Cohort births exceed resident births: check source periods/processing",
    "Resident denominator missing or zero" = c("No matching resident birth count",
      "No positive resident birth denominator", "No matching LA/age resident calibration"),
    "Pathway counts inconsistent" = c("Unknown/missing resident pathway group",
      "Cohort pregnancies exceed estimated resident total",
      "LA antenatal-only count below observed cohort no-delivery count"))
  # Keep Trust rows only: ICB and cluster rows repeat the same activity.
  gaps <- gaps[gaps$Level == "Trust", , drop = FALSE]
  answer <- do.call(rbind, lapply(names(checks), function(label) {
    rows <- gaps[gaps$Reason %in% checks[[label]], , drop = FALSE]
    data.frame(Check = label, Problem = if (nrow(rows)) "YES" else "No",
      Bookings = sum(rows$Cohort_rows[rows$Allocation == "Booking"]),
      Deliveries = sum(rows$Cohort_rows[rows$Allocation == "Delivery"]),
      LA_age_cells = nrow(unique(rows[c("LA", "Age")])) )
  }))
  cat("\nIS THIS ACTUALLY A PROBLEM IN THE CURRENT MODEL RUN?\n")
  if (any(answer$Problem == "YES"))
    cat("YES: the model reports one or more of these calibration failures.\n") else
    cat("NO: none of these calibration checks is blocking provider projections.\n")
  hh_print(answer, row.names = FALSE, right = FALSE)
  cat("Bookings/Deliveries count affected cohort records, not the size of the discrepancy.\n",
    "LA_age_cells counts distinct residence-LA and assigned-age combinations.\n",
    "Pathway counts inconsistent means an unrecognised resident pathway group, too few resident pregnancies,\n",
    "or too few resident antenatal-only pregnancies for the corresponding cohort cells.\n",
    "A YES does not establish that country coverage caused the problem. The specific reasons follow.\n",
    "Bookings and deliveries can overlap; do not add them. No calculations were changed by this check.\n", sep = "")
  if (nrow(gaps)) {
    reasons <- gaps |> dplyr::group_by(Reason) |>
      dplyr::summarise(Bookings = sum(Cohort_rows[Allocation == "Booking"]),
        Deliveries = sum(Cohort_rows[Allocation == "Delivery"]),
        LA_age_cells = nrow(dplyr::distinct(dplyr::pick(LA, Age))), .groups = "drop")
    hh_print(reasons, n = Inf, width = Inf)
  }
  invisible(answer)
}
if (exists("first_draft_projections")) {
  hh_calibration_check <- hh_calibration_problem_check(first_draft_projections)
} else cat("\nRun the analysis first, then rerun this CALIBRATION CHECK block.\n")
# END CALIBRATION CHECK ------------------------------------------------

# Print projection residence exclusions and any remaining model issues.
if (exists("first_draft_projections")) {
  if (identical(first_draft_projections$projection_geography$version, "english-ons-la-v1"))
    print_projection_scope_check(first_draft_projections) else
    cat("Rerun the analysis: these saved results predate the projection residence rule.\n")
} else cat("Run the analysis first to print projection exclusions.\n")

# UNKNOWN AGES: exact internal counts after birth/booking-age fallback
# Count-weighted resident totals; overlapping files, not additive
source("R/report_cohort.R")
# Check assigned-age completeness in both MSDS inputs.
hh_show("Check assigned-age completeness in both MSDS inputs", {
dplyr::bind_rows(
  final_cohort_2326 = prepare_report_cohort(final_cohort_2326) |>
    dplyr::summarise(Total = dplyr::n(), Unknown_age = sum(is.na(Age))),
  total_births_by_LA_2326 = prepare_la_records(total_births_by_LA_2326) |>
    dplyr::summarise(Total = sum(count), Unknown_age = sum(count[is.na(Age)])),
  .id = "MSDS_file") |>
  dplyr::mutate(Unknown_pct = round(100 * Unknown_age / Total, 2)) |> hh_print()
})

# MSDS COUNTRY CONTRIBUTIONS: run this block or source this file --------
# Internal QA: raw pregnancy counts, percentages of all countries.
library(dplyr)
source("R/report_cohort.R")
stopifnot(exists("final_cohort_2326"))

hh_country <- prepare_report_cohort(final_cohort_2326) |>
  transmute(FY = coalesce(Pregnancy_FY, "Unknown FY"),
    Country = report_la_country(LA),
    Booking = Booking_provider_supplied | Booking_date_supplied,
    Delivery = Birth_recorded)

hh_country_summary <- function(rows) {
  countries <- c("England", "Wales", "Scotland", "Northern Ireland", "Unknown/other code")
  counts <- rows |> group_by(Country) |>
    summarise(Cohort_n = n(), Bookings_n = sum(Booking), Deliveries_n = sum(Delivery), .groups = "drop") |>
    tidyr::complete(Country = countries,
      fill = list(Cohort_n = 0L, Bookings_n = 0L, Deliveries_n = 0L)) |>
    arrange(match(Country, countries))
  total <- counts |> summarise(across(ends_with("_n"), sum)) |>
    mutate(Country = "All countries")
  non_england <- counts |> filter(Country %in% c("Wales", "Scotland", "Northern Ireland")) |>
    summarise(across(ends_with("_n"), sum)) |>
    mutate(Country = "Known non-England subtotal")
  pct <- function(n, denominator) if (denominator > 0) round(100 * n / denominator, 2) else rep(NA_real_, length(n))
  bind_rows(counts, non_england, total) |>
    mutate(Cohort_pct = pct(Cohort_n, total$Cohort_n),
      Bookings_pct = pct(Bookings_n, total$Bookings_n),
      Deliveries_pct = pct(Deliveries_n, total$Deliveries_n)) |>
    select(Country, Cohort_n, Cohort_pct, Bookings_n, Bookings_pct, Deliveries_n, Deliveries_pct)
}

hh_country_by_fy <- bind_rows(lapply(sort(unique(c("2023/24", "2024/25", "2025/26", hh_country$FY))), function(fy) {
  hh_country_summary(filter(hh_country, FY == fy)) |> mutate(FY = fy, .before = 1)
}))
hh_country_all_years <- hh_country_summary(hh_country)

cat("\nMSDS COUNTRY CONTRIBUTIONS BY PREGNANCY FY\n")
hh_print(hh_country_by_fy, n = Inf, width = Inf)
cat("\nMSDS COUNTRY CONTRIBUTIONS: ALL SUPPLIED YEARS COMBINED\n")
hh_print(hh_country_all_years, n = Inf, width = Inf)
cat("\nCounts are pregnancies, not distinct mothers. FY = birth_yr first, otherwise edd_yr.\n",
  "Each percentage uses all countries for the same FY and activity; pooled percentages use all supplied years.\n",
  "Known non-England = Wales + Scotland + Northern Ireland; unknown residence is separate.\n",
  "Subtotal/total rows overlap the country rows. Bookings and deliveries also overlap; do not add them.\n",
  "NA percentages mean no activity in the denominator.\n", sep = "")
# END MSDS COUNTRY CONTRIBUTIONS ---------------------------------------

# WHY PROVIDER PROJECTIONS ARE PARTIAL ----------------------------------
# Run this block after the analysis, using the SAME loaded pregnancy cohort.
# Internal QA only: exact counts; no files saved and no model inputs changed.
# Use the model's saved reasons, not all unmatched codes in the national lookup.
# Trust rows only: do not double-count their ICB/cluster copies. Bookings and
# deliveries can refer to the same pregnancy and must not be added together.
library(dplyr)
source("R/report_cohort.R")

hh_explain_projection_gaps <- function(projections, cohort) {
  needed <- c("provider_projection_gaps", "provider_coverage", "organisations", "allocation_tables")
  if (!all(needed %in% names(projections)))
    stop("Run the current analysis first: provider coverage diagnostics are missing.")
  episodes <- prepare_report_cohort(cohort)
  if (identical(projections$projection_geography$version, "english-ons-la-v1"))
    episodes <- episodes[projection_la_scope(episodes$LA,
      projections$projection_geography$eligible_las)$Included, , drop = FALSE]
  orgs <- projections$organisations |> filter(Level == "Trust") |>
    distinct(Trust = Code, Organisation)
  ages <- projections$allocation_tables$ages
  if (!length(ages)) stop("The saved projection age range is missing; rerun the analysis.")
  records <- bind_rows(
    episodes |> mutate(Allocation = "Booking", Trust = projection_booking_destination(episodes)),
    episodes |> mutate(Allocation = "Delivery", Trust = projection_delivery_destination(episodes))) |>
    inner_join(orgs, by = "Trust", relationship = "many-to-one")
  # This is exactly the scope that can trigger Partial. Known ages outside the
  # configured range are reported separately, never described as mismatches.
  eligible <- records |> filter(is.na(Age) | Age %in% ages)
  gaps <- projections$provider_projection_gaps |> filter(Level == "Trust") |>
    select(Allocation, Trust, LA, Age, Reason, Cohort_rows)
  keys <- c("Allocation", "Trust", "LA", "Age")
  affected <- eligible |> inner_join(gaps, by = keys, relationship = "many-to-one")

  # Do not diagnose a new/reloaded cohort against stale model results.
  actual <- affected |> count(across(all_of(keys)), name = "Actual_rows")
  check <- full_join(gaps, actual, by = keys)
  saved_totals <- projections$provider_coverage |> filter(Level == "Trust") |>
    select(Allocation, Organisation, Saved_rows = Cohort_rows)
  totals_check <- records |> count(Allocation, Organisation, name = "Current_rows") |>
    full_join(saved_totals, by = c("Allocation", "Organisation"))
  if (any(coalesce(check$Actual_rows, 0L) != coalesce(check$Cohort_rows, 0L)) ||
      any(coalesce(totals_check$Current_rows, 0L) != coalesce(totals_check$Saved_rows, 0L)))
    stop("Loaded cohort does not reconcile to the saved coverage counts. Rerun the analysis on this cohort before using this diagnostic.")

  la_names <- read.csv("data/lookups/la_names.csv", stringsAsFactors = FALSE) |>
    distinct(LA, LA_name)
  affected <- affected |>
    mutate(Country = report_la_country(LA),
      Cause = if_else(Reason == "No matching ONS LA/age projection" &
        Country %in% c("Wales", "Scotland", "Northern Ireland"),
        paste0("Outside supplied ONS geography: ", Country), Reason)) |>
    left_join(la_names, by = "LA", relationship = "many-to-one") |>
    mutate(LA_name = coalesce(LA_name, LA))
  scope <- eligible |> count(Allocation, name = "Eligible_records") |>
    full_join(affected |> count(Allocation, name = "Unmatched"), by = "Allocation") |>
    mutate(Unmatched = coalesce(Unmatched, 0L),
      Unmatched_pct = round(100 * Unmatched / Eligible_records, 1))
  causes <- affected |> group_by(Cause) |>
    summarise(Pregnancies = n_distinct(Row), Bookings = sum(Allocation == "Booking"),
      Deliveries = sum(Allocation == "Delivery"), .groups = "drop") |>
    arrange(desc(Pregnancies), Cause)
  trusts <- affected |> group_by(Trust, Organisation) |>
    summarise(Bookings = sum(Allocation == "Booking"), Deliveries = sum(Allocation == "Delivery"),
      .groups = "drop") |> arrange(desc(Bookings + Deliveries))
  cells <- affected |> group_by(LA, LA_name, Country, Age, Cause) |>
    summarise(Pregnancies = n_distinct(Row), Bookings = sum(Allocation == "Booking"),
      Deliveries = sum(Allocation == "Delivery"),
      Trusts = paste(sort(unique(Trust)), collapse = ", "), .groups = "drop")
  # Attach the exact competing source counts for each affected LA/age cell.
  calibration <- projections$allocation_tables$calibration |>
    select(LA, Age, MSDS_resident_count, Cohort_recorded_births, Cohort_pregnancies,
      Shared_maternity_denominator, Resident_antenatal_only, Cohort_no_recorded_delivery,
      Resident_unclassified_rows)
  cells <- cells |> left_join(calibration, by = c("LA", "Age"), relationship = "many-to-one") |>
    mutate(Evidence = case_when(
      Cause == "Cohort births exceed resident births: check source periods/processing" ~
        paste0("Cohort births ", Cohort_recorded_births, " > resident births ", MSDS_resident_count),
      Cause == "Cohort pregnancies exceed estimated resident total" ~
        paste0("Cohort pregnancies ", Cohort_pregnancies, " > resident total ", round(Shared_maternity_denominator, 1)),
      Cause == "LA antenatal-only count below observed cohort no-delivery count" ~
        paste0("LA antenatal-only ", Resident_antenatal_only, " < cohort no-delivery ", Cohort_no_recorded_delivery),
      Cause == "Unknown/missing resident pathway group" ~
        paste0(Resident_unclassified_rows, " resident rows have an unrecognised pathway group"),
      Cause == "No matching resident birth count" ~ "No resident delivery count for this reconciled LA/age",
      Cause == "No matching LA/age resident calibration" ~ "Reconciled LA is present, but this age has no resident calibration cell",
      Cause == "No positive resident birth denominator" ~ paste0("Resident deliveries = ", MSDS_resident_count),
      Cause == "No matching ONS LA/age projection" ~ "Reconciled LA/age absent from the projection master",
      Cause == "No same-age uplift estimate" ~ "LA/age exists, but its required projection factors are unavailable",
      Cause == "Missing/unusable maternal age" ~ "Selected age is still unusable after the agreed fallback",
      Cause == "Missing/unusable resident LA" ~ "Residence code is missing/unusable after the crosswalk",
      TRUE ~ Cause)) |> arrange(desc(Pregnancies), LA, Age)
  # Name the unavailable factor, rather than repeating the model's generic
  # 'No same-age uplift estimate' label. Inspect affected cells only.
  factor_names <- c(Exact_age_live_births = "ONS live births after age split",
    Stillbirth_factor = "stillbirth factor", Multiplicity_factor = "multiplicity factor",
    Private_NHS_factor = "Private\u2013NHS factor", Bookings_uplift_modifier = "pathway factor")
  for (i in which(cells$Cause == "No same-age uplift estimate")) {
    master <- projections$master
    if (is.null(master)) next
    rows <- master[which(master$LA == cells$LA[i] & master$Age == cells$Age[i]), ]
    fields <- intersect(names(factor_names), names(rows))
    missing <- fields[vapply(fields, function(field) any(!is.finite(rows[[field]])), logical(1))]
    if (length(missing)) cells$Evidence[i] <- paste0("Unavailable: ",
      paste(unname(factor_names[missing]), collapse = ", "))
  }

  cat("\nWHY PROVIDER PROJECTIONS ARE PARTIAL \u2014 COHORT RECORDS ONLY\n")
  cat("Selected age unknown/unusable after fallback: ", sum(is.na(episodes$Age)),
    " of ", nrow(episodes), " cohort pregnancies",
    if (!anyNA(episodes$Age)) " \u2014 age missingness is not a cause in this cohort.\n" else ".\n", sep = "")
  cat("Flag scope: cluster-provider activity, ages ", min(ages), "\u2013", max(ages),
    " plus unknown age; pooled supplied cohort. Booking and delivery columns overlap.\n", sep = "")
  hh_print(scope, n = Inf, width = Inf)
  excluded <- records |> filter(!is.na(Age), !Age %in% ages) |> count(Allocation, name = "Out_of_range")
  if (nrow(excluded)) { cat("Known ages outside model scope (NOT Partial causes):\n"); hh_print(excluded, n = Inf) }
  if (!nrow(affected)) cat("No unmatched in-scope provider records: no cohort matching gaps explain Partial.\n") else {
    cat("\nACTUAL CAUSES (unique pregnancies; activity columns overlap)\n")
    hh_print(causes, n = Inf, width = Inf)
    cat("\nAFFECTED TRUSTS (codes match the detail list)\n")
    hh_print(trusts, n = Inf, width = Inf)
    other <- cells |> filter(!grepl("^Outside supplied ONS geography:", Cause))
    cat("\nISSUES OTHER THAN COUNTRY COVERAGE: ", nrow(other), " affected LA/age/cause combinations\n", sep = "")
    if (!nrow(other)) cat("None: the saved gaps are entirely due to country coverage.\n")
    for (i in seq_len(nrow(other))) {
      row <- other[i, ]
      cat("- ", row$LA_name, " [", row$LA, "], age ", ifelse(is.na(row$Age), "Unknown", row$Age),
        ": ", row$Evidence, "; ", row$Bookings, " bookings / ", row$Deliveries,
        " deliveries; trusts ", row$Trusts, ".\n", sep = "")
    }
  }

  invisible(list(records = affected |> select(Row, Pregnancy_FY, Allocation, Trust, Organisation,
      LA_source, LA, LA_name, Country, Age_at_birth, Age_at_booking, Age, Age_source, Cause),
    cells = cells, causes = causes, trusts = trusts, scope = scope))
}

# Print the affected LA/age cells and the competing source counts.
if (exists("first_draft_projections") && exists("final_cohort_2326")) {
  hh_gap_diagnostic <- hh_show("Explain remaining projection gaps", {
    hh_explain_projection_gaps(first_draft_projections, final_cohort_2326)
  })
} else cat("Run the analysis first to check the affected LA/age cells.\n")
# END WHY PROVIDER PROJECTIONS ARE PARTIAL -------------------------------

# Source checks below also print when this whole file is sourced.

# QUICK RAW UPLIFT: same groups, FYs and ages; NA counts stay visible -----
library(dplyr)
source("R/report_cohort.R")
hh_uplift <- hh_show("Prepare resident pathway input check (requires the current model)", {
prepare_la_records(total_births_by_LA_2326) |>
  mutate(LA = report_la_code(la_of_mother)) |>
  filter(LA %in% first_draft_projections$allocation_tables$residents$LA,
    Pregnancy_FY %in% c("2023/24", "2024/25", "2025/26"), Age %in% 13:54) |>
  group_by(LA, Age) |>
  summarise(
    Both = sum(count[Pathway_group %in% "antenatal+delivery"]),
    Birth_only = sum(count[Pathway_group %in% "birth only"]),
    Antenatal_only = sum(count[Pathway_group %in% "antenatal only"]),
    Missing_counts = sum(is.na(count)),
    Unknown_groups = sum(is.na(Pathway_group)), .groups = "drop") |>
  mutate(Deliveries = Both + Birth_only,
    Raw_uplift = if_else(Unknown_groups > 0, NA_real_,
      (Deliveries + Antenatal_only) / Deliveries)) |> invisible()
})
# Check resident pathway inputs for model LAs.
hh_show("Check resident pathway inputs for model LAs", {
hh_uplift |> summarise(LA_age_cells = n(), Missing_count_cells = sum(Missing_counts > 0),
  Unknown_group_cells = sum(Unknown_groups > 0), Undefined_uplift_cells = sum(!is.finite(Raw_uplift))) |>
  hh_print(width = Inf)
})
# List model LA/age cells with missing counts, unknown groups or undefined uplift.
hh_show("List model LA/age cells with missing counts, unknown groups or undefined uplift", {
hh_uplift |> filter(Missing_counts > 0 | Unknown_groups > 0 | !is.finite(Raw_uplift)) |>
  hh_print(n = Inf, width = Inf)
})
# Model counts currently replace NA with zero and use zero for no deliveries.
# This raw check keeps NA/Inf/NaN, restricted to LAs contributing to the model.
# END QUICK RAW UPLIFT --------------------------------------------------


# QUICK COMPARISON: ALL LAs AND COMBINED COHORT LAs ----------------------
# Run this section only. Reads Sarah's two loaded objects directly.
# Cohort LAs = Birmingham, Dudley, Sandwell, Solihull, Walsall, Wolverhampton.
# These are RESIDENT groups, not provider groups. All LAs includes missing LA.
# Total-file pregnancies = deliveries + antenatal-only, plus any unassigned rows.
# Assign one FY: birth_yr first, otherwise edd_yr in both files.
# Sum count in the total file; count rows in the cohort. No deduplication/rounding.
library(dplyr)
source("R/report_cohort.R")

quick_comparison <- bind_rows(lapply(c("All LAs", "Cohort LAs"), function(scope) {
  totals <- total_births_by_LA_2326
  cohort <- final_cohort_2326
  if (scope == "Cohort LAs") {
    totals <- filter(totals, la_of_mother %in%
      c("E08000025", "E08000027", "E08000028", "E08000029", "E08000030", "E08000031"))
    cohort <- filter(cohort, la_of_mother %in%
      c("E08000025", "E08000027", "E08000028", "E08000029", "E08000030", "E08000031"))
  }
  total_counts <- prepare_la_records(totals) |>
    mutate(FY = Pregnancy_FY) |>
    group_by(FY, .drop = FALSE) |>
    summarise(Total_file_pregnancies = sum(count),
      Total_file_antenatal_only = sum(count[Pathway_group %in% "antenatal only"]),
      Total_file_birth_only = sum(count[Pathway_group %in% "birth only"]),
      Total_file_both = sum(count[Pathway_group %in% "antenatal+delivery"]),
      Total_file_deliveries = sum(count[Has_delivery]),
      Total_file_bookings = sum(count[Has_antenatal]),
      Total_file_unknown_group = sum(count[is.na(Pathway_group)]), .groups = "drop") |>
    tidyr::complete(FY = c("2023/24", "2024/25", "2025/26"),
      fill = list(Total_file_pregnancies = 0, Total_file_antenatal_only = 0,
        Total_file_birth_only = 0, Total_file_both = 0, Total_file_deliveries = 0,
        Total_file_bookings = 0, Total_file_unknown_group = 0), explicit = FALSE)
  cohort_counts <- bind_rows(
    prepare_report_cohort(cohort) |>
      filter(Booking_provider_supplied | Booking_date_supplied) |>
      transmute(FY = Pregnancy_FY, Event = "Booking"),
    prepare_report_cohort(cohort) |> filter(Birth_recorded) |>
      transmute(FY = Pregnancy_FY, Event = "Delivery")
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
# Compare cohort and resident-file activity by year.
hh_show("Compare cohort and resident-file activity by year", {
tidyr::expand_grid(Scope = c("All LAs", "Cohort LAs"),
  FY = c("2023/24", "2024/25", "2025/26")) |>
  left_join(quick_comparison, by = c("Scope", "FY")) |>
  hh_print(n = Inf, width = Inf)
})

# Separate small table so missing/other years cannot disappear unnoticed.
# NA-year bookings have booking evidence but no usable birth/EDD FY.
# NA source counts remain NA.
# Check missing and out-of-period years.
hh_show("Check missing and out-of-period years", {
quick_comparison |>
  filter(!FY %in% c("2023/24", "2024/25", "2025/26")) |>
  arrange(Scope, FY) |>
  hh_print(n = Inf, width = Inf)
})
# END QUICK COMPARISON -------------------------------------------------

# LATEST: DEMOGRAPHICS COLUMN NAMES AND TYPES ---------------------------
# Schema only: does not print patient records or change either source object.
# Check demographic column names and types.
hh_show("Check demographic column names and types", {
data.frame(
  column = names(demographics),
  type = vapply(demographics, function(x) paste(class(x), collapse = "/"), character(1))
) |> hh_print(row.names = FALSE)
})

# Cohort mother-ID type, to compare with the demographics join key.
# Check the cohort mother-ID type.
hh_show("Check the cohort mother-ID type", {
class(final_cohort_2326$person_id_mother)
})
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
# Check raw delivery flags by supplied year.
hh_show("Check raw delivery flags by supplied year", {
final_cohort_2326 |>
  group_by(birth_yr, birth_flag, .drop = FALSE) |>
  summarise(Whole_file_rows = n(),
    Cluster_delivery_provider_rows = sum(birth_provider_flag %in% 1),
    Cluster_LA_resident_rows = sum(la_of_mother %in%
      c("E08000025", "E08000027", "E08000028", "E08000029", "E08000030", "E08000031")),
    Missing_delivery_provider_flags = sum(is.na(birth_provider_flag)),
    Missing_LA_rows = sum(is.na(la_of_mother)), .groups = "drop") |>
  hh_print(n = Inf, width = Inf)
})

# 2. RAW APPOINTMENT AUDIT ONLY: not the model's assigned pregnancy FY.
# Sarah supplies ISO dates. The parse is a separate diagnostic column.
# Unparsed source values stay in the NA FY group; the raw values appear below.
# These dates do not assign pregnancy FY. Unknown-date rows remain
# visible rather than being counted as confirmed bookings in an invented year.
# Check appointment years and missing dates.
hh_show("Check appointment years and missing dates", {
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
  hh_print(n = Inf, width = Inf)
})

# 3. BOOKING FY VERSUS SUPPLIED DUE/DELIVERY FY (all combinations).
# Look for 2025/26 appointments with edd_yr or birth_yr = 2026/27.
# Their absence cannot prove upstream exclusion: records excluded before this
# extract cannot be recovered here. Compare with Sarah's current extraction SQL.
# Compare appointment, due and delivery years.
hh_show("Compare appointment, due and delivery years", {
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
  hh_print(n = Inf, width = Inf)
})

# 4. LA EXTRACT: every supplied delivery/due-year pair, without coalescing them.
# group supplies antenatal only, birth only or antenatal+delivery.
# Dates assign FY: birth_yr first, otherwise edd_yr.
# Both years NA = unassigned, still printed. Provider totals cannot be obtained
# from this LA extract because it has no provider field.
# Check resident pathway counts by both supplied years.
hh_show("Check resident pathway counts by both supplied years", {
total_births_by_LA_2326 |>
  group_by(group, birth_yr, edd_yr, .drop = FALSE) |>
  summarise(Source_rows = n(), Whole_file_count = sum(count),
    Cluster_LA_resident_count = sum(count[la_of_mother %in%
      c("E08000025", "E08000027", "E08000028", "E08000029", "E08000030", "E08000031")]),
    Missing_LA_count = sum(count[is.na(la_of_mother)]),
    Missing_count_rows = sum(is.na(count)),
    Missing_birth_age_rows = sum(is.na(age_at_birth_mother)),
    Missing_booking_age_rows = sum(is.na(age_at_booking_mother)), .groups = "drop") |>
  hh_print(n = Inf, width = Inf)
})

# 5. PROVIDER COVERAGE: supplied providers and delivery years, including NA.
# Check supplied booking providers and flags.
hh_show("Check supplied booking providers and flags", {
final_cohort_2326 |>
  count(booking_provider, booking_provider_flag, name = "Rows", .drop = FALSE) |>
  hh_print(n = Inf, width = Inf)
})
# Check supplied delivery providers and flags.
hh_show("Check supplied delivery providers and flags", {
final_cohort_2326 |>
  count(birth_yr, birth_flag, birth_provider, birth_provider_flag,
    name = "Rows", .drop = FALSE) |>
  hh_print(n = Inf, width = Inf)
})

# 6. ORIGINAL FLAGS AND DATES: every supplied combination, no parsing.
# Check source inclusion flags.
hh_show("Check source inclusion flags", {
final_cohort_2326 |>
  count(resident_flag, booking_provider_flag, birth_provider_flag, birth_flag,
    name = "Rows", .drop = FALSE) |>
  hh_print(n = Inf, width = Inf)
})
# Check supplied appointment, due and birth dates.
hh_show("Check supplied appointment, due and birth dates", {
final_cohort_2326 |>
  count(antenatal_app_date, edd_yr, birth_yr, birth_date,
    name = "Rows", .drop = FALSE) |>
  hh_print(n = Inf, width = Inf)
})

# Optional: schema only, to confirm demographic fields without printing IDs.
# List demographic source fields.
hh_show("List demographic source fields", {
names(demographics)
})
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
# Check cohort year and event-flag combinations.
hh_show("Check cohort year and event-flag combinations", {
final_cohort_2326 |>
  count(birth_yr, birth_flag, booking_provider_flag, birth_provider_flag,
    name = "Rows", .drop = FALSE) |>
  hh_print(n = Inf, width = Inf)
})

# 2. COHORT: booking FY derived from appointment date, with all rows retained.
# FY = NA includes absent/unparsed dates; the two explicit checks distinguish them.
# A row without a usable booking date is not labelled a confirmed booking.
# No year-range filter: earlier/later years and NA all appear.
# Check parsed appointment years and date failures.
hh_show("Check parsed appointment years and date failures", {
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
  hh_print(n = Inf, width = Inf)
})

# 3. COHORT: original appointment dates and booking flags, without date parsing.
# This may print many rows. Includes NA, blanks and any unexpected source values.
# Check original appointment dates and booking flags.
hh_show("Check original appointment dates and booking flags", {
final_cohort_2326 |>
  count(antenatal_app_date, booking_provider_flag, name = "Rows", .drop = FALSE) |>
  hh_print(n = Inf, width = Inf)
})

# 4. LA FILE: BOTH supplied year fields together, including null combinations.
# Pathway membership comes from group, never from whether birth_yr is populated.
# Raw field audit only. The model uses birth_yr first, otherwise edd_yr.
# Total_count includes every row; cluster/unknown-LA columns are extra subtotals.
# No na.rm: an NA count stays visible as an NA sum, with Missing_count_rows shown.
# Check resident counts by supplied year pairs.
hh_show("Check resident counts by supplied year pairs", {
total_births_by_LA_2326 |>
  group_by(group, birth_yr, edd_yr, .drop = FALSE) |>
  summarise(Source_rows = n(), Total_count = sum(count),
    Cluster_resident_count = sum(count[la_of_mother %in%
      c("E08000025", "E08000027", "E08000028", "E08000029", "E08000030", "E08000031")]),
    NA_LA_count = sum(count[is.na(la_of_mother)]),
    Missing_count_rows = sum(is.na(count)), .groups = "drop") |>
  hh_print(n = Inf, width = Inf)
})

# 5. LA FILE: combined residence groups by year, never one row per LA.
# Group labels are diagnostic only; source values and missing counts are unchanged.
# Check cluster, other and unknown residence totals.
hh_show("Check cluster, other and unknown residence totals", {
total_births_by_LA_2326 |>
  mutate(Residence_group = case_when(
    is.na(la_of_mother) ~ "Missing LA (NA)",
    la_of_mother == "" ~ "Blank LA",
    la_of_mother %in%
      c("E08000025", "E08000027", "E08000028", "E08000029", "E08000030", "E08000031") ~
        "Combined cluster residents",
    TRUE ~ "All other supplied LA values")) |>
  group_by(Residence_group, group, birth_yr, edd_yr, .drop = FALSE) |>
  summarise(Source_rows = n(), Total_count = sum(count),
    Missing_count_rows = sum(is.na(count)), .groups = "drop") |>
  hh_print(n = Inf, width = Inf)
})

# 6. Whole-file totals: compare these with the sums of the groups above.
# Each cohort table counts the same full set of source rows in a different way.
# Count original cohort rows.
hh_show("Count original cohort rows", {
final_cohort_2326 |>
  summarise(Source_rows = n()) |>
  hh_print(width = Inf)
})
# Count original resident-file pregnancies and missing counts.
hh_show("Count original resident-file pregnancies and missing counts", {
total_births_by_LA_2326 |>
  summarise(Source_rows = n(), Total_count = sum(count),
    Missing_count_rows = sum(is.na(count))) |>
  hh_print(width = Inf)
})

# END LATEST UNFILTERED DIAGNOSTICS.
# OLDER DETAILED CHECKS BELOW: these retain their original filters and processing.

library(dplyr)
source("R/report_cohort.R")

# Use exactly the report's birth-age / booking-age fallback rule.
prepared_age_check <- prepare_report_cohort(final_cohort_2326)
# Raw diagnostic counts behind the report's age-completeness table.
# Denominator distinguishes the whole cohort from records missing birth age.
# Check age-completeness denominators.
hh_show("Check age-completeness denominators", {
summarise_maternal_age_completeness(final_cohort_2326) |>
  mutate(percent = if_else(Denominator > 0, 100 * N / Denominator, NA_real_))
})
age_check <- final_cohort_2326 |>
  mutate(age_numeric = prepared_age_check$Age,
    age_source = prepared_age_check$Age_source)

# Before/after missing-age counts and the number recovered from booking age.
# Check ages recovered from booking by year and birth status.
hh_show("Check ages recovered from booking by year and birth status", {
age_check |>
  group_by(birth_yr, birth_flag) |>
  summarise(pregnancies = n(), birth_age_na = sum(is.na(age_at_birth_mother)),
    recovered_from_booking = sum(age_source == "Booking fallback"),
    still_unknown_or_invalid = sum(is.na(age_numeric)),
    percent_still_unknown = 100 * mean(is.na(age_numeric)), .groups = "drop")
})

# Count pregnancies still lacking an assigned age.
hh_show("Count pregnancies still lacking an assigned age", {
age_check |>
  summarise(
    total_pregnancies = n(),
    pregnancies_without_age = sum(is.na(age_numeric)),
    mothers_without_age = n_distinct(
      person_id_mother[is.na(age_numeric)], na.rm = TRUE
    ),
    percent_without_age = round(100 * mean(is.na(age_numeric)), 2)
  )
})

# List missing, invalid and out-of-range assigned ages.
hh_show("List missing, invalid and out-of-range assigned ages", {
age_check |>
  filter(
    is.na(age_numeric) |
      !between(age_numeric, 13, 54) |
      age_numeric != floor(age_numeric)
  ) |>
  count(age_at_birth_mother, age_at_booking_mother, age_numeric, age_source,
    name = "pregnancies", sort = TRUE)
})


# OPTIONAL SNIPPETS: select and run whichever bits are useful.
# Run Sarah's parquet reader first if the input objects are not loaded.
# These are raw internal diagnostics, not controlled publication outputs.
# Nothing here is sourced by the report runner or saves an output file.

# Missing/invalid ages by delivery status, year or provider ----------------
# Check age issues by year and booking provider.
hh_show("Check age issues by year and booking provider", {
age_check |>
  mutate(age_issue = case_when(
    is.na(age_numeric) ~ "Missing/non-numeric",
    !is.finite(age_numeric) | age_numeric != floor(age_numeric) ~ "Invalid age",
    !between(age_numeric, 13, 54) ~ "Known age outside 13-54",
    TRUE ~ "Usable age 13-54")) |>
  count(birth_yr, birth_flag, booking_provider, age_issue, name = "pregnancies")
})

# Missing age among records with booking evidence, by delivery status.
# Check assigned-age completeness among records with booking evidence.
hh_show("Check assigned-age completeness among records with booking evidence", {
age_check |>
  filter((!is.na(booking_provider) & nzchar(trimws(as.character(booking_provider)))) |
    (!is.na(antenatal_app_date) & nzchar(trimws(as.character(antenatal_app_date))))) |>
  group_by(birth_yr, birth_flag) |>
  summarise(pregnancies = n(), missing_age = sum(is.na(age_numeric)),
    percent_missing_age = 100 * mean(is.na(age_numeric)), .groups = "drop")
})

# Wider resident file: weight by count, not the number of aggregate rows.
# Check assigned ages in the count-weighted resident file.
hh_show("Check assigned ages in the count-weighted resident file", {
prepare_la_records(total_births_by_LA_2326) |>
  mutate(age_numeric = Age,
    count_numeric = suppressWarnings(as.numeric(as.character(count))),
    age_missing = is.na(age_numeric)) |>
  group_by(birth_yr, age_missing) |>
  summarise(source_rows = n(), recorded_count = sum(count_numeric), .groups = "drop")
})

# Other available age/date fields; the report uses only the agreed two ages.
# List available age and birth-date fields.
hh_show("List available age and birth-date fields", {
grep("age|dob|birth_date", names(final_cohort_2326), ignore.case = TRUE, value = TRUE)
})

# Geography coverage summaries: totals only, no individual-LA tables ------
source("R/report_cohort.R")
ons_codes <- unique(read.csv("data/2022 SNPP Births persons.csv")$AREA_CODE)

# Check cohort LA reconciliation and ONS matching.
hh_show("Check cohort LA reconciliation and ONS matching", {
final_cohort_2326 |>
  mutate(LA_2023 = report_la_code(la_of_mother),
    Source_LA_missing = is.na(la_of_mother),
    code_changed = report_code(la_of_mother, "Unknown LA") != LA_2023,
    in_ONS = LA_2023 %in% ons_codes) |>
  count(birth_yr, Source_LA_missing, code_changed, in_ONS,
    name = "pregnancies", .drop = FALSE)
})

# Check resident-file LA reconciliation and ONS matching.
hh_show("Check resident-file LA reconciliation and ONS matching", {
total_births_by_LA_2326 |>
  mutate(LA_2023 = report_la_code(la_of_mother),
    Source_LA_missing = is.na(la_of_mother),
    code_changed = report_code(la_of_mother, "Unknown LA") != LA_2023,
    in_ONS = LA_2023 %in% ons_codes) |>
  group_by(group, birth_yr, edd_yr, Source_LA_missing, code_changed, in_ONS,
    .drop = FALSE) |>
  summarise(Source_rows = n(), Total_count = sum(count),
    Missing_count_rows = sum(is.na(count)), .groups = "drop")
})

# Number of supported code conversions present; no list of individual LAs.
# Count geography conversions actually used.
hh_show("Count geography conversions actually used", {
report_applied_geography(final_cohort_2326$la_of_mother,
  total_births_by_LA_2326$la_of_mother) |>
  summarise(Source_codes_changed = n(), Target_codes = n_distinct(Target_code))
})

# Additional quick checks ------------------------------------------------
# Birth-age-only source check, before the booking fallback.
# Filter missing ages first: count(age_at_birth_mother = NA) would count all rows.
# Count missing birth ages before fallback.
hh_show("Count missing birth ages before fallback", {
final_cohort_2326 |>
  filter(is.na(age_at_birth_mother)) |>
  summarise(pregnancies_without_age = n())
})

# Check the original birth-age distribution.
hh_show("Check the original birth-age distribution", {
final_cohort_2326 |>
  count(age_at_birth_mother, name="n") |>
  mutate(percentage = round(n / sum(n)*100, 2)) |>
  arrange(age_at_birth_mother)  |>
  hh_print(n=Inf)
})

cat("Rendering streams progress directly to the console; render.log is no longer required.\n")

# BOOKING DROP CHECK: 2023/24, 2024/25 AND 2025/26 --------------------------
# Run this setup, then whichever numbered checks are useful. Load the current
# parquet objects with R/Read parquet files from azure.R first; no model run needed.
# Raw internal counts only: do not copy these tables into the shared report.
# This section does not change the source objects, run the model or save files.
library(dplyr)
source("R/report_cohort.R")
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
    Booking_FY = Pregnancy_FY,
    Booking_ICB = coalesce(unname(hh_booking_config$provider_icb[Booking_Trust]),
      "Outside lookup / unknown"),
    Booking_in_cluster = Booking_Trust %in% hh_booking_config$cluster_trusts)
stopifnot(nrow(hh_bookings) == nrow(final_cohort_2326))

# 1. Source rows BEFORE any report filtering, by supplied year and birth flag.
# These are pregnancy rows, not distinct women (a woman may have >1 pregnancy).
# Count source pregnancies by year and birth flag.
hh_show("Count source pregnancies by year and birth flag", {
tibble::as_tibble(final_cohort_2326) |>
  count(birth_yr, birth_flag, name = "raw_pregnancy_rows") |>
  hh_print(n = Inf)
})

# Missing dates/providers and failed appointment-date parsing.
# Assigned FY uses birth_yr first, otherwise edd_yr; appointment dates are audit only.
# Check booking dates, providers and parsing failures.
hh_show("Check booking dates, providers and parsing failures", {
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
  hh_print(width = Inf)
})
# Check the appointment-date field type.
hh_show("Check the appointment-date field type", {
class(final_cohort_2326$antenatal_app_date)
})

# 2. Annual bookings by provider code, including codes outside our lookup.
# No age, LA, ONS match, birth flag or delivery-year filter is applied here.
# Count annual bookings by provider.
hh_show("Count annual bookings by provider", {
hh_bookings |>
  filter(Booking_provider_supplied | Booking_date_supplied) |>
  count(Booking_Trust, Booking_ICB, Booking_FY, name = "raw_bookings") |>
  tidyr::pivot_wider(names_from = Booking_FY, values_from = raw_bookings,
    values_fill = 0) |>
  hh_print(n = Inf, width = Inf)
})
# Count annual bookings by provider ICB.
hh_show("Count annual bookings by provider ICB", {
hh_bookings |>
  filter(Booking_provider_supplied | Booking_date_supplied) |>
  count(Booking_ICB, Booking_FY, name = "raw_bookings") |>
  tidyr::pivot_wider(names_from = Booking_FY, values_from = raw_bookings,
    values_fill = 0) |>
  hh_print(n = Inf, width = Inf)
})
# ICB here means the BOOKING PROVIDER'S ICB, as in the chart, not residence.

# 3. Annual cluster counts, including unknown and out-of-period years.
# Both recorded events use the same assigned pregnancy FY.
# Compare annual cluster bookings and recorded non-home deliveries.
hh_show("Compare annual cluster bookings and recorded non-home deliveries", {
hh_bookings |> filter(Booking_in_cluster) |>
  count(FY = Booking_FY, name = "bookings") |>
  full_join(hh_bookings |> filter(Birth_recorded, Home == 0,
      Trust %in% hh_booking_config$cluster_trusts) |>
      count(FY = Pregnancy_FY, name = "recorded_non_home_deliveries"), by = "FY") |>
  arrange(FY) |>
  hh_print(n = Inf)
})

# 4. Assigned pregnancy FY and its birth/EDD source.
# Check the source of each assigned booking year.
hh_show("Check the source of each assigned booking year", {
hh_bookings |>
  filter(Booking_in_cluster) |>
  count(Booking_FY, Pregnancy_year_source, Source_birth_FY, Due_FY,
    Birth_flag, name = "bookings") |>
  hh_print(n = Inf)
})
# Duplicate pregnancy keys could inflate earlier years; inspect, don't deduplicate.
# Check repeated pregnancy keys without deleting rows.
hh_show("Check repeated pregnancy keys without deleting rows", {
hh_bookings |>
  filter(!is.na(Person), nzchar(Person), !is.na(Pregnancy), nzchar(Pregnancy)) |>
  group_by(Person, Pregnancy) |>
  mutate(repeated_key = n() > 1L) |>
  ungroup() |>
  group_by(Source_birth_FY) |>
  summarise(rows_with_repeated_key = sum(repeated_key), .groups = "drop") |>
  hh_print(n = Inf)
})

# 5. OPTIONAL: reconcile these rows to the SAVED report's raw historical totals.
# Run only when .projections.rds and the loaded parquet are from the same run.
# Differences can also mean the saved report is stale. Comparison is before
# disclosure rounding; the displayed chart can differ slightly through rounding.
# Compare saved projection history with the same residence scope in the loaded cohort.
cat("\nCompare saved booking history with loaded cohort counts\n")
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
      sum(hh_bookings$Booking_Trust %in% codes & hh_bookings$Booking_FY %in% line$Period &
        if (identical(hh_booking_saved$projection_geography$version, "english-ons-la-v1"))
          projection_la_scope(hh_bookings$LA, hh_booking_saved$projection_geography$eligible_las)$Included
        else rep(TRUE, nrow(hh_bookings)))
    }, numeric(1))
  hh_booking_reconcile |>
    transmute(Organisation, Period, Saved_raw_count = Value, Raw_recount,
      Difference = Saved_raw_count - Raw_recount) |>
    hh_print(n = Inf)
} else message("No .projections.rds here; checks 1-4 do not need a rendered report.")
