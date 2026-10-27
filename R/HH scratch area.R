# FIRST CHECK: IS THIS ACTUALLY A PROBLEM? ------------------------------
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
  print(answer, row.names = FALSE, right = FALSE)
  cat("Counts are affected cohort activity. Bookings and deliveries can overlap; do not add them.\n",
    "This checks calibration causes only, separately from country/geography coverage.\n",
    "No changes made. If YES, use this summary to decide what to investigate next.\n", sep = "")
  invisible(answer)
}
if (exists("first_draft_projections")) {
  hh_calibration_check <- hh_calibration_problem_check(first_draft_projections)
} else cat("\nRun the analysis first, then rerun this FIRST CHECK block.\n")
# END FIRST CHECK ------------------------------------------------------

# UNKNOWN AGES: exact internal counts after birth/booking-age fallback
# Count-weighted resident totals; overlapping files, not additive
source("R/report_cohort.R")
dplyr::bind_rows(
  final_cohort_2326 = prepare_report_cohort(final_cohort_2326) |>
    dplyr::summarise(Total = dplyr::n(), Unknown_age = sum(is.na(Age))),
  total_births_by_LA_2326 = prepare_la_records(total_births_by_LA_2326) |>
    dplyr::summarise(Total = sum(count), Unknown_age = sum(count[is.na(Age)])),
  .id = "MSDS_file") |>
  dplyr::mutate(Unknown_pct = round(100 * Unknown_age / Total, 2)) |> print()

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
print(hh_country_by_fy, n = Inf, width = Inf)
cat("\nMSDS COUNTRY CONTRIBUTIONS: ALL SUPPLIED YEARS COMBINED\n")
print(hh_country_all_years, n = Inf, width = Inf)
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

hh_explain_projection_gaps <- function(projections, cohort, detail_limit = 12L) {
  needed <- c("provider_projection_gaps", "provider_coverage", "organisations", "allocation_tables")
  if (!all(needed %in% names(projections)))
    stop("Run the current analysis first: provider coverage diagnostics are missing.")
  episodes <- prepare_report_cohort(cohort)
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
    Private_NHS_factor = "Private–NHS factor", Bookings_uplift_modifier = "pathway factor")
  for (i in which(cells$Cause == "No same-age uplift estimate")) {
    master <- projections$master
    if (is.null(master)) next
    rows <- master[which(master$LA == cells$LA[i] & master$Age == cells$Age[i]), ]
    fields <- intersect(names(factor_names), names(rows))
    missing <- fields[vapply(fields, function(field) any(!is.finite(rows[[field]])), logical(1))]
    if (length(missing)) cells$Evidence[i] <- paste0("Unavailable: ",
      paste(unname(factor_names[missing]), collapse = ", "))
  }

  cat("\nWHY PROVIDER PROJECTIONS ARE PARTIAL — COHORT RECORDS ONLY\n")
  cat("Selected age unknown/unusable after fallback: ", sum(is.na(episodes$Age)),
    " of ", nrow(episodes), " cohort pregnancies",
    if (!anyNA(episodes$Age)) " — age missingness is not a cause in this cohort.\n" else ".\n", sep = "")
  cat("Flag scope: cluster-provider activity, ages ", min(ages), "–", max(ages),
    " plus unknown age; pooled supplied cohort. Booking and delivery columns overlap.\n", sep = "")
  print(scope, n = Inf, width = Inf)
  excluded <- records |> filter(!is.na(Age), !Age %in% ages) |> count(Allocation, name = "Out_of_range")
  if (nrow(excluded)) { cat("Known ages outside model scope (NOT Partial causes):\n"); print(excluded, n = Inf) }
  if (!nrow(affected)) cat("No unmatched in-scope provider records: no cohort matching gaps explain Partial.\n") else {
    cat("\nACTUAL CAUSES (unique pregnancies; activity columns overlap)\n")
    print(causes, n = Inf, width = Inf)
    cat("\nAFFECTED TRUSTS (codes match the detail list)\n")
    print(trusts, n = Inf, width = Inf)
    other <- cells |> filter(!grepl("^Outside supplied ONS geography:", Cause))
    cat("\nISSUES OTHER THAN COUNTRY COVERAGE: ", nrow(other), " affected LA/age/cause combinations\n", sep = "")
    if (!nrow(other)) cat("None: the saved gaps are entirely due to country coverage.\n")
    for (i in seq_len(min(nrow(other), detail_limit))) {
      row <- other[i, ]
      cat("- ", row$LA_name, " [", row$LA, "], age ", ifelse(is.na(row$Age), "Unknown", row$Age),
        ": ", row$Evidence, "; ", row$Bookings, " bookings / ", row$Deliveries,
        " deliveries; trusts ", row$Trusts, ".\n", sep = "")
    }
    if (nrow(other) > detail_limit) cat("Showing the ", detail_limit,
      " largest; all ", nrow(other), " are in hh_unmatched_cells (no lookup-only issues).\n", sep = "")
  }
  cat("\nhh_unmatched_records: original cohort Row, FY, source/reconciled LA, ages, trust and actual reason.\n",
    "Inspect a source pregnancy locally with final_cohort_2326[hh_unmatched_records$Row[1], ].\n", sep = "")
  list(records = affected |> select(Row, Pregnancy_FY, Allocation, Trust, Organisation,
      LA_source, LA, LA_name, Country, Age_at_birth, Age_at_booking, Age, Age_source, Cause),
    cells = cells, causes = causes, trusts = trusts, scope = scope)
}

# Optional detailed investigation later: run these lines only when needed.
# hh_gap_diagnostic <- hh_explain_projection_gaps(first_draft_projections, final_cohort_2326)
# hh_unmatched_records <- hh_gap_diagnostic$records
# hh_unmatched_cells <- hh_gap_diagnostic$cells
# END WHY PROVIDER PROJECTIONS ARE PARTIAL -------------------------------

# Older investigations: select their lines to run; sourcing skips them.
if (FALSE) {

# QUICK RAW UPLIFT: same groups, FYs and ages; NA counts stay visible -----
library(dplyr)
source("R/report_cohort.R")
hh_uplift <- prepare_la_records(total_births_by_LA_2326) |>
  mutate(LA = report_la_code(la_of_mother)) |>
  filter(Pregnancy_FY %in% c("2023/24", "2024/25", "2025/26"), Age %in% 13:54) |>
  group_by(LA, Age) |>
  summarise(
    Both = sum(count[Pathway_group %in% "antenatal+delivery"]),
    Birth_only = sum(count[Pathway_group %in% "birth only"]),
    Antenatal_only = sum(count[Pathway_group %in% "antenatal only"]),
    Missing_counts = sum(is.na(count)),
    Unknown_groups = sum(is.na(Pathway_group)), .groups = "drop") |>
  mutate(Deliveries = Both + Birth_only,
    Raw_uplift = if_else(Unknown_groups > 0, NA_real_,
      (Deliveries + Antenatal_only) / Deliveries))
print(hh_uplift, n = 30, width = Inf)
hh_uplift |> filter(Missing_counts > 0 | Unknown_groups > 0 | !is.finite(Raw_uplift)) |>
  print(n = Inf, width = Inf)
# Model counts currently replace NA with zero and use zero for no deliveries.
# This raw check keeps NA/Inf/NaN and includes all supplied LAs, before catchment selection.
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
tidyr::expand_grid(Scope = c("All LAs", "Cohort LAs"),
  FY = c("2023/24", "2024/25", "2025/26")) |>
  left_join(quick_comparison, by = c("Scope", "FY")) |>
  print(n = Inf, width = Inf)

# Separate small table so missing/other years cannot disappear unnoticed.
# NA-year bookings have booking evidence but no usable birth/EDD FY.
# NA source counts remain NA.
quick_comparison |>
  filter(!FY %in% c("2023/24", "2024/25", "2025/26")) |>
  arrange(Scope, FY) |>
  print(n = Inf, width = Inf)
# END QUICK COMPARISON -------------------------------------------------

# LATEST: DEMOGRAPHICS COLUMN NAMES AND TYPES ---------------------------
# Schema only: does not print patient records or change either source object.
data.frame(
  column = names(demographics),
  type = vapply(demographics, function(x) paste(class(x), collapse = "/"), character(1))
) |> print(row.names = FALSE)

# Cohort mother-ID type, to compare with the demographics join key.
class(final_cohort_2326$person_id_mother)
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
final_cohort_2326 |>
  group_by(birth_yr, birth_flag, .drop = FALSE) |>
  summarise(Whole_file_rows = n(),
    Cluster_delivery_provider_rows = sum(birth_provider_flag %in% 1),
    Cluster_LA_resident_rows = sum(la_of_mother %in%
      c("E08000025", "E08000027", "E08000028", "E08000029", "E08000030", "E08000031")),
    Missing_delivery_provider_flags = sum(is.na(birth_provider_flag)),
    Missing_LA_rows = sum(is.na(la_of_mother)), .groups = "drop") |>
  print(n = Inf, width = Inf)

# 2. RAW APPOINTMENT AUDIT ONLY: not the model's assigned pregnancy FY.
# Sarah supplies ISO dates. The parse is a separate diagnostic column.
# Unparsed source values stay in the NA FY group; the raw values appear below.
# These dates do not assign pregnancy FY. Unknown-date rows remain
# visible rather than being counted as confirmed bookings in an invented year.
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
  print(n = Inf, width = Inf)

# 3. BOOKING FY VERSUS SUPPLIED DUE/DELIVERY FY (all combinations).
# Look for 2025/26 appointments with edd_yr or birth_yr = 2026/27.
# Their absence cannot prove upstream exclusion: records excluded before this
# extract cannot be recovered here. Compare with Sarah's current extraction SQL.
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
  print(n = Inf, width = Inf)

# 4. LA EXTRACT: every supplied delivery/due-year pair, without coalescing them.
# group supplies antenatal only, birth only or antenatal+delivery.
# Dates assign FY: birth_yr first, otherwise edd_yr.
# Both years NA = unassigned, still printed. Provider totals cannot be obtained
# from this LA extract because it has no provider field.
total_births_by_LA_2326 |>
  group_by(group, birth_yr, edd_yr, .drop = FALSE) |>
  summarise(Source_rows = n(), Whole_file_count = sum(count),
    Cluster_LA_resident_count = sum(count[la_of_mother %in%
      c("E08000025", "E08000027", "E08000028", "E08000029", "E08000030", "E08000031")]),
    Missing_LA_count = sum(count[is.na(la_of_mother)]),
    Missing_count_rows = sum(is.na(count)),
    Missing_birth_age_rows = sum(is.na(age_at_birth_mother)),
    Missing_booking_age_rows = sum(is.na(age_at_booking_mother)), .groups = "drop") |>
  print(n = Inf, width = Inf)

# 5. PROVIDER COVERAGE: supplied providers and delivery years, including NA.
final_cohort_2326 |>
  count(booking_provider, booking_provider_flag, name = "Rows", .drop = FALSE) |>
  print(n = Inf, width = Inf)
final_cohort_2326 |>
  count(birth_yr, birth_flag, birth_provider, birth_provider_flag,
    name = "Rows", .drop = FALSE) |>
  print(n = Inf, width = Inf)

# 6. ORIGINAL FLAGS AND DATES: every supplied combination, no parsing.
final_cohort_2326 |>
  count(resident_flag, booking_provider_flag, birth_provider_flag, birth_flag,
    name = "Rows", .drop = FALSE) |>
  print(n = Inf, width = Inf)
final_cohort_2326 |>
  count(antenatal_app_date, edd_yr, birth_yr, birth_date,
    name = "Rows", .drop = FALSE) |>
  print(n = Inf, width = Inf)

# Optional: schema only, to confirm demographic fields without printing IDs.
names(demographics)
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
final_cohort_2326 |>
  count(birth_yr, birth_flag, booking_provider_flag, birth_provider_flag,
    name = "Rows", .drop = FALSE) |>
  print(n = Inf, width = Inf)

# 2. COHORT: booking FY derived from appointment date, with all rows retained.
# FY = NA includes absent/unparsed dates; the two explicit checks distinguish them.
# A row without a usable booking date is not labelled a confirmed booking.
# No year-range filter: earlier/later years and NA all appear.
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
  print(n = Inf, width = Inf)

# 3. COHORT: original appointment dates and booking flags, without date parsing.
# This may print many rows. Includes NA, blanks and any unexpected source values.
final_cohort_2326 |>
  count(antenatal_app_date, booking_provider_flag, name = "Rows", .drop = FALSE) |>
  print(n = Inf, width = Inf)

# 4. LA FILE: BOTH supplied year fields together, including null combinations.
# Pathway membership comes from group, never from whether birth_yr is populated.
# Raw field audit only. The model uses birth_yr first, otherwise edd_yr.
# Total_count includes every row; cluster/unknown-LA columns are extra subtotals.
# No na.rm: an NA count stays visible as an NA sum, with Missing_count_rows shown.
total_births_by_LA_2326 |>
  group_by(group, birth_yr, edd_yr, .drop = FALSE) |>
  summarise(Source_rows = n(), Total_count = sum(count),
    Cluster_resident_count = sum(count[la_of_mother %in%
      c("E08000025", "E08000027", "E08000028", "E08000029", "E08000030", "E08000031")]),
    NA_LA_count = sum(count[is.na(la_of_mother)]),
    Missing_count_rows = sum(is.na(count)), .groups = "drop") |>
  print(n = Inf, width = Inf)

# 5. LA FILE: combined residence groups by year, never one row per LA.
# Group labels are diagnostic only; source values and missing counts are unchanged.
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
  print(n = Inf, width = Inf)

# 6. Whole-file totals: compare these with the sums of the groups above.
# Each cohort table counts the same full set of source rows in a different way.
final_cohort_2326 |>
  summarise(Source_rows = n()) |>
  print(width = Inf)
total_births_by_LA_2326 |>
  summarise(Source_rows = n(), Total_count = sum(count),
    Missing_count_rows = sum(is.na(count))) |>
  print(width = Inf)

# END LATEST UNFILTERED DIAGNOSTICS.
# OLDER DETAILED CHECKS BELOW: these retain their original filters and processing.

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

# Geography coverage summaries: totals only, no individual-LA tables ------
source("R/report_cohort.R")
ons_codes <- unique(read.csv("data/2022 SNPP Births persons.csv")$AREA_CODE)

final_cohort_2326 |>
  mutate(LA_2023 = report_la_code(la_of_mother),
    Source_LA_missing = is.na(la_of_mother),
    code_changed = report_code(la_of_mother, "Unknown LA") != LA_2023,
    in_ONS = LA_2023 %in% ons_codes) |>
  count(birth_yr, Source_LA_missing, code_changed, in_ONS,
    name = "pregnancies", .drop = FALSE)

total_births_by_LA_2326 |>
  mutate(LA_2023 = report_la_code(la_of_mother),
    Source_LA_missing = is.na(la_of_mother),
    code_changed = report_code(la_of_mother, "Unknown LA") != LA_2023,
    in_ONS = LA_2023 %in% ons_codes) |>
  group_by(group, birth_yr, edd_yr, Source_LA_missing, code_changed, in_ONS,
    .drop = FALSE) |>
  summarise(Source_rows = n(), Total_count = sum(count),
    Missing_count_rows = sum(is.na(count)), .groups = "drop")

# Number of supported code conversions present; no list of individual LAs.
report_applied_geography(final_cohort_2326$la_of_mother,
  total_births_by_LA_2326$la_of_mother) |>
  summarise(Source_codes_changed = n(), Target_codes = n_distinct(Target_code))

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
tibble::as_tibble(final_cohort_2326) |>
  count(birth_yr, birth_flag, name = "raw_pregnancy_rows") |>
  print(n = Inf)

# Missing dates/providers and failed appointment-date parsing.
# Assigned FY uses birth_yr first, otherwise edd_yr; appointment dates are audit only.
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
  print(width = Inf)
class(final_cohort_2326$antenatal_app_date)

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

# 3. Annual cluster counts, including unknown and out-of-period years.
# Both recorded events use the same assigned pregnancy FY.
hh_bookings |> filter(Booking_in_cluster) |>
  count(FY = Booking_FY, name = "bookings") |>
  full_join(hh_bookings |> filter(Birth_recorded,
      Trust %in% hh_booking_config$cluster_trusts) |>
      count(FY = Pregnancy_FY, name = "recorded_deliveries"), by = "FY") |>
  arrange(FY) |>
  print(n = Inf)

# 4. Assigned pregnancy FY and its birth/EDD source.
hh_bookings |>
  filter(Booking_in_cluster) |>
  count(Booking_FY, Pregnancy_year_source, Source_birth_FY, Due_FY,
    Birth_flag, name = "bookings") |>
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
}
