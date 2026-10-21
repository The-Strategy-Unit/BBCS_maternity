# Shared birth/EDD assignment and resident-file partition; invented data only.
library(dplyr)
source("R/report_cohort.R")
cohort <- data.frame(person_id_mother = as.character(1:8), uniq_preg_id = as.character(1:8),
  birth_yr = c("2025/26", NA, "", "2024/25", NA, "bad", "2025/27", NA),
  edd_yr = c("2024/25", "2025/26", "2024/25", NA, NA, "2025/26", "2025/26", "2023/24"),
  birth_flag = c(1, 0, 0, 1, 0, 1, 1, 1), birth_date = as.Date(NA),
  la_of_mother = "E08000031", age_at_birth_mother = 30, age_at_booking_mother = 29,
  birth_provider = "RL4", home_birth = 0, resident_icb = "QUA",
  booking_provider = c(rep("RL4", 7), NA), antenatal_app_date = as.Date(NA))
before <- cohort
prepared <- prepare_report_cohort(cohort)
expected <- c("2025/26", "2025/26", "2024/25", "2024/25", NA, NA, NA, "2023/24")
stopifnot(identical(prepared$Pregnancy_FY, expected), identical(cohort, before),
  identical(prepared$Year, expected), nrow(prepared) == nrow(cohort),
  identical(prepared$Booking_year, prepared$Delivery_year),
  identical(prepared$Booking_year, prepared$Pregnancy_year),
  identical(prepared$Birth_recorded, cohort$birth_flag == 1),
  projection_booking_destination(prepared)[8] == "No_recorded_booking",
  identical(prepared$Pregnancy_year_source, c("Birth year", "EDD year", "EDD year", "Birth year",
    "Missing", "Birth year", "Birth year", "EDD year")))
without_edd <- cohort; without_edd$edd_yr <- NULL
stopifnot(all(is.na(prepare_report_cohort(without_edd)$Pregnancy_FY[c(2, 3, 5, 8)])))
stopifnot(nrow(prepare_report_cohort(cohort[0, ])) == 0)

residents <- data.frame(la_of_mother = "E08000031",
  birth_yr = c("2025/26", "2025/26", NA, NA, "2024/25", "2023/24", NA),
  edd_yr = c("2024/25", NA, "2024/25", "2025/26", NA, "2024/25", NA),
  group = c("antenatal+delivery", "birth only", "antenatal only", "antenatal only",
    "birth only", "antenatal+delivery", "unknown"),
  age_at_birth_mother = c(30, NA, 31, NA, 30, 32, 30), age_at_booking_mother = 29,
  count = c(10, 5, 4, 3, 2, 7, 11))
before <- residents
deliveries <- prepare_la_deliveries(residents)
antenatal <- prepare_la_antenatal_only(residents)
prepared_residents <- prepare_la_records(residents)
stopifnot(sum(deliveries$count) == 24, sum(antenatal$count) == 7,
  sum(deliveries$count) + sum(antenatal$count) + 11 == sum(residents$count),
  sum(deliveries$count[deliveries$birth_yr == "2025/26"]) == 15,
  antenatal$count[antenatal$edd_yr %in% "2024/25"] == 4,
  antenatal$count[antenatal$edd_yr %in% "2025/26"] == 3,
  deliveries$age_at_birth_mother[deliveries$count == 5] == 29,
  is.na(prepared_residents$Pathway_group[7]), identical(residents, before))
residents$count[2] <- NA_real_
stopifnot(is.na(prepare_la_deliveries(residents)$count[
  prepare_la_deliveries(residents)$age_at_birth_mother == 29]))
cat("PASS: shared birth/EDD FY, three source pathway groups, birth-age fallback, unknown groups and retained NA counts.\n")
