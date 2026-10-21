source("R/disclosure.R")
source("R/report_cohort.R")

episodes <- data.frame(Pregnancy_FY = c(rep("2023/24", 26), "2024/25", NA),
  LA = c(rep("E08000025", 20), "W06000002", rep("S12000033", 2),
    rep("N09000001", 2), NA, "W06000002", "S12000033"),
  Booking_provider_supplied = TRUE, Booking_date_supplied = FALSE,
  Birth_recorded = TRUE)
episodes$Booking_provider_supplied[1:2] <- FALSE
episodes$Booking_date_supplied[2] <- TRUE
episodes$Birth_recorded[27] <- FALSE
result <- report_country_coverage(episodes)
raw <- subset(result$counts, FY == "2023/24" & Activity == "Whole cohort")
shown <- subset(result$display, FY == "2023/24" & Activity == "Whole cohort")
stopifnot(sum(raw$N) == 26, all(raw$Denominator == 26),
  all(shown$Denominator == 25), shown$N[shown$Country == "Wales"] == 5,
  shown$Share[shown$Country == "Wales"] == 5/25,
  sum(subset(result$counts, FY == "2023/24" & Activity == "Bookings")$N) == 25,
  sum(subset(result$counts, FY == "2024/25" & Activity == "Deliveries")$N) == 0,
  sum(subset(result$counts, FY == "Unknown FY" & Activity == "Whole cohort")$N) == 1,
  all(is.na(subset(result$display, FY == "2025/26")$Share)))
for (table in result$tables) {
  stopifnot(identical(table$`_data`$Country, c("England", "Wales", "Scotland")),
    "Unknown FY_N" %in% names(table$`_data`),
    nchar(gt::as_raw_html(table)) > 0)
}
empty <- report_country_coverage(episodes[0, ])
stopifnot(all(empty$display$N == 0), all(is.na(empty$display$Share)),
  nrow(empty$tables$cohort$`_data`) == 3)
cat("PASS: three country rows, all-country denominators, separate activity totals, missing FY and controlled display values.\n")

scratch <- new.env()
scratch$final_cohort_2326 <- data.frame(person_id_mother = c("1", "1", "2", "3", "4", "5"),
  uniq_preg_id = as.character(1:6), birth_yr = c("2023/24", "2023/24", NA, "2023/24", "2023/24", NA),
  edd_yr = c(NA, NA, "2024/25", "2024/25", NA, NA), birth_flag = c(1, 1, 0, 1, 1, 0),
  birth_date = "2023-12-01", age_at_birth_mother = 30, age_at_booking_mother = 29,
  la_of_mother = c("E08000025", "E08000025", "W06000002", "S12000033", "N09000001", NA),
  booking_provider = c("RQ3", "RQ3", "RQ3", NA, "RQ3", NA),
  antenatal_app_date = NA_character_, birth_provider = "RQ3", home_birth = 0,
  resident_icb = "QHL")
before <- scratch$final_cohort_2326
console <- capture.output(source("R/HH scratch area.R", local = scratch))
pooled <- scratch$hh_country_all_years
total <- subset(pooled, Country == "All countries")
outside <- subset(pooled, Country == "Known non-England subtotal")
stopifnot(total$Cohort_n == 6, total$Bookings_n == 4, total$Deliveries_n == 4,
  outside$Cohort_n == 3, outside$Bookings_pct == 50, outside$Deliveries_pct == 50,
  subset(scratch$hh_country_by_fy, FY == "2023/24" & Country == "Scotland")$Deliveries_n == 1,
  all(is.na(subset(scratch$hh_country_by_fy, FY == "2025/26")$Bookings_pct)),
  !exists("hh_uplift", envir = scratch, inherits = FALSE),
  !exists("quick_comparison", envir = scratch, inherits = FALSE),
  !any(grepl("E08000025|W06000002|S12000033|N09000001", console)),
  identical(before, scratch$final_cohort_2326), nrow(pooled) == 7)
cat("PASS: sourcing HH scratch prints only country summaries; raw counts, FY assignment, pooled percentages and repeated mothers are correct.\n")
