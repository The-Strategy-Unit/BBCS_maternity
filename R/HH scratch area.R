#scratch area 

library(dplyr)

age_check <- final_cohort_2326 |>
  mutate(age_numeric = suppressWarnings(
    as.numeric(as.character(age_at_birth_mother))
  ))

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
  count(age_at_birth_mother, name = "pregnancies", sort = TRUE)


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

# Other age/date fields available for discussion; no substitute is assumed.
grep("age|dob|birth_date", names(final_cohort_2326), ignore.case = TRUE, value = TRUE)

# Raw and harmonised LA codes, plus remaining ONS gaps ---------------------
source("R/report_cohort.R")
ons_codes <- unique(read.csv("data/2022 SNPP Births persons.csv")$AREA_CODE)

final_cohort_2326 |>
  count(la_of_mother, name = "pregnancies") |>
  mutate(LA_2023 = report_la_code(la_of_mother),
    code_changed = report_code(la_of_mother, "Unknown LA") != LA_2023,
    in_ONS = LA_2023 %in% ons_codes) |>
  arrange(in_ONS, desc(code_changed), desc(pregnancies))

total_births_by_LA_2326 |>
  mutate(LA_2023 = report_la_code(la_of_mother)) |>
  filter(!LA_2023 %in% ons_codes) |>
  group_by(la_of_mother, LA_2023, birth_yr) |>
  summarise(recorded_count = sum(as.numeric(as.character(count))), .groups = "drop")

# Supported conversions present in either input (codes only).
report_applied_geography(final_cohort_2326$la_of_mother,
  total_births_by_LA_2326$la_of_mother)

# Additional quick checks ------------------------------------------------
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
