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


final_cohort_2326 |>
  count(age_at_birth_mother = NA)

final_cohort_2326 |>
  count(age_at_birth_mother, name="n") |>
  mutate(percentage = round(n / sum(n)*100, 2)) |>
  arrange(age_at_birth_mother)  |>
  print(n=Inf)

file.exists("render.log")