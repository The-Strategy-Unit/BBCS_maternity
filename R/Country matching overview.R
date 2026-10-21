# Console only. Uses final_cohort_2326; no report build or individual-LA tables.
library(dplyr)
source("R/report_cohort.R")
stopifnot(exists("final_cohort_2326"))

country_names <- c("England", "Wales", "Scotland", "Northern Ireland", "Unknown/other code")
country_ons_las <- read.csv("data/2022 SNPP Births persons.csv") |>
  filter(AGE_GROUP != "All ages") |> distinct(LA = AREA_CODE)
country_source_coverage <- bind_rows(
  read.csv("data/lookups/la_names.csv") |> transmute(Source = "Lookup_LAs", LA),
  country_ons_las |> mutate(Source = "ONS_LAs")) |>
  mutate(Country = report_la_country(LA)) |>
  count(Country, Source, name = "LAs") |>
  tidyr::complete(Country = country_names, Source, fill = list(LAs = 0)) |>
  tidyr::pivot_wider(names_from = Source, values_from = LAs)

# Read the current provider-to-ICB definitions without running the analysis.
country_settings <- new.env()
for (statement in parse("R/Maternity analysis.R")) {
  if (is.call(statement) && identical(statement[[1]], as.name("<-")) &&
      as.character(statement[[2]])[1] %in% c("icb_labels", "icb_provider_codes", "provider_icb"))
    eval(statement, country_settings)
}
country_episodes <- prepare_report_cohort(final_cohort_2326) |>
  transmute(FY = Pregnancy_FY, LA, Country = report_la_country(LA),
    In_ONS = LA %in% country_ons_las$LA,
    Age_band = if_else(!is.finite(Age) | Age < 0 | Age != floor(Age),
      "Unknown/invalid", as.character(Age_band)),
    Birth_recorded, Home, Trust, Booking_Trust,
    Booking_provider_supplied, Booking_date_supplied)
country_activity <- bind_rows(
  country_episodes |> filter(Booking_provider_supplied | Booking_date_supplied) |>
    mutate(Activity = "Booking", Provider = if_else(Booking_provider_supplied,
      Booking_Trust, "Booking provider unknown")),
  country_episodes |> filter(Birth_recorded) |>
    mutate(Activity = "Delivery", Provider = case_when(
      Home %in% 1 ~ "Home birth",
      Home %in% 0 & Trust != "Unknown provider" ~ Trust,
      TRUE ~ "Delivery location unknown"))) |>
  mutate(Non_England = Country %in% c("Wales", "Scotland", "Northern Ireland"),
    ICB = unname(country_settings$provider_icb[Provider]),
    ICB = coalesce(ICB, if_else(Provider %in% c("Home birth",
      "Booking provider unknown", "Delivery location unknown"), Provider, "Unmapped provider ICB")))

# 1. Every FY, including missing FY: percentage of all activity of that type.
country_by_fy <- country_activity |>
  group_by(FY, Activity, Country) |>
  summarise(Pregnancies = n(), Without_ONS_LA = sum(!In_ONS), .groups = "drop") |>
  tidyr::complete(FY = unique(c("2023/24", "2024/25", "2025/26", country_episodes$FY)),
    Activity = c("Booking", "Delivery"), Country = country_names,
    fill = list(Pregnancies = 0L, Without_ONS_LA = 0L)) |>
  group_by(FY, Activity) |>
  mutate(All_activity = sum(Pregnancies),
    Pct_all_activity = 100 * Pregnancies / All_activity,
    Pct_all_without_ONS = 100 * Without_ONS_LA / All_activity) |>
  ungroup() |> arrange(FY, Activity, Country)

country_activity_totals <- country_activity |> count(Activity, name = "All_activity")
country_split <- function(data, by) {
  data |> group_by(across(all_of(c("Activity", by)))) |>
    summarise(All_activity_in_group = n(),
      Wales = sum(Country == "Wales"), Scotland = sum(Country == "Scotland"),
      Northern_Ireland = sum(Country == "Northern Ireland"),
      Non_England = sum(Non_England),
      Non_England_without_ONS = sum(Country %in% c("Wales", "Scotland", "Northern Ireland") & !In_ONS),
      .groups = "drop") |>
    left_join(country_activity_totals, by = "Activity") |>
    mutate(Pct_of_group = 100 * Non_England / All_activity_in_group,
      Pct_of_all_activity = 100 * Non_England / All_activity)
}

# 2. Age split pooled over the supplied cohort; all ages remain in denominators.
country_by_age <- country_split(country_activity, "Age_band") |>
  arrange(Activity, match(Age_band, c(report_age_bands, "Unknown/invalid")))

# 3. Destination trust and provider ICB, pooled over the supplied cohort.
country_provider_rows <- bind_rows(
  country_activity |> mutate(Level = "Trust/destination", Code = Provider),
  country_activity |> mutate(Level = "ICB", Code = ICB))
country_organisation_names <- bind_rows(
  read.csv("data/lookups/trust_names.csv") |>
    transmute(Level = "Trust/destination", Code = Trust, Organisation = Trust_name),
  tibble(Level = "ICB", Code = names(country_settings$icb_labels),
    Organisation = unname(country_settings$icb_labels)))
country_by_organisation <- country_split(country_provider_rows, c("Level", "Code")) |>
  filter(Non_England > 0) |>
  left_join(country_organisation_names, by = c("Level", "Code")) |>
  mutate(Organisation = coalesce(Organisation, Code), .after = Code) |>
  arrange(Activity, Level, desc(Non_England))

cat("\nCOUNTRY COVERAGE OF THE SUPPLIED LOOKUPS / PROJECTIONS\n")
print(country_source_coverage, n = Inf, width = Inf)
cat("\nCOUNTRY BY PREGNANCY FY: counts and percentages of all bookings / deliveries\n")
print(country_by_fy, n = Inf, width = Inf)
cat("\nNON-ENGLISH RESIDENTS BY AGE BAND: pooled over the supplied cohort\n")
print(country_by_age, n = Inf, width = Inf)
cat("\nNON-ENGLISH RESIDENTS BY DESTINATION TRUST / ICB: pooled over the supplied cohort\n")
print(country_by_organisation, n = Inf, width = Inf)
cat("\nEach pregnancy counts once per activity; separate pregnancies for one mother remain separate.\n",
  "FY uses birth_yr first, otherwise edd_yr. Booking and delivery use the same assigned FY.\n",
  "Pct_of_group uses all activity in that age band or organisation; Pct_of_all_activity uses the whole cohort's bookings or deliveries.\n",
  "Non-English means Wales, Scotland or Northern Ireland; unknown residence stays separate in the FY table.\n",
  "Home births and unknown destinations remain in delivery totals. ICB is the provider's ICB, not residence ICB.\n",
  "Organisation rows show only destinations with non-English activity. Trust and ICB are alternative views; do not add them.\n",
  "Without_ONS counts measure absent LA series only, not age/calibration failures. Percentages are 0-100; undefined percentages stay NaN.\n", sep = "")
