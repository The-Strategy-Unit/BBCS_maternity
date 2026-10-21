# Invented records only. Run from project root: Rscript R/test_geography.R
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
  supplied <- c("E08000038", "E08000039", "E07000164", "E07000165",
    "E07000190", "E07000191", "E07000246", "E07000028")
  expected <- c("E08000016", "E08000019", "E06000065", "E06000065",
    "E06000066", "E06000066", "E06000066", "E06000063")
  stopifnot(identical(report_la_code(supplied), expected),
    identical(report_la_code(expected), expected),
    identical(report_la_code(c("W06000002", "S12000047", "L99999999", "E09999999")),
      c("W06000002", "S12000047", "L99999999", "E09999999")),
    identical(report_la_code(c(NA, "", " e08000038 ")), c("Unknown LA", "Unknown LA", "E08000016")),
    identical(report_code("E08000038"), "E08000038"),
    all(report_la_crosswalk$Target_code %in% read.csv("data/2022 SNPP Births persons.csv")$AREA_CODE))

  cells <- tidyr::expand_grid(LA = supplied, FY = c("2023/24", "2024/25", "2025/26"), Row = 1:12)
  cohort <- data.frame(person_id_mother = seq_len(nrow(cells)), uniq_preg_id = seq_len(nrow(cells)),
    birth_yr = cells$FY, birth_flag = as.integer(cells$Row <= 10),
    birth_date = as.Date(paste0(substr(cells$FY, 1, 4), "-12-01")),
    la_of_mother = cells$LA, age_at_birth_mother = 25, age_at_booking_mother = NA_real_, birth_provider = "RQ3", home_birth = 0L,
    resident_icb = "QHL", booking_provider = "RQ3",
    antenatal_app_date = as.Date(paste0(substr(cells$FY, 1, 4), "-06-01")))
  resident <- unique(cells[c("LA", "FY")]) |>
    dplyr::transmute(la_of_mother = LA, birth_yr = FY, age_at_birth_mother = 25, count = 10)
  # Include a cluster LA alongside outside LAs. Each uses its own expanded
  # extract, so positive allocations also exercise harmonised antenatal counts.
  local_rows <- cohort[cohort$la_of_mother == supplied[1], ]
  local_rows$la_of_mother <- "E08000025"
  local_rows$person_id_mother <- local_rows$uniq_preg_id <- nrow(cohort) + seq_len(nrow(local_rows))
  cohort <- rbind(cohort, local_rows)
  resident <- rbind(resident, data.frame(la_of_mother = "E08000025",
    birth_yr = c("2023/24", "2024/25", "2025/26"), age_at_birth_mother = 25, count = 10))
  resident <- resident |> dplyr::mutate(
    group = "antenatal+delivery", edd_yr = NA_character_, age_at_booking_mother = age_at_birth_mother)
  resident <- dplyr::bind_rows(resident, resident |>
    dplyr::mutate(group = "antenatal only", edd_yr = birth_yr, birth_yr = NA_character_,
      age_at_birth_mother = NA_real_, count = 2))
  original <- list(cohort = cohort, resident = resident)
  canonical_cohort <- cohort
  canonical_cohort$la_of_mother <- ifelse(cohort$la_of_mother %in% supplied,
    expected[match(cohort$la_of_mother, supplied)], cohort$la_of_mother)
  canonical_resident <- resident |>
    dplyr::mutate(la_of_mother = ifelse(la_of_mother %in% supplied,
      expected[match(la_of_mother, supplied)], la_of_mother)) |>
    dplyr::group_by(group, la_of_mother, birth_yr, edd_yr, age_at_birth_mother, age_at_booking_mother) |>
    dplyr::summarise(count = sum(count), .groups = "drop")
  ons <- tidyr::expand_grid(area_code = c(unique(expected), "E08000025"), age_group = as.character(15:44), year = 2025:2026) |>
    dplyr::mutate(area_name = area_code, projected_births = 100)
  original_ons <- ons
  counts <- read_national_age_counts()
  counts$live_births <- counts$total_births <- counts$maternities <- 100
  counts$stillbirths <- 0
  mapped <- build_projection_master(cohort, resident, ons, counts)
  canonical <- build_projection_master(canonical_cohort, canonical_resident, ons, counts)
  stopifnot(isTRUE(all.equal(mapped, canonical, check.attributes = FALSE)),
    any(mapped$Delivery_count_RQ3[mapped$LA %in% expected] > 0, na.rm = TRUE),
    identical(attr(mapped, "la_source_matching"), attr(canonical, "la_source_matching")),
    all(attr(mapped, "la_source_matching")$Issue == ""),
    identical(cohort, original$cohort), identical(resident, original$resident), identical(ons, original_ons),
    identical(prepare_report_cohort(cohort)$LA_source, cohort$la_of_mother),
    nrow(prepare_report_cohort(cohort)) == nrow(cohort),
    sum(attr(mapped, "allocation_tables")$residents$MSDS_resident_count) ==
      sum(resident$count[!is.na(resident$birth_yr)]),
    setequal(attr(mapped, "geography_changes")$Source_code, supplied),
    nrow(attr(canonical, "geography_changes")) == 0,
    nrow(report_applied_geography(character())) == 0)
  # Non-English feeders use supplied projections; absent projections stay gaps.
  country_codes <- c("W06000002", "S12000047")
  country_cohort <- dplyr::bind_rows(lapply(country_codes, function(code)
    transform(local_rows, la_of_mother = code)))
  country_residents <- dplyr::bind_rows(lapply(country_codes, function(code)
    transform(resident[resident$la_of_mother == "E08000025", ], la_of_mother = code)))
  country_ons <- dplyr::bind_rows(lapply(country_codes, function(code)
    transform(ons[ons$area_code == "E08000025", ], area_code = code, area_name = code)))
  country_model <- build_projection_master(dplyr::bind_rows(cohort, country_cohort),
    dplyr::bind_rows(resident, country_residents), dplyr::bind_rows(ons, country_ons), counts)
  stopifnot(all(country_codes %in% country_model$LA),
    all(country_codes %in% country_model$LA[country_model$Delivery_count_RQ3 > 0 &
      !is.na(country_model$Delivery_count_RQ3)]),
    identical(report_la_country(c(country_codes, NA, "L99999999")),
      c("Wales", "Scotland", "Unknown/other code", "Unknown/other code")))
  missing_country_model <- build_projection_master(dplyr::bind_rows(cohort, country_cohort),
    dplyr::bind_rows(resident, country_residents), ons, counts)
  country_sources <- attr(missing_country_model, "la_source_matching") |>
    dplyr::filter(LA %in% country_codes)
  stopifnot(all(country_sources$In_resident_counts), !any(country_sources$In_ONS),
    all(country_codes %in% attr(missing_country_model, "allocation_tables")$cohort_counts$LA))
  # Cohort pooling has no date filter: the audit must include a conversion
  # found only outside the three resident-count financial years.
  earlier <- cohort
  earlier_row <- which(earlier$la_of_mother == "E07000028")[1]
  earlier$la_of_mother[earlier_row] <- "E07000026" # also Cumberland
  earlier$birth_yr[earlier_row] <- "2022/23"
  earlier_model <- build_projection_master(earlier, resident, ons, counts)
  stopifnot("E07000026" %in% attr(earlier_model, "geography_changes")$Source_code,
    isTRUE(all.equal(earlier_model, mapped, check.attributes = FALSE)))
  # Existing canonical codes and old districts can coexist as disjoint records.
  # Mapping changes keys once; it does not replicate or deduplicate observations.
  mixed <- rbind(cohort, transform(cohort[1, ], la_of_mother = "E08000016"))
  stopifnot(nrow(prepare_report_cohort(mixed)) == nrow(cohort) + 1L)
  message("PASS: 2023 targets, chained mergers, Wales/Scotland feeders with/without ONS; raw counts preserved; model agrees with independently harmonised inputs.")
})
