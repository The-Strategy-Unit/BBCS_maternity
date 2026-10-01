# Reusable preparation functions for the ONS component of BBCS maternity.
# Source files remain unchanged in data/. All transformations happen in memory.

library(dplyr)
library(janitor)
library(purrr)
library(readr)
library(readxl)
library(tidyr)

ons_data_dir <- function() "data"

read_area_config <- function(path = file.path("config", "local_authorities.csv")) {
  config <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = col_character())) |>
    mutate(display_order = as.integer(display_order)) |>
    arrange(display_order)

  if (anyDuplicated(config$area_code) || anyDuplicated(config$area_name)) {
    stop("Local-authority configuration contains duplicate codes or names.")
  }
  if (nrow(config) == 0 || anyNA(config$area_code) || anyNA(config$area_name)) {
    stop("Local-authority configuration must contain at least one complete area.")
  }

  config
}

area_colour_palette <- function(config) {
  colours <- config$colour
  missing <- is.na(colours) | colours == ""
  if (any(missing)) {
    colours[missing] <- grDevices::hcl.colors(sum(missing), palette = "Dark 3")
  }
  stats::setNames(colours, config$area_name)
}

read_analysis_parameters <- function(path = file.path("config", "analysis_parameters.csv")) {
  values <- read_csv(path, show_col_types = FALSE) |>
    mutate(value = as.integer(value)) |>
    tibble::deframe()

  required <- c("minimum_maternal_age", "maximum_maternal_age")
  if (!all(required %in% names(values)) || values[[required[[1]]]] > values[[required[[2]]]]) {
    stop("Analysis parameters must contain a valid minimum and maximum maternal age.")
  }
  values
}

configured_age_band_spec <- function(parameters = read_analysis_parameters()) {
  minimum_age <- unname(parameters[["minimum_maternal_age"]])
  maximum_age <- unname(parameters[["maximum_maternal_age"]])
  ends <- sort(unique(c(
    seq(14, maximum_age, by = 5)[seq(14, maximum_age, by = 5) >= minimum_age],
    maximum_age
  )))
  starts <- c(minimum_age, head(ends, -1) + 1)
  tibble(
    start = starts,
    end = ends,
    label = ifelse(starts == ends, as.character(starts), paste0(starts, "-", ends))
  )
}

configured_age_band <- function(age, parameters = read_analysis_parameters()) {
  spec <- configured_age_band_spec(parameters)
  cut(
    age,
    breaks = c(spec$start[[1]] - 1, spec$end),
    labels = spec$label,
    include.lowest = TRUE
  )
}

ons_source_files <- function(data_dir = ons_data_dir()) {
  tibble::tribble(
    ~source_id, ~file_name, ~source_name, ~source_type, ~period, ~current_use,
    "snpp_2022", "ons_2022_based_births_migration_category.csv", "ONS 2022-based subnational population projections: births by age of mother", "Projection", "2023–2047", "Corrected migration-category variant on 2021 geography; projected rates and estimated maternities.",
    "snpp_female_population_2022", "ons_2022_based_female_population_projections.csv", "ONS 2022-based subnational population projections: female population by single year of age", "Projection", "2023–2047", "Migration-category population: denominator for the projected ratio and female-population context.",
    "historic_live_births", "ons_live_births_la_2013_2025.csv", "ONS live births in England and Wales down to local authority area, via Nomis", "Observed", "2013–2025", "Show registered live-birth trends and prepare age-adjusted maternity estimates.",
    "registrations_2023", "2023birthregistrations.xlsx", "ONS Births in England and Wales: birth registrations", "Observed", "2023", "Table 1: published GFR; Table 3: maternities; Table 7: national conversion counts.",
    "registrations_2024", "2024birthregistrations.xlsx", "ONS Births in England and Wales: birth registrations", "Observed", "2024", "Table 1: published GFR; Table 3: maternities; Table 7: national conversion counts.",
    "registrations_2025", "2025birthregistrations.xlsx", "ONS Births in England and Wales: birth registrations", "Observed", "2025", "Table 1: published GFR; Table 3: maternities; Table 7: national conversion counts."
  ) |>
    mutate(path = file.path(data_dir, file_name), available = file.exists(path))
}

read_female_population_single_age <- function(
    path = file.path(ons_data_dir(), "ons_2022_based_female_population_projections.csv"),
    area_config = read_area_config(),
    parameters = read_analysis_parameters()) {
  minimum_age <- unname(parameters[["minimum_maternal_age"]])
  maximum_age <- unname(parameters[["maximum_maternal_age"]])

  result <- read_csv(path, show_col_types = FALSE) |>
    clean_names() |>
    transmute(
      area_code = geography_code,
      area_name = geography_name,
      year = as.integer(projected_year),
      age = as.integer(sub("Age ", "", c_age_name, fixed = TRUE)),
      female_population = as.numeric(obs_value)
    ) |>
    filter(
      area_code %in% area_config$area_code,
      age >= minimum_age,
      age <= maximum_age
    ) |>
    mutate(area_name = factor(area_name, levels = area_config$area_name))

  missing_areas <- setdiff(area_config$area_code, unique(result$area_code))
  if (length(missing_areas) > 0) {
    stop(
      "Female population projection snapshot does not contain configured area code(s): ",
      paste(missing_areas, collapse = ", "),
      ". Refresh the Nomis extract after changing the area configuration."
    )
  }

  result |> arrange(area_name, year, age)
}

read_female_population_projections <- function(
    path = file.path(ons_data_dir(), "ons_2022_based_female_population_projections.csv"),
    area_config = read_area_config(),
    parameters = read_analysis_parameters()) {
  minimum_age <- unname(parameters[["minimum_maternal_age"]])
  maximum_age <- unname(parameters[["maximum_maternal_age"]])

  read_female_population_single_age(path, area_config, parameters) |>
    mutate(age_band = configured_age_band(age, parameters)) |>
    group_by(area_code, area_name, year, age_band) |>
    summarise(female_population = sum(female_population), .groups = "drop") |>
    arrange(age_band, area_name, year)
}

read_snpp_births <- function(path = file.path(ons_data_dir(), "ons_2022_based_births_migration_category.csv")) {
  read_csv(path, show_col_types = FALSE) |>
    clean_names() |>
    pivot_longer(
      cols = matches("^x?20[0-9]{2}$"),
      names_to = "year",
      values_to = "projected_births"
    ) |>
    mutate(
      year = as.integer(sub("^x", "", year)),
      projected_births = as.numeric(projected_births)
    )
}

read_registration_table <- function(path, sheet = "Table_1") {
  read_excel(path, sheet = sheet, skip = 5, na = c("", "[x]", "[z]")) |>
    clean_names()
}

read_registration_births <- function(data_dir = ons_data_dir()) {
  ons_source_files(data_dir) |>
    filter(grepl("^registrations_", source_id), available) |>
    transmute(year = as.integer(period), path) |>
    mutate(data = map(path, read_registration_table)) |>
    select(year, data) |>
    unnest(data, names_repair = "unique")
}

read_local_authority_maternities <- function(
    local_authorities,
    data_dir = ons_data_dir()) {
  source_files <- ons_source_files(data_dir) |>
    filter(grepl("^registrations_", source_id), available) |>
    transmute(year = as.integer(period), path)

  result <- map2_dfr(source_files$path, source_files$year, function(path, year) {
    data <- read_registration_table(path, sheet = "Table_3")

    # The 2023 workbook uses `maternities`; later workbooks use
    # `number_of_maternities`. Standardise this source-schema change here.
    if ("maternities" %in% names(data)) {
      data <- rename(data, number_of_maternities = maternities)
    }

    data |>
      filter(area_of_usual_residence_name %in% local_authorities) |>
      transmute(
        year = year,
        area_code = area_of_usual_residence_code,
        local_authority = area_of_usual_residence_name,
        maternities = as.numeric(number_of_maternities)
      )
  })

  expected <- tidyr::expand_grid(
    year = source_files$year,
    local_authority = local_authorities
  )
  missing_rows <- anti_join(expected, result, by = c("year", "local_authority"))
  if (nrow(missing_rows) > 0) {
    stop("Missing requested local-authority maternity values in the ONS source files.")
  }

  result |>
    mutate(local_authority = factor(local_authority, levels = local_authorities)) |>
    arrange(local_authority, year)
}

read_local_authority_birth_rates <- function(
    local_authorities,
    data_dir = ons_data_dir()) {
  source_files <- ons_source_files(data_dir) |>
    filter(grepl("^registrations_", source_id), available) |>
    transmute(year = as.integer(period), path)

  result <- map2_dfr(source_files$path, source_files$year, function(path, source_year) {
    data <- read_registration_table(path, sheet = "Table_1")
    gfr_column <- intersect(
      c("general_fertility_rate_gfr", "general_fertility_rate_gfr_ages_15_to_44"),
      names(data)
    )
    if (length(gfr_column) != 1) {
      stop("Could not identify one general fertility rate column in ", basename(path), ".")
    }

    data |>
      filter(
        area_of_usual_residence_name %in% local_authorities,
        sex == "All births",
        registration_type == "All registration types"
      ) |>
      transmute(
        year = source_year,
        area_code = area_of_usual_residence_code,
        area_name = area_of_usual_residence_name,
        general_fertility_rate = as.numeric(.data[[gfr_column]])
      )
  })

  expected <- tidyr::expand_grid(year = source_files$year, area_name = local_authorities)
  if (nrow(anti_join(expected, result, by = c("year", "area_name"))) > 0) {
    stop("Missing requested local-authority fertility rates in the ONS source files.")
  }

  result |>
    mutate(area_name = factor(area_name, levels = local_authorities)) |>
    arrange(area_name, year)
}

read_national_multiplicity <- function(data_dir = ons_data_dir()) {
  age_levels <- c("<20", "20-24", "25-29", "30-34", "35-39", "40-44", "45+")
  multiplicity_levels <- c("1", "2", "3", "4+")

  source_files <- ons_source_files(data_dir) |>
    filter(grepl("^registrations_", source_id), available) |>
    transmute(year = as.integer(period), path)

  annual <- map2_dfr(source_files$path, source_files$year, function(path, source_year) {
      raw <- read_registration_table(path, sheet = "Table_7")
      if ("maternities" %in% names(raw)) {
        raw <- rename(raw, number_of_maternities = maternities)
      }

      live_columns <- grep("^(number_of_)?live_birth_(male|female)$", names(raw), value = TRUE)
      if (length(live_columns) != 2) stop("Missing live-birth columns in Table 7.")
      raw$live_births <- rowSums(as.data.frame(lapply(raw[live_columns], as.numeric)))

      prepared <- raw |>
        filter(grepl("^Any number", number_of_previous_live_born_children)) |>
        transmute(
          year = .env$source_year,
          age_source = age_of_mother_at_birth_years,
          multiplicity = recode(
            multiplicity,
            "Singleton" = "1",
            "Twins" = "2",
            "Triplets" = "3",
            "Quadruplets and above" = "4+",
            "All maternities" = "All"
          ),
          maternities = as.numeric(number_of_maternities),
          live_births = live_births
        )

      detail <- prepared |>
        filter(age_source %in% c(
          "Under 20", "20 to 24", "25 to 29", "30 to 34",
          "35 to 39", "40 to 44", "45 and over", "45 to 49", "50 and over"
        )) |>
        mutate(age_band = recode(
          age_source,
          "Under 20" = "<20", "20 to 24" = "20-24",
          "25 to 29" = "25-29", "30 to 34" = "30-34",
          "35 to 39" = "35-39", "40 to 44" = "40-44",
          "45 and over" = "45+", "45 to 49" = "45+", "50 and over" = "45+"
        )) |>
        select(year, age_band, multiplicity, maternities, live_births) |>
        group_by(year, age_band, multiplicity) |>
        summarise(maternities = sum(maternities), live_births = sum(live_births), .groups = "drop")

      detail
    })

  annual <- annual |>
    ungroup() |>
    filter(multiplicity %in% c(multiplicity_levels, "All"))

  totals <- annual |>
    filter(multiplicity == "All") |>
    transmute(year, age_band, all_maternities = maternities)

  annual |>
    left_join(totals, by = c("year", "age_band")) |>
    mutate(proportion = maternities / all_maternities) |>
    mutate(
      age_band = factor(age_band, levels = age_levels),
      multiplicity = factor(multiplicity, levels = c(multiplicity_levels, "All"))
    ) |>
    arrange(year, age_band, multiplicity)
}

average_multiplicity_rates <- function(multiplicity_data) {
  multiplicity_data |>
    filter(multiplicity != "All") |>
    group_by(age_band, multiplicity) |>
    summarise(average_proportion = mean(proportion), .groups = "drop")
}

births_to_maternities_modifiers <- function(multiplicity_data) {
  # Recalculate annual shares after any age grouping (including 40+).
  annual <- multiplicity_data |>
    group_by(year, age_band, multiplicity) |>
    summarise(maternities = sum(maternities), .groups = "drop")
  totals <- annual |>
    filter(multiplicity == "All") |>
    select(year, age_band, all_maternities = maternities)
  annual |>
    filter(multiplicity != "All") |>
    left_join(totals, by = c("year", "age_band")) |>
    mutate(proportion = maternities / all_maternities) |>
    group_by(age_band, multiplicity) |>
    summarise(average_proportion = mean(proportion), .groups = "drop") |>
    mutate(babies_per_maternity = recode(
      as.character(multiplicity), "1" = 1, "2" = 2, "3" = 3, "4+" = 4
    )) |>
    group_by(age_band) |>
    summarise(
      expected_babies_per_maternity = sum(average_proportion * babies_per_maternity),
      maternity_modifier = 1 / expected_babies_per_maternity,
      .groups = "drop"
    )
}

pooled_births_to_maternities_modifier <- function(multiplicity_data) {
  multiplicity_data |>
    filter(multiplicity != "All") |>
    mutate(babies_per_maternity = recode(
      as.character(multiplicity), "1" = 1, "2" = 2, "3" = 3, "4+" = 4
    )) |>
    summarise(maternity_modifier = sum(maternities) / sum(maternities * babies_per_maternity)) |>
    pull(maternity_modifier)
}

count_ratio_maternity_modifiers <- function(multiplicity_data) {
  multiplicity_data |>
    filter(multiplicity == "All") |>
    group_by(age_band) |>
    summarise(
      live_births_per_maternity = sum(live_births) / sum(maternities),
      maternity_modifier = sum(maternities) / sum(live_births),
      .groups = "drop"
    )
}

pooled_count_ratio_modifier <- function(multiplicity_data) {
  multiplicity_data |>
    filter(multiplicity == "All") |>
    summarise(modifier = sum(maternities) / sum(live_births)) |>
    pull(modifier)
}

read_historic_live_births <- function(
    path = file.path(ons_data_dir(), "ons_live_births_la_2013_2025.csv"),
    area_config = read_area_config()) {
  age_lookup <- c(
    "Total" = "Total",
    "Mother aged under 20" = "<20",
    "Mother aged 20-24" = "20-24",
    "Mother aged 25-29" = "25-29",
    "Mother aged 30-34" = "30-34",
    "Mother aged 35-39" = "35-39",
    "Mother aged 40-44" = "40-44",
    "Mother aged 45 and over" = "45+",
    "Age of mother unknown or not stated" = "Unknown"
  )

  result <- read_csv(path, show_col_types = FALSE) |>
    clean_names() |>
    transmute(
      year = as.integer(date),
      area_code = geography_code,
      local_authority = geography_name,
      age_band = recode(age_of_mother_name, !!!age_lookup, .default = NA_character_),
      live_births = as.numeric(obs_value)
    ) |>
    filter(area_code %in% area_config$area_code)

  missing_areas <- setdiff(area_config$area_code, unique(result$area_code))
  if (length(missing_areas) > 0) {
    stop(
      "Historic live-birth snapshot does not contain configured area code(s): ",
      paste(missing_areas, collapse = ", "),
      ". Refresh the Nomis extract after changing the area configuration."
    )
  }
  if (anyNA(result$age_band)) {
    stop("Historic live-birth snapshot contains an unmapped maternal age category.")
  }

  totals <- result |>
    filter(age_band == "Total") |>
    select(year, area_code, reported_total = live_births)
  detail_check <- result |>
    filter(age_band != "Total") |>
    group_by(year, area_code) |>
    summarise(detail_total = sum(live_births), .groups = "drop") |>
    left_join(totals, by = c("year", "area_code"))
  if (any(detail_check$detail_total != detail_check$reported_total)) {
    stop("Historic live-birth age bands do not reconcile to the published total.")
  }

  result |>
    mutate(
      local_authority = factor(local_authority, levels = area_config$area_name),
      age_band = factor(
        age_band,
        levels = c("Total", "<20", "20-24", "25-29", "30-34", "35-39", "40-44", "45+", "Unknown")
      )
    ) |>
    arrange(local_authority, year, age_band)
}

estimate_historic_maternities <- function(
    historic_live_births,
    age_specific_modifiers,
    multiplicity_data,
    unknown_age_modifier = pooled_births_to_maternities_modifier(multiplicity_data)) {
  pooled_modifier <- unknown_age_modifier

  adjusted <- historic_live_births |>
    filter(age_band != "Total") |>
    mutate(modifier_age_band = as.character(age_band)) |>
    left_join(
      age_specific_modifiers |>
        transmute(modifier_age_band = as.character(age_band), maternity_modifier),
      by = "modifier_age_band"
    ) |>
    mutate(
      maternity_modifier = if_else(
        modifier_age_band == "Unknown",
        pooled_modifier,
        maternity_modifier
      )
    )

  if (anyNA(adjusted$maternity_modifier)) {
    stop("A births-to-maternities modifier is missing for a historic maternal age band.")
  }

  totals <- historic_live_births |>
    filter(age_band == "Total") |>
    select(year, area_code, local_authority, live_births)

  adjusted |>
    mutate(estimated_maternity_component = live_births * maternity_modifier) |>
    group_by(year, area_code, local_authority) |>
    summarise(estimated_maternities = sum(estimated_maternity_component), .groups = "drop") |>
    left_join(totals, by = c("year", "area_code", "local_authority")) |>
    select(year, area_code, local_authority, live_births, estimated_maternities) |>
    arrange(local_authority, year)
}

project_maternities <- function(
    snpp_births,
    modifiers,
    local_authorities) {
  projected <- snpp_births |>
    filter(area_name %in% local_authorities, age_group != "All ages") |>
    mutate(
      age = as.integer(age_group),
      age_band = case_when(
        age < 20 ~ "<20",
        age <= 24 ~ "20-24",
        age <= 29 ~ "25-29",
        age <= 34 ~ "30-34",
        age <= 39 ~ "35-39",
        age <= 44 ~ "40+",
        TRUE ~ "45+"
      ),
      modifier_age_band = case_when(
        age < 20 ~ "<20",
        age <= 24 ~ "20-24",
        age <= 29 ~ "25-29",
        age <= 34 ~ "30-34",
        age <= 39 ~ "35-39",
        age <= 44 ~ "40+",
        TRUE ~ "45+"
      )
    ) |>
    left_join(
      modifiers |>
        transmute(
          modifier_age_band = as.character(age_band),
          maternity_modifier
        ),
      by = "modifier_age_band"
    )

  if (anyNA(projected$maternity_modifier)) {
    stop("A births-to-maternities modifier is missing for an SNPP maternal age band.")
  }

  projected |>
    mutate(
      projected_maternity_component = projected_births * maternity_modifier
    ) |>
    group_by(area_code, area_name, year, age_band) |>
    summarise(
      projected_births = sum(projected_births),
      projected_maternities = sum(projected_maternity_component),
      .groups = "drop"
    ) |>
    mutate(
      area_name = factor(area_name, levels = local_authorities),
      age_band = factor(
        age_band,
        levels = c("<20", "20-24", "25-29", "30-34", "35-39", "40+")
      )
    ) |>
    arrange(age_band, area_name, year)
}



total_projected_maternities <- function(age_band_projections) {
  age_band_projections |>
    group_by(area_code, area_name, year) |>
    summarise(
      projected_births = sum(projected_births),
      projected_maternities = sum(projected_maternities),
      .groups = "drop"
    ) |>
    arrange(area_name, year)
}

workbook_catalogue <- function(path) {
  sheets <- excel_sheets(path)
  tibble(
    sheet = sheets,
    role = case_when(
      sheet == "Cover_sheet" ~ "Publication information",
      sheet == "Contents" ~ "Workbook contents",
      sheet == "Notes" ~ "Definitions and caveats",
      sheet == "Table_lookup" ~ "Dataset change log",
      grepl("^Table_", sheet) ~ "ONS data table",
      TRUE ~ "Other"
    )
  )
}

build_source_summary <- function(data_dir = ons_data_dir()) {
  sources <- ons_source_files(data_dir)

  sources |>
    mutate(
      records = map2_int(path, source_id, function(file, id) {
        if (!file.exists(file)) return(NA_integer_)
        if (id == "snpp_2022") nrow(read_snpp_births(file))
        else if (id == "snpp_female_population_2022") nrow(read_csv(file, show_col_types = FALSE))
        else if (id == "historic_live_births") nrow(read_csv(file, show_col_types = FALSE))
        else nrow(read_registration_table(file))
      }),
      sheets = map2_int(path, file_name, function(file, name) {
        if (!file.exists(file) || !grepl("\\.xlsx$", name, ignore.case = TRUE)) return(NA_integer_)
        length(excel_sheets(file))
      })
    )
}
