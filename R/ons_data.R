# Reusable preparation functions for the ONS component of BBCS maternity.
# Source files remain unchanged in data/. All transformations happen in memory.

library(dplyr)
library(janitor)
library(purrr)
library(readr)
library(readxl)
library(tidyr)

## 1. Files and report settings ----------------------------------------------
ons_data_dir <- function() "data"

# Check the inputs together before either ONS method starts reading them.
check_ons_input_files <- function(data_dir = ons_data_dir()) {
  paths <- c(ons_source_files(data_dir)$path,
             "config/local_authorities.csv", "config/analysis_parameters.csv")
  missing <- paths[!file.exists(paths)]
  if (length(missing)) stop("Missing ONS report inputs: ",
                            paste(missing, collapse = ", "), call. = FALSE)
  invisible(paths)
}

read_area_config <- function(path = file.path("config", "local_authorities.csv")) {
  config <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = col_character())) |>
    mutate(display_order = as.integer(display_order)) |>
    arrange(display_order)

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
    "snpp_2022", "2022 SNPP Births persons.csv", "ONS 2022-based subnational population projections: births by age of mother", "Projection", "2023–2047", "Existing supplied migration-category variant on 2023 geography; projected rates and estimated maternities.",
    "snpp_female_population_2022", "ons_2022_based_female_population_projections_2023_geography.csv", "ONS 2022-based subnational population projections: female population by single year of age", "Projection", "2023–2047", "Direct ONS migration-category population on matching 2023 geography: denominator for the projected ratio and female-population context.",
    "historic_live_births", "ons_live_births_la_2013_2025.csv", "ONS live births in England and Wales down to local authority area, via Nomis", "Observed", "2013–2025", "Show registered live-birth trends and prepare age-adjusted maternity estimates.",
    "registrations_2023", "2023birthregistrations.xlsx", "ONS Births in England and Wales: birth registrations", "Observed", "2023", "Table 1: published GFR; Table 3: maternities; Table 7: national conversion counts.",
    "registrations_2024", "2024birthregistrations.xlsx", "ONS Births in England and Wales: birth registrations", "Observed", "2024", "Table 1: published GFR; Table 3: maternities; Table 7: national conversion counts.",
    "registrations_2025", "2025birthregistrations.xlsx", "ONS Births in England and Wales: birth registrations", "Observed", "2025", "Table 1: published GFR; Table 3: maternities; Table 7: national conversion counts."
  ) |>
    mutate(path = file.path(data_dir, file_name), available = file.exists(path))
}

## 2. Read projected births and female population ----------------------------
read_female_population_single_age <- function(
    path = file.path(ons_data_dir(), "ons_2022_based_female_population_projections_2023_geography.csv"),
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

  result |> arrange(area_name, year, age)
}

read_female_population_projections <- function(
    path = file.path(ons_data_dir(), "ons_2022_based_female_population_projections_2023_geography.csv"),
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

read_snpp_births <- function(path = file.path(ons_data_dir(), "2022 SNPP Births persons.csv")) {
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

## 3. Read registration counts and published birth rates ----------------------
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

## 4. Calculate the two age-specific maternity modifiers ----------------------
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

## 5. Apply modifiers to historic and projected live births --------------------
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

  if (anyNA(result$age_band)) {
    stop("Historic live-birth snapshot contains an unmapped maternal age category.")
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
  # The report displays source descriptions; record and sheet counts belong in QA.
  ons_source_files(data_dir)
}

## 6. Shared chart and table formatting --------------------------------------
# Colours are passed in from local_authorities.csv, not taken from another plot.
ons_plot_theme <- function(plot_margin = ggplot2::margin(8, 12, 8, 8)) {
  ggplot2::theme_minimal(base_family = "Arial") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(face = "bold"),
      legend.position = "bottom", legend.text = ggplot2::element_text(size = 9),
      plot.caption = ggplot2::element_text(colour = "#5f5b58", hjust = 0),
      plot.margin = plot_margin)
}

ons_table_style <- function(table) {
  gt::tab_options(table, table.font.names = c("Open Sans", "Arial", "sans-serif"))
}

projection_plot <- function(data, caption, colours) {
  ggplot2::ggplot(data, ggplot2::aes(year, projected_maternities,
                                    colour = area_name, group = area_name)) +
    ggplot2::geom_line(linewidth = 1.05) +
    ggplot2::scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 4)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                              expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::labs(x = NULL, y = "Projected maternities", colour = NULL, caption = caption) +
    ons_plot_theme()
}

population_plot <- function(data, caption, colours) {
  ggplot2::ggplot(data, ggplot2::aes(year, female_population,
                                    colour = area_name, group = area_name)) +
    ggplot2::geom_line(linewidth = 1.05) +
    ggplot2::scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 4)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                              expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::labs(x = NULL, y = "Projected female population", colour = NULL, caption = caption) +
    ons_plot_theme()
}

historic_trend_plot <- function(data, value, y_label, caption, colours) {
  ggplot2::ggplot(data, ggplot2::aes(year, {{ value }},
                                    colour = local_authority, group = local_authority)) +
    ggplot2::geom_line(linewidth = 1.05) + ggplot2::geom_point(size = 2.1) +
    ggplot2::scale_x_continuous(breaks = seq(2013, 2025, by = 2)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                              expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::labs(x = NULL, y = y_label, colour = NULL, caption = caption) +
    ons_plot_theme()
}

published_maternities_plot <- function(data, colours) {
  ggplot2::ggplot(data, ggplot2::aes(year, maternities,
                                    colour = local_authority, group = local_authority)) +
    ggplot2::geom_line(linewidth = 1.15) + ggplot2::geom_point(size = 2.8) +
    ggplot2::scale_x_continuous(breaks = 2023:2025) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                              expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::labs(x = NULL, y = "Number of maternities", colour = NULL,
      caption = "Source: ONS Births in England and Wales: birth registrations, Table 3.") +
    ons_plot_theme()
}

maternity_trajectory_plot <- function(observed, projected, caption, colours) {
  ggplot2::ggplot() +
    ggplot2::geom_line(data = observed,
      ggplot2::aes(year, maternities, colour = area_name, group = area_name), linewidth = 1.2) +
    ggplot2::geom_point(data = observed,
      ggplot2::aes(year, maternities, colour = area_name), size = 2.7) +
    ggplot2::geom_line(data = projected,
      ggplot2::aes(year, maternities, colour = area_name, group = area_name),
      linewidth = 1.05, linetype = "22") +
    ggplot2::scale_x_continuous(breaks = c(2023:2026, seq(2030, 2047, by = 4), 2047),
                              expand = ggplot2::expansion(add = c(0.4, 0.4))) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                              expand = ggplot2::expansion(mult = c(0.02, 0.1))) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::labs(x = NULL, y = "Maternities", colour = NULL, caption = caption) +
    ons_plot_theme(ggplot2::margin(12, 14, 10, 10))
}

birth_rate_plot <- function(observed, projected, colours) {
  ggplot2::ggplot() +
    ggplot2::geom_line(data = projected,
      ggplot2::aes(year, general_fertility_rate, colour = area_name, group = area_name),
      linewidth = 1.05, linetype = "22") +
    ggplot2::geom_line(data = observed,
      ggplot2::aes(year, general_fertility_rate, colour = area_name, group = area_name), linewidth = 1.25) +
    ggplot2::geom_point(data = observed,
      ggplot2::aes(year, general_fertility_rate, colour = area_name), size = 2.7) +
    ggplot2::scale_x_continuous(breaks = c(2023:2027, seq(2031, 2047, by = 4))) +
    ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 1),
                              expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::labs(x = NULL, y = "Live births per 1,000 women aged 15-44", colour = NULL,
      caption = paste("Solid: observed ONS general fertility rate, 2023-2025.",
        "Dashed: calculated mid-year projection ratio, 2023-2047; periods differ from observed rates.")) +
    ons_plot_theme()
}

## 7. Tables called by the two ONS method sections ----------------------------
ons_year_table <- function(data, value, source_note) {
  data |>
    select(local_authority, year, {{ value }}) |>
    pivot_wider(names_from = year, values_from = {{ value }}) |>
    gt::gt(rowname_col = "local_authority") |>
    gt::tab_stubhead(label = "Local authority") |>
    gt::fmt_integer(columns = everything(), sep_mark = ",") |>
    gt::cols_align(align = "right", columns = everything()) |>
    gt::tab_source_note(source_note = source_note) |>
    ons_table_style()
}

multiplicity_table <- function(data, year = NULL) {
  if (is.null(year)) {
    display <- data |>
      transmute(measure = "Average proportion", multiplicity = as.character(multiplicity),
                age_band, value = average_proportion)
  } else {
    selected <- data |> filter(.data$year == .env$year)
    display <- bind_rows(
      selected |> transmute(measure = "Maternities", multiplicity = as.character(multiplicity),
                            age_band, value = maternities),
      selected |> filter(multiplicity != "All") |>
        transmute(measure = "Proportion", multiplicity = as.character(multiplicity),
                  age_band, value = proportion))
  }
  display |>
    mutate(measure = factor(measure, levels = c("Maternities", "Proportion", "Average proportion")),
           multiplicity = factor(multiplicity, levels = c("1", "2", "3", "4+", "All"))) |>
    arrange(measure, multiplicity) |>
    pivot_wider(names_from = age_band, values_from = value) |>
    gt::gt(groupname_col = "measure", rowname_col = "multiplicity") |>
    gt::tab_stubhead(label = "Babies per maternity") |>
    gt::fmt_integer(columns = where(is.numeric), rows = measure == "Maternities", sep_mark = ",") |>
    gt::fmt_percent(columns = where(is.numeric),
                    rows = measure %in% c("Proportion", "Average proportion"), decimals = 2) |>
    gt::sub_missing(columns = everything(), missing_text = "n.a.") |>
    ons_table_style()
}

maternity_modifier_table <- function(maternity_modifiers, projection_modifiers,
                                     method = c("multiplicity", "count_ratio")) {
  method <- match.arg(method)
  data <- bind_rows(maternity_modifiers |> mutate(age_band = as.character(age_band)),
                    projection_modifiers |> filter(age_band == "40+"))
  if (method == "multiplicity") {
    display <- data |> transmute(`Age band` = age_band,
      `Expected babies per maternity` = expected_babies_per_maternity,
      `Multiplicity modifier` = maternity_modifier)
    note <- paste("Mean 2023-2025 age-specific multiplicity proportions.",
      "Modifier = 1 / (p1 + 2*p2 + 3*p3 + 4*p4+); four-plus is treated as four.")
  } else {
    display <- data |> transmute(`Age band` = age_band,
      `Live births per maternity` = live_births_per_maternity, `Pooled count ratio` = maternity_modifier)
    note <- "Pooled 2023-2025 Table 7 counts: modifier = maternities / live births within each age band."
  }
  display |> gt::gt() |>
    gt::fmt_number(columns = where(is.numeric), decimals = 5) |>
    gt::tab_source_note(source_note = gt::md(note)) |>
    ons_table_style()
}

female_population_change_table <- function(data) {
  years <- as.character(range(data$year))
  data |>
    filter(year %in% range(year)) |>
    pivot_wider(names_from = year, values_from = female_population) |>
    mutate(change = .data[[years[2]]] - .data[[years[1]]],
           percentage_change = change / .data[[years[1]]]) |>
    select(area_name, age_band, all_of(years), change, percentage_change) |>
    gt::gt(groupname_col = "area_name") |>
    gt::cols_label(age_band = "Age band", change = "Change", percentage_change = "% change") |>
    gt::fmt_integer(columns = c(all_of(years), change), sep_mark = ",") |>
    gt::fmt_percent(columns = percentage_change, decimals = 1) |>
    gt::tab_source_note(source_note = paste("Source: ONS 2022-based migration-category",
      "population projections, 2023 geography; Strategy Unit aggregation.")) |>
    ons_table_style()
}

ons_source_table <- function(sources) {
  sources |>
    transmute(Source = source_name, Type = source_type, Period = period, File = file_name,
              `How it is used` = current_use, Status = if_else(available, "Available", "Missing")) |>
    gt::gt() |>
    gt::sub_missing(columns = everything(), missing_text = "n.a.") |>
    ons_table_style()
}
