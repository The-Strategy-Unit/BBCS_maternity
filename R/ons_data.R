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

# Check the inputs together before preparing the ONS report.
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
    "snpp_2022", "2022 SNPP Births persons.csv", "ONS 2022-based subnational population projections: births by age of mother", "Projection", "2023\u20132047", "Existing supplied migration-category variant on 2023 geography; projected rates and estimated maternities.",
    "snpp_female_population_2022", "ons_2022_based_female_population_projections_2023_geography.csv", "ONS 2022-based subnational population projections: female population by single year of age", "Projection", "2023\u20132047", "Direct ONS migration-category population on matching 2023 geography: denominator for the projected ratio and female-population context.",
    "historic_live_births", "ons_live_births_la_2013_2025.csv", "ONS live births in England and Wales down to local authority area, via Nomis", "Observed", "2013\u20132025", "Show registered births (L) directly; no conversion of historical age-band counts.",
    "registrations_2023", "2023birthregistrations.xlsx", "ONS Births in England and Wales: birth registrations", "Observed", "2023", "Table 1: published GFR; Table 3: maternities; Table 4: England Private-NHS age-band modifier; Table 5: individual-age conversion counts; Table 7: multiplicity context.",
    "registrations_2024", "2024birthregistrations.xlsx", "ONS Births in England and Wales: birth registrations", "Observed", "2024", "Table 1: published GFR; Table 3: maternities; Table 4: England Private-NHS age-band modifier; Table 5: individual-age conversion counts; Table 7: multiplicity context.",
    "registrations_2025", "2025birthregistrations.xlsx", "ONS Births in England and Wales: birth registrations", "Observed", "2025", "Table 1: published GFR; Table 3: maternities; Table 4: England Private-NHS age-band modifier; Table 5: individual-age conversion counts; Table 7: multiplicity context."
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
  age_levels <- c("Under 20", "20-24", "25-29", "30-34", "35-39", "40-44", "45+")
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
          "Under 20" = "Under 20", "20 to 24" = "20-24",
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

# National registration counts at the finest published maternal-age resolution.
# Table 5 contains overlapping father/registration breakdowns: select totals once.
read_national_age_counts <- function(data_dir = ons_data_dir()) {
  files <- ons_source_files(data_dir) |>
    filter(grepl("^registrations_", source_id), available)
  result <- map2_dfr(files$path, as.integer(files$period), function(path, source_year) {
    raw <- read_registration_table(path, "Table_5")
    maternity_column <- intersect(c("maternities", "number_of_maternities"), names(raw))
    live_columns <- grep("^(number_of_)?live_births_(male|female)$", names(raw), value = TRUE)
    still_column <- intersect(c("stillbirths", "number_of_stillbirths"), names(raw))
    if (length(maternity_column) != 1 || length(live_columns) != 2 || length(still_column) != 1) {
      stop("Missing or ambiguous Table 5 birth/maternity columns in ", basename(path))
    }
    raw |>
      filter(registration_type == "All registration types", age_of_father_at_birth_years == "All ages",
             age_of_mother_at_birth_years != "All ages") |>
      transmute(year = source_year,
        age = recode(age_of_mother_at_birth_years, "Under 14" = "<14",
                     "50 and over" = "50+", "Not stated" = "Unknown"),
        maternities = as.numeric(.data[[maternity_column]]),
        live_births = as.numeric(.data[[live_columns[1]]]) + as.numeric(.data[[live_columns[2]]]),
        stillbirths = as.numeric(.data[[still_column]]))
  })
  result |> mutate(total_births = live_births + stillbirths)
}

# Separate age-specific factors for the projection master and method tables.
# MODEL_INPUT: ONS_STILLBIRTH_RATIO and ONS_MULTIPLICITY
prepare_projection_age_factors <- function(counts = read_national_age_counts(), ages = NULL) {
  # Annual ratios are retained for the variation charts only.
  annual <- counts[counts$age != "Unknown", ]
  annual$Stillbirth_ratio <- annual$stillbirths / annual$live_births
  annual$Stillbirth_factor <- 1 + annual$Stillbirth_ratio
  annual$Multiplicity_factor <- annual$maternities / annual$total_births
  factors <- annual |>
    dplyr::group_by(age) |>
    dplyr::summarise(Pooled_live_births = sum(live_births),
      Pooled_stillbirths = sum(stillbirths), Pooled_maternities = sum(maternities),
      .groups = "drop")
  # Pool the three years at each published age, then calculate each factor once.
  factors$Pooled_total_births <- factors$Pooled_live_births + factors$Pooled_stillbirths
  factors$Stillbirth_ratio <- factors$Pooled_stillbirths / factors$Pooled_live_births
  factors$Stillbirth_factor <- 1 + factors$Stillbirth_ratio
  factors$Multiplicity_factor <- factors$Pooled_maternities / factors$Pooled_total_births
  factors$National_conversion_factor <- factors$Stillbirth_factor * factors$Multiplicity_factor
  parameters <- read_analysis_parameters()
  if (is.null(ages)) ages <- seq(parameters[["minimum_maternal_age"]], parameters[["maximum_maternal_age"]])
  modifications <- data.frame(Age = sort(unique(ages)))
  modifications$Registration_age <- ifelse(modifications$Age < 14, "<14",
    ifelse(modifications$Age >= 50, "50+", as.character(modifications$Age)))
  modifications$ONS_source_age <- pmin(44L, pmax(15L, modifications$Age))
  modifications <- dplyr::left_join(modifications, factors, by = c("Registration_age" = "age"))
  modifications$Aggregation_level <- "National: England and Wales"
  modifications$Time_period <- paste(range(annual$year), collapse = "--")
  modifications$Calculation_basis <- "Pool counts across 2023--2025, then divide"
  modifications$Age_application <- ifelse(modifications$Registration_age %in% c("<14", "50+"),
    "Same published group factor for each individual age", "Own individual-age factor")
  modifications <- modifications[c("Aggregation_level", "Time_period", "ONS_source_age",
    "Registration_age", "Age", "Age_application", "Pooled_live_births", "Pooled_stillbirths",
    "Pooled_total_births", "Pooled_maternities", "Stillbirth_ratio", "Stillbirth_factor",
    "Multiplicity_factor", "National_conversion_factor", "Calculation_basis")]
  # Explicit fallback rows for a combined ONS age with no local age split.
  grouped <- counts |> filter(age != "Unknown") |>
    mutate(numeric_age = suppressWarnings(as.integer(age)),
      ONS_age = case_when(age == "<14" | numeric_age <= 15 ~ 15L,
        age == "50+" | numeric_age >= 44 ~ 44L, TRUE ~ NA_integer_)) |>
    filter(!is.na(ONS_age)) |> group_by(ONS_age) |>
    summarise(L = sum(live_births), S = sum(stillbirths), M = sum(maternities), .groups = "drop")
  fallback <- data.frame(Aggregation_level = "National: England and Wales",
    Time_period = paste(range(annual$year), collapse = "--"),
    ONS_source_age = grouped$ONS_age,
    Registration_age = c("<=15 combined", "44+ combined"), Age = NA_integer_,
    Age_application = "Unsplit fallback only: no local MSDS age distribution",
    Pooled_live_births = grouped$L, Pooled_stillbirths = grouped$S,
    Pooled_total_births = grouped$L + grouped$S, Pooled_maternities = grouped$M,
    Stillbirth_ratio = grouped$S / grouped$L,
    Stillbirth_factor = 1 + grouped$S / grouped$L,
    Multiplicity_factor = grouped$M / (grouped$L + grouped$S),
    National_conversion_factor = (1 + grouped$S / grouped$L) * grouped$M / (grouped$L + grouped$S),
    Calculation_basis = "Pool counts across 2023--2025, then divide")
  modifications <- dplyr::bind_rows(modifications, fallback)
  modifications$Source <- "ONS birth registrations, Table 5"
  list(annual = annual, factors = factors, modifications = modifications)
}

# Table 4: England-resident maternities by delivery setting and age band.
# Select all previous-child counts once. Preserve source totals and derive the
# unknown-age residual explicitly; never allocate it to known maternal ages.
read_private_nhs_counts <- function(data_dir = ons_data_dir()) {
  files <- ons_source_files(data_dir) |> filter(grepl("^registrations_", source_id), available)
  bands <- c("Under 20", "20-24", "25-29", "30-34", "35-39", "40-44", "45+")
  result <- map2_dfr(files$path, as.integer(files$period), function(path, source_year) {
    raw <- read_registration_table(path, "Table_4")
    column <- intersect(c("maternities", "number_of_maternities"), names(raw))
    if (length(column) != 1L) stop("Ambiguous Table 4 maternity count in ", basename(path))
    selected <- raw |>
      filter(country == "England", number_of_previous_live_born_children %in%
        c("Any number", "Any number of previous live-born children")) |>
      transmute(Age_source = age_of_mother_at_birth_years, Place = place_of_birth,
        Count = as.numeric(.data[[column]]))
    selected <- selected |>
      mutate(Private_NHS_age_band = case_when(Age_source %in% c("45 and over", "45 to 49", "50 and over") ~ "45+",
        TRUE ~ gsub(" to ", "-", Age_source))) |>
      group_by(Private_NHS_age_band, Place) |> summarise(Count = sum(Count), .groups = "drop")
    unknown <- selected |> group_by(Place) |>
      summarise(Count = Count[Private_NHS_age_band == "All ages"] -
        sum(Count[Private_NHS_age_band != "All ages"]), .groups = "drop") |>
      mutate(Private_NHS_age_band = "Unknown (derived)")
    bind_rows(selected, unknown) |>
      tidyr::pivot_wider(names_from = Place, values_from = Count) |>
      transmute(Year = source_year, Private_NHS_age_band,
        Total_maternities = `Any place of birth`, NHS_maternities = `NHS establishments`,
        Non_NHS_maternities = `Non-NHS establishments`, Home_maternities = `At home`,
        Elsewhere_maternities = Elsewhere,
        Unallocated_place = Total_maternities - NHS_maternities - Non_NHS_maternities - Home_maternities - Elsewhere_maternities)
  })
  result
}

prepare_private_nhs_factors <- function(counts = read_private_nhs_counts(), ages = 13:54) {
  bands <- c("Under 20", "20-24", "25-29", "30-34", "35-39", "40-44", "45+")
  annual <- counts |> mutate(Private_NHS_rate = Non_NHS_maternities / Total_maternities,
    Private_NHS_factor = 1 - Private_NHS_rate)
  pooled <- counts |> group_by(Private_NHS_age_band) |>
    summarise(across(c(Total_maternities, NHS_maternities, Non_NHS_maternities,
      Home_maternities, Elsewhere_maternities, Unallocated_place), sum), .groups = "drop") |>
    mutate(Private_NHS_rate = Non_NHS_maternities / Total_maternities,
      Private_NHS_factor = 1 - Private_NHS_rate,
      Period = "2023-2025", Population = "England-resident mothers")
  pooled <- pooled[match(c(bands, "Unknown (derived)", "All ages"), pooled$Private_NHS_age_band), ]
  modifications <- data.frame(Age = ages, Private_NHS_age_band = as.character(cut(ages,
    breaks = c(-Inf, 19, 24, 29, 34, 39, 44, Inf), labels = bands))) |>
    left_join(pooled |> select(Private_NHS_age_band, Private_NHS_rate, Private_NHS_factor), by = "Private_NHS_age_band")
  list(annual = annual, pooled = pooled, modifications = modifications)
}

private_nhs_variation_chart <- function(evidence) {
  bands <- evidence$pooled$Private_NHS_age_band[1:7]
  annual <- evidence$annual |> filter(Private_NHS_age_band %in% bands) |>
    transmute(Series = as.character(Year), Private_NHS_age_band, Private_NHS_rate)
  rows <- bind_rows(annual, evidence$pooled |> filter(Private_NHS_age_band %in% bands) |>
    transmute(Series = "Pooled 2023-2025", Private_NHS_age_band, Private_NHS_rate))
  colours <- c("2023" = "#005EB8", "2024" = "#D55E00", "2025" = "#00856A", "Pooled 2023-2025" = "#222222")
  chart <- plotly::plot_ly(height = 440)
  for (series in names(colours)) {
    data <- rows[rows$Series == series, ]
    data <- data[match(bands, data$Private_NHS_age_band), ]
    chart <- plotly::add_trace(chart, type = "scatter", mode = "lines+markers",
      x = data$Private_NHS_age_band, y = data$Private_NHS_rate, name = series,
      line = list(color = colours[[series]]), marker = list(color = colours[[series]], symbol = "circle"),
      text = paste0(series, "<br>", data$Private_NHS_age_band, "<br>Non-NHS share: ",
        scales::percent(data$Private_NHS_rate, accuracy = 0.001)), hovertemplate = "%{text}<extra></extra>")
  }
  chart |> plotly::layout(xaxis = list(title = "Maternal age band", categoryorder = "array", categoryarray = bands),
    yaxis = list(title = "Non-NHS share of all maternities", tickformat = ".1%", rangemode = "tozero"),
    legend = list(orientation = "h", y = -0.22), margin = list(l = 75, r = 20, t = 20, b = 95)) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE) |> dc_widget()
}

private_nhs_modifier_table <- function(pooled) {
  pooled |> select(Private_NHS_age_band, Total_maternities, Non_NHS_maternities,
    Private_NHS_rate, Private_NHS_factor) |> gt::gt() |>
    gt::cols_label(Private_NHS_age_band = "Maternal age", Total_maternities = "All maternities",
      Non_NHS_maternities = "Non-NHS maternities", Private_NHS_rate = "Non-NHS share",
      Private_NHS_factor = "Private\u2013NHS modifier") |>
    gt::fmt_number(columns = c(Total_maternities, Non_NHS_maternities), decimals = 0) |>
    gt::fmt_percent(columns = Private_NHS_rate, decimals = 3) |>
    gt::fmt_number(columns = Private_NHS_factor, decimals = 6) |>
    gt::tab_source_note("ONS Table 4, England residents, pooled 2023\u20132025. The modifier is 1 minus the non-NHS share. The 2025 groups 45\u201349 and 50+ are combined to match 45+ in 2023\u20132024. Unknown age is the all-age total minus published ages; unknown and all-age rows are context only, not rates assigned to known model ages.") |>
    gt::tab_options(container.width = "100%", container.overflow.x = TRUE)
}

# 4. Table 7 multiplicity proportions: descriptive context only ----------
pooled_multiplicity_rates <- function(multiplicity_data) {
  multiplicity_data |>
    filter(multiplicity != "All") |>
    group_by(age_band, multiplicity) |>
    summarise(maternities = sum(maternities), all_maternities = sum(all_maternities),
      .groups = "drop") |>
    mutate(pooled_proportion = maternities / all_maternities)
}

# 5. Historical births (L): source counts only ---------------------------
read_historic_live_births <- function(
    path = file.path(ons_data_dir(), "ons_live_births_la_2013_2025.csv"),
    area_config = read_area_config()) {
  age_lookup <- c(
    "Total" = "Total",
    "Mother aged under 20" = "Under 20",
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


  result |>
    mutate(
      local_authority = factor(local_authority, levels = area_config$area_name),
      age_band = factor(
        age_band,
        levels = c("Total", "Under 20", "20-24", "25-29", "30-34", "35-39", "40-44", "45+", "Unknown")
      )
    ) |>
    arrange(local_authority, year, age_band)
}

# 6. Resident projection summaries: completed master values only ---------
# No second conversion; Age (B) and LA totals only add finished Age (SY) rows.
summarise_resident_projections <- function(master, area_config = read_area_config()) {
  exact <- master |>
    filter(LA %in% area_config$area_code) |>
    transmute(area_code = LA,
      area_name = factor(LA_name, levels = area_config$area_name),
      year = Year, age = Age, ONS_age, age_band = report_age_band(Age),
      projected_births = Exact_age_live_births, projected_maternities = Adjusted_maternities)
  exact$age_band[is.na(exact$age)] <- "Unsplit ages"
  bands <- exact |> group_by(area_code, area_name, year, age_band) |>
    summarise(projected_births = sum(projected_births),
      projected_maternities = sum(projected_maternities), .groups = "drop") |>
    mutate(age_band = factor(age_band, levels = c(report_age_bands, "Unsplit ages"))) |>
    arrange(age_band, area_name, year)
  totals <- exact |> group_by(area_code, area_name, year) |>
    summarise(projected_births = sum(projected_births),
      projected_maternities = sum(projected_maternities), .groups = "drop") |>
    arrange(area_name, year)
  list(single_age_maternity_projections = exact, age_band_projections = bands,
    total_maternity_projections = totals,
    projected_trajectory = totals |> transmute(area_name, year, maternities = projected_maternities))
}

build_source_summary <- function(data_dir = ons_data_dir()) {
  # The report displays source descriptions; record and sheet counts belong in QA.
  ons_source_files(data_dir)
}

## 6. Shared chart and table formatting --------------------------------------
# Colours are passed in from local_authorities.csv, not taken from another plot.
ons_plot_theme <- function(plot_margin = ggplot2::margin(8, 12, 8, 8)) {
  ggplot2::theme_minimal(base_family = "Arial", base_size = 11) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(face = "bold"),
      legend.position = "bottom", legend.text = ggplot2::element_text(size = 11),
      legend.key.width = grid::unit(2.4, "cm"),
      plot.caption = ggplot2::element_text(colour = "#5f5b58", hjust = 0),
      plot.margin = plot_margin)
}

ons_line_guides <- function() {
  ggplot2::guides(
    colour = ggplot2::guide_legend(order = 1,
      override.aes = list(linewidth = 2.5, linetype = "solid", shape = NA)),
    linetype = ggplot2::guide_legend(title = NULL, order = 2,
      override.aes = list(colour = "black", linewidth = 1.4, shape = NA)))
}

ons_table_style <- function(table) {
  gt::tab_options(table, table.font.names = c("Open Sans", "Arial", "sans-serif"))
}

projection_plot <- function(data, caption, colours) {
  data <- dc_counts(data, "projected_maternities")
  ggplot2::ggplot(data, ggplot2::aes(year, projected_maternities,
                                    colour = area_name, group = area_name)) +
    ggplot2::geom_line(linewidth = 1.05) +
    ggplot2::scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 4)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                              expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::labs(title = "Adjusted resident maternity projections", x = "Year",
      y = "Adjusted resident maternities", colour = NULL, caption = caption) +
    ons_line_guides() +
    ons_plot_theme()
}

# Resident geography totals, before allocation to booking/delivery providers.
# Complete each LA/ICB/cluster aggregation from original values, then disclose.
resident_projection_totals_table <- function(data) {
  resident_icb <- c(E08000025 = "Birmingham and Solihull ICB",
    E08000027 = "Black Country ICB", E08000028 = "Black Country ICB",
    E08000029 = "Birmingham and Solihull ICB", E08000030 = "Black Country ICB",
    E08000031 = "Black Country ICB")
  areas <- read_area_config() |> filter(area_code %in% names(resident_icb))
  stopifnot(nrow(areas) == 6L, !anyDuplicated(areas$area_code))
  years <- c(2025, 2030, 2035, 2040, 2045, 2047)
  shown <- tidyr::expand_grid(area_code = areas$area_code, year = years) |>
    left_join(data |> select(area_code, year, projected_maternities),
      by = c("area_code", "year"), relationship = "one-to-one") |>
    left_join(areas |> select(area_code, area_name), by = "area_code", relationship = "many-to-one") |>
    mutate(Resident_ICB = unname(resident_icb[area_code]),
      Value = projected_maternities)
  # A missing LA/year makes its subtotal unavailable, rather than a partial sum.
  icb_rows <- shown |> group_by(Resident_ICB, year) |>
    summarise(Value = sum(Value), .groups = "drop") |>
    transmute(Residence = Resident_ICB, year, Value)
  cluster_label <- "BBCS cluster (all six resident LAs)"
  cluster_rows <- shown |> group_by(year) |>
    summarise(Value = sum(Value), .groups = "drop") |>
    mutate(Residence = cluster_label)
  row_order <- c(areas$area_name, "Black Country ICB", "Birmingham and Solihull ICB", cluster_label)
  totals <- bind_rows(shown |> transmute(Residence = area_name, year, Value), icb_rows, cluster_rows) |>
    dc_counts("Value") |>
    mutate(Residence = factor(Residence, levels = row_order)) |>
    arrange(Residence, year) |>
    tidyr::pivot_wider(names_from = year, values_from = Value) |>
    select(Residence, all_of(as.character(years)))
  totals |> gt::gt(rowname_col = "Residence") |>
    gt::tab_header(title = "Adjusted resident maternity projections") |>
    gt::tab_stubhead(label = "Resident area") |>
    gt::fmt_number(columns = all_of(as.character(years)), decimals = 0, use_seps = TRUE) |>
    gt::sub_missing(missing_text = "n.a.") |>
    gt::tab_style(style = list(gt::cell_text(weight = "bold"), gt::cell_fill(color = "#F5F4F3")),
      locations = list(gt::cells_stub(rows = 7:9), gt::cells_body(rows = 7:9))) |>
    gt::tab_style(style = gt::cell_fill(color = "#FFF3CC"),
      locations = list(gt::cells_stub(rows = 9), gt::cells_body(rows = 9))) |>
    gt::cols_width(Residence ~ gt::px(300), everything() ~ gt::px(84)) |>
    gt::tab_source_note("LA, ICB and cluster totals are calculated from original figures, then disclosure-controlled separately. Displayed totals may therefore differ from the sum of displayed LA figures. Unavailable estimates are shown as n.a.") |>
    gt::tab_options(table.font.names = c("Arial", "sans-serif"), table.font.size = gt::px(44 / 3),
      heading.title.font.size = gt::px(56 / 3), heading.align = "left",
      table.width = gt::px(300 + 84 * length(years)), table.layout = "fixed",
      source_notes.font.size = gt::px(44 / 3),
      container.width = "100%", container.overflow.x = TRUE)
}

# Selected-year provider totals for the MSDS/modelled chart versions.
# Use the existing provider aggregates, not the resident-LA subtotals.
provider_projection_totals_table <- function(data, measure = c("deliveries", "antenatal bookings")) {
  measure <- match.arg(measure)
  years <- c(2025, 2030, 2035, 2040, 2045, 2047)
  aggregate_keys <- c("icb-qua", "icb-qhl", "cluster")
  organisations <- data |> distinct(Key, Level, Organisation) |>
    filter(Level == "Trust" | Key %in% aggregate_keys)
  stopifnot(all(aggregate_keys %in% organisations$Key), !anyDuplicated(organisations$Key))
  row_order <- c(organisations$Key[organisations$Level == "Trust"], aggregate_keys)
  organisations <- organisations |> arrange(match(Key, row_order)) |>
    mutate(Organisation = if_else(Level == "Cluster", "BBCS cluster", as.character(Organisation)))
  modelled <- data |> filter(Series == "Modelled projection", Year %in% years) |>
    select(Key, Year, Value, Coverage_note) |> mutate(Value = dc_count(Value))
  shown <- tidyr::expand_grid(Key = organisations$Key, Year = years) |>
    left_join(modelled, by = c("Key", "Year"), relationship = "one-to-one")
  totals <- shown |> select(Key, Year, Value) |>
    tidyr::pivot_wider(names_from = Year, values_from = Value) |>
    left_join(organisations, by = "Key", relationship = "one-to-one") |>
    arrange(match(Key, row_order)) |> select(Organisation, all_of(as.character(years)))
  aggregate_rows <- which(organisations$Level != "Trust")
  cluster_row <- which(organisations$Level == "Cluster")
  table <- totals |> gt::gt(rowname_col = "Organisation") |>
    gt::tab_header(title = paste("Projected", measure, "by provider")) |>
    gt::tab_stubhead(label = "Provider organisation") |>
    gt::fmt_number(columns = all_of(as.character(years)), decimals = 0, use_seps = TRUE) |>
    gt::sub_missing(missing_text = "n.a.") |>
    gt::tab_style(style = list(gt::cell_text(weight = "bold"), gt::cell_fill(color = "#F5F4F3")),
      locations = list(gt::cells_stub(rows = aggregate_rows), gt::cells_body(rows = aggregate_rows))) |>
    gt::tab_style(style = gt::cell_fill(color = "#FFF3CC"),
      locations = list(gt::cells_stub(rows = cluster_row), gt::cells_body(rows = cluster_row))) |>
    gt::cols_width(Organisation ~ gt::px(350), everything() ~ gt::px(84)) |>
    gt::tab_source_note("All columns show modelled projections. Provider totals are disclosure-rounded independently, so aggregates may differ slightly from the sum of displayed trust figures. Unavailable estimates are shown as n.a.") |>
    gt::tab_options(table.font.names = c("Arial", "sans-serif"), table.font.size = gt::px(44 / 3),
      heading.title.font.size = gt::px(56 / 3), heading.align = "left",
      table.width = gt::px(350 + 84 * length(years)), table.layout = "fixed",
      source_notes.font.size = gt::px(44 / 3), footnotes.font.size = gt::px(44 / 3),
      container.width = "100%", container.overflow.x = TRUE)
  for (year in years) {
    partial_keys <- shown$Key[shown$Year == year & grepl("^Partial", shown$Coverage_note)]
    partial_rows <- which(organisations$Key %in% partial_keys)
    if (length(partial_rows)) table <- gt::tab_footnote(table,
      footnote = "Partial projection: includes available allocations only; see provider coverage in section 3.3.",
      locations = gt::cells_body(columns = all_of(as.character(year)), rows = partial_rows))
  }
  table
}

population_age_grid <- function(data) {
  bands <- unique(as.character(data$age_band))
  bands <- bands[order(as.numeric(sub("-.*", "", bands)))]
  colours <- age_series_colours(bands)
  areas <- unique(as.character(data$area_name))
  key <- htmltools::div(class = "demo-ribbon-key population-age-key",
    lapply(bands, function(band) htmltools::span(class = "demo-ribbon-key-item",
      htmltools::span(style = paste0("display:inline-block;width:30px;height:5px;background:", colours[[band]], ";")),
      band)))
  charts <- lapply(areas, function(area) {
    rows <- data[data$area_name == area, ]
    chart <- plotly::plot_ly(height = 245)
    for (band in bands) {
      series <- rows[as.character(rows$age_band) == band, ]
      series <- series[order(series$year), ]
      chart <- plotly::add_trace(chart, x = series$year, y = series$female_population,
        type = "scatter", mode = "lines", name = band,
        line = list(color = colours[[band]], width = 2.5),
        hovertemplate = paste0("Age ", band, "<br>%{x}: %{y:,.0f} women<extra></extra>"))
    }
    chart <- plotly::layout(chart, showlegend = FALSE,
      meta = list(compact_population_grid = TRUE), font = list(family = "Arial", size = 13),
      margin = list(l = 56, r = 12, t = 10, b = 42),
      xaxis = list(title = "", tickvals = sort(unique(c(min(rows$year), 2035, max(rows$year))))),
      yaxis = list(title = "Women", tickformat = ",.0f", rangemode = "tozero"),
      paper_bgcolor = "white", plot_bgcolor = "white")
    htmltools::div(class = "demo-sankey-panel",
      htmltools::h4(area), dc_widget(plotly::config(chart, displayModeBar = FALSE, responsive = TRUE)))
  })
  htmltools::tagList(key, htmltools::div(class = "population-age-grid", charts))
}

projection_age_grid <- function(data, colours) {
  data <- dc_counts(data, "projected_maternities")
  bands <- c("Under 20", "20-24", "25-29", "30-34", "35-39", "40-44", "45+")
  areas <- unique(as.character(data$area_name))
  charts <- lapply(bands, function(band) {
    rows <- data[as.character(data$age_band) == band, ]
    chart <- plotly::plot_ly(height = 400)
    for (area in areas) {
      series <- rows[rows$area_name == area, ]
      series <- series[order(series$year), ]
      chart <- plotly::add_trace(chart, x = series$year, y = series$projected_maternities,
        type = "scatter", mode = "lines", name = area,
        line = list(color = colours[[area]], width = 2.5),
        hovertemplate = paste0(area, "<br>%{x}: %{y:,.0f} maternities<extra></extra>"))
    }
    chart <- plotly::layout(chart, showlegend = TRUE,
      title = list(text = paste0("Adjusted resident maternity projections<br>Age ", band),
        x = 0.02, xanchor = "left", font = list(size = 14)),
      legend = list(orientation = "h", x = 0, xanchor = "left", y = -0.25,
        yanchor = "top", itemwidth = 50, font = list(size = 12)),
      meta = list(compact_population_grid = TRUE, native_line_legend = TRUE),
      font = list(family = "Arial", size = 13),
      margin = list(l = 56, r = 20, t = 72, b = 140),
      xaxis = list(title = "Year", tickvals = sort(unique(c(min(rows$year), 2035, max(rows$year))))),
      yaxis = list(title = "Adjusted resident maternities", tickformat = ",.0f", rangemode = "tozero"),
      paper_bgcolor = "white", plot_bgcolor = "white")
    htmltools::div(class = "demo-sankey-panel",
      dc_widget(plotly::config(chart, displayModeBar = FALSE, responsive = TRUE)))
  })
  htmltools::tagList(
    htmltools::div(class = "population-age-grid", charts),
    htmltools::p(class = "chart-grid-note",
      "ONS 2022-based projected live births on 2023 geography, with local splits of the youngest and oldest ONS age groups, then stillbirth, multiplicity, Private\u2013NHS and maternity pathway adjustments. Estimates are calculated before allocation to providers; Under 20 covers ages 13\u201319 and 45+ covers ages 45\u201354."))
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
    ons_line_guides() +
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
    ons_line_guides() +
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
    ons_line_guides() +
    ons_plot_theme()
}

maternity_trajectory_plot <- function(observed, projected, caption, colours) {
  projected <- dc_counts(projected, "maternities")
  ggplot2::ggplot() +
    ggplot2::geom_line(data = observed,
      ggplot2::aes(year, maternities, colour = area_name, group = area_name,
        linetype = "ONS recorded"), linewidth = 1.2) +
    ggplot2::geom_point(data = observed,
      ggplot2::aes(year, maternities, colour = area_name), size = 2.7, show.legend = FALSE) +
    ggplot2::geom_line(data = projected,
      ggplot2::aes(year, maternities, colour = area_name, group = area_name,
        linetype = "ONS projected"), linewidth = 1.05) +
    ggplot2::scale_linetype_manual(values = c("ONS recorded" = "solid", "ONS projected" = "44"),
      breaks = c("ONS recorded", "ONS projected")) +
    ggplot2::scale_x_continuous(breaks = c(2023:2026, seq(2030, 2047, by = 4), 2047),
                              expand = ggplot2::expansion(add = c(0.4, 0.4))) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                              expand = ggplot2::expansion(mult = c(0.02, 0.1))) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::labs(x = NULL, y = "Maternities", colour = NULL, caption = caption) +
    ons_line_guides() +
    ons_plot_theme(ggplot2::margin(12, 14, 10, 10))
}

birth_rate_plot <- function(observed, projected, colours) {
  ggplot2::ggplot() +
    ggplot2::geom_line(data = projected,
      ggplot2::aes(year, general_fertility_rate, colour = area_name, group = area_name,
        linetype = "ONS projected"), linewidth = 1.05) +
    ggplot2::geom_line(data = observed,
      ggplot2::aes(year, general_fertility_rate, colour = area_name, group = area_name,
        linetype = "ONS recorded"), linewidth = 1.25) +
    ggplot2::geom_point(data = observed,
      ggplot2::aes(year, general_fertility_rate, colour = area_name), size = 2.7, show.legend = FALSE) +
    ggplot2::scale_linetype_manual(values = c("ONS recorded" = "solid", "ONS projected" = "44"),
      breaks = c("ONS recorded", "ONS projected")) +
    ggplot2::scale_x_continuous(breaks = c(2023:2027, seq(2031, 2047, by = 4))) +
    ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 1),
                              expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::labs(x = NULL, y = "Live births per 1,000 women aged 15-44", colour = NULL,
      caption = paste("Solid: observed ONS general fertility rate, 2023-2025.",
        "Dashed: projected births per 1,000 women, 2023-2047.")) +
    ons_line_guides() +
    ons_plot_theme()
}

## 7. Tables called by the ONS exploration section ----------------------------
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
      transmute(measure = "Pooled proportion", multiplicity = as.character(multiplicity),
                age_band, value = pooled_proportion)
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
    mutate(measure = factor(measure, levels = c("Maternities", "Proportion", "Pooled proportion")),
           multiplicity = factor(multiplicity, levels = c("1", "2", "3", "4+", "All"))) |>
    arrange(measure, multiplicity) |>
    pivot_wider(names_from = age_band, values_from = value) |>
    gt::gt(groupname_col = "measure", rowname_col = "multiplicity") |>
    gt::tab_stubhead(label = "Babies per maternity") |>
    gt::fmt_integer(columns = where(is.numeric), rows = measure == "Maternities", sep_mark = ",") |>
    gt::fmt_percent(columns = where(is.numeric),
                    rows = measure %in% c("Proportion", "Pooled proportion"), decimals = 2) |>
    gt::sub_missing(columns = everything(), missing_text = "n.a.") |>
    ons_table_style()
}

# Show the exact factors that the master joins, including explicit fallback rows.
maternity_modifier_table <- function(modifications) {
  modifications |>
    select(`ONS source age` = ONS_source_age, `Registration age` = Registration_age,
      `Applied Age (SY)` = Age, `Age treatment` = Age_application,
      `Stillbirth factor` = Stillbirth_factor, `Multiplicity factor` = Multiplicity_factor,
      `National factor product` = National_conversion_factor) |>
    gt::gt() |> gt::fmt_number(columns = c("Stillbirth factor", "Multiplicity factor", "National factor product"), decimals = 5) |>
    gt::tab_source_note("ONS Table 5, pooled 2023-2025 counts at each published age. These two factors convert live births to ONS-defined maternities. The model also applies the local age split, Private\u2013NHS modifier and maternity pathway adjustment.") |>
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
