# Age-boundary evidence for the common older-age growth assumption.
# Chart summaries only; none of these comparisons feeds the projection.

review_age_band <- function(age) {
  x <- suppressWarnings(as.numeric(as.character(age)))
  dplyr::case_when(x >= 20 & x <= 24 ~ "20-24", x <= 29 & x >= 25 ~ "25-29",
    x >= 30 & x <= 34 ~ "30-34", x >= 35 & x <= 39 ~ "35-39",
    x >= 40 & x <= 43 ~ "40-43", x == 44 ~ "44", x >= 45 & x <= 49 ~ "45-49",
    x >= 50 | as.character(age) == "50+" ~ "50+", TRUE ~ NA_character_)
}

review_index <- function(data, keys, value, reference) {
  ref <- data |> dplyr::filter(year == reference) |>
    dplyr::select(dplyr::all_of(keys), base = dplyr::all_of(value))
  if (anyDuplicated(ref[keys])) stop("Repeated reference cell in age review.")
  data |> dplyr::left_join(ref, by = keys, relationship = "many-to-one") |>
    dplyr::mutate(index = ifelse(is.finite(base) & base > 0, .data[[value]] / base * 100, NA_real_))
}

review_line <- function(data, value, title, y, caption, facet = NULL, index = FALSE) {
  p <- ggplot2::ggplot(data, ggplot2::aes(year, .data[[value]], colour = series, group = series)) +
    ggplot2::geom_point(size = 1.6, na.rm = TRUE) +
    ggplot2::scale_x_continuous(breaks = if (length(unique(data$year)) <= 5) sort(unique(data$year)) else pretty(range(data$year), n = 5)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::scale_colour_viridis_d(end = 0.85) +
    ggplot2::labs(title = title, x = "Year", y = y, colour = NULL, caption = caption) +
    ggplot2::theme_minimal(base_family = "sans", base_size = 11) +
    ggplot2::theme(legend.position = "bottom", legend.text = ggplot2::element_text(size = 10),
      plot.title = ggplot2::element_text(size = 13, face = "bold"),
      plot.caption = ggplot2::element_text(hjust = 0, size = 9), panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2, byrow = TRUE))
  if (length(unique(data$year)) > 1) p <- p + ggplot2::geom_line(linewidth = 0.85, na.rm = TRUE)
  if (index) p <- p + ggplot2::geom_hline(yintercept = 100, colour = "grey55", linetype = 2)
  if (!is.null(facet)) p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet]]), ncol = 3,
    scales = if (index) "fixed" else "free_y")
  if (identical(facet, "age")) p <- p + ggplot2::theme(legend.position = "none")
  p
}

build_ons_age_review <- function(counts, historic, snpp, women, area_config) {
  # Table 8 and Table 10 contain longer published histories in the latest workbook.
  path <- file.path(ons_data_dir(), "2025birthregistrations.xlsx")
  band_names <- c("40 to 44" = "40-44", "45 and over" = "45+")
  history <- read_registration_table(path, "Table_8") |>
    dplyr::filter(year >= 2018, age_of_mother_at_birth_years %in% names(band_names)) |>
    dplyr::transmute(year, series = unname(band_names[age_of_mother_at_birth_years]),
      maternities = number_of_maternities,
      multiple_rate = number_of_maternities_with_multiple_births / number_of_maternities * 1000)
  if (anyDuplicated(history[c("year", "series")])) stop("Overlapping Table 8 age groups in review.")
  old <- counts |> dplyr::filter(age %in% c(as.character(40:49), "50+")) |>
    dplyr::mutate(series = age, babies_per_maternity = total_births / maternities)
  old <- review_index(old, "series", "live_births", 2023)
  summary <- counts |> dplyr::mutate(series = review_age_band(age)) |>
    dplyr::filter(age %in% c(as.character(40:49), "50+")) |> dplyr::group_by(year, series) |>
    dplyr::summarise(maternities = sum(maternities), live_births = sum(live_births),
      stillbirths = sum(stillbirths), total_births = sum(total_births), .groups = "drop") |>
    dplyr::mutate(stillbirth_rate = stillbirths / total_births * 1000)
  rates <- read_registration_table(path, "Table_10") |>
    dplyr::filter(parent == "Mother", country == "England, Wales and Elsewhere", year >= 2013,
      age_group_years %in% c(names(band_names), "40 and over")) |>
    dplyr::transmute(year, series = dplyr::recode(age_group_years, !!!band_names, "40 and over" = "40+"),
      rate = as.numeric(age_specific_fertility_rate))
  if (anyDuplicated(rates[c("year", "series")])) stop("Repeated Table 10 rate cell.")
  population <- women |> dplyr::filter(age >= 40, age <= 54) |>
    dplyr::mutate(series = ifelse(age >= 50, "50-54", review_age_band(age))) |>
    dplyr::group_by(year, series) |>
    dplyr::summarise(women = sum(female_population), .groups = "drop")
  population <- review_index(population, "series", "women", 2025)
  population_exact <- women |> dplyr::filter(age >= 40, age <= 54, year %in% c(2025, 2035, 2047)) |>
    dplyr::group_by(year, age) |> dplyr::summarise(women = sum(female_population), .groups = "drop")
  population_profile <- ggplot2::ggplot(population_exact, ggplot2::aes(age, women, colour = factor(year), group = year)) +
    ggplot2::geom_line() + ggplot2::geom_point() + ggplot2::scale_x_continuous(breaks = 40:54) +
    ggplot2::labs(title = "Women by exact age: what do the groups conceal?", x = "Exact age", y = "Resident women",
      colour = "Projection year", caption = "ONS female population projections, six selected LAs combined.\nAge profiles for three projection years, not the same cohort followed through time.") +
    ggplot2::theme_minimal(base_family = "sans") + ggplot2::theme(legend.position = "bottom")
  births <- snpp |> dplyr::filter(area_code %in% area_config$area_code, age_group != "All ages") |>
    dplyr::mutate(age = as.integer(age_group), series = dplyr::case_when(
      age >= 40 & age <= 43 ~ as.character(age), age == 44 ~ "44+")) |>
    dplyr::filter(!is.na(series)) |> dplyr::group_by(area_name, year, series) |>
    dplyr::summarise(births = sum(projected_births), .groups = "drop")
  births <- review_index(births, c("area_name", "series"), "births", 2025)
  local <- historic |> dplyr::filter(age_band %in% c("40-44", "45+")) |>
    dplyr::transmute(year, area_name = as.character(local_authority), series = as.character(age_band), births = live_births)
  local <- review_index(local, c("area_name", "series"), "births", 2013)
  pooled <- old |> dplyr::filter(age %in% c(as.character(44:49), "50+")) |> dplyr::group_by(age) |>
    dplyr::summarise(maternities = sum(maternities), live_births = sum(live_births),
      stillbirths = sum(stillbirths), total_births = sum(total_births), .groups = "drop") |>
    dplyr::mutate(live_birth_share = live_births / sum(live_births), stillbirth_rate = stillbirths / total_births * 1000)
  plots <- list(
    women_exact = population_profile,
    mothers = review_line(history, "maternities", "Registered maternities: age bands in context", "Maternities",
      "ONS Table 8, 2018-2025. England and Wales registrations, including non-residents.\nMaternities are pregnancies reaching birth, not a count of unique women."),
    older_counts = review_line(old, "live_births", "Older mothers: live births at each available age", "Live-born babies",
      "ONS Table 5, 2023-2025. Panels have different vertical scales; 50+ is one combined source category.", "age"),
    older_maternities = review_line(old, "maternities", "Maternities at each available older age", "Maternities",
      "ONS Table 5. Panels have different vertical scales. Maternities are not unique women.", "age"),
    older_change = review_line(old, "index", "Do the older ages move together?", "Live births: 2023 = 100",
      "Each series is indexed separately. Check counts before interpreting large percentage changes.", "age", TRUE),
    stillbirths = review_line(summary, "stillbirths", "Stillbirth counts: compare the size of the evidence", "Stillborn babies",
      "ONS Table 5, 2023-2025. Counts are babies, not maternities; 50+ is combined."),
    stillbirth_rates = review_line(summary, "stillbirth_rate", "Stillbirth rates by maternal age", "Stillbirths per 1,000 total births",
      "Rate = stillbirths / (live births + stillbirths) x 1,000. Rates from very small counts fluctuate sharply."),
    multiplicity = review_line(history, "multiple_rate", "Multiple maternities: the longer age-band trend", "Multiple maternities per 1,000 maternities",
      "ONS Table 8, 2018-2025. Counts every multiple pregnancy once; 45+ remains combined."),
    babies_per_maternity = review_line(old, "babies_per_maternity", "Annual babies per maternity at older ages", "All babies per maternity",
      "ONS Table 5: births (L+S) / maternities in each year.\nAnnual diagnostic only; national model factors use pooled 2023-2025 counts.", "age"),
    women = review_line(population, "women", "Women living in the six selected LAs: projected counts", "Women",
      "ONS 2022-based migration-category population projections, 2023 geography.\n44 is one year of age; other groups span several years. These are women, not mothers."),
    women_change = review_line(population, "index", "Does the older female population grow at the same rate?", "Women: 2025 = 100",
      "Six selected LAs combined; ages 50-54 exclude women aged 55+. Population change is not itself birth growth.", index = TRUE),
    fertility = review_line(rates |> dplyr::filter(!series %in% c("40-44", "45+")), "rate",
      "Published fertility rates: the longer age pattern", "Live births per 1,000 women",
      "ONS Table 10, 2013-2025, national published rates. The open-ended 40+ rate is not an exact-age-44 rate."),
    fertility_latest = review_line(rates |> dplyr::filter(series %in% c("40-44", "45+")), "rate",
      "Additional older-age fertility rates available for 2025", "Live births per 1,000 women",
      "ONS Table 10: these categories have only one year in this source.\nThe 45+ numerator includes older births; its denominator is women aged 45-49."),
    local_counts = review_line(local |> dplyr::filter(series %in% c("40-44", "45+")), "births",
      "Observed local births at older ages", "Registered live births",
      "ONS/Nomis 2013-2025, usual-residence LA. Published 40-44 and 45+ bands; not the ONS projection's 44+ group.", "area_name"),
    local_change = review_line(local, "index", "Local historical change within ages 40+", "Live births: 2013 = 100",
      "ONS/Nomis. Indices show proportional change; the counts above show how many older births underpin it.", "area_name", TRUE),
    projected_counts = review_line(births, "births", "What ONS actually projects at the older boundary", "Projected live births",
      "ONS 2022-based migration-category variant, 2023 geography. 44+ is one source row; no separate 45-54 projections.", "area_name"),
    projected_change = review_line(births, "index", "ONS change underlying the allocated counts", "Projected births: 2025 = 100",
      "Main model: split 44+ births (L) using local MSDS age shares, then apply age-specific conversion factors.\nFixed shares and factors imply common proportional change.", "area_name", TRUE)
  )
  extra <- build_older_outcome_review(path, pooled, old)
  list(plots = c(plots, extra$plots), older = old, bands = summary, pooled = pooled, population = population,
    births = births, local = local, history = history, rates = rates, outcomes = extra)
}

build_older_outcome_review <- function(path, pooled, old) {
  composition <- pooled |> dplyr::select(age, live_births, maternities, stillbirths) |>
    tidyr::pivot_longer(-age, names_to = "measure", values_to = "count") |>
    dplyr::group_by(measure) |> dplyr::mutate(share = count / sum(count)) |> dplyr::ungroup()
  composition_plot <- ggplot2::ggplot(composition, ggplot2::aes(measure, share, fill = age)) +
    ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::coord_flip() + ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::scale_x_discrete(labels = c(live_births = "Live births", maternities = "Maternities", stillbirths = "Stillbirths")) +
    ggplot2::scale_fill_viridis_d(end = 0.85) +
    ggplot2::labs(title = "Who makes up 44+, and who contributes its outcomes?", x = NULL,
      y = "Share of the 44+ total", fill = "Maternal age",
      caption = "ONS Table 5, pooled 2023-2025. Each bar has its own denominator.\nDisproportionate outcome shares do not establish causation.") +
    ggplot2::theme_minimal(base_family = "sans") + ggplot2::theme(legend.position = "bottom")
  bands <- c("40 to 44" = "40-44", "45 to 49" = "45-49", "50 and over" = "50+")
  raw <- read_registration_table(path, "Table_7") |>
    dplyr::filter(number_of_previous_live_born_children == "Any number",
      age_of_mother_at_birth_years %in% names(bands))
  mc <- intersect(c("maternities", "number_of_maternities"), names(raw))
  multiple <- raw |>
    dplyr::transmute(age_band = unname(bands[age_of_mother_at_birth_years]), multiplicity,
      count = as.numeric(.data[[mc]])) |>
    tidyr::pivot_wider(names_from = multiplicity, values_from = count) |>
    dplyr::transmute(age_band, denominator = `All maternities`, Twins,
      `All multiple maternities` = Twins + Triplets + `Quadruplets and above`) |>
    tidyr::pivot_longer(c(Twins, `All multiple maternities`), names_to = "measure", values_to = "count") |>
    dplyr::mutate(rate = count / denominator * 1000)
  home <- read_registration_table(path, "Table_4") |>
    dplyr::filter(country == "England, Wales and Elsewhere", number_of_previous_live_born_children == "Any number",
      age_of_mother_at_birth_years %in% names(bands), place_of_birth %in% c("Any place of birth", "At home")) |>
    dplyr::transmute(age_band = unname(bands[age_of_mother_at_birth_years]), place_of_birth,
      count = as.numeric(number_of_maternities)) |>
    tidyr::pivot_wider(names_from = place_of_birth, values_from = count) |>
    dplyr::transmute(age_band, denominator = `Any place of birth`, count = `At home`,
      rate = count / denominator * 1000, measure = "Home maternities")
  outcome_comparison <- dplyr::bind_rows(multiple, home) |>
    dplyr::group_by(measure) |>
    dplyr::mutate(reference_rate = rate[age_band == "40-44"],
      rate_ratio = ifelse(reference_rate > 0, rate / reference_rate, NA_real_)) |>
    dplyr::ungroup()
  # Separate the stillbirth uplift from the conversion of babies to pregnancies.
  conversion <- old |> dplyr::mutate(uplift = total_births / live_births,
    babies_modifier = maternities / total_births,
    complete_modifier = maternities / live_births,
    recovered_without_uplift = live_births * babies_modifier / maternities * 100,
    complete_recovery = live_births * uplift * babies_modifier / maternities * 100)
  latest_conversion <- conversion |> dplyr::filter(year == 2025) |>
    dplyr::select(age, babies_modifier, complete_modifier) |>
    tidyr::pivot_longer(-age, names_to = "measure", values_to = "modifier") |>
    dplyr::mutate(measure = dplyr::recode(measure,
      babies_modifier = "Maternities / all babies", complete_modifier = "Maternities / live births"))
  conversion_plot <- ggplot2::ggplot(latest_conversion, ggplot2::aes(age, modifier, colour = measure, group = measure)) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::labs(title = "Conversion to maternities: with and without the stillbirth allowance", x = "Maternal age",
      y = "Maternities per baby", colour = NULL,
      caption = "ONS Table 5, 2025: annual diagnostic. National model factors use pooled 2023-2025 counts.\nThe chart compares conversions, not future clinical risk.") +
    ggplot2::theme_minimal(base_family = "sans") + ggplot2::theme(legend.position = "bottom")
  uplift_plot <- review_line(conversion |> dplyr::mutate(extra_percent = (uplift - 1) * 100), "extra_percent",
    "Stillbirth allowance: extra total babies per 100 live births", "Additional babies (%)",
    "ONS Table 5: annual diagnostic; model ratios use pooled 2023-2025 counts.\nAdditional babies (%) = stillbirths / live births x 100. The stillbirth factor is not applied to MSDS recorded births.", "age")
  outcome_chart <- function(d, title, caption) {
    p <- ggplot2::ggplot(d, ggplot2::aes(age_band, rate, colour = measure, group = measure)) +
      ggplot2::geom_line() + ggplot2::geom_point(size = 2) +
      ggplot2::geom_text(ggplot2::aes(label = paste0("n=", count)), vjust = -0.8, size = 3, show.legend = FALSE) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.25))) +
      ggplot2::labs(title = title, x = "Published maternal age group", y = "Per 1,000 maternities",
        colour = NULL, caption = caption) + ggplot2::theme_minimal(base_family = "sans") +
      ggplot2::theme(legend.position = "none")
    if (length(unique(d$measure)) > 1) p <- p + ggplot2::facet_wrap(ggplot2::vars(measure), nrow = 1)
    p
  }
  pooled_exact <- old |> dplyr::group_by(age) |>
    dplyr::summarise(stillbirths = sum(stillbirths), total_births = sum(total_births), .groups = "drop") |>
    dplyr::mutate(rate = stillbirths / total_births * 1000,
      # Wilson interval describes count uncertainty; it is not a test of age differences.
      p = stillbirths / total_births, z = qnorm(0.975), den = 1 + z^2 / total_births,
      centre = (p + z^2 / (2 * total_births)) / den,
      half = z * sqrt(p * (1-p) / total_births + z^2 / (4 * total_births^2)) / den,
      lower = pmax(0, centre - half) * 1000, upper = pmin(1, centre + half) * 1000)
  stillbirth_plot <- ggplot2::ggplot(pooled_exact, ggplot2::aes(age, rate)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0.15, colour = "#005EB8") +
    ggplot2::geom_point(colour = "#005EB8", size = 2) +
    ggplot2::geom_text(ggplot2::aes(label = paste0("n=", stillbirths)), vjust = -0.9, hjust = -0.05, size = 3) +
    ggplot2::labs(title = "Stillbirth rates from age 40: could a small group be hidden?", x = "Maternal age (50+ combined)",
      y = "Stillbirths per 1,000 total births", caption = "ONS Table 5, pooled 2023-2025. Labels: stillborn babies. Whiskers: descriptive 95% Wilson intervals.\nIntervals assume independent birth outcomes; babies from one multiple pregnancy may be correlated. Not a significance test.") +
    ggplot2::theme_minimal(base_family = "sans")
  list(composition = composition, multiple = multiple, home = home, stillbirths = pooled_exact,
    comparison = outcome_comparison, conversion = conversion,
    plots = list(composition = composition_plot, exact_stillbirth_rates = stillbirth_plot,
      conversion = conversion_plot, stillbirth_uplift = uplift_plot,
      twins = outcome_chart(multiple, "Twins and all multiple maternities, 2025",
        "ONS Table 7. Labels show outcome counts. Twins are included in all multiples; do not add the two series.\nPublished age bands cannot reveal differences within 50+."),
      home = outcome_chart(home, "Home maternities, 2025",
        "ONS Table 4. Labels show home-maternity counts; denominator is all maternities in the same age band.\nHome birth does not imply absence of NHS care.")))
}

build_msds_age_review <- function(cohort, projections, historical, trust_labels, cluster_trusts) {
  years <- c("2023/24", "2024/25", "2025/26")
  orgs <- projections$organisations |> dplyr::filter(Level == "Trust", Code %in% cluster_trusts) |>
    dplyr::select(Key, Trust = Code, Organisation)
  # Match section 3's hospital-location population, including unmatched LA rows.
  eligible <- prepare_report_cohort(cohort) |>
    dplyr::filter(Year %in% years, Birth_recorded, Home == 0, Trust %in% cluster_trusts)
  observed <- eligible |> dplyr::count(Trust, Year, Age, name = "Observed") |>
    dplyr::left_join(orgs, by = "Trust")
  exact <- tidyr::expand_grid(Key = orgs$Key, Year = years, Age = 10:60) |>
    dplyr::left_join(observed |> dplyr::select(Key, Year, Age, Observed), by = c("Key", "Year", "Age")) |>
    dplyr::mutate(Observed = tidyr::replace_na(Observed, 0), series = review_age_band(Age),
      year = as.integer(substr(Year, 1, 4))) |>
    dplyr::left_join(orgs |> dplyr::select(Key, Organisation), by = "Key")
  bands <- exact |> dplyr::filter(Age >= 40, !is.na(series)) |>
    dplyr::group_by(Key, Organisation, year, series) |>
    dplyr::summarise(Observed = sum(Observed), .groups = "drop")
  share <- exact |> dplyr::group_by(Key, Organisation, year) |>
    dplyr::summarise(Known_age = sum(Observed), Older = sum(Observed[Age >= 44]), .groups = "drop") |>
    dplyr::mutate(Share = ifelse(Known_age > 0, Older / Known_age * 100, NA_real_))
  # Reuse the retrospective model with full-cohort proportions.
  # The observed comparison years are included in calibration; not held-out validation.
  model <- historical$single_age_projections |> dplyr::filter(Key %in% orgs$Key, !is.na(Age)) |>
    dplyr::transmute(Key, Year, Age, Modelled = Projected, In_model = TRUE)
  exact_model <- exact |>
    dplyr::left_join(model, by = c("Key", "year" = "Year", "Age")) |>
    dplyr::mutate(Modelled = dplyr::if_else(is.na(In_model), 0, Modelled))
  gaps <- historical$provider_projection_gaps |>
    dplyr::filter(Allocation == "Delivery", Level == "Trust")
  incomplete <- paste(exact_model$Key, exact_model$Age) %in% paste(gaps$Key, gaps$Age) |
    exact_model$Key %in% gaps$Key[is.na(gaps$Age)]
  exact_model$Modelled[incomplete] <- NA_real_
  comparison <- exact_model |>
    dplyr::filter(Age >= 40) |>
    dplyr::group_by(Key, Organisation, year, series) |>
    dplyr::summarise(Observed = sum(Observed), Modelled = sum(Modelled), .groups = "drop")
  coverage <- eligible |> dplyr::group_by(Year) |>
    dplyr::summarise(Deliveries = dplyr::n(), Missing_age = sum(is.na(Age)),
      Age_44_plus = sum(Age >= 44, na.rm = TRUE), .groups = "drop")
  # Aggregate charts from exact-age cells only after the model comparison is complete.
  counts <- bands |> dplyr::group_by(year, series) |>
    dplyr::summarise(Observed = sum(Observed), .groups = "drop")
  counts <- review_index(counts, "series", "Observed", 2023)
  old <- exact |> dplyr::filter(Age >= 40) |> dplyr::group_by(Age, year) |>
    dplyr::summarise(Observed = sum(Observed), .groups = "drop")
  profile <- ggplot2::ggplot(old, ggplot2::aes(Age, Observed, colour = factor(year), group = year)) +
    ggplot2::geom_line() + ggplot2::geom_point() + ggplot2::scale_x_continuous(breaks = 40:60) +
    ggplot2::labs(title = "Actual older-age starting counts: cluster trusts combined", x = "Exact age at birth",
      y = "Recorded non-home deliveries", colour = "Year",
      caption = "MSDS: birth flag = 1, known age 10-60, cluster delivery provider, home flag = 0. No extra deduplication.") +
    ggplot2::theme_minimal(base_family = "sans") + ggplot2::theme(legend.position = "bottom")
  combined <- comparison |> dplyr::group_by(year, series) |>
    dplyr::summarise(Observed = sum(Observed), Modelled = sum(Modelled), .groups = "drop")
  long <- combined |> tidyr::pivot_longer(c(Observed, Modelled), names_to = "measure", values_to = "value") |>
    dplyr::rename(age_group = series) |> dplyr::mutate(series = measure)
  by_trust <- comparison |> dplyr::filter(series != "40-43") |>
    dplyr::group_by(Organisation, year) |>
    dplyr::summarise(Observed = sum(Observed), Modelled = sum(Modelled), .groups = "drop") |>
    tidyr::pivot_longer(c(Observed, Modelled), names_to = "series", values_to = "value")
  last_year <- exact_model |> dplyr::filter(year == 2025, Age >= 40) |>
    dplyr::group_by(Age) |> dplyr::summarise(Observed = sum(Observed), Modelled = sum(Modelled), .groups = "drop") |>
    tidyr::pivot_longer(c(Observed, Modelled), names_to = "series", values_to = "value")
  exact_check <- ggplot2::ggplot(last_year, ggplot2::aes(Age, value, colour = series, group = series)) +
    ggplot2::geom_line() + ggplot2::geom_point() + ggplot2::scale_x_continuous(breaks = 40:60) +
    ggplot2::labs(title = "2025/26 check at each exact age: where do differences sit?", x = "Exact maternal age",
      y = "Recorded / modelled non-home deliveries", colour = NULL,
      caption = "Cluster trusts combined. ONS counts use all four modifiers and full-cohort allocation proportions.\nIncomplete provider/age estimates remain unavailable; unmatched records are not treated as zero demand.") +
    ggplot2::theme_minimal(base_family = "sans") + ggplot2::theme(legend.position = "bottom")
  age_mix <- counts |> dplyr::filter(series != "40-43") |> dplyr::group_by(year) |>
    dplyr::mutate(denominator = sum(Observed), share = ifelse(denominator > 0, Observed / denominator * 100, NA_real_)) |>
    dplyr::ungroup()
  readout <- combined |> dplyr::group_by(series) |>
    dplyr::summarise(start = Observed[year == 2023], finish = Observed[year == 2025],
      model_start = Modelled[year == 2023], model_finish = Modelled[year == 2025], .groups = "drop") |>
    dplyr::mutate(observed_change = ifelse(start > 0, finish/start-1, NA_real_),
      model_change = ifelse(model_start > 0, model_finish/model_start-1, NA_real_),
      difference = model_finish - finish,
      percent_difference = ifelse(finish > 0, difference/finish, NA_real_))
  plots <- list(profile = profile,
    exact_backtest = exact_check,
    age_mix = review_line(age_mix, "share", "Is the observed mix within 44+ stable?", "% of known-age 44+ deliveries",
      "MSDS cluster trusts combined. Denominator = recorded deliveries aged 44-60 in that financial year.\nChanging LA/provider mix can alter these shares even with common growth within each LA."),
    counts = review_line(counts, "Observed", "MSDS deliveries: the actual age-band trends", "Recorded non-home deliveries",
      "Cluster trusts combined. 50+ includes valid ages 50-60."),
    change = review_line(counts, "index", "MSDS age trends compared on a common scale", "Deliveries: 2023/24 = 100",
      "A zero reference count has no index. Differences can reflect case mix, provider flows or recording, not just demography.", index = TRUE),
    backtest = review_line(long, "value", "Retrospective check: does common boundary growth track actual activity?", "Deliveries",
      "ONS 2023 to 2025 counts use fixed pooled 2023/24--2025/26 allocation proportions.\nThe first count is not fitted to MSDS. Current inputs make this a retrospective diagnostic.", "age_group"),
    trust_backtest = review_line(by_trust, "value", "The same check for each trust: ages 44+", "Deliveries",
      "Observed versus ONS-count estimates. Unmatched MSDS LA/age records have no projected contribution.", "Organisation"))
  list(version = "age-review-counts-v3", plots = plots, exact = exact, bands = bands, share = share,
    comparison = comparison, exact_comparison = exact_model, coverage = coverage, readout = readout, age_mix = age_mix)
}

show_msds_age_review <- function(review, plot) {
  if (is.null(review) || !identical(review$version, "age-review-counts-v3")) {
    htmltools::div(class = "callout callout-note",
      "MSDS evidence is not available in this render. Run Run analysis.R again in the secure environment. ")
  } else review$plots[[plot]]
}
