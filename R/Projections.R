# Run after the Azure reader and additional_analysis.R, from the project root.
# First draft: actual 2025/26 delivery episodes followed by Method 1 ONS growth.
# All preparation, charts and tables live here; index.qmd displays saved outputs.

build_first_draft_projections <- function(baseline_fy = "2025/26", ons_reference_year = 2026L,
                                          projection_end = NULL, prepare_outputs = TRUE) {
  required_objects <- c("final_cohort_2326", "total_births_by_LA_2326",
                        "trust_labels", "icb_labels", "provider_icb", "cluster_icbs", "cluster_trusts")
  absent <- required_objects[!vapply(required_objects, exists, logical(1), inherits = TRUE)]
  if (length(absent)) stop("Missing objects: ", paste(absent, collapse = ", "),
                           ". Run the analysis runner first.", call. = FALSE)
  source("R/ons_data.R", local = TRUE)

  age_bands <- c("<20", "20-24", "25-29", "30-34", "35-39", "40+")
  age_colours <- c("#005EB8", "#00856A", "#D55E00", "#8E5EA2", "#B07300", "#727272")
  band_age <- function(age) {
    dplyr::case_when(
      is.na(age) | age < 10 | age > 60 ~ "Unknown",
      age < 20 ~ "<20", age < 25 ~ "20-24", age < 30 ~ "25-29",
      age < 35 ~ "30-34", age < 40 ~ "35-39", TRUE ~ "40+")
  }
  clean_code <- function(x) toupper(trimws(as.character(x)))
  relative_change <- function(value, reference) {
    reference <- rep_len(reference, length(value))
    result <- rep(NA_real_, length(value))
    valid <- is.finite(value) & is.finite(reference) & reference > 0
    result[valid] <- value[valid] / reference[valid] - 1
    result
  }
  base_table <- function(data, title, subtitle = NULL, rowname = NULL) {
    gt::gt(data, rowname_col = rowname) |>
      gt::tab_header(title = title, subtitle = subtitle) |>
      gt::sub_missing(missing_text = "n.a.") |>
      gt::tab_options(table.font.names = c("Open Sans", "Arial", "sans-serif"),
                      table.font.size = gt::px(12), container.width = "100%",
                      container.overflow.x = TRUE)
  }
  require_columns <- function(data, required, name) {
    missing <- setdiff(required, names(data))
    if (length(missing)) stop(name, " is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  require_columns(final_cohort_2326,
    c("person_id_mother", "uniq_preg_id", "birth_yr", "birth_flag", "la_of_mother",
      "age_at_birth_mother", "birth_provider", "home_birth"), "final_cohort_2326")
  require_columns(total_births_by_LA_2326,
    c("birth_yr", "la_of_mother", "age_at_birth_mother", "count"), "total_births_by_LA_2326")

  birth_records <- final_cohort_2326 |>
    dplyr::filter(trimws(as.character(birth_yr)) == baseline_fy,
                  as.character(birth_flag) == "1") |>
    dplyr::transmute(Person = trimws(as.character(person_id_mother)),
      Pregnancy = trimws(as.character(uniq_preg_id)), LA = clean_code(la_of_mother),
      Age = as.numeric(age_at_birth_mother), Trust = clean_code(birth_provider),
      Home = as.numeric(as.character(home_birth)))
  missing_ids <- is.na(birth_records$Person) | birth_records$Person == "" |
    is.na(birth_records$Pregnancy) | birth_records$Pregnancy == ""
  if (any(missing_ids)) stop(sum(missing_ids),
    " recorded-delivery rows lack a mother/pregnancy identifier. Resolve before allocating projections.",
    call. = FALSE)
  episodes <- dplyr::distinct(birth_records)
  if (anyDuplicated(episodes[c("Person", "Pregnancy")])) stop(
    "Conflicting ", baseline_fy, " delivery details for the same mother/pregnancy pair. Resolve before projecting.",
    call. = FALSE)
  if (!nrow(episodes)) stop("No recorded deliveries in ", baseline_fy, ".", call. = FALSE)
  if (any(episodes$Trust %in% cluster_trusts & !(episodes$Home %in% c(0, 1)))) stop(
    "Some deliveries associated with cluster providers have a missing/invalid home-birth flag.", call. = FALSE)
  episodes <- episodes |>
    dplyr::mutate(Age_band = band_age(Age))
  # Location classification: a flagged home birth is never a delivery at a trust.
  target_episodes <- episodes |>
    dplyr::filter(Trust %in% cluster_trusts, Home == 0)
  if (!nrow(target_episodes)) stop("No non-home deliveries at cluster trusts in ", baseline_fy, ".", call. = FALSE)
  unusable <- is.na(target_episodes$LA) | target_episodes$LA == "" |
    target_episodes$Age_band == "Unknown"
  usable_targets <- target_episodes[!unusable, , drop = FALSE]
  if (!nrow(usable_targets)) stop("No cluster-trust deliveries have usable LA and maternal age.", call. = FALSE)

  # Fix the organisation list across both the forward projection and retrospective check.
  recorded_trusts <- final_cohort_2326 |>
    dplyr::filter(trimws(as.character(birth_yr)) %in% c("2023/24", "2024/25", "2025/26"),
                  as.character(birth_flag) == "1", as.character(home_birth) == "0") |>
    dplyr::pull(birth_provider) |>
    clean_code()
  trust_codes <- unique(c(names(trust_labels)[names(trust_labels) != "Other"],
                          sort(intersect(recorded_trusts, cluster_trusts))))
  trust_names <- unname(trust_labels[trust_codes])
  trust_names[is.na(trust_names)] <- paste0("Trust ", trust_codes[is.na(trust_names)])
  organisations <- dplyr::bind_rows(
    data.frame(Key = paste0("trust-", tolower(trust_codes)), Level = "Trust",
               Code = trust_codes, Organisation = trust_names),
    data.frame(Key = paste0("icb-", tolower(cluster_icbs)), Level = "ICB",
               Code = cluster_icbs, Organisation = unname(icb_labels[cluster_icbs])),
    data.frame(Key = "cluster", Level = "Cluster", Code = "Cluster", Organisation = "Combined cluster"))

  raw_totals <- total_births_by_LA_2326 |>
    dplyr::filter(trimws(as.character(birth_yr)) == baseline_fy) |>
    dplyr::transmute(LA = clean_code(la_of_mother), Age = as.numeric(age_at_birth_mother),
                     Denominator = as.numeric(count))
  if (!nrow(raw_totals) || any(!is.finite(raw_totals$Denominator) | raw_totals$Denominator < 0 |
      raw_totals$Denominator != floor(raw_totals$Denominator))) stop(
    baseline_fy, " LA totals are missing or count contains invalid pregnancy counts.", call. = FALSE)
  if (anyDuplicated(raw_totals[c("LA", "Age")])) stop(
    "Repeated LA/single-age rows in the ", baseline_fy, " totals. Check their meaning before adding counts.", call. = FALSE)
  denominators <- raw_totals |>
    dplyr::mutate(Age_band = band_age(Age)) |>
    dplyr::group_by(LA, Age_band) |>
    dplyr::summarise(Denominator = sum(Denominator), .groups = "drop")

  ons_births <- read_snpp_births()
  la_names <- ons_births |>
    dplyr::distinct(area_code, area_name) |>
    dplyr::rename(LA = area_code, LA_name = area_name)
  feeder_las <- sort(unique(usable_targets$LA))
  feeder_names <- data.frame(LA = feeder_las) |>
    dplyr::left_join(la_names, by = "LA") |>
    dplyr::mutate(LA_name = dplyr::coalesce(LA_name, LA))

  select_org <- function(data, organisation) {
    if (organisation$Level[[1]] == "Trust") {
      data[data$Trust == organisation$Code[[1]], , drop = FALSE]
    } else if (organisation$Level[[1]] == "ICB") {
      data[unname(provider_icb[data$Trust]) == organisation$Code[[1]], , drop = FALSE]
    } else data
  }
  actual_totals <- dplyr::bind_rows(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, , drop = FALSE]
    data.frame(Key = org$Key, Observed_baseline = nrow(select_org(target_episodes, org)))
  }))
  mapping <- dplyr::bind_rows(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, , drop = FALSE]
    counts <- select_org(usable_targets, org) |>
      dplyr::count(LA, Age_band, name = "Numerator")
    # Retain all age bands for each contributing LA, including zero flows.
    org_las <- unique(counts$LA)
    if (!length(org_las)) return(NULL)
    tidyr::expand_grid(LA = org_las, Age_band = age_bands) |>
      dplyr::left_join(counts, by = c("LA", "Age_band")) |>
      dplyr::mutate(Numerator = dplyr::coalesce(Numerator, 0L)) |>
      dplyr::left_join(denominators, by = c("LA", "Age_band")) |>
      dplyr::mutate(Key = org$Key, Level = org$Level, Organisation = org$Organisation,
        Share = ifelse(Denominator > 0, Numerator / Denominator, NA_real_)) |>
      dplyr::left_join(feeder_names, by = "LA")
  }))
  bad_totals <- mapping |>
    dplyr::filter(Numerator > 0 & (is.na(Denominator) | Denominator <= 0 | Numerator > Denominator))
  if (nrow(bad_totals)) stop(nrow(bad_totals),
    " LA/age/organisation cells have absent, zero or smaller-than-numerator LA totals. Check count and population coverage.",
    call. = FALSE)
  # Each level is an alternative view of the same recorded trust deliveries.
  counts_by_level <- mapping |>
    dplyr::group_by(Level) |>
    dplyr::summarise(N = sum(Numerator), .groups = "drop")
  stopifnot(length(unique(counts_by_level$N)) == 1L,
            all(mapping$Share[!is.na(mapping$Share)] >= 0 & mapping$Share[!is.na(mapping$Share)] <= 1))

  # Accounting includes home births and all destinations outside the target trusts.
  accounting <- denominators |>
    dplyr::filter(LA %in% feeder_las, Age_band %in% age_bands) |>
    dplyr::left_join(mapping |>
      dplyr::filter(Level == "Cluster") |>
      dplyr::select(LA, Age_band, Cluster_trusts = Numerator), by = c("LA", "Age_band")) |>
    dplyr::left_join(episodes |>
      dplyr::filter(LA %in% feeder_las, Home == 1, Age_band %in% age_bands) |>
      dplyr::count(LA, Age_band, name = "Recorded_home_births"), by = c("LA", "Age_band")) |>
    dplyr::mutate(Cluster_trusts = dplyr::coalesce(Cluster_trusts, 0L),
      Recorded_home_births = dplyr::coalesce(Recorded_home_births, 0L),
      Other_or_unrepresented = Denominator - Cluster_trusts - Recorded_home_births) |>
    dplyr::left_join(feeder_names, by = "LA")
  if (any(accounting$Other_or_unrepresented < 0)) stop(
    "Recorded cluster-trust and home births exceed LA pregnancy totals. Check denominator definitions.", call. = FALSE)

  scroll_chart <- function(chart, height = 650) {
    htmltools::div(style = paste0("max-height: ", height,
      "px; overflow-y: auto;"), chart)
  }
  mapping_outputs <- stats::setNames(lapply(organisations$Key, function(key) {
    data <- mapping[mapping$Key == key, , drop = FALSE]
    if (!nrow(data)) return(list(chart = htmltools::div(paste("No recorded deliveries in", baseline_fy)),
      table = base_table(data.frame(Note = paste("No recorded deliveries in", baseline_fy)), "LA allocation")))
    la_order <- data |>
      dplyr::group_by(LA_name) |>
      dplyr::summarise(N = sum(Numerator), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(N), LA_name) |>
      dplyr::pull(LA_name)
    data <- data |>
      dplyr::mutate(Age_band = factor(Age_band, levels = age_bands),
                     LA_name = factor(LA_name, levels = la_order)) |>
      dplyr::arrange(LA_name, Age_band)
    share_matrix <- matrix(data$Share, nrow = length(la_order), byrow = TRUE)
    hover <- matrix(paste0(as.character(data$LA_name), " (", data$LA, ")<br>Age: ", data$Age_band,
      "<br>Deliveries here: ", data$Numerator, "<br>All LA pregnancies: ", data$Denominator,
      "<br>LA allocation: ", ifelse(is.na(data$Share), "n.a.", scales::percent(data$Share, accuracy = 0.1))),
      nrow = length(la_order), byrow = TRUE)
    chart <- plotly::plot_ly(x = age_bands, y = la_order, z = share_matrix,
      text = hover, type = "heatmap", zmin = 0, zmax = 1,
      colorscale = list(c(0, "#F1F6FA"), c(1, "#005EB8")),
      colorbar = list(title = "% of LA age band", tickformat = ".0%"),
      hovertemplate = "%{text}<extra></extra>", height = max(400, length(la_order) * 24 + 100)) |>
      plotly::layout(xaxis = list(title = "Maternal age band", side = "top"),
        yaxis = list(title = "", autorange = "reversed", categoryorder = "array", categoryarray = la_order),
        margin = list(l = 235, r = 130, t = 65, b = 25), font = list(family = "Arial")) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
    table <- data |>
      dplyr::select(LA_name, LA, Age_band, Numerator, Denominator, Share) |>
      base_table(paste(baseline_fy, "LA and age-band allocation")) |>
      gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age_band = "Age band",
        Numerator = "Deliveries here", Denominator = "All LA/age pregnancies", Share = "% sent here") |>
      gt::fmt_number(columns = c("Numerator", "Denominator"), decimals = 0) |>
      gt::fmt_percent(columns = "Share", decimals = 1)
    list(chart = scroll_chart(chart), table = table)
  }), organisations$Key)
  accounting_table <- accounting |>
    dplyr::select(LA_name, LA, Age_band, Denominator, Cluster_trusts,
                  Recorded_home_births, Other_or_unrepresented) |>
    base_table("All destinations remain in the LA denominator") |>
    gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age_band = "Age band",
      Denominator = "All LA pregnancies", Cluster_trusts = "At cluster trusts",
      Recorded_home_births = "Recorded home births",
      Other_or_unrepresented = "Other / unrepresented destinations") |>
    gt::fmt_number(columns = c("Denominator", "Cluster_trusts", "Recorded_home_births",
                               "Other_or_unrepresented"), decimals = 0)
  result <- list(mapping_ready = TRUE, forecast_ready = FALSE, organisations = organisations,
    mapping = mapping, mapping_outputs = mapping_outputs, accounting_table = accounting_table,
    actual_totals = actual_totals, baseline_fy = baseline_fy, ons_reference_year = ons_reference_year)

  # Do not present an incomplete projection as a whole-trust projection.
  uncovered_las <- setdiff(feeder_las, la_names$LA)
  if (any(unusable) || length(uncovered_las)) {
    result$note <- paste(c(
      if (any(unusable)) paste(sum(unusable), "cluster-trust episodes have missing/unusable LA or age; resolve before projecting full totals."),
      if (length(uncovered_las)) paste("ONS projection codes are missing for feeder LAs:",
                                       paste(uncovered_las, collapse = ", "))), collapse = " ")
    return(result)
  }

  # Method 1 only. Combine 40-44 and 45+ before calculating the multiplicity modifier.
  modifiers <- read_national_multiplicity() |>
    dplyr::mutate(age_band = ifelse(as.character(age_band) %in% c("40-44", "45+"),
                                   "40+", as.character(age_band))) |>
    births_to_maternities_modifiers()
  la_projection <- project_maternities(ons_births, modifiers,
    local_authorities = feeder_names$LA_name) |>
    dplyr::transmute(LA = area_code, Age_band = as.character(age_band), Year = year,
                     ONS_maternities = projected_maternities) |>
    dplyr::filter(Year >= ons_reference_year)
  if (!is.null(projection_end)) la_projection <- dplyr::filter(la_projection, Year <= projection_end)
  projection_years <- sort(unique(la_projection$Year))
  if (!length(projection_years) || min(projection_years) != ons_reference_year) stop(
    "ONS maternity series lacks the ", ons_reference_year, " reference year.", call. = FALSE)
  required_grid <- tidyr::expand_grid(LA = feeder_las, Age_band = age_bands, Year = projection_years)
  missing_projection_cells <- dplyr::anti_join(required_grid, la_projection,
                                                by = c("LA", "Age_band", "Year"))
  if (nrow(missing_projection_cells) || any(!is.finite(la_projection$ONS_maternities) |
      la_projection$ONS_maternities < 0)) stop("Incomplete or invalid ONS LA/age/year maternity projections.", call. = FALSE)
  la_projection <- la_projection |>
    dplyr::group_by(LA, Age_band) |>
    dplyr::mutate(ONS_reference = ONS_maternities[Year == ons_reference_year],
      Growth_factor = ifelse(ONS_reference > 0, ONS_maternities / ONS_reference, NA_real_),
      ONS_change = Growth_factor - 1) |>
    dplyr::ungroup()
  active_cells <- mapping |> dplyr::filter(Numerator > 0) |> dplyr::distinct(LA, Age_band)
  invalid_growth <- la_projection |>
    dplyr::semi_join(active_cells, by = c("LA", "Age_band")) |>
    dplyr::filter(!is.finite(Growth_factor))
  if (nrow(invalid_growth)) stop(
    "An occupied LA/age cell has a zero/unavailable ONS reference. Its growth cannot be calculated.", call. = FALSE)

  flow_projection <- mapping |>
    dplyr::left_join(la_projection, by = c("LA", "Age_band"), relationship = "many-to-many") |>
    dplyr::mutate(Projected = ifelse(Numerator == 0, 0, Numerator * Growth_factor),
      Year_label = ifelse(Year == ons_reference_year, baseline_fy, paste0("Mid-", Year)))
  # Equivalent to Share * (the observed LA denominator * ONS age-specific growth).
  # Anchor each LA/age flow to actual MSDS counts, without rescaling the observed year.
  age_totals <- flow_projection |>
    dplyr::group_by(Key, Age_band, Year, Year_label) |>
    dplyr::summarise(Projected = sum(Projected), .groups = "drop")
  total_projection <- tidyr::expand_grid(Key = organisations$Key, Year = projection_years) |>
    dplyr::left_join(age_totals |>
      dplyr::group_by(Key, Year) |>
      dplyr::summarise(Projected = sum(Projected), .groups = "drop"), by = c("Key", "Year")) |>
    dplyr::mutate(Projected = dplyr::coalesce(Projected, 0),
      Year_label = ifelse(Year == ons_reference_year, baseline_fy, paste0("Mid-", Year))) |>
    dplyr::left_join(actual_totals, by = "Key") |>
    dplyr::mutate(Change = Projected - Observed_baseline,
      Percent_change = relative_change(Projected, Observed_baseline)) |>
    dplyr::left_join(organisations |> dplyr::select(Key, Level, Organisation), by = "Key")
  stopifnot(all(abs(total_projection$Projected[total_projection$Year == ons_reference_year] -
    total_projection$Observed_baseline[total_projection$Year == ons_reference_year]) < 1e-8))
  # Trust and ICB sums must equal the cluster in every future year, not only at baseline.
  level_totals <- total_projection |>
    dplyr::group_by(Level, Year) |>
    dplyr::summarise(N = sum(Projected), .groups = "drop") |>
    tidyr::pivot_wider(names_from = Level, values_from = N)
  stopifnot(all(abs(level_totals$Trust - level_totals$Cluster) < 1e-7),
            all(abs(level_totals$ICB - level_totals$Cluster) < 1e-7))

  result$forecast_ready <- TRUE
  result$note <- ""
  result$modifiers <- modifiers
  result$la_projection <- la_projection
  result$flow_projection <- flow_projection
  result$age_totals <- age_totals
  result$total_projection <- total_projection
  if (!prepare_outputs) return(result)

  style_line_chart <- function(chart, y_title, percent = FALSE, ons_only = FALSE) {
    tick_years <- sort(unique(c(ons_reference_year,
      projection_years[projection_years %% 5 == 0], max(projection_years))))
    plotly::layout(chart,
      xaxis = list(title = "Observed financial year / ONS year ending mid-year",
        tickmode = "array", tickvals = tick_years,
        ticktext = ifelse(tick_years == ons_reference_year & !ons_only, baseline_fy, paste0("Mid-", tick_years))),
      yaxis = list(title = y_title, tickformat = if (percent) ".0%" else ",.0f",
                   rangemode = "tozero", zeroline = TRUE),
      legend = list(orientation = "h", x = 0, y = -0.25),
      margin = list(l = 85, r = 20, t = 20, b = 125), font = list(family = "Arial")) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }
  age_line_chart <- function(data, value, title, percent = FALSE) {
    chart <- plotly::plot_ly(height = 480)
    for (i in seq_along(age_bands)) {
      line <- data |> dplyr::filter(Age_band == age_bands[i]) |> dplyr::arrange(Year)
      if (!nrow(line)) next
      values <- line[[value]]
      hover <- paste0(ifelse(line$Year == ons_reference_year & !percent, baseline_fy, paste0("Mid-", line$Year)),
        "<br>Age: ", age_bands[i], "<br>", title, ": ",
        if (percent) scales::percent(values, accuracy = 0.1) else scales::comma(values, accuracy = 1))
      chart <- plotly::add_trace(chart, x = line$Year, y = values, text = hover,
        type = "scatter", mode = "lines+markers", name = age_bands[i],
        line = list(color = age_colours[i], width = 2), marker = list(color = age_colours[i], size = 4),
        hovertemplate = "%{text}<extra></extra>", connectgaps = FALSE)
    }
    style_line_chart(chart, title, percent, ons_only = percent)
  }
  forecast_outputs <- stats::setNames(lapply(organisations$Key, function(key) {
    org_mapping <- mapping[mapping$Key == key, , drop = FALSE]
    org_flows <- flow_projection[flow_projection$Key == key, , drop = FALSE]
    org_ages <- age_totals[age_totals$Key == key, , drop = FALSE]
    org_totals <- total_projection[total_projection$Key == key, , drop = FALSE]
    org_totals <- dplyr::arrange(org_totals, Year)
    org_feeders <- unique(org_mapping$LA)
    ons_age_growth <- la_projection |>
      dplyr::filter(LA %in% org_feeders) |>
      dplyr::group_by(Age_band, Year) |>
      dplyr::summarise(ONS_maternities = sum(ONS_maternities), .groups = "drop") |>
      dplyr::group_by(Age_band) |>
      dplyr::mutate(ONS_change = relative_change(ONS_maternities,
                         ONS_maternities[Year == ons_reference_year])) |>
      dplyr::ungroup()
    growth_chart <- age_line_chart(ons_age_growth, "ONS_change", "% change in feeder-LA maternities", TRUE)
    incoming_chart <- age_line_chart(org_ages, "Projected", "Incoming maternities")
    # Tables retain every forecast year; their columns scroll horizontally.
    growth_table <- if (nrow(org_flows)) org_flows |>
      dplyr::mutate(ONS_year_label = paste0("Mid-", Year)) |>
      dplyr::select(LA_name, LA, Age_band, Numerator, Denominator, Share, ONS_year_label, ONS_change) |>
      tidyr::pivot_wider(names_from = ONS_year_label, values_from = ONS_change) |>
      dplyr::arrange(LA_name, match(Age_band, age_bands)) |>
      base_table("ONS change by feeder LA and age", paste("Relative to the ONS", ons_reference_year, "reference")) |>
      gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age_band = "Age band",
        Numerator = "Observed deliveries here", Denominator = "All LA/age pregnancies", Share = "% sent here") |>
      gt::fmt_number(columns = c("Numerator", "Denominator"), decimals = 0) |>
      gt::fmt_percent(columns = c("Share", paste0("Mid-", projection_years)), decimals = 1) else
      base_table(data.frame(Note = paste("No recorded feeder-LA flows in", baseline_fy)), "ONS change")
    incoming_table <- if (nrow(org_flows)) org_flows |>
      dplyr::select(LA_name, LA, Age_band, Year_label, Projected) |>
      tidyr::pivot_wider(names_from = Year_label, values_from = Projected) |>
      dplyr::arrange(LA_name, match(Age_band, age_bands)) |>
      base_table("Incoming maternities by feeder LA and age") |>
      gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age_band = "Age band") |>
      gt::fmt_number(columns = c(baseline_fy, paste0("Mid-", projection_years[-1])), decimals = 0) else
      base_table(data.frame(Note = paste("No recorded feeder-LA flows in", baseline_fy)), "Incoming maternities")
    if (!nrow(org_flows)) {
      growth_chart <- htmltools::div(paste("No recorded feeder-LA flows in", baseline_fy))
      incoming_chart <- htmltools::div(paste("No recorded feeder-LA flows in", baseline_fy))
    }
    total_chart <- plotly::plot_ly(org_totals, height = 480, x = ~Year, y = ~Projected,
      type = "scatter", mode = "lines+markers", line = list(color = "#005EB8", width = 2),
      text = ~paste0(Year_label, "<br>Maternities: ", scales::comma(Projected, accuracy = 1)),
      hovertemplate = "%{text}<extra></extra>", showlegend = FALSE) |>
      style_line_chart("Total maternities")
    change_chart <- plotly::plot_ly(org_totals, height = 480, x = ~Year, y = ~Percent_change,
      type = "scatter", mode = "lines+markers", line = list(color = "#005EB8", width = 2),
      text = ~paste0(Year_label, "<br>Change from ", baseline_fy, ": ",
        ifelse(is.na(Percent_change), "n.a.", scales::percent(Percent_change, accuracy = 0.1))),
      hovertemplate = "%{text}<extra></extra>", showlegend = FALSE) |>
      style_line_chart(paste("% change from observed", baseline_fy), TRUE)
    if (org_totals$Observed_baseline[1] == 0) change_chart <- plotly::layout(change_chart,
      annotations = list(list(text = paste("No observed deliveries in", baseline_fy, "; relative change unavailable."),
        x = 0, y = 1.05, xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "left")),
      margin = list(t = 45))
    total_table <- org_totals |>
      dplyr::select(Year_label, Projected) |>
      base_table("Total maternity projections") |>
      gt::cols_label(Year_label = "Year", Projected = "Maternities") |>
      gt::fmt_number(columns = "Projected", decimals = 0)
    change_table <- org_totals |>
      dplyr::select(Year_label, Observed_baseline, Projected, Change, Percent_change) |>
      base_table(paste("Change from observed", baseline_fy)) |>
      gt::cols_label(Year_label = "Year", Observed_baseline = paste("Observed", baseline_fy),
        Projected = "Maternities", Change = "Count change", Percent_change = "% change") |>
      gt::fmt_number(columns = c("Observed_baseline", "Projected", "Change"), decimals = 0) |>
      gt::fmt_percent(columns = "Percent_change", decimals = 1)
    list(growth_chart = growth_chart, incoming_chart = incoming_chart,
      growth_table = growth_table, incoming_table = incoming_table,
      total_chart = total_chart, total_table = total_table,
      change_chart = change_chart, change_table = change_table)
  }), organisations$Key)

  all_totals_table <- total_projection |>
    dplyr::select(Level, Organisation, Year_label, Projected) |>
    tidyr::pivot_wider(names_from = Year_label, values_from = Projected) |>
    gt::gt(groupname_col = "Level", rowname_col = "Organisation") |>
    gt::tab_header(title = "All organisation maternity projections") |>
    gt::fmt_number(columns = c(baseline_fy, paste0("Mid-", projection_years[-1])), decimals = 0) |>
    gt::tab_options(container.width = "100%", container.overflow.x = TRUE,
                    table.font.names = c("Open Sans", "Arial", "sans-serif"))
  all_changes_table <- total_projection |>
    dplyr::select(Level, Organisation, Year_label, Percent_change) |>
    tidyr::pivot_wider(names_from = Year_label, values_from = Percent_change) |>
    gt::gt(groupname_col = "Level", rowname_col = "Organisation") |>
    gt::tab_header(title = paste("All organisation changes from observed", baseline_fy)) |>
    gt::fmt_percent(columns = c(baseline_fy, paste0("Mid-", projection_years[-1])), decimals = 1) |>
    gt::sub_missing(missing_text = "n.a.") |>
    gt::tab_options(container.width = "100%", container.overflow.x = TRUE,
                    table.font.names = c("Open Sans", "Arial", "sans-serif"))
  result$forecast_outputs <- forecast_outputs
  result$all_totals_table <- all_totals_table
  result$all_changes_table <- all_changes_table
  result
}

build_projection_backtest <- function() {
  # Refit only the 2023/24 LA/age shares; do not use 2024/25 or 2025/26 shares.
  historical <- build_first_draft_projections(baseline_fy = "2023/24",
    ons_reference_year = 2024L, projection_end = 2026L, prepare_outputs = FALSE)
  if (!isTRUE(historical$forecast_ready)) return(list(ready = FALSE, note = historical$note))
  organisations <- historical$organisations
  years <- c("2023/24", "2024/25", "2025/26")
  observed <- final_cohort_2326 |>
    dplyr::filter(trimws(as.character(birth_yr)) %in% years, as.character(birth_flag) == "1") |>
    dplyr::transmute(Person = trimws(as.character(person_id_mother)),
      Pregnancy = trimws(as.character(uniq_preg_id)), Year_label = trimws(as.character(birth_yr)),
      LA = toupper(trimws(as.character(la_of_mother))), Age = as.numeric(age_at_birth_mother),
      Trust = toupper(trimws(as.character(birth_provider))), Home = as.numeric(as.character(home_birth))) |>
    dplyr::distinct()
  if (any(is.na(observed$Person) | observed$Person == "" | is.na(observed$Pregnancy) |
      observed$Pregnancy == "") || anyDuplicated(observed[c("Person", "Pregnancy")])) stop(
    "The observed 2023/24–2025/26 comparison has missing identifiers or conflicting pregnancy details.",
    call. = FALSE)
  if (any(observed$Trust %in% cluster_trusts & !(observed$Home %in% c(0, 1)))) stop(
    "The comparison has cluster-provider deliveries with missing/invalid home-birth flags.", call. = FALSE)
  observed <- observed |> dplyr::filter(Trust %in% cluster_trusts, Home == 0)
  # Missing source years are not zero activity; a zero-activity organisation is valid.
  absent_years <- setdiff(years, observed$Year_label)
  if (length(absent_years)) stop("No observed cluster-trust deliveries for ",
                                paste(absent_years, collapse = ", "), call. = FALSE)
  observed_totals <- dplyr::bind_rows(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, , drop = FALSE]
    keep <- if (org$Level == "Trust") observed$Trust == org$Code else
      if (org$Level == "ICB") unname(provider_icb[observed$Trust]) == org$Code else rep(TRUE, nrow(observed))
    data <- observed[keep, , drop = FALSE]
    data.frame(Key = org$Key, Year_label = years,
      Observed = vapply(years, function(year) sum(data$Year_label == year), integer(1)))
  }))
  comparison <- historical$total_projection |>
    dplyr::transmute(Key, Level, Organisation, Year,
      Year_label = paste0(Year - 1L, "/", substr(Year, 3, 4)), Modelled = Projected) |>
    dplyr::left_join(observed_totals, by = c("Key", "Year_label")) |>
    dplyr::mutate(Difference = Modelled - Observed,
      Percent_difference = ifelse(Observed > 0, Difference / Observed, NA_real_)) |>
    dplyr::arrange(match(Key, organisations$Key), Year)
  if (anyNA(comparison$Observed)) stop("An observed organisation/year comparison is missing.", call. = FALSE)
  stopifnot(all(abs(comparison$Difference[comparison$Year == 2024L]) < 1e-8))
  comparison_table <- function(data) {
    data |>
      dplyr::select(Year_label, Observed, Modelled, Difference, Percent_difference) |>
      gt::gt() |>
      gt::tab_header(title = "Observed versus modelled maternities", subtitle = "Mapping fixed at 2023/24") |>
      gt::cols_label(Year_label = "Financial year", Observed = "MSDS observed", Modelled = "ONS-based model",
        Difference = "Model − observed", Percent_difference = "% difference from observed") |>
      gt::fmt_number(columns = c("Observed", "Modelled", "Difference"), decimals = 0) |>
      gt::fmt_percent(columns = "Percent_difference", decimals = 1) |>
      gt::sub_missing(missing_text = "n.a.") |>
      gt::tab_options(table.font.names = c("Open Sans", "Arial", "sans-serif"))
  }
  outputs <- stats::setNames(lapply(organisations$Key, function(key) {
    data <- comparison[comparison$Key == key, , drop = FALSE]
    chart <- plotly::plot_ly(height = 440) |>
      plotly::add_trace(x = data$Year, y = data$Observed, type = "scatter", mode = "lines+markers",
        name = "MSDS observed", line = list(color = "#005EB8", width = 2),
        text = paste0(data$Year_label, "<br>Observed: ", scales::comma(data$Observed, accuracy = 1)),
        hovertemplate = "%{text}<extra></extra>") |>
      plotly::add_trace(x = data$Year, y = data$Modelled, type = "scatter", mode = "lines+markers",
        name = "ONS-based model", line = list(color = "#D55E00", width = 2),
        text = paste0(data$Year_label, "<br>Modelled: ", scales::comma(data$Modelled, accuracy = 1),
          "<br>Difference from observed: ", ifelse(is.na(data$Percent_difference), "n.a.",
                                                   scales::percent(data$Percent_difference, accuracy = 0.1))),
        hovertemplate = "%{text}<extra></extra>") |>
      plotly::layout(xaxis = list(title = "Financial year (ONS growth uses matching year ending mid-year)",
        tickmode = "array", tickvals = 2024:2026, ticktext = years),
        yaxis = list(title = "Maternities", tickformat = ",.0f", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = -0.22),
        margin = list(l = 80, r = 20, t = 20, b = 95), font = list(family = "Arial")) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
    list(chart = chart, table = comparison_table(data))
  }), organisations$Key)

  # One organisation per row, with observed/modelled/error columns under each year.
  metrics <- c("Observed", "Modelled", "Difference", "Percent_difference")
  wide <- comparison |>
    dplyr::select(Level, Organisation, Year_label, dplyr::all_of(metrics)) |>
    tidyr::pivot_wider(names_from = Year_label, values_from = dplyr::all_of(metrics), names_sep = "_")
  column_order <- unlist(lapply(years, function(year) paste0(metrics, "_", year)), use.names = FALSE)
  wide <- dplyr::select(wide, Level, Organisation, dplyr::all_of(column_order))
  percent_columns <- paste0("Percent_difference_", years)
  count_columns <- setdiff(column_order, percent_columns)
  labels <- stats::setNames(rep(c("MSDS observed", "ONS-based model", "Model − observed", "% difference"),
                                  length(years)), column_order)
  all_table <- gt::gt(wide, groupname_col = "Level", rowname_col = "Organisation") |>
    gt::tab_header(title = "Observed versus modelled maternities: all organisations") |>
    gt::cols_label(.list = as.list(labels)) |>
    gt::fmt_number(columns = dplyr::all_of(count_columns), decimals = 0) |>
    gt::fmt_percent(columns = dplyr::all_of(percent_columns), decimals = 1) |>
    gt::sub_missing(missing_text = "n.a.") |>
    gt::tab_source_note(source_note = "% difference = (modelled − observed) / observed × 100; positive means overprediction. n.a. = zero observed count.") |>
    gt::tab_options(table.font.names = c("Open Sans", "Arial", "sans-serif"),
                    container.width = "100%", container.overflow.x = TRUE,
                    table.width = gt::px(1500), table.font.size = gt::px(12))
  for (year in years) all_table <- gt::tab_spanner(all_table, label = year,
    columns = paste0(metrics, "_", year), id = paste0("backtest-", year), gather = FALSE)
  list(ready = TRUE, note = "", organisations = organisations,
       data = comparison, outputs = outputs, all_table = all_table)
}

# Refresh even when preparation is blocked, so the QMD never reuses stale forecasts.
first_draft_projections <- tryCatch(build_first_draft_projections(), error = function(e) {
  list(mapping_ready = FALSE, forecast_ready = FALSE, note = conditionMessage(e))
})
projection_backtest <- tryCatch(build_projection_backtest(), error = function(e) {
  list(ready = FALSE, note = conditionMessage(e))
})
first_draft_projections$backtest <- projection_backtest
if (isTRUE(projection_backtest$ready)) {
  first_draft_projections$backtest_outputs <- projection_backtest$outputs
  if (is.null(first_draft_projections$organisations)) {
    first_draft_projections$organisations <- projection_backtest$organisations
  }
}
saveRDS(first_draft_projections, ".projections.rds")
if (!isTRUE(first_draft_projections$forecast_ready)) {
  message("First draft projections need attention: ", first_draft_projections$note)
} else {
  message("First draft projections prepared: observed 2025/26 plus Method 1 age-specific ONS growth.")
}
if (!isTRUE(projection_backtest$ready)) {
  message("Retrospective check needs attention: ", projection_backtest$note)
} else {
  message("Retrospective check prepared: 2023/24 mapping compared with observed 2024/25 and 2025/26.")
}
