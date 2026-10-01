# Run after additional_analysis.R. Preparation for report section 8 only.
# Hospital shares use each receiving organisation's total delivery count.
build_delivery_geography <- function() {
  required <- c("final_cohort_2326", "trust_labels", "icb_labels", "provider_icb",
                "cluster_icbs", "cluster_trusts")
  absent <- required[!vapply(required, function(name) exists(name, inherits = TRUE), logical(1))]
  if (length(absent)) stop("Missing objects: ", paste(absent, collapse = ", "),
                           ". Run the analysis runner first.", call. = FALSE)
  columns <- c("person_id_mother", "uniq_preg_id", "birth_yr", "birth_flag",
               "resident_icb", "la_of_mother", "age_at_birth_mother", "birth_provider", "home_birth")
  absent <- setdiff(columns, names(final_cohort_2326))
  if (length(absent)) stop("final_cohort_2326 is missing: ", paste(absent, collapse = ", "), call. = FALSE)
  years <- c("2023/24", "2024/25", "2025/26")
  age_bands <- c("<20", "20-24", "25-29", "30-34", "35-39", "40+", "Unknown")
  clean_code <- function(value, missing = "Unknown") {
    value <- toupper(trimws(as.character(value)))
    value[is.na(value) | value == ""] <- missing
    value
  }
  change_from <- function(value, reference) {
    reference <- rep_len(reference, length(value))
    result <- rep(NA_real_, length(value))
    valid <- is.finite(value) & is.finite(reference) & reference > 0
    result[valid] <- value[valid] / reference[valid] - 1
    result
  }
  table_style <- function(data, title, rowname = NULL, groupname = NULL) {
    gt::gt(data, rowname_col = rowname, groupname_col = groupname) |>
      gt::tab_header(title = title) |>
      gt::sub_missing(missing_text = "n.a.") |>
      gt::tab_options(table.font.names = c("Open Sans", "Arial", "sans-serif"),
        table.font.size = gt::px(12), container.width = "100%", container.overflow.x = TRUE)
  }
  scroll_chart <- function(chart) htmltools::div(style = "max-height: 650px; overflow-y: auto;", chart)

  episodes <- final_cohort_2326 |>
    dplyr::filter(trimws(as.character(birth_yr)) %in% years, as.character(birth_flag) == "1") |>
    dplyr::transmute(Person = trimws(as.character(person_id_mother)),
      Pregnancy = trimws(as.character(uniq_preg_id)), Year = trimws(as.character(birth_yr)),
      Resident_ICB = clean_code(resident_icb), LA = clean_code(la_of_mother, "Unknown LA"),
      Age = suppressWarnings(as.numeric(as.character(age_at_birth_mother))),
      Trust = clean_code(birth_provider, "Unknown provider"),
      Home = suppressWarnings(as.numeric(as.character(home_birth)))) |>
    dplyr::distinct()
  if (!nrow(episodes)) stop("No recorded deliveries for 2023/24-2025/26.", call. = FALSE)
  absent_years <- setdiff(years, episodes$Year)
  if (length(absent_years)) stop("No recorded deliveries for ", paste(absent_years, collapse = ", "), call. = FALSE)
  if (any(is.na(episodes$Person) | episodes$Person == "" |
          is.na(episodes$Pregnancy) | episodes$Pregnancy == "")) stop(
    "Recorded deliveries lack mother/pregnancy identifiers; resolve before counting episodes.", call. = FALSE)
  if (anyDuplicated(episodes[c("Person", "Pregnancy")])) stop(
    "Conflicting delivery details for the same mother/pregnancy pair.", call. = FALSE)
  if (any(!(episodes$Home %in% c(0, 1)))) stop(
    "Some recorded deliveries have missing/invalid home-birth flags; location cannot be classified.", call. = FALSE)
  episodes <- episodes |>
    dplyr::mutate(Age_band = dplyr::case_when(
      is.na(Age) | Age < 10 | Age > 60 ~ "Unknown", Age < 20 ~ "<20", Age < 25 ~ "20-24",
      Age < 30 ~ "25-29", Age < 35 ~ "30-34", Age < 40 ~ "35-39", TRUE ~ "40+"),
      Destination = dplyr::if_else(Home == 1, "Home", Trust))

  # Names only: these sources do not change the original LA codes or counts.
  la_lookup <- readr::read_csv("data/2022 SNPP Births persons.csv", show_col_types = FALSE) |>
    dplyr::transmute(LA = AREA_CODE, LA_name = AREA_NAME) |>
    dplyr::distinct()
  registrations <- readxl::read_excel("data/2025birthregistrations.xlsx", sheet = "Table_3", skip = 5)
  recent_names <- data.frame(LA = as.character(registrations[[1]]),
                            LA_name = as.character(registrations[[2]])) |>
    dplyr::filter(grepl("^[EWSN][0-9]{8}$", LA))
  la_lookup <- dplyr::bind_rows(recent_names, la_lookup,
      data.frame(LA = "Unknown LA", LA_name = "Unknown LA"),
      data.frame(LA = "L99999999", LA_name = "Unknown LA")) |>
    dplyr::distinct(LA, .keep_all = TRUE)
  la_names <- episodes |>
    dplyr::distinct(LA) |>
    dplyr::left_join(la_lookup, by = "LA") |>
    dplyr::mutate(LA_name = dplyr::coalesce(LA_name, LA),
      Resident_LA = ifelse(LA == "Unknown LA" | LA_name == LA, LA_name,
                           paste0(LA_name, " (", LA, ")")))
  episodes <- dplyr::left_join(episodes, la_names, by = "LA")
  trust_codes <- unique(c(names(trust_labels)[names(trust_labels) != "Other"],
    sort(intersect(episodes$Trust[episodes$Home == 0], cluster_trusts))))
  trust_names <- unname(trust_labels[trust_codes])
  trust_names[is.na(trust_names)] <- paste("Trust", trust_codes[is.na(trust_names)])
  organisations <- dplyr::bind_rows(
    data.frame(Key = paste0("trust-", tolower(trust_codes)), Level = "Trust",
               Code = trust_codes, Organisation = trust_names),
    data.frame(Key = paste0("icb-", tolower(cluster_icbs)), Level = "ICB", Code = cluster_icbs,
               Organisation = unname(icb_labels[cluster_icbs])),
    data.frame(Key = "cluster", Level = "Cluster", Code = "Cluster", Organisation = "Combined cluster"))
  destination_names <- function(codes) {
    labels <- unname(trust_labels[codes])
    labels[is.na(labels)] <- paste("Trust", codes[is.na(labels)])
    labels[codes == "Home"] <- "Home birth"
    labels[codes == "Unknown provider"] <- "Unknown delivery provider"
    labels
  }
  hospital <- episodes |>
    dplyr::filter(Home == 0, Trust %in% cluster_trusts)
  if (!nrow(hospital)) stop("No non-home deliveries at cluster trusts.", call. = FALSE)
  select_org <- function(data, org) {
    keep <- if (org$Level[[1]] == "Trust") data$Trust == org$Code[[1]] else
      if (org$Level[[1]] == "ICB") unname(provider_icb[data$Trust]) == org$Code[[1]] else rep(TRUE, nrow(data))
    data[!is.na(keep) & keep, , drop = FALSE]
  }

  # Cluster residents plus all recorded deliveries associated with cluster providers.
  # Provider-associated home births stay home destinations, including outside residents.
  matrix_episodes <- episodes |>
    dplyr::filter(Year == "2025/26", Resident_ICB %in% cluster_icbs | Trust %in% cluster_trusts)
  if (!nrow(matrix_episodes)) stop("No 2025/26 activity in the delivery matrix.", call. = FALSE)
  matrix_data <- matrix_episodes |>
    dplyr::count(LA, Resident_LA, Destination, name = "N") |>
    dplyr::group_by(LA) |>
    dplyr::mutate(Origin_total = sum(N), Share = N / Origin_total) |>
    dplyr::ungroup() |>
    dplyr::mutate(Delivery_destination = destination_names(Destination)) |>
    dplyr::arrange(dplyr::desc(Origin_total), Resident_LA, Delivery_destination)
  matrix_wide <- matrix_data |>
    dplyr::select(Resident_LA, Delivery_destination, N) |>
    tidyr::pivot_wider(names_from = Delivery_destination, values_from = N, values_fill = 0)
  matrix_columns <- setdiff(names(matrix_wide), "Resident_LA")
  matrix_wide$Total <- rowSums(as.data.frame(matrix_wide[matrix_columns]))
  matrix_table <- matrix_wide |>
    table_style("2025/26 resident LA to delivery destination", rowname = "Resident_LA") |>
    gt::fmt_number(columns = dplyr::all_of(c(matrix_columns, "Total")), decimals = 0) |>
    gt::tab_options(table.font.size = gt::px(11))
  matrix_values_table <- matrix_data |>
    dplyr::select(Resident_LA, Delivery_destination, N, Origin_total, Share) |>
    table_style("All recorded LA-to-destination flows, 2025/26") |>
    gt::cols_label(Resident_LA = "Resident LA", Delivery_destination = "Delivery destination",
      N = "Deliveries", Origin_total = "Recorded LA deliveries in scope", Share = "% of recorded LA deliveries") |>
    gt::fmt_number(columns = c("N", "Origin_total"), decimals = 0) |>
    gt::fmt_percent(columns = "Share", decimals = 1)
  stopifnot(sum(matrix_data$N) == nrow(matrix_episodes), sum(matrix_wide$Total) == nrow(matrix_episodes))

  origin_rank <- matrix_data |>
    dplyr::group_by(LA, Resident_LA) |>
    dplyr::summarise(N = sum(N), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(N), Resident_LA)
  unknown_las <- origin_rank$LA[grepl("^Unknown|^L999", origin_rank$LA)]
  top_las <- head(origin_rank$LA[!origin_rank$LA %in% unknown_las], 15)
  sankey_links <- matrix_data |>
    dplyr::mutate(Origin = ifelse(LA %in% top_las | LA %in% unknown_las, Resident_LA, "Other LAs")) |>
    dplyr::group_by(Origin, Delivery_destination) |>
    dplyr::summarise(N = sum(N), .groups = "drop")
  origins <- sankey_links |>
    dplyr::group_by(Origin) |>
    dplyr::summarise(N = sum(N), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(N), Origin) |>
    dplyr::pull(Origin)
  destinations <- sort(unique(sankey_links$Delivery_destination))
  palette <- grDevices::hcl.colors(length(origins), palette = "Dark 3")
  sankey_chart <- plotly::plot_ly(type = "sankey", height = max(500, length(origins) * 30),
    arrangement = "snap", node = list(label = c(origins, destinations), pad = 14, thickness = 18,
      color = c(palette, rep("#727272", length(destinations))),
      hovertemplate = "%{label}<br>Deliveries: %{value:,.0f}<extra></extra>"),
    link = list(source = match(sankey_links$Origin, origins) - 1L,
      target = length(origins) + match(sankey_links$Delivery_destination, destinations) - 1L,
      value = sankey_links$N,
      color = grDevices::adjustcolor(palette[match(sankey_links$Origin, origins)], alpha.f = 0.45),
      customdata = paste0(sankey_links$Origin, " to ", sankey_links$Delivery_destination,
                          "<br>Deliveries: ", scales::comma(sankey_links$N)),
      hovertemplate = "%{customdata}<extra></extra>")) |>
    plotly::layout(font = list(family = "Arial", size = 11), margin = list(l = 12, r = 12, t = 15, b = 15)) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
  stopifnot(sum(sankey_links$N) == nrow(matrix_episodes))

  hospital_mapping <- dplyr::bind_rows(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, , drop = FALSE]
    data <- select_org(hospital, org)
    org_las <- unique(data$LA)
    if (!length(org_las)) return(NULL)
    counts <- data |> dplyr::count(Year, LA, Age_band, name = "N")
    totals <- data |> dplyr::count(Year, name = "Organisation_total")
    tidyr::expand_grid(Year = years, LA = org_las, Age_band = age_bands) |>
      dplyr::left_join(counts, by = c("Year", "LA", "Age_band")) |>
      dplyr::left_join(totals, by = "Year") |>
      dplyr::mutate(N = dplyr::coalesce(N, 0L), Organisation_total = dplyr::coalesce(Organisation_total, 0L),
        Share = ifelse(Organisation_total > 0, N / Organisation_total, NA_real_),
        Key = org$Key, Level = org$Level, Organisation = org$Organisation) |>
      dplyr::left_join(la_names, by = "LA")
  }))
  la_trends <- hospital_mapping |>
    dplyr::group_by(Key, Level, Organisation, LA, Resident_LA, Year) |>
    dplyr::summarise(N = sum(N), Organisation_total = dplyr::first(Organisation_total), .groups = "drop") |>
    dplyr::mutate(Share = ifelse(Organisation_total > 0, N / Organisation_total, NA_real_)) |>
    dplyr::group_by(Key, LA) |>
    dplyr::mutate(Base_count = N[Year == "2023/24"], Base_share = Share[Year == "2023/24"],
      Count_change = change_from(N, Base_count), Share_change = Share - Base_share,
      Relative_share_change = change_from(Share, Base_share)) |>
    dplyr::ungroup()
  wide_trend_table <- function(data, title, grouped = FALSE) {
    metrics <- c("N", "Share", "Share_change", "Count_change")
    identifiers <- if (grouped) c("Organisation", "Resident_LA") else "Resident_LA"
    wide <- data |>
      dplyr::select(dplyr::all_of(identifiers), Year, dplyr::all_of(metrics)) |>
      tidyr::pivot_wider(names_from = Year, values_from = dplyr::all_of(metrics), names_sep = "_")
    order <- unlist(lapply(years, function(year) paste0(metrics, "_", year)), use.names = FALSE)
    wide <- dplyr::select(wide, dplyr::all_of(identifiers), dplyr::all_of(order))
    for (column in paste0("Share_change_", years)) wide[[column]] <- wide[[column]] * 100
    labels <- stats::setNames(rep(c("Deliveries", "% from LA", "Share change (pp)", "% count change"), 3), order)
    table <- table_style(wide, title, rowname = "Resident_LA", groupname = if (grouped) "Organisation" else NULL) |>
      gt::cols_label(.list = as.list(labels)) |>
      gt::fmt_number(columns = dplyr::all_of(paste0("N_", years)), decimals = 0) |>
      gt::fmt_number(columns = dplyr::all_of(paste0("Share_change_", years)), decimals = 1) |>
      gt::fmt_percent(columns = dplyr::all_of(c(paste0("Share_", years), paste0("Count_change_", years))), decimals = 1) |>
      gt::tab_source_note(source_note = "Changes use 2023/24. pp = percentage points; n.a. = zero/unavailable base.") |>
      gt::tab_options(table.width = gt::px(1450))
    for (year in years) table <- gt::tab_spanner(table, label = year,
      columns = paste0(metrics, "_", year), id = paste0("delivery-geography-", year), gather = FALSE)
    table
  }
  line_chart <- function(data, field, y_title) {
    groups <- unique(data$Origin_group)
    colours <- grDevices::hcl.colors(length(groups), palette = "Dark 3")
    chart <- plotly::plot_ly(height = 480)
    for (j in seq_along(groups)) {
      line <- data[data$Origin_group == groups[j], , drop = FALSE]
      line <- line[match(years, line$Year), , drop = FALSE]
      chart <- plotly::add_trace(chart, x = seq_along(years), y = line[[field]],
        type = "scatter", mode = "lines+markers", name = groups[j],
        line = list(color = colours[j], width = 2), marker = list(color = colours[j], size = 5),
        text = paste0(line$Year, "<br>", groups[j], "<br>Deliveries: ", scales::comma(line$N),
          "<br>", y_title, ": ", ifelse(is.na(line[[field]]), "n.a.", scales::percent(line[[field]], accuracy = 0.1))),
        hovertemplate = "%{text}<extra></extra>", connectgaps = FALSE)
    }
    plotly::layout(chart, xaxis = list(title = "Financial year", tickmode = "array", tickvals = 1:3, ticktext = years),
      yaxis = list(title = y_title, tickformat = ".0%", zeroline = TRUE),
      legend = list(orientation = "h", x = 0, y = -0.25),
      margin = list(l = 75, r = 20, t = 20, b = 135), font = list(family = "Arial", size = 11)) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }
  outputs <- stats::setNames(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, , drop = FALSE]
    ages <- hospital_mapping[hospital_mapping$Key == org$Key & hospital_mapping$Year == "2025/26", , drop = FALSE]
    trend <- la_trends[la_trends$Key == org$Key, , drop = FALSE]
    if (!nrow(ages)) {
      empty <- htmltools::div("No recorded hospital deliveries in the three years.")
      table <- table_style(data.frame(Note = "No recorded hospital deliveries in the three years."), org$Organisation)
      return(list(origin_chart = empty, age_chart = empty, table = table, age_table = table,
                  stability_chart = empty, change_chart = empty, stability_table = table))
    }
    origin <- trend |>
      dplyr::filter(Year == "2025/26", N > 0) |>
      dplyr::arrange(dplyr::desc(N), Resident_LA)
    if (!nrow(origin)) {
      origin_chart <- htmltools::div("No recorded hospital deliveries in 2025/26.")
      age_chart <- origin_chart
    } else {
      origin_chart <- plotly::plot_ly(origin, height = max(380, nrow(origin) * 24 + 100),
        x = ~Share, y = ~Resident_LA, type = "bar", orientation = "h",
        marker = list(color = origin$Share, colorscale = list(c(0, "#DCEAF4"), c(1, "#005EB8")),
          cmin = 0, cmax = max(origin$Share), showscale = FALSE),
        text = ~paste0(Resident_LA, "<br>Deliveries: ", scales::comma(N), "<br>% of organisation: ", scales::percent(Share, accuracy = 0.1)),
        hovertemplate = "%{text}<extra></extra>", showlegend = FALSE) |>
        plotly::layout(xaxis = list(title = "% of all hospital deliveries at this organisation", tickformat = ".0%"),
          yaxis = list(title = "", categoryorder = "array", categoryarray = origin$Resident_LA, autorange = "reversed"),
          margin = list(l = 255, r = 25, t = 15, b = 65), font = list(family = "Arial", size = 11)) |>
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
      plot_ages <- ages |>
        dplyr::filter(LA %in% origin$LA) |>
        dplyr::arrange(match(LA, origin$LA), match(Age_band, age_bands))
      heat <- matrix(plot_ages$Share, nrow = nrow(origin), byrow = TRUE)
      hover <- matrix(paste0(plot_ages$Resident_LA, "<br>Age: ", plot_ages$Age_band,
        "<br>Deliveries: ", plot_ages$N, "<br>% of organisation: ", scales::percent(plot_ages$Share, accuracy = 0.1)),
        nrow = nrow(origin), byrow = TRUE)
      age_chart <- plotly::plot_ly(x = age_bands, y = origin$Resident_LA, z = heat, text = hover,
        type = "heatmap", height = max(380, nrow(origin) * 24 + 100), zmin = 0, zmax = max(heat),
        colorscale = list(c(0, "#F1F6FA"), c(1, "#005EB8")),
        colorbar = list(title = "% of all deliveries", tickformat = ".0%"),
        hovertemplate = "%{text}<extra></extra>") |>
        plotly::layout(xaxis = list(title = "Maternal age band", side = "top"),
          yaxis = list(title = "", autorange = "reversed", categoryorder = "array", categoryarray = origin$Resident_LA),
          margin = list(l = 255, r = 135, t = 65, b = 25), font = list(family = "Arial", size = 11)) |>
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    }
    origin_table <- origin |>
      dplyr::select(Resident_LA, N, Organisation_total, Share) |>
      table_style("2025/26 hospital deliveries by resident LA") |>
      gt::cols_label(Resident_LA = "Resident LA", N = "Deliveries", Organisation_total = "All deliveries here", Share = "% from LA") |>
      gt::fmt_number(columns = c("N", "Organisation_total"), decimals = 0) |>
      gt::fmt_percent(columns = "Share", decimals = 1)
    age_wide <- ages |>
      dplyr::select(Resident_LA, Age_band, N) |>
      tidyr::pivot_wider(names_from = Age_band, values_from = N, values_fill = 0) |>
      dplyr::select(Resident_LA, dplyr::all_of(age_bands))
    age_wide$Total <- rowSums(as.data.frame(age_wide[age_bands]))
    age_wide <- age_wide |>
      dplyr::filter(Total > 0) |>
      dplyr::arrange(dplyr::desc(Total), Resident_LA)
    age_table <- table_style(age_wide, "2025/26 hospital delivery counts: LA by age band", rowname = "Resident_LA") |>
      gt::fmt_number(columns = dplyr::all_of(c(age_bands, "Total")), decimals = 0)
    unknown <- unique(trend$LA[grepl("^Unknown|^L999", trend$LA)])
    top <- head(origin$LA[!origin$LA %in% unknown], 7)
    grouped <- trend |>
      dplyr::mutate(Origin_group = ifelse(LA %in% top | LA %in% unknown, Resident_LA, "Other LAs")) |>
      dplyr::group_by(Origin_group, Year) |>
      dplyr::summarise(N = sum(N), Organisation_total = dplyr::first(Organisation_total), .groups = "drop") |>
      dplyr::mutate(Share = ifelse(Organisation_total > 0, N / Organisation_total, NA_real_)) |>
      dplyr::group_by(Origin_group) |>
      dplyr::mutate(Count_change = change_from(N, N[Year == "2023/24"])) |>
      dplyr::ungroup()
    list(origin_chart = scroll_chart(origin_chart), age_chart = scroll_chart(age_chart), table = origin_table, age_table = age_table,
      stability_chart = line_chart(grouped, "Share", "% of organisation deliveries from LA"),
      change_chart = line_chart(grouped, "Count_change", "% change in deliveries from 2023/24"),
      stability_table = wide_trend_table(trend, "All resident LA contributions, 2023/24-2025/26"),
      trend_groups = grouped)
  }), organisations$Key)
  reconciliation <- hospital_mapping |>
    dplyr::group_by(Level, Year) |>
    dplyr::summarise(N = sum(N), .groups = "drop") |>
    tidyr::pivot_wider(names_from = Level, values_from = N)
  stopifnot(all(reconciliation$Trust == reconciliation$Cluster), all(reconciliation$ICB == reconciliation$Cluster))
  list(ready = TRUE, note = "", organisations = organisations, episodes = episodes,
    matrix_data = matrix_data, matrix_episodes = matrix_episodes, matrix_table = matrix_table,
    matrix_values_table = matrix_values_table, sankey_chart = scroll_chart(sankey_chart), sankey_links = sankey_links,
    hospital_mapping = hospital_mapping, la_trends = la_trends, outputs = outputs,
    all_results_table = wide_trend_table(la_trends, "All organisations: hospital deliveries by resident LA", grouped = TRUE))
}

# Save a fresh unavailable result on error so the report cannot reuse stale data.
delivery_geography <- tryCatch(build_delivery_geography(), error = function(e) {
  list(ready = FALSE, note = conditionMessage(e))
})
saveRDS(delivery_geography, ".delivery_geography.rds")
if (isTRUE(delivery_geography$ready)) {
  message("Deliveries by LA and Age Bands prepared: destination matrix, hospital mapping, trends and Sankey.")
} else {
  message("Deliveries by LA and Age Bands need attention: ", delivery_geography$note)
}
