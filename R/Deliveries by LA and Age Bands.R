# Section 8: Deliveries by LA and Age Bands.
# Run after additional_analysis.R, or use R/Run analysis.R.
source("R/report_cohort.R")

# Settings: retain ten additional chart areas; show five additional flows within each 8.1 chart.
delivery_years <- c("2023/24", "2024/25", "2025/26")
delivery_age_bands <- c(report_age_bands, "Unknown")
additional_las_to_show <- 10L
sankey_focus_n <- 10L
sankey_top_n <- 5L
delivery_chart_blue <- "#005EB8"
delivery_palette <- "Dark 3"
cluster_las <- readr::read_csv("config/local_authorities.csv", show_col_types = FALSE)$area_code

# Check required objects and columns once. Data checks are in R/QA.R.
unlink(".delivery_geography.rds")
required <- c("final_cohort_2326", "trust_labels", "icb_labels", "provider_icb",
              "cluster_icbs", "cluster_trusts")
absent <- required[!vapply(required, function(name) exists(name, inherits = TRUE), logical(1))]
if (length(absent)) stop("Missing objects: ", paste(absent, collapse = ", "),
                         ". Run the analysis runner first.", call. = FALSE)
columns <- c("person_id_mother", "uniq_preg_id", "birth_yr", "birth_flag",
             "resident_icb", "la_of_mother", "age_at_birth_mother", "birth_provider", "home_birth")
absent <- setdiff(columns, names(final_cohort_2326))
if (length(absent)) stop("final_cohort_2326 is missing: ", paste(absent, collapse = ", "), call. = FALSE)

build_delivery_geography <- function(years = delivery_years, age_bands = delivery_age_bands) {
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

  # Step 1: keep recorded deliveries and put LA, age and destination into clear groups.
  delivery_episodes <- prepare_report_cohort(final_cohort_2326) |>
    dplyr::filter(Year %in% years, Birth_recorded) |>
    dplyr::mutate(Destination = dplyr::case_when(Home == 1 ~ "Home", Home == 0 ~ Trust,
                                               TRUE ~ "Unknown location"))

  # Step 2: use the local public name lookups. Codes remain the matching keys.
  la_lookup <- readr::read_csv("data/lookups/la_names.csv", show_col_types = FALSE)
  la_lookup <- dplyr::bind_rows(la_lookup,
      data.frame(LA = "Unknown LA", LA_name = "Unknown LA"),
      data.frame(LA = "L99999999", LA_name = "Unknown LA"))
  trust_lookup <- readr::read_csv("data/lookups/trust_names.csv", show_col_types = FALSE) |>
    dplyr::mutate(Trust_name = stringr::str_replace_all(stringr::str_to_title(Trust_name), "\\bNhs\\b", "NHS"))
  main_trusts <- names(trust_labels)[names(trust_labels) != "Other"]
  # Keep the six report labels consistent; the national lookup names every other trust.
  trust_lookup$Trust_name[match(main_trusts, trust_lookup$Trust)] <- unname(trust_labels[main_trusts])
  la_names <- delivery_episodes |>
    dplyr::distinct(LA) |>
    dplyr::left_join(la_lookup, by = "LA") |>
    dplyr::mutate(LA_name = dplyr::coalesce(LA_name, "Unmatched LA"), Resident_LA = LA_name)
  delivery_episodes <- dplyr::left_join(delivery_episodes, la_names, by = "LA")
  trust_codes <- unique(c(main_trusts,
    sort(intersect(delivery_episodes$Trust[delivery_episodes$Home == 0], cluster_trusts))))
  trust_names <- trust_lookup$Trust_name[match(trust_codes, trust_lookup$Trust)]
  organisations <- dplyr::bind_rows(
    data.frame(Key = paste0("trust-", tolower(trust_codes)), Level = "Trust",
               Code = trust_codes, Organisation = trust_names),
    data.frame(Key = paste0("icb-", tolower(cluster_icbs)), Level = "ICB", Code = cluster_icbs,
               Organisation = unname(icb_labels[cluster_icbs])),
    data.frame(Key = "cluster", Level = "Cluster", Code = "Cluster", Organisation = "Combined cluster"))
  hospital_deliveries <- delivery_episodes |>
    dplyr::filter(Home == 0, Trust %in% cluster_trusts)

  # Step 3: make the 2025/26 LA-to-destination matrix, including home births.
  # Include cluster residents and deliveries associated with cluster providers.
  # Provider-associated home births stay home destinations, including outside residents.
  matrix_episodes <- delivery_episodes |>
    dplyr::filter(Year == "2025/26", Resident_ICB %in% cluster_icbs | Trust %in% cluster_trusts)
  destination_matrix <- matrix_episodes |>
    dplyr::count(LA, Resident_LA, Destination, name = "N") |>
    dplyr::group_by(LA) |>
    dplyr::mutate(Origin_total = sum(N), Share = N / Origin_total) |>
    dplyr::ungroup() |>
    dplyr::left_join(trust_lookup, by = c("Destination" = "Trust")) |>
    dplyr::mutate(Delivery_destination = dplyr::case_when(
      Destination == "Home" ~ "Home birth",
      Destination == "Unknown provider" ~ "Unknown delivery provider",
      Destination == "Unknown location" ~ "Home/non-home status unknown",
      !is.na(Trust_name) ~ Trust_name,
      TRUE ~ "Unmatched delivery provider")) |>
    dplyr::arrange(dplyr::desc(Origin_total), Resident_LA, Delivery_destination)
  # Keep major destinations visible, then combine the remaining trusts.
  other_trusts <- destination_matrix |>
    dplyr::filter(!Destination %in% c(main_trusts, "Home", "Unknown provider", "Unknown location")) |>
    dplyr::group_by(Destination) |>
    dplyr::summarise(N = sum(N), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(N), Destination) |>
    dplyr::slice_head(n = sankey_focus_n) |>
    dplyr::pull(Destination)
  destination_matrix <- destination_matrix |>
    dplyr::mutate(Destination_group = dplyr::case_when(
      Destination %in% c(main_trusts, other_trusts, "Home", "Unknown provider", "Unknown location") ~ Destination,
      TRUE ~ "All other trusts"))
  destination_names <- destination_matrix |>
    dplyr::distinct(Destination_group, Delivery_destination) |>
    dplyr::mutate(Delivery_destination = ifelse(Destination_group == "All other trusts",
                                              "All other trusts", Delivery_destination)) |>
    dplyr::distinct()
  grouped_matrix <- destination_matrix |>
    dplyr::mutate(LA = ifelse(LA %in% c("Unknown LA", "L99999999"), "Unknown LA", LA)) |>
    dplyr::group_by(LA, Resident_LA, Destination_group) |>
    dplyr::summarise(N = sum(N), .groups = "drop")
  matrix_wide <- grouped_matrix |>
    tidyr::pivot_wider(names_from = Destination_group, values_from = N, values_fill = 0)
  matrix_columns <- setdiff(names(matrix_wide), c("LA", "Resident_LA"))
  matrix_wide$Total <- rowSums(as.data.frame(matrix_wide[matrix_columns]))
  matrix_wide <- dplyr::arrange(matrix_wide, dplyr::desc(Total), Resident_LA)
  matrix_labels <- stats::setNames(destination_names$Delivery_destination, destination_names$Destination_group)
  matrix_table <- matrix_wide |>
    table_style("2025/26 resident LA to delivery destination", rowname = "Resident_LA") |>
    gt::cols_label(LA = "LA code", .list = as.list(matrix_labels)) |>
    gt::fmt_number(columns = dplyr::all_of(c(matrix_columns, "Total")), decimals = 0) |>
    gt::tab_options(table.font.size = gt::px(11))
  matrix_values_table <- destination_matrix |>
    dplyr::select(Resident_LA, LA, Delivery_destination, Destination, N, Origin_total, Share) |>
    table_style("All recorded LA-to-destination flows, 2025/26") |>
    gt::cols_label(Resident_LA = "Resident LA", Delivery_destination = "Delivery destination",
      LA = "LA code", Destination = "Destination code", N = "Deliveries",
      Origin_total = "Recorded LA deliveries in scope", Share = "% of recorded LA deliveries") |>
    gt::fmt_number(columns = c("N", "Origin_total"), decimals = 0) |>
    gt::fmt_percent(columns = "Share", decimals = 1)

  # Step 4: choose a manageable set of LA and destination charts using overall counts.
  # Within each chart, the five additional flows are chosen again from its own raw data.
  origin_rank <- destination_matrix |>
    dplyr::group_by(LA, Resident_LA) |>
    dplyr::summarise(N = sum(N), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(N), Resident_LA)
  unknown_las <- origin_rank$LA[grepl("^Unknown|^L999", origin_rank$LA)]
  top_las <- c(intersect(cluster_las, origin_rank$LA),
    head(origin_rank$LA[!origin_rank$LA %in% c(cluster_las, unknown_las)], sankey_focus_n))
  sankey_links <- destination_matrix |>
    dplyr::mutate(Origin = dplyr::case_when(LA %in% unknown_las ~ "Unknown LA",
      LA %in% top_las ~ LA, TRUE ~ "All other feeder LAs")) |>
    dplyr::group_by(Origin, Destination_group) |>
    dplyr::summarise(N = sum(N), .groups = "drop")
  origins <- sankey_links |>
    dplyr::group_by(Origin) |>
    dplyr::summarise(N = sum(N), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(N), Origin) |>
    dplyr::pull(Origin)
  origin_labels <- la_names$Resident_LA[match(origins, la_names$LA)]
  origin_labels[origins %in% c("Unknown LA", "All other feeder LAs")] <- origins[origins %in% c("Unknown LA", "All other feeder LAs")]
  destinations <- sankey_links |>
    dplyr::group_by(Destination_group) |>
    dplyr::summarise(N = sum(N), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(N), Destination_group) |>
    dplyr::pull(Destination_group)
  destination_labels <- destination_names$Delivery_destination[match(destinations, destination_names$Destination_group)]
  sankey_links <- sankey_links |>
    dplyr::group_by(Origin) |>
    dplyr::mutate(Origin_total = sum(N), Share = N / Origin_total) |>
    dplyr::ungroup()
  palette <- grDevices::hcl.colors(length(origins), palette = delivery_palette)
  sankey_chart <- plotly::plot_ly(type = "sankey", height = max(650, max(length(origins), length(destinations)) * 32),
    arrangement = "snap", node = list(label = c(origin_labels, destination_labels), pad = 16, thickness = 18,
      x = c(rep(0.01, length(origins)), rep(0.99, length(destinations))),
      y = c(seq(0.02, 0.98, length.out = length(origins)), seq(0.02, 0.98, length.out = length(destinations))),
      color = c(palette, rep("#727272", length(destinations))),
      hovertemplate = "%{label}<br>Deliveries: %{value:,.0f}<extra></extra>"),
    link = list(source = match(sankey_links$Origin, origins) - 1L,
      target = length(origins) + match(sankey_links$Destination_group, destinations) - 1L,
      value = sankey_links$N,
      color = grDevices::adjustcolor(palette[match(sankey_links$Origin, origins)], alpha.f = 0.45),
      customdata = paste0(origin_labels[match(sankey_links$Origin, origins)], " to ",
        destination_labels[match(sankey_links$Destination_group, destinations)],
        "<br>Deliveries: ", scales::comma(sankey_links$N),
        "<br>% of this origin group: ", scales::percent(sankey_links$Share, accuracy = 0.1)),
      hovertemplate = "%{customdata}<extra></extra>")) |>
    plotly::layout(font = list(family = "Arial", size = 11), margin = list(l = 12, r = 12, t = 15, b = 15)) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)

  # Step 4a: keep raw destinations and LAs until each chart's top five has been selected.
  # The focus groups choose which records belong to each chart; they do not group its flows.
  raw_sankey_flows <- destination_matrix |>
    dplyr::mutate(LA = ifelse(LA %in% unknown_las, "Unknown LA", LA),
      Origin_focus = ifelse(LA %in% c(top_las, "Unknown LA"), LA, "All other feeder LAs"),
      Destination_focus = Destination_group) |>
    dplyr::select(LA, Destination, Origin_focus, Destination_focus, N)

  # Select five additional delivery trusts separately for every LA chart.
  # Main trusts, home births and unknown providers never enter the top-five competition.
  sankey_by_la_links <- stats::setNames(lapply(origins, function(origin) {
    raw <- raw_sankey_flows[raw_sankey_flows$Origin_focus == origin, , drop = FALSE]
    local_other_trusts <- raw |>
      dplyr::filter(!Destination %in% c(main_trusts, "Home", "Unknown provider", "Unknown location")) |>
      dplyr::group_by(Destination) |>
      dplyr::summarise(N = sum(N), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(N), Destination) |>
      dplyr::slice_head(n = sankey_top_n) |>
      dplyr::pull(Destination)
    raw |>
      dplyr::mutate(Origin = origin, Destination_group = ifelse(
        Destination %in% c(main_trusts, local_other_trusts, "Home", "Unknown provider", "Unknown location"),
        Destination, "All other trusts")) |>
      dplyr::group_by(Origin, Destination_group) |>
      dplyr::summarise(N = sum(N), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(N), Destination_group)
  }), origins)

  # Select five additional feeder LAs separately for every delivery destination chart.
  sankey_by_destination_links <- stats::setNames(lapply(destinations, function(destination) {
    raw <- raw_sankey_flows[raw_sankey_flows$Destination_focus == destination, , drop = FALSE]
    local_other_las <- raw |>
      dplyr::filter(!LA %in% c(cluster_las, "Unknown LA")) |>
      dplyr::group_by(LA) |>
      dplyr::summarise(N = sum(N), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(N), LA) |>
      dplyr::slice_head(n = sankey_top_n) |>
      dplyr::pull(LA)
    raw |>
      dplyr::mutate(Origin = ifelse(LA %in% c(cluster_las, local_other_las, "Unknown LA"),
        LA, "All other feeder LAs"), Destination_group = destination) |>
      dplyr::group_by(Origin, Destination_group) |>
      dplyr::summarise(N = sum(N), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(N), Origin)
  }), destinations)

  # Use complete name/colour maps: a locally important feeder may not be in the overall chart list.
  all_origin_labels <- stats::setNames(la_names$Resident_LA, la_names$LA)
  all_origin_labels["Unknown LA"] <- "Unknown LA"
  all_origin_labels["All other feeder LAs"] <- "All other feeder LAs"
  all_destination_labels <- destination_matrix |>
    dplyr::distinct(Destination, Delivery_destination)
  all_destination_labels <- stats::setNames(all_destination_labels$Delivery_destination,
    all_destination_labels$Destination)
  all_destination_labels["All other trusts"] <- "All other trusts"
  all_origins <- c(sort(unique(raw_sankey_flows$LA)), "All other feeder LAs")
  origin_colours <- stats::setNames(grDevices::hcl.colors(length(all_origins), palette = delivery_palette), all_origins)

  # Draw names without wrapping, with every node ranked by that chart's own volume.
  small_sankey <- function(links, origin_codes, destination_codes, percentage_label) {
    source_names <- unname(all_origin_labels[origin_codes])
    target_names <- unname(all_destination_labels[destination_codes])
    source_counts <- vapply(origin_codes, function(code) sum(links$N[links$Origin == code]), numeric(1))
    target_counts <- vapply(destination_codes, function(code) sum(links$N[links$Destination_group == code]), numeric(1))
    total <- sum(links$N)
    chart_height <- max(360, max(length(origin_codes), length(destination_codes)) * 42 + 80)
    # Leave a gap between nodes, while keeping their heights proportional to delivery counts.
    node_positions <- function(counts) {
      gap <- 28 / (chart_height - 40)
      0.02 + (cumsum(counts) - counts / 2) / sum(counts) * (0.96 - gap * (length(counts) - 1L)) +
        gap * (seq_along(counts) - 1L)
    }
    node_names <- c(source_names, target_names)
    node_counts <- c(source_counts, target_counts)
    plotly::plot_ly(type = "sankey", height = chart_height, arrangement = "fixed",
      node = list(
        label = node_names,
        pad = 28, thickness = 18,
        x = c(rep(0.02, length(origin_codes)), rep(0.98, length(destination_codes))),
        y = c(node_positions(source_counts), node_positions(target_counts)),
        color = c(unname(origin_colours[origin_codes]), rep("#727272", length(destination_codes))),
        customdata = paste0(node_names, "<br>Recorded deliveries: ", scales::comma(node_counts, accuracy = 1)),
        hovertemplate = "%{customdata}<extra></extra>"),
      link = list(
        source = match(links$Origin, origin_codes) - 1L,
        target = length(origin_codes) + match(links$Destination_group, destination_codes) - 1L,
        value = links$N,
        color = grDevices::adjustcolor(unname(origin_colours[links$Origin]), alpha.f = 0.45),
        customdata = paste0(unname(all_origin_labels[links$Origin]), " to ",
          unname(all_destination_labels[links$Destination_group]),
          "<br>Deliveries: ", scales::comma(links$N, accuracy = 1),
          "<br>% of ", percentage_label, ": ", scales::percent(links$N / total, accuracy = 0.1)),
        hovertemplate = "%{customdata}<extra></extra>")) |>
      plotly::layout(font = list(family = "Arial"), margin = list(l = 12, r = 12, t = 15, b = 15)) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }
  sankey_by_la_grid <- htmltools::div(
    style = "display: grid; grid-template-columns: repeat(2, minmax(600px, 1fr)); gap: 24px; overflow-x: auto;",
    htmltools::tagList(lapply(origins, function(origin) {
      links <- sankey_by_la_links[[origin]]
      htmltools::div(style = "min-width: 0;",
        htmltools::h3(style = "font-size: 15px; margin: 0 0 8px;", origin_labels[match(origin, origins)]),
        small_sankey(links, origin, links$Destination_group, "recorded deliveries from this LA group"))
    })))

  # Step 4b: show each destination's locally selected feeder LAs, including the home-birth chart.
  sankey_by_destination_grid <- htmltools::div(
    style = "display: grid; grid-template-columns: repeat(2, minmax(600px, 1fr)); gap: 24px; overflow-x: auto;",
    htmltools::tagList(lapply(destinations, function(destination) {
      links <- sankey_by_destination_links[[destination]]
      htmltools::div(style = "min-width: 0;",
        htmltools::h3(style = "font-size: 15px; margin: 0 0 8px;", destination_labels[match(destination, destinations)]),
        small_sankey(links, links$Origin, destination, "recorded deliveries at this destination"))
    })))

  # Step 5: count hospital deliveries by LA and age for each trust, ICB and the cluster.
  # Each share uses the receiving organisation's total deliveries in that year.
  hospital_mapping <- dplyr::bind_rows(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, , drop = FALSE]
    keep <- switch(org$Level[[1]],
      Trust = hospital_deliveries$Trust == org$Code[[1]],
      ICB = unname(provider_icb[hospital_deliveries$Trust]) == org$Code[[1]],
      Cluster = rep(TRUE, nrow(hospital_deliveries)))
    org_deliveries <- hospital_deliveries[!is.na(keep) & keep, , drop = FALSE]
    org_las <- unique(org_deliveries$LA)
    if (!length(org_las)) return(NULL)
    counts <- org_deliveries |> dplyr::count(Year, LA, Age_band, name = "N")
    totals <- org_deliveries |> dplyr::count(Year, name = "Organisation_total")
    tidyr::expand_grid(Year = years, LA = org_las, Age_band = age_bands) |>
      dplyr::left_join(counts, by = c("Year", "LA", "Age_band")) |>
      dplyr::left_join(totals, by = "Year") |>
      dplyr::mutate(N = dplyr::coalesce(N, 0L), Organisation_total = dplyr::coalesce(Organisation_total, 0L),
        Share = ifelse(Organisation_total > 0, N / Organisation_total, NA_real_),
        Key = org$Key, Level = org$Level, Organisation = org$Organisation) |>
      dplyr::left_join(la_names, by = "LA")
  }))
  # Step 6: compare each LA's contribution with 2023/24.
  la_trends <- hospital_mapping |>
    dplyr::group_by(Key, Level, Organisation, LA, Resident_LA, Year) |>
    dplyr::summarise(N = sum(N), Organisation_total = dplyr::first(Organisation_total), .groups = "drop") |>
    dplyr::mutate(Share = ifelse(Organisation_total > 0, N / Organisation_total, NA_real_)) |>
    dplyr::group_by(Key, LA) |>
    dplyr::mutate(Base_count = N[Year == "2023/24"], Base_share = Share[Year == "2023/24"],
      Count_change = change_from(N, Base_count), Share_change = Share - Base_share,
      Relative_share_change = change_from(Share, Base_share)) |>
    dplyr::ungroup()
  # 8.3a keeps counts and denominators; 8.3b shows changes against the same base.
  wide_trend_table <- function(data, title, grouped = FALSE, changes = FALSE) {
    metrics <- if (changes) c("Count_change", "Relative_share_change", "Share_change") else c("N", "Organisation_total", "Share")
    identifiers <- if (grouped) c("Organisation", "LA", "Resident_LA") else c("LA", "Resident_LA")
    if (changes) identifiers <- c(identifiers, "Base_count", "Base_share")
    wide <- data |>
      dplyr::select(dplyr::all_of(identifiers), Year, dplyr::all_of(metrics)) |>
      tidyr::pivot_wider(names_from = Year, values_from = dplyr::all_of(metrics), names_sep = "_")
    order <- unlist(lapply(years, function(year) paste0(metrics, "_", year)), use.names = FALSE)
    wide <- dplyr::select(wide, dplyr::all_of(identifiers), dplyr::all_of(order))
    metric_labels <- if (changes) c("Count change (%)", "LA share change (%)", "LA share change (pp)") else
      c("Deliveries from LA", "All deliveries here", "% from LA")
    if (changes) for (column in paste0("Share_change_", years)) wide[[column]] <- wide[[column]] * 100
    labels <- stats::setNames(rep(metric_labels, length(years)), order)
    table <- table_style(wide, title, rowname = "Resident_LA", groupname = if (grouped) "Organisation" else NULL) |>
      gt::cols_label(LA = "LA code", .list = as.list(labels)) |>
      gt::tab_options(table.width = gt::px(1450))
    if (changes) {
      table <- table |>
        gt::cols_label(Base_count = "2023/24 deliveries", Base_share = "2023/24 LA share") |>
        gt::fmt_number(columns = paste0("Share_change_", years), decimals = 1) |>
        gt::fmt_number(columns = "Base_count", decimals = 0) |>
        gt::fmt_percent(columns = c("Base_share", paste0("Count_change_", years),
          paste0("Relative_share_change_", years)), decimals = 1) |>
        gt::tab_source_note(source_note = "Changes use 2023/24. pp = percentage points; n.a. = zero/unavailable base.")
    } else {
      table <- table |>
        gt::fmt_number(columns = c(paste0("N_", years), paste0("Organisation_total_", years)), decimals = 0) |>
        gt::fmt_percent(columns = paste0("Share_", years), decimals = 1)
    }
    for (year in years) table <- gt::tab_spanner(table, label = year,
      columns = paste0(metrics, "_", year), id = paste0("delivery-geography-", year), gather = FALSE)
    table
  }
  line_chart <- function(data, field, y_title, units = "percent") {
    groups <- unique(data$Origin_group)
    colours <- grDevices::hcl.colors(length(groups), palette = delivery_palette)
    chart <- plotly::plot_ly(height = 480)
    for (j in seq_along(groups)) {
      line <- data[data$Origin_group == groups[j], , drop = FALSE]
      line <- line[match(years, line$Year), , drop = FALSE]
      label <- line$Origin_name[[1]]
      values <- line[[field]]
      value_labels <- if (units == "percent") scales::percent(values, accuracy = 0.1) else
        if (units == "pp") paste0(scales::number(values, accuracy = 0.1), " pp") else scales::comma(values, accuracy = 1)
      value_labels[is.na(values)] <- "n.a."
      chart <- plotly::add_trace(chart, x = seq_along(years), y = line[[field]],
        type = "scatter", mode = "lines+markers", name = label,
        line = list(color = colours[j], width = 2), marker = list(color = colours[j], size = 5),
        text = paste0(line$Year, "<br>", label, "<br>Deliveries: ", scales::comma(line$N),
          "<br>", y_title, ": ", value_labels),
        hovertemplate = "%{text}<extra></extra>", connectgaps = FALSE)
    }
    plotly::layout(chart, xaxis = list(title = "Financial year", tickmode = "array", tickvals = 1:3, ticktext = years),
      yaxis = list(title = y_title, tickformat = if (units == "percent") ".0%" else if (units == "pp") ".1f" else ",.0f", zeroline = TRUE),
      legend = list(orientation = "h", x = 0, y = -0.25),
      margin = list(l = 75, r = 20, t = 20, b = 185), font = list(family = "Arial", size = 11)) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }
  # Step 7: make the organisation charts and the tables shown in section 8.
  age_colours <- grDevices::hcl.colors(length(age_bands), palette = delivery_palette)
  organisation_plots <- stats::setNames(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, , drop = FALSE]
    ages <- hospital_mapping[hospital_mapping$Key == org$Key & hospital_mapping$Year == "2025/26", , drop = FALSE]
    trend <- la_trends[la_trends$Key == org$Key, , drop = FALSE]
    if (!nrow(ages)) {
      empty <- htmltools::div("No recorded hospital deliveries in the three years.")
      table <- table_style(data.frame(Note = "No recorded hospital deliveries in the three years."), org$Organisation)
      return(list(origin_chart = empty, age_chart = empty, table = table, age_table = table,
        count_chart = empty, stability_chart = empty, change_chart = empty, share_change_chart = empty,
        count_table = table, change_table = table))
    }
    origin <- trend |>
      dplyr::filter(Year == "2025/26", N > 0) |>
      dplyr::arrange(dplyr::desc(N), Resident_LA)
    # Use the same LA groups in every chart for this organisation.
    unknown <- unique(trend$LA[grepl("^Unknown|^L999", trend$LA)])
    ranked_las <- trend |> dplyr::filter(Year == "2025/26", N > 0) |> dplyr::arrange(dplyr::desc(N), LA)
    visible_las <- c(intersect(cluster_las, trend$LA),
      head(ranked_las$LA[!ranked_las$LA %in% c(cluster_las, unknown)], additional_las_to_show))
    grouped <- trend |>
      dplyr::mutate(Origin_group = dplyr::case_when(LA %in% unknown ~ "Unknown LA",
        LA %in% visible_las ~ LA, TRUE ~ "All other feeder LAs"),
        Origin_name = dplyr::case_when(LA %in% unknown ~ "Unknown LA",
          LA %in% visible_las ~ Resident_LA, TRUE ~ "All other feeder LAs")) |>
      dplyr::group_by(Origin_group, Origin_name, Year) |>
      dplyr::summarise(N = sum(N), Organisation_total = dplyr::first(Organisation_total), .groups = "drop") |>
      dplyr::mutate(Share = ifelse(Organisation_total > 0, N / Organisation_total, NA_real_)) |>
      dplyr::group_by(Origin_group) |>
      dplyr::mutate(Count_change = change_from(N, N[Year == "2023/24"]),
        Share_change_pp = (Share - Share[Year == "2023/24"]) * 100) |>
      dplyr::ungroup() |>
      dplyr::arrange(match(Origin_group, c(visible_las, "All other feeder LAs", "Unknown LA")), match(Year, years))
    chart_origin <- grouped |>
      dplyr::filter(Year == "2025/26", N > 0) |>
      dplyr::transmute(LA = Origin_group, Resident_LA = Origin_name, N, Organisation_total, Share) |>
      dplyr::arrange(dplyr::desc(N), LA)
    if (!nrow(origin)) {
      origin_chart <- htmltools::div("No recorded hospital deliveries in 2025/26.")
      age_chart <- origin_chart
    } else {
      origin_chart <- plotly::plot_ly(chart_origin, height = max(380, nrow(chart_origin) * 24 + 100),
        x = ~Share, y = ~LA, type = "bar", orientation = "h",
        marker = list(color = chart_origin$Share, colorscale = list(c(0, "#DCEAF4"), c(1, delivery_chart_blue)),
          cmin = 0, cmax = max(chart_origin$Share), showscale = FALSE),
        hovertext = ~paste0(Resident_LA, "<br>Deliveries: ", scales::comma(N), "<br>% of organisation: ", scales::percent(Share, accuracy = 0.1)),
        hovertemplate = "%{hovertext}<extra></extra>", showlegend = FALSE) |>
        plotly::layout(xaxis = list(title = "% of all hospital deliveries at this organisation", tickformat = ".0%"),
          yaxis = list(title = "", categoryorder = "array", categoryarray = chart_origin$LA, autorange = "reversed",
            tickmode = "array", tickvals = chart_origin$LA, ticktext = chart_origin$Resident_LA, automargin = TRUE),
          margin = list(l = 30, r = 25, t = 15, b = 65), font = list(family = "Arial")) |>
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
      plot_ages <- ages |>
        dplyr::mutate(LA = dplyr::case_when(LA %in% unknown ~ "Unknown LA",
          LA %in% visible_las ~ LA, TRUE ~ "All other feeder LAs")) |>
        dplyr::group_by(LA, Age_band) |>
        dplyr::summarise(N = sum(N), .groups = "drop") |>
        dplyr::filter(LA %in% chart_origin$LA) |>
        dplyr::left_join(dplyr::select(chart_origin, LA, Resident_LA), by = "LA") |>
        dplyr::group_by(LA) |>
        dplyr::mutate(LA_total = sum(N), Age_share = N / LA_total) |>
        dplyr::ungroup() |>
        dplyr::arrange(match(LA, chart_origin$LA), match(Age_band, age_bands))
      age_chart <- plotly::plot_ly(height = max(380, nrow(chart_origin) * 24 + 130))
      for (j in seq_along(age_bands)) {
        band <- plot_ages[plot_ages$Age_band == age_bands[j], , drop = FALSE]
        age_chart <- plotly::add_trace(age_chart, x = band$Age_share, y = band$LA,
          type = "bar", orientation = "h", name = age_bands[j], marker = list(color = age_colours[j]),
          hovertext = paste0(band$Resident_LA, "<br>Age: ", age_bands[j], "<br>Deliveries: ", scales::comma(band$N),
            "<br>All deliveries from this LA here: ", scales::comma(band$LA_total),
            "<br>% within this LA: ", scales::percent(band$Age_share, accuracy = 0.1)),
          hovertemplate = "%{hovertext}<extra></extra>")
      }
      age_chart <- age_chart |>
        plotly::layout(barmode = "stack", xaxis = list(title = "% of this LA's deliveries here", tickformat = ".0%", range = c(0, 1)),
          yaxis = list(title = "", autorange = "reversed", categoryorder = "array", categoryarray = chart_origin$LA,
            tickmode = "array", tickvals = chart_origin$LA, ticktext = chart_origin$Resident_LA, automargin = TRUE),
          showlegend = FALSE,
          margin = list(l = 30, r = 25, t = 15, b = 65), font = list(family = "Arial")) |>
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    }
    origin_table <- origin |>
      dplyr::select(Resident_LA, LA, N, Organisation_total, Share) |>
      table_style("2025/26 hospital deliveries by resident LA") |>
      gt::cols_label(Resident_LA = "Resident LA", LA = "LA code", N = "Deliveries", Organisation_total = "All deliveries here", Share = "% from LA") |>
      gt::fmt_number(columns = c("N", "Organisation_total"), decimals = 0) |>
      gt::fmt_percent(columns = "Share", decimals = 1)
    age_wide <- ages |>
      dplyr::select(LA, Resident_LA, Age_band, N) |>
      tidyr::pivot_wider(names_from = Age_band, values_from = N, values_fill = 0) |>
      dplyr::select(LA, Resident_LA, dplyr::all_of(age_bands))
    age_wide$Total <- rowSums(as.data.frame(age_wide[age_bands]))
    age_wide <- age_wide |>
      dplyr::filter(Total > 0) |>
      dplyr::arrange(dplyr::desc(Total), Resident_LA)
    age_table <- table_style(age_wide, "2025/26 hospital delivery counts: LA by age band", rowname = "Resident_LA") |>
      gt::cols_label(LA = "LA code") |>
      gt::fmt_number(columns = dplyr::all_of(c(age_bands, "Total")), decimals = 0)
    list(origin_chart = scroll_chart(origin_chart), age_chart = scroll_chart(age_chart), table = origin_table, age_table = age_table,
      count_chart = line_chart(grouped, "N", "Deliveries", units = "count"),
      stability_chart = line_chart(grouped, "Share", "% of organisation deliveries from LA"),
      change_chart = line_chart(grouped, "Count_change", "% change in deliveries from 2023/24"),
      share_change_chart = line_chart(grouped, "Share_change_pp", "Change in LA contribution (percentage points)", units = "pp"),
      count_table = wide_trend_table(trend, "All resident LA contributions, 2023/24-2025/26"),
      change_table = wide_trend_table(trend, "Change in all resident LA contributions from 2023/24", changes = TRUE),
      trend_groups = grouped, visible_las = visible_las, age_mix = if (nrow(origin)) plot_ages else NULL)
  }), organisations$Key)
  # Step 8: compare the six main trusts, then the two ICBs and combined cluster.
  # Wide charts and automatic label margins keep names on one line; scroll if needed.
  mapping_groups <- list(
    Trust = organisations[organisations$Level == "Trust" &
      organisations$Code %in% setdiff(names(trust_labels), "Other"), , drop = FALSE],
    ICB_Cluster = organisations[organisations$Level %in% c("ICB", "Cluster"), , drop = FALSE])
  age_key <- htmltools::div(
    style = "display: flex; flex-wrap: wrap; gap: 12px 24px; margin-bottom: 20px;",
    htmltools::tagList(lapply(seq_along(age_bands), function(i) {
      htmltools::span(style = "display: inline-flex; align-items: center; gap: 6px;",
        htmltools::span(style = paste0("display: inline-block; width: 16px; height: 12px; background: ",
          age_colours[i], ";"), `aria-hidden` = "true"), age_bands[i])
    })))
  mapping_grid <- function(rows, field) {
    htmltools::tagList(if (field == "age_chart") age_key,
      htmltools::div(
        style = "display: grid; grid-template-columns: repeat(3, minmax(600px, 1fr)); gap: 24px; overflow-x: auto;",
        htmltools::tagList(lapply(seq_len(nrow(rows)), function(i) {
          htmltools::div(style = "min-width: 0;",
            htmltools::h4(style = "margin: 0 0 12px;", rows$Organisation[i]),
            organisation_plots[[rows$Key[i]]][[field]])
        }))))
  }
  mapping_grids <- lapply(mapping_groups, function(rows) list(
    origin_chart = mapping_grid(rows, "origin_chart"),
    age_chart = mapping_grid(rows, "age_chart")))
  # Keep the original full LA tables together under organisation headings.
  mapping_table_rows <- dplyr::bind_rows(mapping_groups)
  mapping_tables <- stats::setNames(lapply(c("table", "age_table"), function(field) {
    htmltools::tagList(lapply(seq_len(nrow(mapping_table_rows)), function(i) {
      htmltools::tagList(htmltools::h3(mapping_table_rows$Organisation[i]),
        organisation_plots[[mapping_table_rows$Key[i]]][[field]])
    }))
  }), c("table", "age_table"))
  list(organisations = organisations, episodes = delivery_episodes,
    matrix_data = destination_matrix, matrix_episodes = matrix_episodes, matrix_table = matrix_table,
    grouped_matrix = grouped_matrix, visible_las = top_las, visible_other_trusts = other_trusts,
    la_lookup = la_lookup, trust_lookup = trust_lookup,
    matrix_values_table = matrix_values_table, sankey_chart = scroll_chart(sankey_chart), sankey_links = sankey_links,
    sankey_by_la_grid = sankey_by_la_grid, sankey_by_destination_grid = sankey_by_destination_grid,
    sankey_by_la_links = sankey_by_la_links, sankey_by_destination_links = sankey_by_destination_links,
    sankey_focus_n = sankey_focus_n, sankey_top_n = sankey_top_n,
    hospital_mapping = hospital_mapping, la_trends = la_trends, outputs = organisation_plots,
    mapping_grids = mapping_grids, mapping_tables = mapping_tables,
    all_results_table = wide_trend_table(la_trends, "All organisations: hospital deliveries by resident LA", grouped = TRUE),
    all_changes_table = wide_trend_table(la_trends, "All organisations: changes from 2023/24", grouped = TRUE, changes = TRUE))
}

# Save the finished charts and tables for index.qmd.
# Errors stop here; the previous saved output was removed before preparation.
delivery_geography <- build_delivery_geography()
saveRDS(delivery_geography, ".delivery_geography.rds")
message("Deliveries by LA and Age Bands prepared: destination matrix, hospital mapping, trends and Sankey.")
