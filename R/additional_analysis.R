# Report sections 4-7: patient flows and delivery pathways.
# Run this after Sarah's two preparation scripts, or use R/Run analysis.R.

# Settings: years, organisation labels and chart colours
analysis_years <- 2023:2025
latest_year <- max(analysis_years)
cluster_icbs <- c("QUA", "QHL")
cluster_label <- "Combined cluster"
flow_palette <- c("#005EB8", "#D55E00", "#00856A", "#8E5EA2",
                  "#B07300", "#C44E52", "#4477AA", "#727272")
outcome_columns <- c("Within", "Outside", "No_delivery", "Unknown")
outcome_labels <- c("Delivery within cluster", "Delivery outside cluster",
                    "No recorded delivery", "Delivery provider unknown")
outcome_colours <- c("#00856A", "#D55E00", "#727272", "#8E5EA2")
trend_grid_columns <- c(Trust = 3L, ICB_Cluster = 3L)
main_chart_colour <- "#005EB8"

icb_labels <- c(
  QHL = "Birmingham and Solihull ICB", QUA = "Black Country ICB",
  QNC = "Staffordshire and Stoke-on-Trent ICB",
  QWU = "Coventry and Warwickshire ICB",
  QGH = "Herefordshire and Worcestershire ICB",
  QOC = "Shropshire, Telford and Wrekin ICB",
  QJ2 = "Derby and Derbyshire ICB", Other = "Other ICBs"
)
trust_labels <- c(
  RBK = "Walsall Healthcare NHS Trust",
  RL4 = "The Royal Wolverhampton NHS Trust",
  RNA = "The Dudley Group NHS Foundation Trust",
  RQ3 = "Birmingham Women's and Children's NHS Foundation Trust",
  RRK = "University Hospitals Birmingham NHS Foundation Trust",
  RXK = "Sandwell and West Birmingham NHS Trust", Other = "Other trusts"
)

icb_source_order <- c("QJ2", "QWU", "QUA", "QHL", "Other", "QGH", "QOC", "QNC")
icb_target_order <- c("Other", "QUA", "QWU", "QJ2", "QHL", "QNC", "QGH", "QOC")
trust_source_order <- c("RNA", "RXK", "RBK", "RL4", "Other", "RRK", "RQ3")
trust_target_order <- c("RNA", "RL4", "RQ3", "RXK", "Other", "RBK", "RRK")

# Trust-to-ICB grouping from NHS ODS etr.csv
icb_provider_codes <- list(
  QGH = c("R1A", "RLQ", "RWP"),
  QHL = c("RQ3", "RRJ", "RRK", "RXT", "RYW"),
  QJ2 = c("RFS", "RTG", "RXM", "RY8"),
  QNC = c("RJE", "RLY", "RRE"),
  QOC = c("R1D", "RL1", "RXW"),
  QUA = c("RBK", "RL4", "RNA", "RXK", "RYA", "RYK"),
  QWU = c("RJC", "RKB", "RLT", "RYG")
)
provider_icb <- stats::setNames(
  rep(names(icb_provider_codes), lengths(icb_provider_codes)),
  unlist(icb_provider_codes, use.names = FALSE)
)
cluster_trusts <- names(provider_icb)[provider_icb %in% cluster_icbs]

# Check the required inputs once; data QA is in R/QA.R.
unlink(".patient_flows.rds")
required_objects <- c("icb_matrix_data", "trust_matrix_data", "final_cohort_2326")
missing_objects <- required_objects[!vapply(required_objects, function(name) exists(name, inherits = TRUE), logical(1))]
if (length(missing_objects)) stop("Run the preparation scripts first. Missing: ",
                                  paste(missing_objects, collapse = ", "), call. = FALSE)
required_columns <- list(
  final_cohort_2326 = c("person_id_mother", "uniq_preg_id", "birth_yr", "birth_date",
                      "booking_provider", "birth_provider", "resident_icb", "antenatal_app_date", "home_birth"),
  icb_matrix_data = c("resident_icb_interest", "birth_icb_interest", "n"),
  trust_matrix_data = c("booking_provider_grouped", "birth_provider_grouped", "n"))
for (name in names(required_columns)) {
  missing_columns <- setdiff(required_columns[[name]], names(get(name)))
  if (length(missing_columns)) stop(name, " is missing: ",
                                    paste(missing_columns, collapse = ", "), call. = FALSE)
}

# Section 4: patient flows in 2025/26
# This bit makes the residence-to-birth and booking-to-birth Sankeys.
# Clean the supplied codes and group providers outside the labelled areas as Other.
# Each call below names its source, destination and count explicitly.
make_flows <- function(source, target, count, keep_codes) {
  source_code <- toupper(trimws(as.character(source)))
  target_code <- toupper(trimws(as.character(target)))
  count <- as.numeric(as.character(count))
  valid <- !is.na(source_code) & nzchar(source_code) &
    !is.na(target_code) & nzchar(target_code) & is.finite(count) & count > 0
  links <- data.frame(
    source = ifelse(source_code[valid] %in% keep_codes, source_code[valid], "Other"),
    target = ifelse(target_code[valid] %in% keep_codes, target_code[valid], "Other"),
    n = count[valid])
  list(links = links)
}


# Sarah's matrices are already aggregated: use their n column as the count.
icb_flow <- make_flows(
  icb_matrix_data$resident_icb_interest,
  icb_matrix_data$birth_icb_interest,
  icb_matrix_data$n,
  names(icb_labels)[names(icb_labels) != "Other"]
)
trust_flow <- make_flows(
  trust_matrix_data$booking_provider_grouped,
  trust_matrix_data$birth_provider_grouped,
  trust_matrix_data$n,
  names(trust_labels)[names(trust_labels) != "Other"]
)

# Sankey charts

make_sankey <- function(flow, labels, source_suffix, target_suffix,
                        source_order, target_order) {
  links <- flow$links
  codes <- unique(c(source_order, target_order))
  colours <- setNames(flow_palette[seq_along(codes)], codes)
  display_label <- function(x) unname(labels[x])
  total <- sum(links$n)

  source_nodes <- source_order[source_order %in% links$source]
  target_nodes <- target_order[target_order %in% links$target]
  node_codes <- c(source_nodes, target_nodes)
  node_labels <- c(paste0(display_label(source_nodes), source_suffix),
                   paste0(display_label(target_nodes), target_suffix))
  node_totals <- c(vapply(source_nodes, function(x) sum(links$n[links$source == x]), numeric(1)),
                   vapply(target_nodes, function(x) sum(links$n[links$target == x]), numeric(1)))
  from_id <- match(links$source, source_nodes) - 1L
  to_id <- length(source_nodes) + match(links$target, target_nodes) - 1L
  source_totals <- vapply(links$source, function(x) sum(links$n[links$source == x]), numeric(1))
  hover <- paste0(display_label(links$source), " to ", display_label(links$target),
                  "<br>Records: ", scales::comma(links$n),
                  "<br>", scales::percent(links$n / source_totals, accuracy = 0.1),
                  " of records from this origin",
                  "<br>", scales::percent(links$n / total, accuracy = 0.1),
                  " of all records")

  plotly::plot_ly(
    type = "sankey", height = max(500, length(node_codes) * 38),
    orientation = "h", arrangement = "snap",
    node = list(
      label = node_labels, color = unname(colours[node_codes]),
      customdata = scales::percent(node_totals / total, accuracy = 0.1),
      pad = 14, thickness = 18,
      line = list(color = "rgba(44,40,37,0.35)", width = 0.5),
      hovertemplate = "%{label}<br>Records: %{value:,.0f}<br>%{customdata} of all records<extra></extra>"
    ),
    link = list(
      source = from_id, target = to_id, value = links$n,
      color = unname(grDevices::adjustcolor(colours[links$source], alpha.f = 0.48)),
      customdata = hover, hovertemplate = "%{customdata}<extra></extra>"
    )
  ) |>
    plotly::layout(
      font = list(family = "Arial", size = 11, color = "#2c2825"),
      margin = list(l = 12, r = 12, t = 18, b = 18),
      paper_bgcolor = "white", plot_bgcolor = "white"
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

# Chart objects
icb_sankey <- make_sankey(
  icb_flow, icb_labels, " (residence)", " (birth)",
  icb_source_order, icb_target_order
)
trust_sankey <- make_sankey(
  trust_flow, trust_labels, " (booking)", " (birth)",
  trust_source_order, trust_target_order
)

# Section 5: patient flows by financial year
# This bit repeats the two Sankeys for each year and makes their tables.

# Dates: preserve Date/POSIX values; ISO or UK text
parse_activity_date <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x))
  text <- trimws(as.character(x))
  date <- as.Date(text, format = "%Y-%m-%d")
  missing <- is.na(date) & !is.na(text)
  date[missing] <- as.Date(text[missing], format = "%d/%m/%Y")
  date
}

# Birth financial year: existing cohort labels, e.g. 2025/26
birth_year_label <- trimws(as.character(final_cohort_2326$birth_yr))
financial_year_start <- rep(NA_integer_, length(birth_year_label))
has_birth_year <- !is.na(birth_year_label) & grepl("^[0-9]{4}/[0-9]{2}$", birth_year_label)
financial_year_start[has_birth_year] <- as.integer(substr(birth_year_label[has_birth_year], 1, 4))
birth_date <- parse_activity_date(final_cohort_2326$birth_date)


flow_code <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[is.na(x) | !nzchar(x)] <- "Other"
  x
}

make_cohort_year_flows <- function(start_year) {
  cohort <- final_cohort_2326[!is.na(financial_year_start) &
                                financial_year_start == start_year, ]

  booking_provider <- flow_code(cohort$booking_provider)
  birth_provider <- flow_code(cohort$birth_provider)
  birth_icb <- unname(provider_icb[birth_provider])
  birth_icb[is.na(birth_icb)] <- "Other"

  # The earlier-year cohort has one episode per row, so each contributes one.
  trust_year <- make_flows(
    booking_provider, birth_provider, rep(1, nrow(cohort)),
    names(trust_labels)[names(trust_labels) != "Other"]
  )
  icb_year <- make_flows(
    flow_code(cohort$resident_icb), birth_icb, rep(1, nrow(cohort)),
    names(icb_labels)[names(icb_labels) != "Other"]
  )
  trust_year$links <- stats::aggregate(n ~ source + target, trust_year$links, sum)
  icb_year$links <- stats::aggregate(n ~ source + target, icb_year$links, sum)
  list(icb = icb_year, trust = trust_year)
}

# Existing matrices supply 2025/26; cohort supplies earlier years

make_year_sankeys <- function(start_year) {
  flows <- make_cohort_year_flows(start_year)
  list(
    icb_flow = flows$icb, trust_flow = flows$trust,
    icb_sankey = make_sankey(flows$icb, icb_labels, " (residence)", " (birth)",
                             icb_source_order, icb_target_order),
    trust_sankey = make_sankey(flows$trust, trust_labels, " (booking)", " (birth)",
                               trust_source_order, trust_target_order)
  )
}

by_year <- list(
  "2023/24" = make_year_sankeys(2023L),
  "2024/25" = make_year_sankeys(2024L),
  "2025/26" = list(icb_flow = icb_flow, trust_flow = trust_flow,
                   icb_sankey = icb_sankey, trust_sankey = trust_sankey)
)

# Section 5 tables: use the same counts and order as the Sankeys
make_flow_table <- function(flow, labels, source_label, target_label,
                            source_order, target_order, year) {
  flow_counts <- stats::aggregate(n ~ source + target, flow$links, sum)
  flow_counts <- flow_counts[order(match(flow_counts$source, source_order),
                                   match(flow_counts$target, target_order)), ]
  flow_counts$origin_share <- flow_counts$n / ave(flow_counts$n, flow_counts$source, FUN = sum)
  flow_counts$total_share <- flow_counts$n / sum(flow_counts$n)
  flow_counts$source <- unname(labels[flow_counts$source])
  flow_counts$target <- unname(labels[flow_counts$target])
  gt::gt(flow_counts) |>
    gt::tab_header(title = paste(source_label, "to", target_label), subtitle = year) |>
    gt::cols_label(source = source_label, target = target_label, n = "Records",
                   origin_share = "% of origin", total_share = "% of all records") |>
    gt::fmt_number(columns = "n", decimals = 0, use_seps = TRUE) |>
    gt::fmt_percent(columns = c("origin_share", "total_share"), decimals = 1) |>
    gt::tab_options(table.font.names = c("Open Sans", "Arial", "sans-serif"))
}

for (year in names(by_year)) {
  by_year[[year]]$icb_table <- make_flow_table(
    by_year[[year]]$icb_flow, icb_labels, "ICB of residence", "ICB of birth",
    icb_source_order, icb_target_order, year
  )
  by_year[[year]]$trust_table <- make_flow_table(
    by_year[[year]]$trust_flow, trust_labels, "Booking trust", "Birth trust",
    trust_source_order, trust_target_order, year
  )
}

# Section 6: delivery activity in the Combined cluster
# This bit counts each mother/pregnancy pair once and groups delivery locations.
activity_cohort <- as.data.frame(final_cohort_2326)
activity_cohort$delivery_date <- birth_date
activity_cohort$delivery_year <- financial_year_start
activity_cohort$birth_code <- flow_code(activity_cohort$birth_provider)
activity_cohort$booking_code <- flow_code(activity_cohort$booking_provider)
activity_cohort$residence_code <- flow_code(activity_cohort$resident_icb)

# Booking financial year; outcomes may fall in a later year
activity_cohort$booking_date <- parse_activity_date(activity_cohort$antenatal_app_date)
activity_cohort$booking_year <- as.integer(format(activity_cohort$booking_date, "%Y")) -
  as.integer(as.integer(format(activity_cohort$booking_date, "%m")) < 4L)

# Keep identified mother/pregnancy pairs; duplicate and coverage checks are in R/QA.R.
identified <- !is.na(activity_cohort$person_id_mother) &
  nzchar(trimws(as.character(activity_cohort$person_id_mother))) &
  !is.na(activity_cohort$uniq_preg_id) &
  nzchar(trimws(as.character(activity_cohort$uniq_preg_id)))
episode_fields <- c("person_id_mother", "uniq_preg_id", "delivery_date",
                    "delivery_year", "birth_code", "booking_code", "residence_code",
                    "booking_date", "booking_year", "home_birth")
activity_episodes <- unique(activity_cohort[identified, episode_fields])

# Shared pathway definition for the latest-year and three-year outputs.
classify_pathway_outcome <- function(episodes) {
  # Home births remain in all starts, but are outside cluster-trust delivery locations.
  ifelse(is.na(episodes$delivery_year) & is.na(episodes$delivery_date), "No recorded delivery",
    ifelse(episodes$home_birth %in% 1L, "Outside cluster",
      ifelse(episodes$birth_code %in% cluster_trusts, "Within cluster",
        ifelse(grepl("^R[A-Z0-9]{2}$", episodes$birth_code), "Outside cluster", "Unknown provider"))))
}

cluster_activity <- list(
  note = "Combined cluster: Black Country and Birmingham and Solihull. Trust delivery totals exclude flagged home births; pathway starts retain them as outside cluster trust locations. Each mother/pregnancy pair is counted once; delivery year comes from birth_yr."
)

safe_share <- function(n, total) ifelse(rep_len(total, length(n)) > 0, n / total, NA_real_)
count_text <- function(x) scales::comma(x, accuracy = 1)
percent_text <- function(x) ifelse(is.na(x), "not available", scales::percent(x, accuracy = 0.1))
activity_episodes$residence_group <- ifelse(
  activity_episodes$residence_code %in% names(icb_labels)[names(icb_labels) != "Other"],
  activity_episodes$residence_code,
  ifelse(grepl("^Q[A-Z0-9]{2}$", activity_episodes$residence_code), "Other", "Unknown")
)
activity_episodes$residence_type <- ifelse(
  activity_episodes$residence_group %in% cluster_icbs, "Cluster residents",
  ifelse(activity_episodes$residence_group == "Unknown", "Unknown residence", "Outside-cluster residents")
)
deliveries <- activity_episodes[
  !is.na(activity_episodes$delivery_year) &
    activity_episodes$delivery_year %in% analysis_years &
    activity_episodes$home_birth %in% 0L &
    activity_episodes$birth_code %in% cluster_trusts, ]
deliveries_2526 <- deliveries[deliveries$delivery_year == latest_year, ]
total_2526 <- nrow(deliveries_2526)
resident_types <- c("Cluster residents", "Outside-cluster residents", "Unknown residence")
residence_totals <- data.frame(
  Residence = c("All cluster deliveries", resident_types),
  Episodes = c(total_2526, vapply(resident_types, function(x)
    sum(deliveries_2526$residence_type == x), integer(1)))
)
residence_totals$Share <- safe_share(residence_totals$Episodes, total_2526)
cluster_activity$total_table <- gt::gt(residence_totals) |>
  gt::tab_header(title = "Delivery episodes within the cluster", subtitle = "2025/26") |>
  gt::cols_label(Share = "% of cluster deliveries") |>
  gt::fmt_number(columns = "Episodes", decimals = 0) |>
  gt::fmt_percent(columns = "Share", decimals = 1)
cluster_activity$total_text <- paste0(count_text(total_2526),
  " delivery episodes recorded within the cluster in 2025/26.")

local_deliveries <- data.frame(
  ICB = unname(icb_labels[cluster_icbs]),
  Episodes = vapply(cluster_icbs, function(x)
    sum(deliveries_2526$residence_group == x), integer(1))
)
local_deliveries$Share <- safe_share(local_deliveries$Episodes, total_2526)
cluster_activity$local_table <- gt::gt(local_deliveries) |>
  gt::tab_header(title = "Cluster residents delivering within the cluster", subtitle = "2025/26") |>
  gt::cols_label(ICB = "ICB of residence", Share = "% of cluster deliveries") |>
  gt::fmt_number(columns = "Episodes", decimals = 0) |>
  gt::fmt_percent(columns = "Share", decimals = 1)
cluster_activity$local_text <- paste0(count_text(sum(local_deliveries$Episodes)), " (",
  percent_text(safe_share(sum(local_deliveries$Episodes), total_2526)),
  ") of cluster delivery episodes were for cluster residents.")

# Outside residents: named ICBs and Other; unknown residence kept separate
outside_deliveries <- deliveries_2526[deliveries_2526$residence_type == "Outside-cluster residents", ]
outside_codes <- c(setdiff(names(icb_labels), cluster_icbs))
outside_summary <- data.frame(
  ICB = unname(icb_labels[outside_codes]),
  Episodes = vapply(outside_codes, function(x) sum(outside_deliveries$residence_group == x), integer(1))
)
outside_summary <- outside_summary[order(-outside_summary$Episodes), ]
outside_summary$Share <- safe_share(outside_summary$Episodes, nrow(outside_deliveries))
outside_summary$Cluster_share <- safe_share(outside_summary$Episodes, total_2526)
cluster_activity$outside_table <- gt::gt(outside_summary) |>
  gt::tab_header(title = "Outside-cluster residents delivering within the cluster", subtitle = "2025/26") |>
  gt::cols_label(ICB = "ICB of residence", Share = "% of outside-resident deliveries",
                 Cluster_share = "% of cluster deliveries") |>
  gt::fmt_number(columns = "Episodes", decimals = 0) |>
  gt::fmt_percent(columns = c("Share", "Cluster_share"), decimals = 1)
cluster_activity$outside_diagram <- plotly::plot_ly(
  outside_summary, x = ~Episodes, y = ~ICB, type = "bar", orientation = "h", height = 420,
  marker = list(color = main_chart_colour),
  hovertemplate = "%{y}<br>Delivery episodes: %{x:,.0f}<extra></extra>"
) |>
  plotly::layout(xaxis = list(title = "Delivery episodes"),
    yaxis = list(title = "", categoryorder = "array", categoryarray = rev(outside_summary$ICB)),
    margin = list(l = 300), font = list(family = "Arial")) |>
  plotly::config(displayModeBar = FALSE, responsive = TRUE)
cluster_activity$outside_text <- paste0(count_text(nrow(outside_deliveries)), " (",
  percent_text(safe_share(nrow(outside_deliveries), total_2526)),
  ") of cluster deliveries were for residents outside the cluster.",
  if (nrow(outside_deliveries) > 0) paste0(" The largest origin group was ",
    paste(outside_summary$ICB[outside_summary$Episodes == max(outside_summary$Episodes)], collapse = " and "),
    ", with ", count_text(max(outside_summary$Episodes)), " episodes per group.") else "",
  " Residence was unknown for ", count_text(sum(deliveries_2526$residence_type == "Unknown residence")),
  " episodes; these are excluded from the outside-resident totals.")

# Same delivery-year definition across all three years
delivery_trend <- do.call(rbind, lapply(analysis_years, function(year) {
  year_deliveries <- deliveries[deliveries$delivery_year == year, ]
  data.frame(Year = paste0(year, "/", substr(year + 1L, 3, 4)),
    Total = nrow(year_deliveries), Local = sum(year_deliveries$residence_type == "Cluster residents"),
    Outside = sum(year_deliveries$residence_type == "Outside-cluster residents"),
    Unknown = sum(year_deliveries$residence_type == "Unknown residence"))
}))
delivery_trend$Outside_share <- safe_share(delivery_trend$Outside, delivery_trend$Total)
delivery_trend$Change <- c(NA_real_, diff(delivery_trend$Total))
cluster_activity$trend_table <- gt::gt(delivery_trend) |>
  gt::cols_label(Year = "Financial year", Total = "Cluster deliveries", Local = "Cluster residents",
    Outside = "Outside-cluster residents", Unknown = "Unknown residence",
    Outside_share = "Outside-resident share", Change = "Change in deliveries from previous year") |>
  gt::fmt_number(columns = c("Total", "Local", "Outside", "Unknown", "Change"), decimals = 0) |>
  gt::fmt_percent(columns = "Outside_share", decimals = 1) |>
  gt::sub_missing(missing_text = "—")
cluster_activity$trend_text <- paste0(
  "Cluster deliveries changed from ", count_text(delivery_trend$Total[1]), " in 2023/24 to ",
  count_text(delivery_trend$Total[3]), " in 2025/26 (",
  if (delivery_trend$Total[3] >= delivery_trend$Total[1]) "+" else "", count_text(delivery_trend$Total[3] - delivery_trend$Total[1]),
  "). The outside-resident share changed from ", percent_text(delivery_trend$Outside_share[1]),
  " to ", percent_text(delivery_trend$Outside_share[3]), ".")

# Antenatal starts: booking year 2025/26; all recorded delivery dates
starts <- activity_episodes[
  !is.na(activity_episodes$booking_year) & activity_episodes$booking_year == latest_year &
    activity_episodes$booking_code %in% cluster_trusts, ]
starts$outcome <- classify_pathway_outcome(starts)
booking_codes <- unique(c(names(trust_labels)[names(trust_labels) != "Other"], starts$booking_code))
pathway_summary <- function(codes, level) {
  do.call(rbind, lapply(codes, function(code) {
    keep <- if (level == "Trust") starts$booking_code == code else
      unname(provider_icb[starts$booking_code]) == code
    org_starts <- starts[keep, ]
    organisation <- if (level == "Trust") {
      ifelse(is.na(trust_labels[code]), code, unname(trust_labels[code]))
    } else unname(icb_labels[code])
    data.frame(Level = level, Organisation = organisation,
      Starts = nrow(org_starts), Within = sum(org_starts$outcome == "Within cluster"),
      Outside = sum(org_starts$outcome == "Outside cluster"),
      No_delivery = sum(org_starts$outcome == "No recorded delivery"),
      Unknown = sum(org_starts$outcome == "Unknown provider"))
  }))
}
pathways <- rbind(pathway_summary(cluster_icbs, "ICB"), pathway_summary(booking_codes, "Trust"))
cluster_activity$pathway_table <- gt::gt(pathways, groupname_col = "Level") |>
  gt::tab_header(title = "Recorded outcomes of cluster antenatal starts", subtitle = "Bookings in 2025/26") |>
  gt::cols_label(Organisation = "Booking organisation", Starts = "Starts",
    Within = "Delivery within cluster", Outside = "Delivery outside cluster",
    No_delivery = "No recorded delivery", Unknown = "Delivery provider unknown") |>
  gt::fmt_number(columns = c("Starts", "Within", "Outside", "No_delivery", "Unknown"), decimals = 0)
cluster_activity$pathway_text <- paste0(
  count_text(nrow(starts)), " antenatal starts recorded at cluster providers in 2025/26; ",
  count_text(sum(starts$outcome == "Outside cluster")), " had a recorded delivery outside the cluster and ",
  count_text(sum(starts$outcome == "No recorded delivery")), " had no recorded delivery. ",
  "ICB rows group the booking trusts; trust and ICB totals are alternative views.")
cluster_activity$pathway_needed <- paste0(
  "To confirm all starts that did not end in a cluster delivery: confirmation that final_cohort_2326 ",
  "includes bookings without births, the extract end date and follow-up beyond March 2026, ",
  "and pregnancy outcome/transfer information. No recorded delivery is not a confirmed non-delivery.")

cluster_activity$residence_totals <- residence_totals
cluster_activity$local_totals <- local_deliveries
cluster_activity$outside_totals <- outside_summary
cluster_activity$delivery_trend <- delivery_trend
cluster_activity$pathways <- pathways

# Section 6e and 6f: antenatal pathway Sankeys
# This bit plots the outcomes above, with one common key for each grid.

outcome_key <- function(i, text) {
  htmltools::span(style = "display: inline-flex; align-items: center; gap: 6px;",
    title = outcome_labels[i],
    htmltools::span(style = paste0("display: inline-block; width: 14px; height: 14px; background: ",
                                   outcome_colours[i], ";"), `aria-hidden` = "true"),
    text)
}
pathway_legend <- htmltools::div(
  style = "display: flex; flex-wrap: wrap; gap: 12px 24px; margin-bottom: 20px; font-size: 14px;",
  htmltools::tagList(lapply(seq_along(outcome_labels), function(i) outcome_key(i, outcome_labels[i])))
)

make_pathway_sankey <- function(row) {
  organisation <- row$Organisation[[1]]
  counts <- as.numeric(unlist(row[outcome_columns], use.names = FALSE))
  total <- row$Starts[[1]]
  shares <- if (total > 0) counts / total else rep(NA_real_, 4)
  outcome_text <- paste0(outcome_labels, " (", organisation, ")")
  labels <- paste0(outcome_text, "<br>", count_text(counts), " (",
                   percent_text(shares), ")")
  chart <- plotly::plot_ly(type = "sankey", height = 170,
    arrangement = "snap", orientation = "h",
    node = list(
      label = c(count_text(total), percent_text(shares)),
      color = c(main_chart_colour, outcome_colours),
      x = c(0.01, rep(0.99, 4)), y = c(0.5, 0.08, 0.35, 0.62, 0.89),
      pad = 18, thickness = 18,
      customdata = c(organisation, outcome_text),
      hovertemplate = "%{customdata}<br>Starts: %{value:,.0f}<extra></extra>"
    ),
    link = list(source = rep(0L, 4), target = 1:4, value = counts,
      color = grDevices::adjustcolor(outcome_colours, alpha.f = 0.48),
      customdata = labels,
      hovertemplate = "%{customdata}<extra></extra>")
  ) |>
    plotly::layout(font = list(family = "Arial", size = 14),
      margin = list(l = 12, r = 12, t = 20, b = 20),
      paper_bgcolor = "white", plot_bgcolor = "white") |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
  if (total == 0) {
    return(htmltools::div("No antenatal starts recorded for this organisation in 2025/26."))
  }
  # Zero-width flows: colour keys and percentages below the chart
  htmltools::tagList(chart, if (any(counts == 0)) {
    htmltools::div(style = "display: flex; gap: 16px; font-size: 14px;",
      htmltools::tagList(lapply(which(counts == 0), function(i) outcome_key(i, "0%"))))
  })
}

# ICBs: two columns; trusts: three columns
pathway_grid <- function(level) {
  rows <- pathways[pathways$Level == level, ]
  columns <- if (level == "Trust") 3L else 2L
  htmltools::tagList(pathway_legend,
    htmltools::div(style = paste0("display: grid; grid-template-columns: repeat(",
                                  columns, ", minmax(0, 1fr)); gap: 24px;"),
    htmltools::tagList(lapply(seq_len(nrow(rows)), function(i) {
      htmltools::div(style = "min-width: 0;",
        htmltools::h3(htmltools::span(style = "font-size: calc(1em - 2px);", rows$Organisation[i])),
        make_pathway_sankey(rows[i, , drop = FALSE]))
    }))))
}
cluster_activity$pathway_icb_sankeys <- pathway_grid("ICB")
cluster_activity$pathway_trust_sankeys <- pathway_grid("Trust")

# Section 7: delivery episodes over time
# This bit counts all booking starts, including those without a recorded delivery.
# ICBs group the booking trusts, as in section 6.
pathway_years <- analysis_years
trend_starts <- activity_episodes[
  !is.na(activity_episodes$booking_year) &
    activity_episodes$booking_year %in% pathway_years &
    activity_episodes$booking_code %in% cluster_trusts, , drop = FALSE]
delivery_over_time <- list()

trend_starts$outcome <- classify_pathway_outcome(trend_starts)
trend_outcomes <- c("Within cluster", "Outside cluster", "No recorded delivery", "Unknown provider")
trend_labels <- outcome_labels
trend_colours <- outcome_colours
# Always show the six named trusts; include any additional cluster booking trusts observed.
trend_trusts <- unique(c(names(trust_labels)[names(trust_labels) != "Other"],
                         sort(unique(trend_starts$booking_code))))
trend_trust_names <- unname(trust_labels[trend_trusts])
trend_trust_names[is.na(trend_trust_names)] <- paste0(
  "Trust ", trend_trusts[is.na(trend_trust_names)])
organisations <- rbind(
  data.frame(Key = paste0("trust-", tolower(trend_trusts)), Level = "Trust",
             Code = trend_trusts, Organisation = trend_trust_names),
  data.frame(Key = paste0("icb-", tolower(cluster_icbs)), Level = "ICB",
             Code = cluster_icbs, Organisation = unname(icb_labels[cluster_icbs])),
  data.frame(Key = "cluster", Level = "Cluster", Code = "Cluster",
             Organisation = cluster_label)
)

relative_share_change <- function(current, reference) {
  result <- rep(NA_real_, length(current))
  valid <- !is.na(current) & !is.na(reference) & reference > 0
  result[valid] <- (current[valid] / reference[valid]) - 1
  result
}
summarise_pathway_trend <- function(organisation) {
  keep <- switch(organisation$Level[[1]],
    Trust = trend_starts$booking_code == organisation$Code[[1]],
    ICB = unname(provider_icb[trend_starts$booking_code]) == organisation$Code[[1]],
    Cluster = rep(TRUE, nrow(trend_starts)))
  selected <- trend_starts[keep, , drop = FALSE]
  result <- do.call(rbind, lapply(pathway_years, function(year) {
    episodes <- selected[selected$booking_year == year, , drop = FALSE]
    numerator <- vapply(trend_outcomes, function(outcome)
      sum(episodes$outcome == outcome), integer(1))
    denominator <- rep(nrow(episodes), length(trend_outcomes))
    data.frame(Key = organisation$Key[[1]], Level = organisation$Level[[1]],
      Organisation = organisation$Organisation[[1]], Year_start = year,
      Year = paste0(year, "/", substr(year + 1L, 3, 4)),
      Outcome = trend_labels, Numerator = unname(numerator),
      Denominator = denominator, Share = ifelse(denominator > 0, numerator / denominator, NA_real_))
  }))
  result$YoY_change <- NA_real_
  result$Base_change <- NA_real_
  for (outcome in trend_labels) {
    rows <- which(result$Outcome == outcome)
    shares <- result$Share[rows]
    result$YoY_change[rows] <- relative_share_change(shares, c(NA_real_, head(shares, -1)))
    result$Base_change[rows] <- relative_share_change(shares, rep(shares[1], length(shares)))
  }
  result
}
pathway_trends <- do.call(rbind, lapply(seq_len(nrow(organisations)), function(i)
  summarise_pathway_trend(organisations[i, , drop = FALSE])))

# Section 7 charts: show each outcome share and its change from the first year.
make_delivery_trend_chart <- function(key, measure = c("share", "base_change"), compact = FALSE) {
  measure <- match.arg(measure)
  show_change <- measure == "base_change"
  data <- pathway_trends[pathway_trends$Key == key, , drop = FALSE]
  chart <- plotly::plot_ly(height = if (compact) 300 else 480)
  for (i in seq_along(trend_labels)) {
    line <- data[data$Outcome == trend_labels[i], , drop = FALSE]
    hover <- paste0(line$Year, "<br>", line$Outcome,
      "<br>Episodes: ", scales::comma(line$Numerator, accuracy = 1),
      "<br>All starts: ", scales::comma(line$Denominator, accuracy = 1),
      "<br>Share: ", ifelse(is.na(line$Share), "not available",
                             scales::percent(line$Share, accuracy = 0.1)))
    if (show_change) {
      hover <- paste0(hover,
        "<br>2023/24 share: ", ifelse(is.na(line$Share[1]), "not available",
                                      scales::percent(line$Share[1], accuracy = 0.1)),
        "<br>Change from 2023/24: ", ifelse(is.na(line$Base_change), "not available",
                                           scales::percent(line$Base_change, accuracy = 0.1)))
    }
    values <- if (show_change) line$Base_change else line$Share
    if (all(is.na(values))) next
    chart <- plotly::add_trace(chart, x = line$Year_start, y = values,
      type = "scatter", mode = "lines+markers", name = trend_labels[i],
      line = list(color = trend_colours[i], width = 2),
      marker = list(color = trend_colours[i], size = 7),
      text = hover, hovertemplate = "%{text}<extra></extra>", connectgaps = FALSE)
  }
  chart <- plotly::layout(chart,
    xaxis = list(title = if (compact) "Booking year" else "Booking financial year", tickmode = "array",
      tickvals = pathway_years, ticktext = c("2023/24", "2024/25", "2025/26")),
    yaxis = if (show_change) {
      list(title = "% change in share from 2023/24", tickformat = ".0%",
           rangemode = "tozero", zeroline = TRUE, zerolinecolor = "#727272", zerolinewidth = 1)
    } else list(title = "% of all starts", range = c(0, 1), tickformat = ".0%"),
    showlegend = !compact,
    legend = list(orientation = "h", x = 0, y = -0.22),
    margin = if (compact) list(l = 65, r = 12, t = 15, b = 55) else
      list(l = 75, r = 20, t = 25, b = 125),
    font = list(family = "Arial", size = if (compact) 11 else 12),
    paper_bgcolor = "white", plot_bgcolor = "white") |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
  empty_years <- unique(data$Year[data$Denominator == 0])
  chart_notes <- if (length(empty_years)) paste0(
    "No starts recorded: ", paste(empty_years, collapse = ", "), "; percentages unavailable.")
  if (show_change) {
    unavailable_outcomes <- data$Outcome[data$Year_start == 2023L &
      (is.na(data$Share) | data$Share == 0)]
    if (length(unavailable_outcomes)) chart_notes <- c(chart_notes,
      paste0("Zero or unavailable 2023/24 share; change unavailable for:<br>",
             paste(unavailable_outcomes, collapse = "<br>"), "."))
  }
  if (compact) {
    return(htmltools::tagList(chart, if (length(chart_notes)) {
      htmltools::div(style = "font-size: 11px; margin-top: 4px;",
        htmltools::HTML(paste(chart_notes, collapse = "<br>")))
    }))
  }
  if (length(chart_notes)) {
    chart <- plotly::layout(chart, annotations = list(list(
      text = paste(chart_notes, collapse = "<br>"),
      x = 0, y = 1.02, xref = "paper", yref = "paper", showarrow = FALSE,
      xanchor = "left", yanchor = "bottom", align = "left")), margin = list(t = 95))
  }
  chart
}
delivery_over_time$organisations <- organisations
delivery_over_time$data <- pathway_trends
delivery_over_time$charts <- stats::setNames(
  lapply(organisations$Key, make_delivery_trend_chart), organisations$Key)
delivery_over_time$change_charts <- stats::setNames(
  lapply(organisations$Key, make_delivery_trend_chart, measure = "base_change"), organisations$Key)
trend_legend <- htmltools::div(
  style = "display: flex; flex-wrap: wrap; gap: 12px 24px; margin-bottom: 20px; font-size: 14px;",
  htmltools::tagList(lapply(seq_along(trend_labels), function(i) {
    htmltools::span(style = "display: inline-flex; align-items: center; gap: 6px;",
      htmltools::span(style = paste0("display: inline-block; width: 18px; height: 3px; background: ",
                                     trend_colours[i], ";"), `aria-hidden` = "true"),
      trend_labels[i])
  }))
)
# Show the two ICBs and combined cluster together, with one shared legend.
make_delivery_trend_grid <- function(group, measure = "share") {
  levels <- if (group == "ICB_Cluster") c("ICB", "Cluster") else group
  rows <- organisations[organisations$Level %in% levels, , drop = FALSE]
  columns <- unname(trend_grid_columns[group])
  htmltools::tagList(trend_legend,
    htmltools::div(style = paste0("display: grid; grid-template-columns: repeat(",
                                  columns, ", minmax(0, 1fr)); gap: 24px;"),
      htmltools::tagList(lapply(seq_len(nrow(rows)), function(i) {
        htmltools::div(style = "min-width: 0;",
          htmltools::h3(htmltools::span(style = "font-size: 14px;", rows$Organisation[i])),
          make_delivery_trend_chart(rows$Key[i], measure = measure, compact = TRUE))
      }))))
}
trend_groups <- c("Trust", "ICB_Cluster")
delivery_over_time$chart_grids <- stats::setNames(
  lapply(trend_groups, make_delivery_trend_grid), trend_groups)
delivery_over_time$change_chart_grids <- stats::setNames(
  lapply(trend_groups, make_delivery_trend_grid, measure = "base_change"), trend_groups)
delivery_over_time$table <- pathway_trends |>
  dplyr::mutate(Group = paste(Level, Organisation, sep = ": ")) |>
  dplyr::select(Group, Year, Outcome, Numerator, Denominator,
                Share, YoY_change, Base_change) |>
  gt::gt(groupname_col = "Group") |>
  gt::tab_header(title = "Delivery pathway outcomes over time",
                 subtitle = "Booking financial years 2023/24 to 2025/26") |>
  gt::cols_label(Year = "Booking year", Outcome = "Outcome",
    Numerator = "Numerator: outcome episodes", Denominator = "Denominator: all starts",
    Share = "% of all starts", YoY_change = "% change in share year on year",
    Base_change = "% change in share from 2023/24") |>
  gt::fmt_number(columns = c("Numerator", "Denominator"), decimals = 0, use_seps = TRUE) |>
  gt::fmt_percent(columns = c("Share", "YoY_change", "Base_change"), decimals = 1) |>
  gt::sub_missing(missing_text = "n.a.") |>
  gt::tab_source_note(source_note = paste0(
    "Share = outcome episodes / all starts in the same organisation and booking year. ",
    "Changes are relative changes in that share: (current share / reference share - 1) x 100, ",
    "not percentage-point differences. n.a. = no starts, no prior year, or a zero/unavailable reference share. ",
    "The base-year change is 0% where the 2023/24 share is positive. ",
    "Trust, ICB and cluster totals are alternative aggregations of the same starts.")) |>
  gt::tab_options(table.font.names = c("Open Sans", "Arial", "sans-serif"))

# Section 7 tables: one row per organisation, with outcomes grouped under each year.
change_column_key <- expand.grid(Outcome_index = seq_along(trend_labels),
                                Year_start = pathway_years)
change_column_key$Column <- paste0("y", change_column_key$Year_start,
                                   "_o", change_column_key$Outcome_index)
change_columns <- change_column_key$Column
change_wide <- pathway_trends |>
  dplyr::mutate(Column = paste0("y", Year_start, "_o", match(Outcome, trend_labels))) |>
  tidyr::pivot_wider(id_cols = c(Key, Level, Organisation),
                     names_from = Column, values_from = Base_change) |>
  dplyr::arrange(match(Key, organisations$Key)) |>
  dplyr::select(Level, Organisation, dplyr::all_of(change_columns))
outcome_short_labels <- c("Within cluster", "Outside cluster", "No recorded delivery", "Provider unknown")
change_table <- gt::gt(change_wide, groupname_col = "Level", rowname_col = "Organisation") |>
  gt::tab_header(title = "Percentage change in delivery outcome shares",
                 subtitle = "Relative change from booking financial year 2023/24") |>
  gt::tab_stubhead(label = "Booking organisation") |>
  gt::cols_label(.list = stats::setNames(
    as.list(outcome_short_labels[change_column_key$Outcome_index]), change_columns)) |>
  gt::fmt_percent(columns = dplyr::all_of(change_columns), decimals = 1) |>
  gt::sub_missing(missing_text = "n.a.") |>
  gt::tab_source_note(source_note = paste0(
    "Every cell is (the year's outcome share / its 2023/24 share - 1) x 100. ",
    "Outcome share = outcome episodes / all starts in that booking organisation and year. ",
    "Positive values indicate a higher share; negative values indicate a lower share. ",
    "2023/24 is 0% where its share is positive; n.a. means no starts or a zero/unavailable baseline share. ",
    "Numerators, denominators, shares and year-on-year changes are in subsection 7.1's results table.")) |>
  gt::tab_options(table.font.names = c("Open Sans", "Arial", "sans-serif"),
                  table.font.size = gt::px(12), table.width = gt::px(1550),
                  container.width = "100%", container.overflow.x = TRUE)
for (year in pathway_years) {
  change_table <- gt::tab_spanner(change_table,
    label = paste0(year, "/", substr(year + 1L, 3, 4)),
    columns = change_column_key$Column[change_column_key$Year_start == year],
    id = paste0("change-year-", year), gather = FALSE)
}
delivery_over_time$change_data <- change_wide
delivery_over_time$change_table <- change_table

# Save the charts and tables for index.qmd; run R/QA.R separately when needed.
saveRDS(
  list(icb_sankey = icb_sankey, trust_sankey = trust_sankey, by_year = by_year,
       cluster_activity = cluster_activity, delivery_over_time = delivery_over_time,
       activity_episodes = activity_episodes, icb_flow = icb_flow, trust_flow = trust_flow),
  ".patient_flows.rds"
)
