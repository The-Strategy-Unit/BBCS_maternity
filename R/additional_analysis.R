# From the project root, run R/Read parquet files from azure.R;
# R/matrix of booking appts and births.R; then source("R/additional_analysis.R")

## 1. Patient flows ----------------------------------------------------------
# Inputs: icb_matrix_data; trust_matrix_data
if (!exists("icb_matrix_data") || !exists("trust_matrix_data")) {
  stop("Missing icb_matrix_data or trust_matrix_data in this R session.")
}

# Flow columns
pick_column <- function(data, choices) {
  found <- choices[choices %in% names(data)]
  if (length(found)) found[1] else NA_character_
}

make_flows <- function(tables, source_choices, target_choices, keep_codes) {
  for (table_name in names(tables)) {
    data <- tables[[table_name]]
    from <- pick_column(data, source_choices)
    to <- pick_column(data, target_choices)
    if (is.na(from) || is.na(to)) next

    # An aggregate table has n; a record-level table contributes one per row
    if ("n" %in% names(data)) {
      count <- suppressWarnings(as.numeric(as.character(data$n)))
      if (any(is.na(count) | !is.finite(count) | count < 0)) {
        stop("Invalid n values in ", table_name, ".")
      }
      count_basis <- "Sum of n"
    } else {
      count <- rep(1, nrow(data))
      count_basis <- "Rows"
    }

    source_code <- toupper(trimws(as.character(data[[from]])))
    target_code <- toupper(trimws(as.character(data[[to]])))
    valid <- !is.na(source_code) & nzchar(source_code) &
      !is.na(target_code) & nzchar(target_code) & count > 0
    if (!any(valid)) stop("No positive, complete flow records in ", table_name, ".")

    source_code <- ifelse(source_code[valid] %in% keep_codes,
                          source_code[valid], "Other")
    target_code <- ifelse(target_code[valid] %in% keep_codes,
                          target_code[valid], "Other")
    links <- data.frame(source = source_code, target = target_code,
                        n = count[valid])
    return(list(links = links, table = table_name, count_basis = count_basis,
                source_column = from, target_column = to))
  }
  available <- vapply(tables, function(x) paste(names(x), collapse = ", "), character(1))
  stop("Flow columns not found. Available columns: ",
       paste(paste(names(available), available, sep = ": "), collapse = " | "))
}

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

icb_flow <- make_flows(
  list(icb_matrix_data = icb_matrix_data),
  "resident_icb_interest",
  "birth_icb_interest",
  names(icb_labels)[names(icb_labels) != "Other"]
)
trust_flow <- make_flows(
  list(trust_matrix_data = trust_matrix_data),
  "booking_provider_grouped",
  "birth_provider_grouped",
  names(trust_labels)[names(trust_labels) != "Other"]
)

# Sankey charts
icb_source_order <- c("QJ2", "QWU", "QUA", "QHL", "Other", "QGH", "QOC", "QNC")
icb_target_order <- c("Other", "QUA", "QWU", "QJ2", "QHL", "QNC", "QGH", "QOC")
trust_source_order <- c("RNA", "RXK", "RBK", "RL4", "Other", "RRK", "RQ3")
trust_target_order <- c("RNA", "RL4", "RQ3", "RXK", "Other", "RBK", "RRK")

make_sankey <- function(flow, labels, source_suffix, target_suffix,
                        source_order, target_order) {
  links <- flow$links
  codes <- unique(c(source_order, target_order))
  palette <- c("#005EB8", "#D55E00", "#00856A", "#8E5EA2",
               "#B07300", "#C44E52", "#4477AA", "#727272")
  colours <- setNames(palette[seq_along(codes)], codes)
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

## 2. Patient flows by financial year ---------------------------------------
# Input: final_cohort_2326 from R/Read parquet files from azure.R
if (!exists("final_cohort_2326")) {
  stop("Run R/Read parquet files from azure.R before R/additional_analysis.R.")
}

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

# Trust to ICB: NHS ODS etr.csv; providers outside these ICBs = Other
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
flow_code <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[is.na(x) | !nzchar(x)] <- "Other"
  x
}

make_cohort_year_flows <- function(start_year) {
  cohort <- final_cohort_2326[!is.na(financial_year_start) &
                                financial_year_start == start_year, ]
  if (!nrow(cohort)) stop("No birth_yr entries for ", start_year, "/",
                          substr(start_year + 1L, 3, 4), " in final_cohort_2326.")

  booking_provider <- flow_code(cohort$booking_provider)
  birth_provider <- flow_code(cohort$birth_provider)
  birth_icb <- unname(provider_icb[birth_provider])
  birth_icb[is.na(birth_icb)] <- "Other"

  trust_year <- make_flows(
    list(cohort = data.frame(booking_provider_grouped = booking_provider,
                             birth_provider_grouped = birth_provider)),
    "booking_provider_grouped", "birth_provider_grouped",
    names(trust_labels)[names(trust_labels) != "Other"]
  )
  icb_year <- make_flows(
    list(cohort = data.frame(resident_icb_interest = flow_code(cohort$resident_icb),
                             birth_icb_interest = birth_icb)),
    "resident_icb_interest", "birth_icb_interest",
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

## 3. Patient flow tables ---------------------------------------------------
# Same flow counts and ordering as the Sankeys
make_flow_table <- function(flow, labels, source_label, target_label,
                            source_order, target_order, year) {
  data <- stats::aggregate(n ~ source + target, flow$links, sum)
  data <- data[order(match(data$source, source_order),
                     match(data$target, target_order)), ]
  data$origin_share <- data$n / ave(data$n, data$source, FUN = sum)
  data$total_share <- data$n / sum(data$n)
  data$source <- unname(labels[data$source])
  data$target <- unname(labels[data$target])
  gt::gt(data) |>
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

## 4. Cluster delivery activity ---------------------------------------------
# Cluster: Black Country; Birmingham and Solihull
# Episodes: distinct mother/pregnancy pairs; year from birth_yr
cluster_icbs <- c("QUA", "QHL")
cluster_trusts <- names(provider_icb)[provider_icb %in% cluster_icbs]
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

# Missing identifiers excluded; conflicting episode details need resolution
has_id <- function(x) !is.na(x) & nzchar(trimws(as.character(x)))
identified <- has_id(activity_cohort$person_id_mother) & has_id(activity_cohort$uniq_preg_id)
excluded_activity_rows <- sum(!identified)
episode_fields <- c("person_id_mother", "uniq_preg_id", "delivery_date",
                    "delivery_year", "birth_code", "booking_code", "residence_code",
                    "booking_date", "booking_year")
activity_episodes <- unique(activity_cohort[identified, episode_fields])
activity_conflicts <- any(duplicated(activity_episodes[c("person_id_mother", "uniq_preg_id")]))
missing_activity_years <- setdiff(2023:2025, activity_episodes$delivery_year)

# Section 6 hand-off; earlier sections remain available
cluster_activity <- list(
  available = !activity_conflicts && !length(missing_activity_years),
  note = paste0("Cluster: Black Country and Birmingham and Solihull. ",
                "Delivery episodes counted once per mother/pregnancy pair; financial year from birth_yr. ",
                "Residence used to describe local and outside-cluster activity."),
  excluded = if (excluded_activity_rows > 0) paste(
    scales::comma(excluded_activity_rows), "cohort rows excluded: missing mother or pregnancy identifier."
  ) else "",
  missing = paste(c(
    if (activity_conflicts) "Conflicting details for the same mother/pregnancy pair: resolve these before counting delivery episodes.",
    if (length(missing_activity_years)) paste0(
      "No delivery episodes with birth_yr in the cohort for financial years starting ",
      paste(missing_activity_years, collapse = ", "),
      ". Check birth_yr and cohort coverage; these years cannot be reported as zero deliveries.")
  ), collapse = " ")
)

if (cluster_activity$available) {
  safe_share <- function(n, total) ifelse(rep_len(total, length(n)) > 0, n / total, NA_real_)
  count_text <- function(x) scales::comma(x, accuracy = 1)
  percent_text <- function(x) ifelse(is.na(x), "not available", scales::percent(x, accuracy = 0.1))
  residence_labels <- c(icb_labels, Unknown = "Unknown residence")
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
      activity_episodes$delivery_year %in% 2023:2025 &
      activity_episodes$birth_code %in% cluster_trusts, ]
  deliveries_2526 <- deliveries[deliveries$delivery_year == 2025L, ]
  total_2526 <- nrow(deliveries_2526)
  resident_types <- c("Cluster residents", "Outside-cluster residents", "Unknown residence")
  totals <- data.frame(
    Residence = c("All cluster deliveries", resident_types),
    Episodes = c(total_2526, vapply(resident_types, function(x)
      sum(deliveries_2526$residence_type == x), integer(1)))
  )
  totals$Share <- safe_share(totals$Episodes, total_2526)
  cluster_activity$total_table <- gt::gt(totals) |>
    gt::tab_header(title = "Delivery episodes within the cluster", subtitle = "2025/26") |>
    gt::cols_label(Share = "% of cluster deliveries") |>
    gt::fmt_number(columns = "Episodes", decimals = 0) |>
    gt::fmt_percent(columns = "Share", decimals = 1)
  cluster_activity$total_text <- paste0(count_text(total_2526),
    " delivery episodes recorded within the cluster in 2025/26.")

  local <- data.frame(
    ICB = unname(icb_labels[cluster_icbs]),
    Episodes = vapply(cluster_icbs, function(x)
      sum(deliveries_2526$residence_group == x), integer(1))
  )
  local$Share <- safe_share(local$Episodes, total_2526)
  cluster_activity$local_table <- gt::gt(local) |>
    gt::tab_header(title = "Cluster residents delivering within the cluster", subtitle = "2025/26") |>
    gt::cols_label(ICB = "ICB of residence", Share = "% of cluster deliveries") |>
    gt::fmt_number(columns = "Episodes", decimals = 0) |>
    gt::fmt_percent(columns = "Share", decimals = 1)
  cluster_activity$local_text <- paste0(count_text(sum(local$Episodes)), " (",
    percent_text(safe_share(sum(local$Episodes), total_2526)),
    ") of cluster delivery episodes were for cluster residents.")

  # Outside residents: named ICBs and Other; unknown residence kept separate
  outside <- deliveries_2526[deliveries_2526$residence_type == "Outside-cluster residents", ]
  outside_codes <- c(setdiff(names(icb_labels), cluster_icbs))
  outside_summary <- data.frame(
    ICB = unname(icb_labels[outside_codes]),
    Episodes = vapply(outside_codes, function(x) sum(outside$residence_group == x), integer(1))
  )
  outside_summary <- outside_summary[order(-outside_summary$Episodes), ]
  outside_summary$Share <- safe_share(outside_summary$Episodes, nrow(outside))
  outside_summary$Cluster_share <- safe_share(outside_summary$Episodes, total_2526)
  cluster_activity$outside_table <- gt::gt(outside_summary) |>
    gt::tab_header(title = "Outside-cluster residents delivering within the cluster", subtitle = "2025/26") |>
    gt::cols_label(ICB = "ICB of residence", Share = "% of outside-resident deliveries",
                   Cluster_share = "% of cluster deliveries") |>
    gt::fmt_number(columns = "Episodes", decimals = 0) |>
    gt::fmt_percent(columns = c("Share", "Cluster_share"), decimals = 1)
  cluster_activity$outside_diagram <- plotly::plot_ly(
    outside_summary, x = ~Episodes, y = ~ICB, type = "bar", orientation = "h",
    marker = list(color = "#005EB8"),
    hovertemplate = "%{y}<br>Delivery episodes: %{x:,.0f}<extra></extra>"
  ) |>
    plotly::layout(xaxis = list(title = "Delivery episodes"),
      yaxis = list(title = "", categoryorder = "array", categoryarray = rev(outside_summary$ICB)),
      margin = list(l = 300), font = list(family = "Arial"), height = 420) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
  cluster_activity$outside_text <- paste0(count_text(nrow(outside)), " (",
    percent_text(safe_share(nrow(outside), total_2526)),
    ") of cluster deliveries were for residents outside the cluster.",
    if (nrow(outside) > 0) paste0(" The largest origin group was ",
      paste(outside_summary$ICB[outside_summary$Episodes == max(outside_summary$Episodes)], collapse = " and "),
      ", with ", count_text(max(outside_summary$Episodes)), " episodes per group.") else "",
    " Residence was unknown for ", count_text(sum(deliveries_2526$residence_type == "Unknown residence")),
    " episodes; these are excluded from the outside-resident totals.")

  # Same delivery-year definition across all three years
  trend <- do.call(rbind, lapply(2023:2025, function(year) {
    data <- deliveries[deliveries$delivery_year == year, ]
    data.frame(Year = paste0(year, "/", substr(year + 1L, 3, 4)),
      Total = nrow(data), Local = sum(data$residence_type == "Cluster residents"),
      Outside = sum(data$residence_type == "Outside-cluster residents"),
      Unknown = sum(data$residence_type == "Unknown residence"))
  }))
  trend$Outside_share <- safe_share(trend$Outside, trend$Total)
  trend$Change <- c(NA_real_, diff(trend$Total))
  cluster_activity$trend_table <- gt::gt(trend) |>
    gt::cols_label(Year = "Financial year", Total = "Cluster deliveries", Local = "Cluster residents",
      Outside = "Outside-cluster residents", Unknown = "Unknown residence",
      Outside_share = "Outside-resident share", Change = "Change in deliveries from previous year") |>
    gt::fmt_number(columns = c("Total", "Local", "Outside", "Unknown", "Change"), decimals = 0) |>
    gt::fmt_percent(columns = "Outside_share", decimals = 1) |>
    gt::sub_missing(missing_text = "—")
  cluster_activity$trend_text <- paste0(
    "Cluster deliveries changed from ", count_text(trend$Total[1]), " in 2023/24 to ",
    count_text(trend$Total[3]), " in 2025/26 (",
    if (trend$Total[3] >= trend$Total[1]) "+" else "", count_text(trend$Total[3] - trend$Total[1]),
    "). The outside-resident share changed from ", percent_text(trend$Outside_share[1]),
    " to ", percent_text(trend$Outside_share[3]), ".")

  # Antenatal starts: booking year 2025/26; all recorded delivery dates
  starts <- activity_episodes[
    !is.na(activity_episodes$booking_year) & activity_episodes$booking_year == 2025L &
      activity_episodes$booking_code %in% cluster_trusts, ]
  starts$outcome <- ifelse(is.na(starts$delivery_year) & is.na(starts$delivery_date), "No recorded delivery",
    ifelse(starts$birth_code %in% cluster_trusts, "Within cluster",
      ifelse(grepl("^R[A-Z0-9]{2}$", starts$birth_code), "Outside cluster", "Unknown provider")))
  booking_codes <- unique(c(names(trust_labels)[names(trust_labels) != "Other"], starts$booking_code))
  booking_name <- function(code) {
    label <- unname(trust_labels[code])
    ifelse(is.na(label), code, label)
  }
  pathway_summary <- function(codes, level) {
    do.call(rbind, lapply(codes, function(code) {
      keep <- if (level == "Trust") starts$booking_code == code else
        unname(provider_icb[starts$booking_code]) == code
      data <- starts[keep, ]
      data.frame(Level = level, Organisation = if (level == "Trust") booking_name(code) else unname(icb_labels[code]),
        Starts = nrow(data), Within = sum(data$outcome == "Within cluster"),
        Outside = sum(data$outcome == "Outside cluster"),
        No_delivery = sum(data$outcome == "No recorded delivery"),
        Unknown = sum(data$outcome == "Unknown provider"))
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
}

## 5. Antenatal pathway Sankeys ---------------------------------------------
# Same rows and organisation order as the antenatal starts table
if (cluster_activity$available) {
  outcome_columns <- c("Within", "Outside", "No_delivery", "Unknown")
  outcome_labels <- c("Delivery within cluster", "Delivery outside cluster",
                       "No recorded delivery", "Delivery provider unknown")
  outcome_colours <- c("#00856A", "#D55E00", "#727272", "#8E5EA2")

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
        color = c("#005EB8", outcome_colours),
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
}

# Index report hand-off
saveRDS(
  list(icb_sankey = icb_sankey, trust_sankey = trust_sankey, by_year = by_year,
       cluster_activity = cluster_activity),
  ".patient_flows.rds"
)
