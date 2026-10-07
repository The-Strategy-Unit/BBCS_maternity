# BBCS MATERNITY ANALYSIS ================================================
# Main editing file: general setup, ONS exploration, MSDS exploration, model
# Run with Run analysis.R from the project root
# The secure parquet reader supplies the input tables used below.

# TERMINOLOGY KEY --------------------------------------------------------
# Births (L)   = live-born babies
# Births (L+S) = live-born + stillborn babies
# Maternities = pregnancies reaching at least one live birth or stillbirth
# Age (1)     = individual year of maternal age (34, 35, 36, ...)
# Age (B)     = age band used for descriptive charts / final summaries
# ONS age     = Age (1) at 16-43, plus combined <=15 and 44+ source rows
# LA(R)       = local authority of residence; column LA in the model
# LA(B)       = LA of booking location, only if a location lookup supplies it
# LA(D)       = LA of delivery location, only if a location lookup supplies it
# LA(C)       = resident LAs contributing to a stated trust/ICB/cluster catchment
# Trust(B/D)  = booking / delivery provider, NOT a local authority
# ICB(R/B/D)  = residence / booking-provider / delivery-provider ICB
# This model uses LA(R), Trust(B/D), and separate booking/delivery-provider ICBs.
# It does not contain a booking-site or delivery-site LA lookup.
# A catchment is a set of contributing LA(R)s, not a new LA code.
# FY          = financial year; ONS projection years end at mid-year
# MODEL_INPUT = source/prepared data used in the projection calculation
# CONTEXT_ONLY = exploration or description, not a model multiplier

# GENERAL SETUP =========================================================
# Libraries -------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(janitor)
library(purrr)
library(ggplot2)
library(gt)
library(plotly)
library(scales)

# Shared source readers, counting rules and age-review functions ----------
# These existing support files contain reusable functions, not a second model.
source("R/ons_data.R")
source("R/report_cohort.R")
source("R/age_boundary_review.R")

# Settings, labels and colours ------------------------------------------
# Settings: years, organisation labels and chart colours
analysis_years <- 2023:2025
latest_year <- max(analysis_years)
cluster_icbs <- c("QUA", "QHL")
cluster_label <- "Combined cluster"
flow_palette <- c("#005EB8", "#D55E00", "#00856A", "#8E5EA2",
                  "#B07300", "#C44E52", "#4477AA", "#727272")
outcome_columns <- c("Within", "Outside", "No_delivery", "Unknown")
outcome_labels <- c("Delivery at cluster trust", "Other trust or home birth",
                    "No recorded delivery", "Delivery location unknown")
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

# Unknown origins/destinations are separate from known organisations grouped as Other.
flow_icb_labels <- c(icb_labels, Unknown = "Unknown ICB")
flow_trust_labels <- c(trust_labels, Unknown = "Unknown provider")
icb_source_order <- c(icb_source_order, "Unknown")
icb_target_order <- c(icb_target_order, "Unknown")
trust_source_order <- c(trust_source_order, "Unknown")
trust_target_order <- c(trust_target_order, "Unknown")


# Settings: retain ten additional chart areas; show five additional flows within each 9.1 chart.
delivery_years <- c("2023/24", "2024/25", "2025/26")
delivery_age_bands <- c(report_age_bands, "Unknown")
additional_las_to_show <- 10L
sankey_focus_n <- 10L
sankey_top_n <- 5L
delivery_chart_blue <- "#005EB8"
delivery_palette <- "Dark 3"
cluster_las <- readr::read_csv("config/local_authorities.csv", show_col_types = FALSE)$area_code


# Projection settings: comparator year, ONS reference and reporting groups
projection_baseline <- "2025/26"
projection_reference_year <- 2026L
projection_end_year <- NULL # NULL uses all years in the ONS file.
projection_history_years <- c("2023/24", "2024/25", "2025/26")
projection_age_bands <- report_age_bands
projection_age_colours <- c("#005EB8", "#00856A", "#D55E00", "#8E5EA2", "#B07300", "#727272", "#C44E52")
projection_other_feeders <- 10L
projection_chart_height <- 480
projection_age_grid_height <- 280
projection_history_chart_height <- 300
projection_history_colours <- c("MSDS observed" = "#005EB8", "ONS-based model" = "#D55E00")


# Input checks ----------------------------------------------------------
required_inputs <- c("final_cohort_2326", "total_births_by_LA_2326")
absent <- required_inputs[!vapply(required_inputs, function(name) exists(name, inherits = TRUE), logical(1))]
if (length(absent)) stop("Missing inputs: ", paste(absent, collapse = ", "),
  ". Use Run analysis.R.", call. = FALSE)

# 1. ONS DATA EXPLORATION ================================================
# 1.1 Source definitions and inputs -------------------------------------
# Shared public inputs for the ONS exploration and resident comparison.
check_ons_input_files()

# Use the configured areas, age range and colours.
area_config <- read_area_config()
analysis_parameters <- read_analysis_parameters()
sources <- build_source_summary()
# MODEL_INPUT: ONS_BIRTHS_L
# Births (L), LA(R) x ONS source age x year ending mid-year
snpp <- read_snpp_births()

# MODEL_INPUT: ONS_REGISTRATIONS
# National L, S and maternities by source maternal age and registration year
registration_counts <- read_national_age_counts()
projection_age_factors <- prepare_projection_age_factors(registration_counts)
ons_modifications <- projection_age_factors$modifications
selected_las <- area_config$area_name
la_maternities <- read_local_authority_maternities(selected_las)
observed_birth_rates <- read_local_authority_birth_rates(selected_las)
# 1.2 Published historical counts and multiplicity context ---------------
# CONTEXT_ONLY: observed births and maternities are shown as published.
# No conversion of historical banded births into estimated maternities.
multiplicity_data <- read_national_multiplicity()
average_rates <- average_multiplicity_rates(multiplicity_data)
historic_live_births <- read_historic_live_births(area_config = area_config)
historic_trends <- historic_live_births |> filter(age_band == "Total")
female_population_single_age <- read_female_population_single_age(
  area_config = area_config,
  parameters = analysis_parameters
)
# 1.3 Population and fertility trends ------------------------------------
# CONTEXT_ONLY: population and fertility are already reflected in ONS births (L)
projected_birth_rates <- snpp |>
  filter(area_name %in% selected_las, age_group == "All ages") |>
  select(area_code, area_name, year, projected_births) |>
  left_join(
    female_population_single_age |>
      filter(age >= 15, age <= 44) |>
      group_by(area_code, area_name, year) |>
      summarise(women_15_to_44 = sum(female_population), .groups = "drop"),
    by = c("area_code", "area_name", "year")
  ) |>
  mutate(general_fertility_rate = projected_births / women_15_to_44 * 1000)

female_population_projections <- read_female_population_projections(
  area_config = area_config,
  parameters = analysis_parameters
)

observed_trajectory <- la_maternities |>
  transmute(
    area_name = factor(as.character(local_authority), levels = selected_las),
    year,
    maternities
  )

# Chart functions live in ons_data.R and take these colours explicitly.
area_colours <- area_colour_palette(area_config)

# 1.4 Age-boundary evidence: descriptive comparisons only -----------------
# CONTEXT_ONLY: this review does not change the projection assumptions
older_age_review <- build_ons_age_review(registration_counts, historic_live_births,
  snpp, female_population_single_age, area_config)

# 1.5 Collect source exploration; add model results after section 3 -------
# Resident projection charts will use the same master as the trust charts.
ons_exploration <- list(
  area_config = area_config,
  analysis_parameters = analysis_parameters,
  sources = sources,
  snpp = snpp,
  selected_las = selected_las,
  la_maternities = la_maternities,
  observed_birth_rates = observed_birth_rates,
  multiplicity_data = multiplicity_data,
  average_rates = average_rates,
  projection_age_factors = projection_age_factors,
  historic_live_births = historic_live_births,
  historic_trends = historic_trends,
  female_population_single_age = female_population_single_age,
  projected_birth_rates = projected_birth_rates,
  female_population_projections = female_population_projections,
  observed_trajectory = observed_trajectory,
  area_colours = area_colours,
  older_age_review = older_age_review
)

# 2. MSDS DATA EXPLORATION ===============================================
# MODEL_INPUT: MSDS_DELIVERIES
# final_cohort_2326: one supplied pregnancy row, with LA(R), Age (1), Trust(B/D)
# MODEL_INPUT: MSDS_BOOKINGS
# All booking-year starts, including pregnancies without a recorded delivery.
# MODEL_INPUT: MSDS_RESIDENTS
# total_births_by_LA_2326: resident maternities by LA(R), Age (1), delivery FY
# Source names containing "births" do not change their maternity counting unit.
# 2.1 Latest-year patient flows (report section 2.2.4) ------------------------
# CONTEXT_ONLY: booking-year charts and Age (B) summaries stay exploratory.
# Section 3 separately pools delivery-year and booking-year counts at LA(R) x Age (1).
# Check the required inputs once; data QA is in R/QA.R.
required_objects <- "final_cohort_2326"
missing_objects <- required_objects[!vapply(required_objects, function(name) exists(name, inherits = TRUE), logical(1))]
if (length(missing_objects)) stop("Run the preparation scripts first. Missing: ",
                                  paste(missing_objects, collapse = ", "), call. = FALSE)
report_episodes <- prepare_report_cohort(final_cohort_2326)

# Section 2.2.4: patient flows in 2025/26
# Two-stage flows and the combined residence-to-booking-to-birth flow.
# Clean the supplied codes and group providers outside the labelled areas as Other.
group_flow_code <- function(x, keep_codes) {
  code <- report_code(x)
  code_pattern <- paste0("^", substr(keep_codes[1], 1, 1), "[A-Z0-9]{2}$")
  ifelse(code %in% keep_codes, code,
    ifelse(code == "OTHER" | grepl(code_pattern, code), "Other", "Unknown"))
}

make_flows <- function(source, target, count, keep_codes) {
  count <- report_number(count)
  valid <- is.finite(count) & count > 0
  links <- data.frame(
    source = group_flow_code(source, keep_codes)[valid],
    target = group_flow_code(target, keep_codes)[valid], n = count[valid]) |>
    dplyr::group_by(source, target) |>
    dplyr::summarise(n = sum(n), .groups = "drop")
  list(links = links)
}


# Use the same pregnancy records, flags and delivery-year definition in every year.
# The separately supplied aggregate matrices have unconfirmed inclusion/period rules.
make_cohort_year_flows <- function(start_year) {
  cohort <- report_episodes |>
    dplyr::filter(Delivery_year == start_year, Birth_recorded, Home == 0)
  birth_icb <- unname(provider_icb[cohort$Trust])
  birth_icb[is.na(birth_icb)] <- ifelse(
    grepl("^R[A-Z0-9]{2}$", cohort$Trust[is.na(birth_icb)]), "Other", "Unknown")
  list(
    trust = make_flows(cohort$Booking_Trust, cohort$Trust, rep(1, nrow(cohort)),
      setdiff(names(trust_labels), "Other")),
    icb = make_flows(cohort$Resident_ICB, birth_icb, rep(1, nrow(cohort)),
      setdiff(names(icb_labels), "Other")))
}
latest_flows <- make_cohort_year_flows(latest_year)
icb_flow <- latest_flows$icb
trust_flow <- latest_flows$trust

# Keep each pregnancy's three organisations together before counting either leg.
# The booking nodes combine origins; ribbons describe adjacent stages only.
make_residence_booking_birth_flow <- function(cohort) {
  paths <- tibble::tibble(
    residence = group_flow_code(cohort$Resident_ICB, setdiff(names(icb_labels), "Other")),
    booking = group_flow_code(cohort$Booking_Trust, setdiff(names(trust_labels), "Other")),
    birth = group_flow_code(cohort$Trust, setdiff(names(trust_labels), "Other"))) |>
    dplyr::count(residence, booking, birth, name = "n")
  list(paths = paths,
    residence_booking = paths |>
      dplyr::group_by(source = residence, target = booking) |>
      dplyr::summarise(n = sum(n), .groups = "drop"),
    booking_birth = paths |>
      dplyr::group_by(source = booking, target = birth) |>
      dplyr::summarise(n = sum(n), .groups = "drop"))
}
residence_booking_birth_flow <- make_residence_booking_birth_flow(report_episodes |>
  dplyr::filter(Delivery_year == latest_year, Birth_recorded, Home == 0))

# Visible inclusion accounting: these mutually exclusive delivery categories add up.
flow_coverage <- report_episodes |>
  dplyr::mutate(Period = dplyr::coalesce(Year, "Missing/invalid delivery year")) |>
  dplyr::group_by(Period) |>
  dplyr::summarise(Supplied_rows = dplyr::n(),
    No_recorded_birth = sum(!Birth_recorded),
    Home_births = sum(Birth_recorded & Home %in% 1),
    Unknown_home_flag = sum(Birth_recorded & !Home %in% c(0, 1)),
    Non_home = sum(Birth_recorded & Home %in% 0),
    Unknown_birth_provider = sum(Birth_recorded & Home %in% 0 &
      !grepl("^R[A-Z0-9]{2}$", Trust)), .groups = "drop")
flow_coverage_table <- gt::gt(flow_coverage) |>
  gt::tab_header(title = "Which supplied pregnancy records enter the delivery flows?") |>
  gt::cols_label(Period = "Delivery year", Supplied_rows = "Supplied rows",
    No_recorded_birth = "Birth flag not 1", Home_births = "Home births",
    Unknown_home_flag = "Home flag unknown/invalid", Non_home = "Recorded non-home deliveries",
    Unknown_birth_provider = "Of non-home: provider unknown/invalid") |>
  gt::fmt_number(columns = -Period, decimals = 0) |>
  gt::tab_source_note("Only recorded non-home deliveries in 2023/24–2025/26 enter the patient-flow views. Unknown providers remain separate. The four inclusion columns sum to supplied rows; the last column is a subset.") |>
  gt::tab_options(container.width = "100%", container.overflow.x = TRUE)

# Sankey charts

make_sankey <- function(flow, labels, source_suffix, target_suffix,
                        source_order, target_order) {
  links <- flow$links
  codes <- unique(c(source_order, target_order))
  colours <- setNames(rep(flow_palette, length.out = length(codes)), codes)
  colours["Unknown"] <- "#999999"
  display_label <- function(x) unname(labels[x])
  total <- sum(links$n)
  if (!nrow(links)) return(htmltools::div("No recorded non-home deliveries in this year."))

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

# Three distinct node layers: the same trust at booking and birth has different IDs.
make_three_stage_sankey <- function(flow) {
  total <- sum(flow$paths$n)
  if (!nrow(flow$paths)) return(htmltools::div("No recorded non-home deliveries in this year."))

  residence <- icb_source_order[icb_source_order %in% flow$paths$residence]
  booking <- trust_source_order[trust_source_order %in% flow$paths$booking]
  birth <- trust_target_order[trust_target_order %in% flow$paths$birth]
  stage <- rep(c("ICB of residence", "Booking trust", "Birth trust"),
               c(length(residence), length(booking), length(birth)))
  full_labels <- unname(c(flow_icb_labels[residence], flow_trust_labels[booking],
                         flow_trust_labels[birth]))
  short_labels <- sub(" NHS.*$", "", sub(" ICB$", "", full_labels))
  short_labels <- vapply(short_labels, function(label)
    paste(strwrap(label, width = 24), collapse = "<br>"), character(1))
  icb_colours <- setNames(rep(flow_palette, length.out = length(icb_source_order)), icb_source_order)
  trust_colours <- setNames(rep(flow_palette, length.out = length(trust_source_order)), trust_source_order)
  icb_colours["Unknown"] <- trust_colours["Unknown"] <- "#999999"
  node_colours <- unname(c(icb_colours[residence], trust_colours[booking], trust_colours[birth]))

  first <- flow$residence_booking
  second <- flow$booking_birth
  from_id <- c(match(first$source, residence) - 1L,
               length(residence) + match(second$source, booking) - 1L)
  to_id <- c(length(residence) + match(first$target, booking) - 1L,
             length(residence) + length(booking) + match(second$target, birth) - 1L)
  counts <- c(first$n, second$n)
  node_totals <- c(
    vapply(residence, function(code) sum(first$n[first$source == code]), numeric(1)),
    vapply(booking, function(code) sum(second$n[second$source == code]), numeric(1)),
    vapply(birth, function(code) sum(second$n[second$target == code]), numeric(1)))
  hover <- paste0(stage[from_id + 1L], ": ", full_labels[from_id + 1L], "<br>",
    stage[to_id + 1L], ": ", full_labels[to_id + 1L],
    "<br>Deliveries: ", scales::comma(counts),
    "<br>", scales::percent(counts / node_totals[from_id + 1L], accuracy = 0.1),
    " of deliveries from this origin at this stage",
    "<br>", scales::percent(counts / total, accuracy = 0.1), " of all included deliveries")

  plotly::plot_ly(type = "sankey", height = max(520, 58 * max(length(residence), length(booking), length(birth))),
    orientation = "h", arrangement = "snap",
    node = list(label = short_labels, color = node_colours, pad = 22, thickness = 18,
      line = list(color = "rgba(44,40,37,0.35)", width = 0.5),
      customdata = paste0(stage, ": ", full_labels, "<br>",
        scales::percent(node_totals / total, accuracy = 0.1), " of all included deliveries"),
      hovertemplate = "%{customdata}<br>Deliveries: %{value:,.0f}<extra></extra>"),
    link = list(source = from_id, target = to_id, value = counts,
      color = grDevices::adjustcolor(node_colours[from_id + 1L], alpha.f = 0.42),
      customdata = hover, hovertemplate = "%{customdata}<extra></extra>")) |>
    plotly::layout(font = list(family = "Arial", size = 11, color = "#2c2825"),
      margin = list(l = 12, r = 12, t = 42, b = 12),
      annotations = list(
        list(x = 0, y = 1.07, xref = "paper", yref = "paper", xanchor = "left",
             text = "<b>ICB of residence</b>", showarrow = FALSE),
        list(x = 0.5, y = 1.07, xref = "paper", yref = "paper", xanchor = "center",
             text = "<b>Booking trust</b>", showarrow = FALSE),
        list(x = 1, y = 1.07, xref = "paper", yref = "paper", xanchor = "right",
             text = "<b>Birth trust</b>", showarrow = FALSE)),
      paper_bgcolor = "white", plot_bgcolor = "white") |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

# Chart objects
icb_sankey <- make_sankey(
  icb_flow, flow_icb_labels, " (residence)", " (birth)",
  icb_source_order, icb_target_order
)
trust_sankey <- make_sankey(
  trust_flow, flow_trust_labels, " (booking)", " (birth)",
  trust_source_order, trust_target_order
)
residence_booking_birth_sankey <- make_three_stage_sankey(residence_booking_birth_flow)

# 2.2 Patient flows by financial year (report section 2.2.5) ------------------
# This bit repeats the two Sankeys for each year and makes their tables.

make_year_sankeys <- function(start_year) {
  flows <- make_cohort_year_flows(start_year)
  list(
    icb_flow = flows$icb, trust_flow = flows$trust,
    icb_sankey = make_sankey(flows$icb, flow_icb_labels, " (residence)", " (birth)",
                             icb_source_order, icb_target_order),
    trust_sankey = make_sankey(flows$trust, flow_trust_labels, " (booking)", " (birth)",
                               trust_source_order, trust_target_order)
  )
}

by_year <- list(
  "2023/24" = make_year_sankeys(2023L),
  "2024/25" = make_year_sankeys(2024L),
  "2025/26" = list(icb_flow = icb_flow, trust_flow = trust_flow,
                   icb_sankey = icb_sankey, trust_sankey = trust_sankey)
)

# Section 2.2.5 tables: use the same counts and order as the Sankeys
make_flow_table <- function(flow, labels, source_label, target_label,
                            source_order, target_order, year) {
  flow_counts <- flow$links
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
    by_year[[year]]$icb_flow, flow_icb_labels, "ICB of residence", "ICB of birth",
    icb_source_order, icb_target_order, year
  )
  by_year[[year]]$trust_table <- make_flow_table(
    by_year[[year]]$trust_flow, flow_trust_labels, "Booking trust", "Birth trust",
    trust_source_order, trust_target_order, year
  )
}

# 2.3 Cluster deliveries and booking starts (report section 2.2.2) ------------
# Use the same supplied pregnancy rows as the patient-flow, geography and projection views.
activity_episodes <- report_episodes |>
  dplyr::transmute(Row, person_id_mother = Person, uniq_preg_id = Pregnancy,
    delivery_date = Delivery_date, delivery_year = Delivery_year,
    birth_code = Trust, booking_code = Booking_Trust, residence_code = Resident_ICB,
    booking_date = Booking_date, booking_year = Booking_year,
    birth_flag = Birth_flag, birth_recorded = Birth_recorded, home_birth = Home)

# Shared pathway definition for the latest-year and three-year outputs.
classify_pathway_outcome <- function(episodes) {
  # A year/date alone is not a recorded birth. Follow-up may extend beyond booking year.
  # Home births remain starts but are outside hospital delivery locations.
  dplyr::case_when(
    !episodes$birth_recorded ~ "No recorded delivery",
    episodes$home_birth %in% 1L ~ "Outside cluster",
    !episodes$home_birth %in% 0L ~ "Unknown location",
    episodes$birth_code %in% cluster_trusts ~ "Within cluster",
    grepl("^R[A-Z0-9]{2}$", episodes$birth_code) ~ "Outside cluster",
    TRUE ~ "Unknown location")
}

# Section 2.2.7 splits section 2.2.6's cluster deliveries using the actual booking trust.
# Compare trusts first, then aggregate those outcomes for the ICB/cluster views.
classify_booking_delivery_outcome <- function(episodes) {
  cluster_outcome <- classify_pathway_outcome(episodes)
  dplyr::case_when(
    cluster_outcome == "Within cluster" & episodes$birth_code == episodes$booking_code ~
      "Same trust",
    cluster_outcome == "Within cluster" ~ "Different cluster trust",
    TRUE ~ cluster_outcome)
}

# Compare the booking and delivery trusts' ICBs, not the mother's residence ICB.
# A move between two trusts in one ICB stays in the same-ICB category.
classify_booking_icb_delivery_outcome <- function(episodes) {
  cluster_outcome <- classify_pathway_outcome(episodes)
  booking_icb <- unname(provider_icb[episodes$booking_code])
  delivery_icb <- unname(provider_icb[episodes$birth_code])
  dplyr::case_when(
    cluster_outcome == "Within cluster" & delivery_icb == booking_icb ~ "Same ICB",
    cluster_outcome == "Within cluster" ~ "Different cluster ICB",
    TRUE ~ cluster_outcome)
}

cluster_activity <- list(
  note = "Combined cluster: Black Country and Birmingham and Solihull. Delivery totals require birth_flag = 1, home_birth = 0 and a cluster delivery provider; delivery year comes from birth_yr. One supplied pregnancy row contributes one. Booking-year pathways retain all starts, including home births, unknown locations and records without birth_flag = 1."
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
  activity_episodes$birth_recorded &
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
  " deliveries recorded at cluster trusts in 2025/26.")

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
  ") of these deliveries were for women living within the cluster.")

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
  ") of cluster deliveries were for women living outside the cluster.",
  if (nrow(outside_deliveries) > 0) paste0(" The largest group came from ",
    paste(outside_summary$ICB[outside_summary$Episodes == max(outside_summary$Episodes)], collapse = " and "),
    ", with ", count_text(max(outside_summary$Episodes)), " deliveries per group.") else "",
  " Residence was unknown for ", count_text(sum(deliveries_2526$residence_type == "Unknown residence")),
  " deliveries; these are excluded from the outside-resident count.")

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
  "). The share for women living outside the cluster changed from ", percent_text(delivery_trend$Outside_share[1]),
  " to ", percent_text(delivery_trend$Outside_share[3]), ".")

# Antenatal starts: booking year 2025/26; all recorded delivery dates
starts <- activity_episodes[
  !is.na(activity_episodes$booking_year) & activity_episodes$booking_year == latest_year &
    activity_episodes$booking_code %in% cluster_trusts, ]
starts$outcome <- classify_pathway_outcome(starts)
booking_codes <- unique(c(names(trust_labels)[names(trust_labels) != "Other"], starts$booking_code))
pathway_summary <- function(codes, level) {
  do.call(rbind, lapply(codes, function(code) {
    keep <- switch(level,
      Trust = starts$booking_code == code,
      ICB = unname(provider_icb[starts$booking_code]) == code,
      Cluster = rep(TRUE, nrow(starts)))
    org_starts <- starts[keep, ]
    organisation <- if (level == "Trust") {
      ifelse(is.na(unname(trust_labels[code])), code, unname(trust_labels[code]))
    } else if (level == "Cluster") cluster_label else unname(icb_labels[code])
    data.frame(Level = level, Organisation = organisation,
      Starts = nrow(org_starts), Within = sum(org_starts$outcome == "Within cluster"),
      Outside = sum(org_starts$outcome == "Outside cluster"),
      No_delivery = sum(org_starts$outcome == "No recorded delivery"),
      Unknown = sum(org_starts$outcome == "Unknown location"))
  }))
}
pathways <- rbind(pathway_summary(cluster_icbs, "ICB"), pathway_summary("Cluster", "Cluster"),
  pathway_summary(booking_codes, "Trust"))
cluster_activity$pathway_table <- gt::gt(pathways, groupname_col = "Level") |>
  gt::tab_header(title = "Recorded outcomes of cluster antenatal starts", subtitle = "Bookings in 2025/26") |>
  gt::cols_label(Organisation = "Booking organisation", Starts = "Starts",
    Within = "Delivery at cluster trust", Outside = "Other trust or home birth",
    No_delivery = "No recorded delivery", Unknown = "Delivery location unknown") |>
  gt::fmt_number(columns = c("Starts", "Within", "Outside", "No_delivery", "Unknown"), decimals = 0)
cluster_activity$pathway_text <- paste0(
  count_text(nrow(starts)), " pregnancies were booked at cluster trusts in 2025/26; ",
  count_text(sum(starts$outcome == "Outside cluster")), " had a recorded delivery outside the cluster or at home and ",
  count_text(sum(starts$outcome == "No recorded delivery")), " had no recorded delivery. ",
  "The charts group the same bookings by trust, ICB and combined cluster.")
cluster_activity$pathway_needed <- paste0(
  "No recorded delivery does not necessarily mean there was no birth; ",
  "recent bookings may not yet have enough follow-up.")

cluster_activity$residence_totals <- residence_totals
cluster_activity$local_totals <- local_deliveries
cluster_activity$outside_totals <- outside_summary
cluster_activity$delivery_trend <- delivery_trend
cluster_activity$pathways <- pathways

# Antenatal pathway views: antenatal pathway Sankeys
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

# Show both ICBs and the combined cluster together; each grid has three columns.
pathway_grid <- function(group) {
  levels <- if (group == "ICB_Cluster") c("ICB", "Cluster") else group
  rows <- pathways[pathways$Level %in% levels, ]
  columns <- 3L
  htmltools::tagList(pathway_legend,
    htmltools::div(style = paste0("display: grid; grid-template-columns: repeat(",
                                  columns, ", minmax(0, 1fr)); gap: 24px;"),
    htmltools::tagList(lapply(seq_len(nrow(rows)), function(i) {
      htmltools::div(style = "min-width: 0;",
        htmltools::h3(htmltools::span(style = "font-size: calc(1em - 2px);", rows$Organisation[i])),
        make_pathway_sankey(rows[i, , drop = FALSE]))
    }))))
}
cluster_activity$pathway_icb_sankeys <- pathway_grid("ICB_Cluster")
cluster_activity$pathway_trust_sankeys <- pathway_grid("Trust")

# 2.4 Booking-cohort outcomes over time (report sections 2.2.6-2.2.7) --------------
# This bit counts all booking starts, including those without a recorded delivery.
# ICBs group the booking trusts, as in the antenatal pathway views.
pathway_years <- analysis_years
trend_starts <- activity_episodes[
  !is.na(activity_episodes$booking_year) &
    activity_episodes$booking_year %in% pathway_years &
    activity_episodes$booking_code %in% cluster_trusts, , drop = FALSE]
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
# Reuse exactly the same percentages, changes, chart grids and table layout for both sections.
build_delivery_outcome_trends <- function(starts, trend_outcomes, trend_labels, trend_colours,
  outcome_short_labels, section_number = 7L, table_title = "Delivery pathway outcomes over time",
  group_levels = c("Trust", "ICB", "Cluster"), comparison_note = "") {
  trend_starts <- starts
  organisations <- organisations[organisations$Level %in% group_levels, , drop = FALSE]
  booking_group_text <- if ("Trust" %in% group_levels) "booking trust, ICB or combined cluster" else
    "booking ICB or combined cluster"
  aggregation_note <- if ("Trust" %in% group_levels)
    "Trust, ICB and cluster totals are alternative aggregations of the same starts." else
    "The combined cluster pools the two booking ICBs after comparing each delivery ICB with its booking ICB."
  delivery_over_time <- list()
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

  # Charts: show each outcome share and its change from the first year.
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
  trend_groups <- c(if ("Trust" %in% group_levels) "Trust",
                    if (any(c("ICB", "Cluster") %in% group_levels)) "ICB_Cluster")
  delivery_over_time$chart_grids <- stats::setNames(
    lapply(trend_groups, make_delivery_trend_grid), trend_groups)
  delivery_over_time$change_chart_grids <- stats::setNames(
    lapply(trend_groups, make_delivery_trend_grid, measure = "base_change"), trend_groups)
  delivery_over_time$table <- pathway_trends |>
    dplyr::mutate(Group = paste(Level, Organisation, sep = ": ")) |>
    dplyr::select(Group, Year, Outcome, Numerator, Denominator,
                  Share, YoY_change, Base_change) |>
    gt::gt(groupname_col = "Group") |>
    gt::tab_header(title = table_title,
                   subtitle = "Booking financial years 2023/24 to 2025/26") |>
    gt::cols_label(Year = "Booking year", Outcome = "Outcome",
      Numerator = "Numerator: outcome episodes", Denominator = "Denominator: all starts",
      Share = "% of all starts", YoY_change = "% change in share year on year",
      Base_change = "% change in share from 2023/24") |>
    gt::fmt_number(columns = c("Numerator", "Denominator"), decimals = 0, use_seps = TRUE) |>
    gt::fmt_percent(columns = c("Share", "YoY_change", "Base_change"), decimals = 1) |>
    gt::sub_missing(missing_text = "n.a.") |>
    gt::tab_source_note(source_note = paste0(
      "Share = outcome episodes / all starts in the same ", booking_group_text, " and booking year. ",
      "Changes are relative changes in that share: (current share / reference share - 1) x 100, ",
      "not percentage-point differences. n.a. = no starts, no prior year, or a zero/unavailable reference share. ",
      "The base-year change is 0% where the 2023/24 share is positive. ",
      aggregation_note, " ", comparison_note)) |>
    gt::tab_options(table.font.names = c("Open Sans", "Arial", "sans-serif"))

  # Tables: one row per organisation, with outcomes grouped under each year.
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
  change_table <- gt::gt(change_wide, groupname_col = "Level", rowname_col = "Organisation") |>
    gt::tab_header(title = "Percentage change in delivery outcome shares",
                   subtitle = "Relative change from booking financial year 2023/24") |>
    gt::tab_stubhead(label = if ("Trust" %in% group_levels) "Booking trust / ICB / cluster" else
                      "Booking ICB / cluster") |>
    gt::cols_label(.list = stats::setNames(
      as.list(outcome_short_labels[change_column_key$Outcome_index]), change_columns)) |>
    gt::fmt_percent(columns = dplyr::all_of(change_columns), decimals = 1) |>
    gt::sub_missing(missing_text = "n.a.") |>
    gt::tab_source_note(source_note = paste0(
      "Every cell is (the year's outcome share / its 2023/24 share - 1) x 100. ",
      "Outcome share = outcome episodes / all starts in that ", booking_group_text, " and year. ",
      "Positive values indicate a higher share; negative values indicate a lower share. ",
      "2023/24 is 0% where its share is positive; n.a. means no starts or a zero/unavailable baseline share. ",
      "Numerators, denominators, shares and year-on-year changes are in subsection ",
      section_number, ".1's results table. ", comparison_note)) |>
    gt::tab_options(table.font.names = c("Open Sans", "Arial", "sans-serif"),
                    table.font.size = gt::px(12), table.width = gt::px(1550),
                    container.width = "100%", container.overflow.x = TRUE)
  for (year in pathway_years) {
    change_table <- gt::tab_spanner(change_table,
      label = paste0(year, "/", substr(year + 1L, 3, 4)),
      columns = change_column_key$Column[change_column_key$Year_start == year],
      id = paste0("section-", section_number, "-change-year-", year), gather = FALSE)
  }
  delivery_over_time$change_data <- change_wide
  delivery_over_time$change_table <- change_table
  delivery_over_time
}

# Section 2.2.6: retain the original four-outcome view.
trend_starts$outcome <- classify_pathway_outcome(trend_starts)
delivery_over_time <- build_delivery_outcome_trends(trend_starts,
  trend_outcomes = c("Within cluster", "Outside cluster", "No recorded delivery", "Unknown location"),
  trend_labels = outcome_labels, trend_colours = outcome_colours,
  outcome_short_labels = c("At cluster trust", "Other trust or home", "No recorded delivery", "Location unknown"))

# Section 2.2.7: distinguish delivery at the booking trust from another cluster trust.
booking_delivery_starts <- trend_starts
booking_delivery_starts$outcome <- classify_booking_delivery_outcome(booking_delivery_starts)
booking_delivery_outcomes <- c("Same trust", "Different cluster trust",
  "Outside cluster", "No recorded delivery", "Unknown location")
booking_delivery_labels <- c("Delivery at same trust as booking",
  "Delivery at different trust within cluster", "Delivery outside cluster or home birth",
  "No recorded delivery", "Delivery location unknown")
booking_delivery_colours <- c("#00856A", "#005EB8", "#D55E00", "#727272", "#8E5EA2")
booking_delivery_over_time <- build_delivery_outcome_trends(booking_delivery_starts,
  trend_outcomes = booking_delivery_outcomes, trend_labels = booking_delivery_labels,
  trend_colours = booking_delivery_colours,
  outcome_short_labels = c("Same trust as booking", "Different cluster trust", "Outside cluster or home",
    "No recorded delivery", "Location unknown"), section_number = 8L,
  table_title = "Booking trust and delivery trust over time",
  comparison_note = "Same/different trust is decided before grouping booking trusts into ICB and cluster totals.")

# An alternative view of the same starts: compare ICBs before aggregating or calculating shares.
booking_icb_delivery_starts <- trend_starts
booking_icb_delivery_starts$outcome <- classify_booking_icb_delivery_outcome(booking_icb_delivery_starts)
booking_icb_delivery_over_time <- build_delivery_outcome_trends(booking_icb_delivery_starts,
  trend_outcomes = c("Same ICB", "Different cluster ICB", "Outside cluster", "No recorded delivery", "Unknown location"),
  trend_labels = c("Delivery ICB same as booking ICB", "Delivery ICB different to booking ICB but within cluster",
    "Delivery outside cluster or home birth", "No recorded delivery", "Delivery location unknown"),
  trend_colours = booking_delivery_colours,
  outcome_short_labels = c("Same ICB as booking", "Different cluster ICB", "Outside cluster or home",
    "No recorded delivery", "Location unknown"), section_number = 8L,
  table_title = "Booking ICB and delivery ICB over time", group_levels = c("ICB", "Cluster"),
  comparison_note = "Booking and delivery ICBs come from the trusts, not residence. Transfers between trusts in the same ICB count as same ICB.")

# Save the exploration outputs; Run analysis.R runs QA before rendering.
saveRDS(
  list(icb_sankey = icb_sankey, trust_sankey = trust_sankey, by_year = by_year,
       residence_booking_birth_flow = residence_booking_birth_flow,
       residence_booking_birth_sankey = residence_booking_birth_sankey,
       cluster_activity = cluster_activity, delivery_over_time = delivery_over_time,
       booking_delivery_over_time = booking_delivery_over_time,
       booking_icb_delivery_over_time = booking_icb_delivery_over_time,
       activity_episodes = activity_episodes, icb_flow = icb_flow, trust_flow = trust_flow,
       flow_coverage = flow_coverage, flow_coverage_table = flow_coverage_table),
  ".patient_flows.rds"
)

# 2.5 Residence and destination exploration ------------------------------
# CONTEXT_ONLY: incoming catchment mix differs from residents' destination shares.
# Check required objects and columns once. Data checks are in R/QA.R.
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
  chart_grid_style <- "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 20px; align-items: start;"
  # Keep every LA visible, with enough space for its label in the smaller charts.
  bar_chart_height <- function(n) max(240, n * 16 + 65)
  short_chart_name <- function(x) {
    x <- stringr::str_remove(x, " NHS( Foundation)? Trust$")
    stringr::str_trunc(x, width = 32)
  }

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
  sankey_chart <- plotly::plot_ly(type = "sankey", height = max(340, max(length(origins), length(destinations)) * 18),
    arrangement = "snap", node = list(label = short_chart_name(c(origin_labels, destination_labels)), pad = 9, thickness = 12,
      x = c(rep(0.01, length(origins)), rep(0.99, length(destinations))),
      y = c(seq(0.02, 0.98, length.out = length(origins)), seq(0.02, 0.98, length.out = length(destinations))),
      color = c(palette, rep("#727272", length(destinations))),
      customdata = c(origin_labels, destination_labels),
      hovertemplate = "%{customdata}<br>Deliveries: %{value:,.0f}<extra></extra>"),
    link = list(source = match(sankey_links$Origin, origins) - 1L,
      target = length(origins) + match(sankey_links$Destination_group, destinations) - 1L,
      value = sankey_links$N,
      color = grDevices::adjustcolor(palette[match(sankey_links$Origin, origins)], alpha.f = 0.45),
      customdata = paste0(origin_labels[match(sankey_links$Origin, origins)], " to ",
        destination_labels[match(sankey_links$Destination_group, destinations)],
        "<br>Deliveries: ", scales::comma(sankey_links$N),
        "<br>% of this origin group: ", scales::percent(sankey_links$Share, accuracy = 0.1)),
      hovertemplate = "%{customdata}<extra></extra>")) |>
    plotly::layout(font = list(family = "Arial", size = 10), margin = list(l = 8, r = 8, t = 10, b = 10)) |>
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

  # Short chart labels fit the compact panels; hover still gives the complete name.
  small_sankey <- function(links, origin_codes, destination_codes, percentage_label) {
    source_names <- unname(all_origin_labels[origin_codes])
    target_names <- unname(all_destination_labels[destination_codes])
    source_counts <- vapply(origin_codes, function(code) sum(links$N[links$Origin == code]), numeric(1))
    target_counts <- vapply(destination_codes, function(code) sum(links$N[links$Destination_group == code]), numeric(1))
    total <- sum(links$N)
    chart_height <- max(220, max(length(origin_codes), length(destination_codes)) * 21 + 40)
    # Leave a gap between nodes, while keeping their heights proportional to delivery counts.
    node_positions <- function(counts) {
      gap <- 14 / (chart_height - 20)
      0.02 + (cumsum(counts) - counts / 2) / sum(counts) * (0.96 - gap * (length(counts) - 1L)) +
        gap * (seq_along(counts) - 1L)
    }
    node_names <- c(source_names, target_names)
    node_counts <- c(source_counts, target_counts)
    plotly::plot_ly(type = "sankey", height = chart_height, arrangement = "fixed",
      node = list(
        label = short_chart_name(node_names),
        pad = 14, thickness = 12,
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
      plotly::layout(font = list(family = "Arial", size = 10), margin = list(l = 8, r = 8, t = 10, b = 10)) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }
  sankey_by_la_grid <- htmltools::div(
    style = chart_grid_style,
    htmltools::tagList(lapply(origins, function(origin) {
      links <- sankey_by_la_links[[origin]]
      htmltools::div(style = "min-width: 0;",
        htmltools::h3(style = "font-size: 13px; margin: 0 0 6px;", origin_labels[match(origin, origins)]),
        small_sankey(links, origin, links$Destination_group, "recorded deliveries from this LA group"))
    })))

  # Step 4b: show each destination's locally selected feeder LAs, including the home-birth chart.
  sankey_by_destination_grid <- htmltools::div(
    style = chart_grid_style,
    htmltools::tagList(lapply(destinations, function(destination) {
      links <- sankey_by_destination_links[[destination]]
      htmltools::div(style = "min-width: 0;",
        htmltools::h3(style = "font-size: 13px; margin: 0 0 6px;", destination_labels[match(destination, destinations)]),
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
  # 9.3a keeps counts and denominators; 9.3b shows changes against the same base.
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
  line_chart <- function(data, field, y_title, units = "percent", colours = NULL, show_legend = TRUE) {
    groups <- unique(data$Origin_group)
    if (is.null(colours)) colours <- stats::setNames(
      grDevices::hcl.colors(length(groups), palette = delivery_palette), groups)
    chart <- plotly::plot_ly(height = 260)
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
        line = list(color = unname(colours[groups[j]]), width = 2),
        marker = list(color = unname(colours[groups[j]]), size = 5),
        text = paste0(line$Year, "<br>", label, "<br>Deliveries: ", scales::comma(line$N),
          "<br>", y_title, ": ", value_labels),
        hovertemplate = "%{text}<extra></extra>", connectgaps = FALSE)
    }
    plotly::layout(chart, xaxis = list(title = "", tickmode = "array", tickvals = 1:3, ticktext = years),
      yaxis = list(title = y_title, tickformat = if (units == "percent") ".0%" else if (units == "pp") ".1f" else ",.0f", zeroline = TRUE),
      showlegend = show_legend, legend = list(orientation = "v", x = 1.02, y = 1, font = list(size = 9)),
      margin = list(l = 65, r = if (show_legend) 160 else 15, t = 10, b = 35),
      font = list(family = "Arial", size = 10)) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }
  # Step 7: make the organisation charts and resident-geography tables.
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
      origin_chart <- plotly::plot_ly(chart_origin, height = bar_chart_height(nrow(chart_origin)),
        x = ~Share, y = ~LA, type = "bar", orientation = "h",
        marker = list(color = chart_origin$Share, colorscale = list(c(0, "#DCEAF4"), c(1, delivery_chart_blue)),
          cmin = 0, cmax = max(chart_origin$Share), showscale = FALSE),
        hovertext = ~paste0(Resident_LA, "<br>Deliveries: ", scales::comma(N), "<br>% of organisation: ", scales::percent(Share, accuracy = 0.1)),
        hovertemplate = "%{hovertext}<extra></extra>", showlegend = FALSE) |>
        plotly::layout(xaxis = list(title = "% of organisation deliveries", tickformat = ".0%"),
          yaxis = list(title = "", categoryorder = "array", categoryarray = chart_origin$LA, autorange = "reversed",
            tickmode = "array", tickvals = chart_origin$LA, ticktext = chart_origin$Resident_LA, automargin = TRUE),
          margin = list(l = 25, r = 10, t = 10, b = 45), font = list(family = "Arial", size = 10)) |>
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
      age_chart <- plotly::plot_ly(height = bar_chart_height(nrow(chart_origin)))
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
          margin = list(l = 25, r = 10, t = 10, b = 45), font = list(family = "Arial", size = 10)) |>
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
    list(origin_chart = origin_chart, age_chart = age_chart, table = origin_table, age_table = age_table,
      count_chart = line_chart(grouped, "N", "Deliveries", units = "count"),
      stability_chart = line_chart(grouped, "Share", "% of organisation deliveries from LA"),
      change_chart = line_chart(grouped, "Count_change", "% change in deliveries from 2023/24"),
      share_change_chart = line_chart(grouped, "Share_change_pp", "Change in LA contribution (percentage points)", units = "pp"),
      count_table = wide_trend_table(trend, "All resident LA contributions, 2023/24-2025/26"),
      change_table = wide_trend_table(trend, "Change in all resident LA contributions from 2023/24", changes = TRUE),
      trend_groups = grouped, visible_las = visible_las, age_mix = if (nrow(origin)) plot_ages else NULL)
  }), organisations$Key)
  # Step 8: compare named and additional recorded trusts, then ICBs and cluster.
  # Two compact columns replace per-organisation chart tabs and horizontal scrolling.
  mapping_groups <- list(
    Trust = organisations[organisations$Level == "Trust", , drop = FALSE],
    ICB_Cluster = organisations[organisations$Level %in% c("ICB", "Cluster"), , drop = FALSE])
  age_key <- htmltools::div(
    style = "display: flex; flex-wrap: wrap; gap: 6px 16px; margin-bottom: 12px; font-size: 11px;",
    htmltools::tagList(lapply(seq_along(age_bands), function(i) {
      htmltools::span(style = "display: inline-flex; align-items: center; gap: 6px;",
        htmltools::span(style = paste0("display: inline-block; width: 16px; height: 12px; background: ",
          age_colours[i], ";"), `aria-hidden` = "true"), age_bands[i])
    })))
  mapping_grid <- function(rows, field) {
    htmltools::tagList(if (field == "age_chart") age_key,
      htmltools::div(
        style = chart_grid_style,
        htmltools::tagList(lapply(seq_len(nrow(rows)), function(i) {
          htmltools::div(style = "min-width: 0;",
            htmltools::h4(style = "font-size: 13px; margin: 0 0 6px;", rows$Organisation[i]),
            organisation_plots[[rows$Key[i]]][[field]])
        }))))
  }
  mapping_grids <- lapply(mapping_groups, function(rows) list(
    origin_chart = mapping_grid(rows, "origin_chart"),
    age_chart = mapping_grid(rows, "age_chart")))
  # A common LA colour key for each grid leaves space for the actual trend charts.
  trend_key <- dplyr::bind_rows(c(
    list(data.frame(Origin_group = character(), Origin_name = character())),
    lapply(organisation_plots, function(x) x$trend_groups))) |>
    dplyr::distinct(Origin_group, Origin_name)
  trend_colours <- stats::setNames(grDevices::hcl.colors(nrow(trend_key), palette = delivery_palette),
    trend_key$Origin_group)
  trend_specs <- list(
    count = list(field = "N", title = "Deliveries", units = "count"),
    contribution = list(field = "Share", title = "% from LA", units = "percent"),
    count_change = list(field = "Count_change", title = "% change in deliveries", units = "percent"),
    share_change = list(field = "Share_change_pp", title = "Change in LA share (pp)", units = "pp"))
  trend_grid <- function(rows, spec) {
    grid_data <- lapply(rows$Key, function(key) organisation_plots[[key]]$trend_groups)
    shown <- unique(unlist(lapply(grid_data, function(data) data$Origin_group), use.names = FALSE))
    key_rows <- trend_key[trend_key$Origin_group %in% shown, , drop = FALSE]
    key <- htmltools::div(
      style = "display: flex; flex-wrap: wrap; gap: 5px 16px; margin-bottom: 12px; font-size: 11px;",
      htmltools::tagList(lapply(seq_len(nrow(key_rows)), function(i) {
        htmltools::span(style = "display: inline-flex; align-items: center; gap: 5px;",
          htmltools::span(style = paste0("display: inline-block; width: 16px; border-top: 3px solid ",
            trend_colours[key_rows$Origin_group[i]], ";"), `aria-hidden` = "true"), key_rows$Origin_name[i])
      })))
    htmltools::tagList(key, htmltools::div(style = chart_grid_style,
      htmltools::tagList(lapply(seq_len(nrow(rows)), function(i) {
        chart <- if (is.null(grid_data[[i]])) {
          htmltools::div("No recorded hospital deliveries in the three years.")
        } else {
          line_chart(grid_data[[i]], spec$field, spec$title, spec$units,
            colours = trend_colours, show_legend = FALSE)
        }
        htmltools::div(style = "min-width: 0;",
          htmltools::h4(style = "font-size: 13px; margin: 0 0 6px;", rows$Organisation[i]), chart)
      }))))
  }
  chart_grids <- list(
    origin = lapply(mapping_grids, function(x) x$origin_chart),
    age = lapply(mapping_grids, function(x) x$age_chart))
  for (metric in names(trend_specs)) {
    chart_grids[[metric]] <- lapply(mapping_groups, trend_grid, spec = trend_specs[[metric]])
  }
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
    matrix_values_table = matrix_values_table,
    sankey_chart = htmltools::div(style = "max-width: 760px;", sankey_chart), sankey_links = sankey_links,
    sankey_by_la_grid = sankey_by_la_grid, sankey_by_destination_grid = sankey_by_destination_grid,
    sankey_by_la_links = sankey_by_la_links, sankey_by_destination_links = sankey_by_destination_links,
    sankey_focus_n = sankey_focus_n, sankey_top_n = sankey_top_n,
    hospital_mapping = hospital_mapping, la_trends = la_trends, outputs = organisation_plots,
    mapping_grids = mapping_grids, mapping_tables = mapping_tables, chart_grids = chart_grids,
    all_results_table = wide_trend_table(la_trends, "All organisations: hospital deliveries by resident LA", grouped = TRUE),
    all_changes_table = wide_trend_table(la_trends, "All organisations: changes from 2023/24", grouped = TRUE, changes = TRUE))
}

# Save the finished charts and tables for index.qmd.
# Errors stop here; the previous saved output was removed before preparation.
delivery_geography <- build_delivery_geography()
saveRDS(delivery_geography, ".delivery_geography.rds")

# 3. COMBINED PROJECTION MODEL ===========================================
# Read the steps in order. Local shares retain LA(R) and Age (1).
# National conversion factors vary by age and apply across all LAs.

# 3.1 Pool MSDS counts for delivery and booking proportions --------------
build_allocation_tables <- function(cohort, resident_totals,
    calibration_years = c("2023/24", "2024/25", "2025/26")) {
  episodes <- prepare_report_cohort(cohort)
  usable <- function(x) !x %in% c("Unknown LA", "L99999999", "") & !is.na(x)
  residents <- resident_totals |>
    dplyr::filter(as.character(birth_yr) %in% calibration_years) |>
    dplyr::transmute(LA = report_code(la_of_mother, "Unknown LA"),
      Age = report_age(age_at_birth_mother), MSDS_resident_count = report_number(count)) |>
    dplyr::filter(usable(LA), !is.na(Age)) |>
    dplyr::group_by(LA, Age) |>
    dplyr::summarise(MSDS_resident_count = sum(MSDS_resident_count), .groups = "drop")

  # Delivery denominator: all recorded resident maternities, including home births.
  deliveries <- episodes |>
    dplyr::filter(Year %in% calibration_years, Birth_recorded, usable(LA), !is.na(Age)) |>
    dplyr::mutate(Destination = dplyr::case_when(Home %in% 1 ~ "Home",
      !Home %in% c(0, 1) | Trust == "Unknown provider" ~ "Unknown_location", TRUE ~ Trust)) |>
    dplyr::count(LA, Age, Destination, name = "Count")
  represented <- deliveries |> dplyr::group_by(LA, Age) |>
    dplyr::summarise(Represented = sum(Count), .groups = "drop")
  coverage <- dplyr::full_join(residents, represented, by = c("LA", "Age"))
  coverage$Represented[is.na(coverage$Represented)] <- 0
  if (anyNA(coverage$MSDS_resident_count) ||
      any(coverage$Represented > coverage$MSDS_resident_count))
    stop("Delivery counts exceed or lack the matching resident LA/Age (1) denominator.")
  deliveries <- dplyr::bind_rows(deliveries, coverage |>
    dplyr::filter(MSDS_resident_count > Represented) |>
    dplyr::transmute(LA, Age, Destination = "Not_represented",
      Count = MSDS_resident_count - Represented)) |>
    dplyr::left_join(residents, by = c("LA", "Age"))
  deliveries$Proportion <- deliveries$Count / deliveries$MSDS_resident_count

  # Booking denominator: ALL recorded bookings in the three booking years.
  # Do not filter to birth_flag = 1 or use the delivery denominator here.
  # Both tables use the supplied age_at_birth_mother field; no booking age is inferred.
  booking_years <- as.integer(substr(calibration_years, 1, 4))
  bookings <- episodes |>
    dplyr::filter(Booking_year %in% booking_years, usable(LA), !is.na(Age)) |>
    dplyr::transmute(LA, Age, Destination = Booking_Trust) |>
    dplyr::count(LA, Age, Destination, name = "Count") |>
    dplyr::group_by(LA, Age) |>
    dplyr::mutate(MSDS_booking_total = sum(Count)) |> dplyr::ungroup()
  bookings$Proportion <- bookings$Count / bookings$MSDS_booking_total
  list(residents = residents, delivery = deliveries, booking = bookings,
    years = calibration_years)
}

build_projection_master <- function(cohort, resident_totals, ons_births, registration_counts,
    baseline_fy = "2025/26", reference_year = 2026L, end_year = NULL,
    include_las = character(), calibration_years = c("2023/24", "2024/25", "2025/26"),
    start_year = reference_year) {
  # MODEL_INPUT: MSDS_RESIDENTS, MSDS_DELIVERIES, MSDS_BOOKINGS
  allocation <- build_allocation_tables(cohort, resident_totals, calibration_years)
  residents <- allocation$residents
  selected_las <- unique(c(include_las, residents$LA, allocation$booking$LA))

  # 3.2 ONS births (L): retain LA(R), source age and projection year --------
  # MODEL_INPUT: ONS_BIRTHS_L
  ons <- ons_births |>
    dplyr::filter(area_code %in% selected_las, age_group != "All ages",
      year >= start_year, year <= if (is.null(end_year)) Inf else end_year) |>
    dplyr::transmute(LA = area_code, LA_name = as.character(area_name),
      ONS_age = as.integer(age_group), Year = year, ONS_live_births = projected_births)
  if (!nrow(ons) || anyDuplicated(ons[c("LA", "ONS_age", "Year")]) ||
      any(!ons$ONS_age %in% 15:44 | !is.finite(ons$ONS_live_births) | ons$ONS_live_births < 0))
    stop("Missing, duplicate or invalid ONS LA/age/year counts.")
  expected <- tidyr::expand_grid(LA = unique(ons$LA), ONS_age = 15:44,
    Year = seq(start_year, max(ons$Year)))
  if (nrow(dplyr::anti_join(expected, ons, by = c("LA", "ONS_age", "Year"))))
    stop("ONS coverage is incomplete for a selected LA/age/year.")

  # 3.3 Split combined ONS ages using the local maternity age mix ----------
  # MODEL_INPUT: MSDS_RESIDENTS (pooled 2023/24--2025/26 counts)
  # Ages 16--43: weight 1 (their own ONS count remains unchanged).
  # At the extremes: exact-age MSDS count / matching combined-group count.
  # Keep additional valid supplied ages; do not discard their contribution.
  parameters <- read_analysis_parameters()
  model_ages <- sort(unique(c(seq(parameters[["minimum_maternal_age"]],
    parameters[["maximum_maternal_age"]]), residents$Age, allocation$booking$Age)))
  age_split <- tidyr::expand_grid(LA = unique(ons$LA), Age = model_ages) |>
    dplyr::left_join(residents, by = c("LA", "Age"))
  age_split$MSDS_resident_count[is.na(age_split$MSDS_resident_count)] <- 0
  age_split$ONS_age <- report_ons_age(age_split$Age)
  age_split <- age_split |> dplyr::group_by(LA, ONS_age) |>
    dplyr::mutate(MSDS_ONS_age_count = sum(MSDS_resident_count)) |> dplyr::ungroup()
  age_split$Age_weight <- 1
  boundary <- age_split$ONS_age %in% c(15, 44)
  age_split$Age_weight[boundary] <- ifelse(age_split$MSDS_ONS_age_count[boundary] > 0,
    age_split$MSDS_resident_count[boundary] / age_split$MSDS_ONS_age_count[boundary], 0)
  # No boundary age evidence: retain its source count as one unsplit row.
  unsplit <- age_split |> dplyr::filter(boundary, MSDS_ONS_age_count == 0) |>
    dplyr::distinct(LA, ONS_age, MSDS_ONS_age_count) |>
    dplyr::mutate(Age = NA_real_, MSDS_resident_count = 0, Age_weight = 1)
  age_split <- dplyr::bind_rows(age_split |> dplyr::filter(!boundary | MSDS_ONS_age_count > 0), unsplit)
  master <- merge(ons, age_split, by = c("LA", "ONS_age"), all.x = TRUE, sort = FALSE)
  master$Exact_age_live_births <- master$ONS_live_births * master$Age_weight

  # 3.4 Stillbirth uplift, then multiplicity conversion -------------------
  # MODEL_INPUT: ONS_STILLBIRTH_RATIO, ONS_MULTIPLICITY
  # Source age <14 applies equally below 14; source 50+ equally to ages 50+.
  ons_modifications <- prepare_projection_age_factors(registration_counts, model_ages)$modifications
  master <- dplyr::left_join(master, ons_modifications,
    by = c("Age", "ONS_age" = "ONS_source_age"))
  no_split <- is.na(master$Age)
  master$Maternity_modifier <- master$Stillbirth_factor * master$Multiplicity_factor
  master$Adjusted_births_ls <- master$Exact_age_live_births * master$Stillbirth_factor
  master$Adjusted_maternities <- master$Adjusted_births_ls * master$Multiplicity_factor
  if (any(!is.finite(master$Adjusted_maternities))) stop("Missing age-specific ONS conversion factor.")

  # 3.5 Separate volumes, then independent destination allocations ---------
  # Delivery base: maternities reaching a live birth or stillbirth.
  master$Delivery_projection_base <- master$Adjusted_maternities
  # Booking base: same volume for now. No early-loss uplift applied.
  # A later agreed booking adjustment belongs HERE, before booking allocation.
  master$Booking_projection_base <- master$Adjusted_maternities

  add_allocations <- function(master, rates, prefix, denominator, base_column) {
    destinations <- sort(unique(c(as.character(rates$Destination), "Unallocated")))
    labels <- make.names(destinations)
    if (anyDuplicated(labels)) stop("Destination column labels are not unique.")
    totals <- unique(rates[c("LA", "Age", denominator)])
    names(totals)[3] <- paste0(prefix, "_denominator")
    master <- dplyr::left_join(master, totals, by = c("LA", "Age"))
    total <- master[[paste0(prefix, "_denominator")]]
    available <- !is.na(total) & total > 0
    for (i in seq_along(destinations)) {
      share_name <- paste0(prefix, "_proportion_", labels[i])
      count_name <- paste0(prefix, "_count_", labels[i])
      if (destinations[i] == "Unallocated") {
        master[[share_name]] <- as.numeric(!available)
      } else {
        destination_rates <- rates[rates$Destination == destinations[i], c("LA", "Age", "Proportion")]
        names(destination_rates)[3] <- share_name
        master <- dplyr::left_join(master, destination_rates, by = c("LA", "Age"))
        master[[share_name]][is.na(master[[share_name]])] <- 0
      }
      master[[count_name]] <- master[[base_column]] * master[[share_name]]
    }
    count_columns <- paste0(prefix, "_count_", labels)
    if (any(abs(rowSums(master[count_columns]) - master[[base_column]]) > 1e-7))
      stop(prefix, " allocations do not conserve their starting volume.")
    attr(master, paste0(prefix, "_destinations")) <- stats::setNames(destinations, labels)
    master
  }
  master <- add_allocations(master, allocation$delivery, "Delivery",
    "MSDS_resident_count", "Delivery_projection_base")
  delivery_names <- attr(master, "Delivery_destinations")
  master <- add_allocations(master, allocation$booking, "Booking",
    "MSDS_booking_total", "Booking_projection_base")
  booking_names <- attr(master, "Booking_destinations")
  master$Age_treatment <- ifelse(no_split, "Unsplit: no local age distribution",
    ifelse(master$ONS_age %in% c(15, 44), "Pooled local MSDS age split", "Exact age"))
  master$Baseline_FY <- baseline_fy
  master$ONS_reference_year <- reference_year
  master$Source_cell_id <- paste(master$LA, master$ONS_age, master$Year, sep = "/")
  master <- master[order(master$LA, master$Age, master$Year), ]
  master$Master_row_id <- seq_len(nrow(master))
  # Put the three review columns beside the LA(R), Age (1) and year keys.
  # Exact_age_live_births is births (L) after the age split ONLY.
  review_columns <- c("LA", "Age", "Year", "Exact_age_live_births",
    "Delivery_projection_base", "Booking_projection_base")
  master <- master[c(review_columns, setdiff(names(master), review_columns))]
  attr(master, "Delivery_destinations") <- delivery_names
  attr(master, "Booking_destinations") <- booking_names
  attr(master, "allocation_tables") <- allocation
  attr(master, "resident_counts") <- residents
  attr(master, "unmatched_las") <- setdiff(selected_las, unique(ons$LA))
  attr(master, "registration_source") <- registration_counts
  attr(master, "ons_modifications") <- ons_modifications
  attr(master, "calculation_version") <- "ons-shared-age-calculations-v8"
  master
}

# 3.6 Read completed columns for charts; no rates are recalculated here ----
projection_destination_rows <- function(master, type = "Delivery") {
  destinations <- attr(master, paste0(type, "_destinations"))
  rates <- attr(master, "allocation_tables")[[tolower(type)]]
  rows <- lapply(names(destinations), function(code) {
    result <- master[c("LA", "LA_name", "ONS_age", "Age", "Year", "Source_cell_id",
      "ONS_live_births", "Exact_age_live_births", "Age_weight", "Stillbirth_ratio",
      "Stillbirth_factor", "Multiplicity_factor", "Maternity_modifier", "Adjusted_maternities",
      "MSDS_resident_count", "Age_treatment", "Baseline_FY", "ONS_reference_year")]
    result$Allocation_volume <- master[[paste0(type, "_projection_base")]]
    result$Allocation_denominator <- master[[paste0(type, "_denominator")]]
    result$Delivery_destination <- destinations[[code]]
    result$Destination_proportion <- master[[paste0(type, "_proportion_", code)]]
    result$Projected_maternities <- master[[paste0(type, "_count_", code)]]
    result$Allocation_available <- destinations[[code]] != "Unallocated"
    result$Allocation_status <- if (destinations[[code]] == "Unallocated") "Unallocated: no MSDS age denominator" else
      if (destinations[[code]] == "Home") "Home" else
      if (destinations[[code]] == "Unknown_location") "Unknown location" else
      if (destinations[[code]] == "Not_represented") "Not represented in linked cohort" else if (type == "Booking" && destinations[[code]] == "Unknown") "Unknown booking provider" else
      if (type == "Booking") "Recorded booking trust" else "Recorded delivery trust"
    selected <- rates[rates$Destination == destinations[[code]], c("LA", "Age", "Count")]
    names(selected)[3] <- "MSDS_destination_count"
    result <- dplyr::left_join(result, selected, by = c("LA", "Age"))
    result$MSDS_destination_count[is.na(result$MSDS_destination_count)] <- 0
    result[result$Destination_proportion > 0, , drop = FALSE]
  })
  result <- dplyr::bind_rows(rows)
  if (type == "Booking") {
    names(result)[names(result) == "Delivery_destination"] <- "Booking_destination"
    names(result)[names(result) == "Projected_maternities"] <- "Projected_bookings"
  }
  result
}

projection_column_dictionary <- function(master) {
  tibble::tibble(Column = names(master)) |>
    dplyr::mutate(Meaning = dplyr::case_when(
      Column == "LA" ~ "LA(R): resident local authority.",
      Column == "Age" ~ "Age (1); NA only where a boundary group cannot be split.",
      Column == "ONS_age" ~ "ONS source row: 15 means <=15; 44 means >=44.",
      Column == "Year" ~ "ONS year ending mid-year.",
      Column == "ONS_live_births" ~ "Source births (L); repeated across ages within a combined group.",
      Column == "Age_weight" ~ "Pooled exact-age resident maternities / matching combined-age total; 1 for ages 16--43.",
      Column == "Exact_age_live_births" ~ "ONS births (L) after the age split only: ONS_live_births * Age_weight; before stillbirth, multiplicity or booking adjustments.",
      Column == "Stillbirth_ratio" ~ "Mean annual Table 5 stillbirths / live births, 2023--2025.",
      Column == "Stillbirth_factor" ~ "1 + Stillbirth_ratio; births (L) to births (L+S).",
      Column == "Multiplicity_factor" ~ "Mean annual Table 5 maternities / births (L+S), 2023--2025.",
      Column == "Adjusted_births_ls" ~ "Exact_age_live_births * Stillbirth_factor.",
      Column == "Adjusted_maternities" ~ "Adjusted_births_ls * Multiplicity_factor; counted once per LA/age/year.",
      Column == "Delivery_projection_base" ~ "Adjusted maternities available for delivery allocation.",
      Column == "Booking_projection_base" ~ "Volume available for booking allocation; currently adjusted maternities, with no early-loss uplift.",
      grepl("_proportion_", Column) ~ "Pooled destination count / corresponding LA/age total, fixed across projection years.",
      grepl("_count_", Column) ~ "Corresponding Delivery_projection_base or Booking_projection_base * destination proportion.",
      grepl("denominator", Column) ~ "Pooled resident maternities (delivery) or ALL bookings (booking) at this LA(R)/Age (1).",
      TRUE ~ "Source identifier or intermediate count; see the numbered model steps."))
}

allocation_table_wide <- function(rates, prefix = "Delivery") {
  total <- if (prefix == "Delivery") "MSDS_resident_count" else "MSDS_booking_total"
  rates |>
    dplyr::mutate(Destination = make.names(Destination)) |>
    tidyr::pivot_wider(id_cols = dplyr::all_of(c("LA", "Age", total)),
      names_from = Destination, values_from = c(Count, Proportion),
      names_glue = paste0(prefix, "_{.value}_{Destination}"), values_fill = 0)
}

# 3.7 Build forward and retrospective master tables ----------------------
# Same calculations and pooled rates; only the projection years differ.
# Keep all source years for resident charts; forward destination charts start at mid-2026.
projection_master_all_years <- build_projection_master(final_cohort_2326, total_births_by_LA_2326,
  snpp, registration_counts, include_las = area_config$area_code, start_year = min(snpp$year))
projection_master <- projection_master_all_years[
  projection_master_all_years$Year >= projection_reference_year, ]
# Same pooled rates for the historical comparison: retrospective, not held-out validation.
projection_master_backtest <- build_projection_master(final_cohort_2326, total_births_by_LA_2326,
  snpp, registration_counts, baseline_fy = "2023/24", reference_year = 2024L,
  end_year = 2026L, include_las = read_area_config()$area_code)
# 3.8 Expose the factors, proportions and column key for inspection -------
# These objects describe the inputs already used in projection_master.
ons_modifications <- attr(projection_master, "ons_modifications")
projection_master_dictionary <- projection_column_dictionary(projection_master)
delivery_proportions <- allocation_table_wide(attr(projection_master, "allocation_tables")$delivery)
booking_proportions <- allocation_table_wide(attr(projection_master, "allocation_tables")$booking, "Booking")

# 3.8a Resident charts: sum the same adjusted counts before allocation ----
# CONTEXT_ONLY: report summaries; no additional conversion or rate applied.
resident_outputs <- summarise_resident_projections(projection_master_all_years, area_config)

# 3.8b Compare resident maternities from ONS, MSDS and the model -----------
# CONTEXT_ONLY: all ages and delivery destinations in the resident-LA file.
# Use the supplied resident totals, including residents outside cluster trusts.
build_resident_comparison <- function(resident_totals, observed, projected,
    areas = read_area_config(), years = projection_history_years) {
  # Sum ages AFTER reading all resident counts; unknown ages still count here.
  msds <- resident_totals |>
    dplyr::transmute(area_code = report_code(la_of_mother, "Unknown LA"),
      Period = as.character(birth_yr), maternities = report_number(count)) |>
    dplyr::filter(area_code %in% areas$area_code, Period %in% years) |>
    dplyr::group_by(area_code, Period) |>
    dplyr::summarise(maternities = sum(maternities), .groups = "drop") |>
    dplyr::left_join(areas[c("area_code", "area_name")], by = "area_code") |>
    dplyr::mutate(Source = "MSDS recorded maternities",
      Date = as.Date(paste0(as.integer(substr(Period, 1, 4)) + 1L, "-03-31")))
  # Keep source periods explicit. Dates position the end of each full year.
  registered <- observed |>
    dplyr::transmute(area_name = as.character(area_name), maternities,
      Period = as.character(year), Source = "ONS published maternities",
      Date = as.Date(paste0(year, "-12-31")))
  estimates <- projected |>
    dplyr::transmute(area_name = as.character(area_name), maternities,
      Period = paste0("Mid-", year), Source = "Adjusted maternity projections",
      Date = as.Date(paste0(year, "-06-30")))
  data <- dplyr::bind_rows(registered, msds |> dplyr::select(-area_code), estimates)
  series <- c("ONS published maternities", "MSDS recorded maternities", "Adjusted maternity projections")
  data$Source <- factor(data$Source, levels = series)
  data$area_name <- factor(data$area_name, levels = areas$area_name)
  data <- data |> dplyr::arrange(Source, area_name, Date)
  recorded <- data |> dplyr::filter(Source != "Adjusted maternity projections")
  boundary <- max(recorded$Date)
  start <- min(data$Date)
  finish <- max(data$Date)

  # LA colour is constant across sources; line and marker distinguish sources.
  chart <- ggplot2::ggplot(data, ggplot2::aes(Date, maternities, colour = area_name,
      group = interaction(area_name, Source), linetype = Source)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(data = recorded, ggplot2::aes(shape = Source), size = 2.5) +
    ggplot2::geom_vline(xintercept = boundary, colour = "#666666", linetype = "dotted") +
    ggplot2::annotate("text", x = start + as.numeric(boundary - start) / 2, y = Inf,
      label = "Historical comparison", vjust = 1.3, size = 3.5) +
    ggplot2::annotate("text", x = boundary + as.numeric(finish - boundary) / 2, y = Inf,
      label = "Future projection", vjust = 1.3, size = 3.5) +
    ggplot2::scale_colour_manual(values = area_colour_palette(areas)) +
    ggplot2::scale_linetype_manual(values = stats::setNames(c("solid", "solid", "dashed"), series)) +
    ggplot2::scale_shape_manual(values = stats::setNames(c(16, 15, NA), series), drop = FALSE) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y",
      expand = ggplot2::expansion(mult = c(0.025, 0.015))) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
      expand = ggplot2::expansion(mult = c(0.02, 0.16))) +
    ggplot2::labs(x = "Source period end", y = "Resident maternities", colour = "LA(R)",
      linetype = "Series", shape = "Series") +
    ons_plot_theme() +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1, nrow = 1,
      override.aes = list(linetype = "solid", shape = NA)),
      linetype = ggplot2::guide_legend(order = 2, nrow = 1),
      shape = ggplot2::guide_legend(order = 2, nrow = 1)) +
    ggplot2::theme(legend.box = "vertical")
  list(data = data, msds = msds, latest_recorded_date = boundary, chart = chart)
}

ons_exploration <- c(ons_exploration, resident_outputs)
ons_exploration$ons_modifications <- ons_modifications
ons_exploration$resident_comparison <- build_resident_comparison(
  total_births_by_LA_2326, observed_trajectory, resident_outputs$projected_trajectory, area_config)
saveRDS(ons_exploration, ".ons_exploration.rds")

# 3.9 Summaries and report outputs ---------------------------------------
# All model multipliers have already been applied at LA(R) x Age (1).
# Check the inputs are available before starting. Counting checks belong in QA.R.
required_objects <- c("projection_master", "projection_master_backtest", "projection_master_dictionary", "final_cohort_2326", "total_births_by_LA_2326",
  "trust_labels", "icb_labels", "provider_icb", "cluster_icbs", "cluster_trusts")
absent <- required_objects[!vapply(required_objects,
  function(name) exists(name, inherits = TRUE), logical(1))]
if (length(absent)) stop("Missing objects: ", paste(absent, collapse = ", "),
  ". Use Run analysis.R.", call. = FALSE)
required_columns <- list(
  final_cohort_2326 = c("person_id_mother", "uniq_preg_id", "birth_yr", "birth_flag",
    "la_of_mother", "age_at_birth_mother", "birth_provider", "home_birth"),
  total_births_by_LA_2326 = c("birth_yr", "la_of_mother", "age_at_birth_mother", "count"))
for (name in names(required_columns)) {
  missing_columns <- setdiff(required_columns[[name]], names(get(name)))
  if (length(missing_columns)) stop(name, " is missing: ",
    paste(missing_columns, collapse = ", "), call. = FALSE)
}

# Local lookup files travel with the project; codes match the data and names label the report.
projection_la_lookup <- readr::read_csv("data/lookups/la_names.csv",
  col_types = readr::cols(LA = readr::col_character(), LA_name = readr::col_character()))
projection_trust_lookup <- readr::read_csv("data/lookups/trust_names.csv",
  col_types = readr::cols(Trust = readr::col_character(), Trust_name = readr::col_character())) |>
  dplyr::mutate(Trust_name = stringr::str_replace_all(stringr::str_to_title(Trust_name),
                                                    "\\bNhs\\b", "NHS"))
projection_catchment_las <- readr::read_csv("config/local_authorities.csv",
  show_col_types = FALSE)$area_code

make_projection_coverage_table <- function(coverage, baseline_fy) {
  coverage |>
    dplyr::select(Level, Organisation, Recorded_births, Home_births, Unknown_home_flag,
      Observed_baseline, Matched_allocation, Unmatched) |>
    gt::gt(groupname_col = "Level", rowname_col = "Organisation") |>
    gt::tab_header(title = paste(baseline_fy, "delivery inclusion and projection coverage")) |>
    gt::cols_label(Recorded_births = "Recorded maternities with this provider",
      Home_births = "Home: excluded", Unknown_home_flag = "Unknown location: separate",
      Observed_baseline = "Non-home baseline", Matched_allocation = "Matched LA/age",
      Unmatched = "Not projected") |>
    gt::fmt_number(columns = c("Recorded_births", "Home_births", "Unknown_home_flag",
      "Observed_baseline", "Matched_allocation", "Unmatched"), decimals = 0) |>
    gt::tab_source_note("One pregnancy per row; birth flag = 1 and the stated delivery year. Recorded maternities = home + unknown home flag + non-home baseline. Baseline = Matched LA/age + not projected. Home births can involve NHS care; they are excluded only from these trust-location totals.") |>
    gt::tab_options(container.width = "100%", container.overflow.x = TRUE)
}

build_first_draft_projections <- function(baseline_fy = projection_baseline,
  ons_reference_year = projection_reference_year, projection_end = projection_end_year,
  prepare_outputs = TRUE, master = if (baseline_fy == "2023/24") projection_master_backtest else projection_master) {
  stopifnot(identical(attr(master, "calculation_version"), "ons-shared-age-calculations-v8"),
    all(master$Baseline_FY == baseline_fy), all(master$ONS_reference_year == ons_reference_year))
  if (!is.null(projection_end)) master <- dplyr::filter(master, Year <= projection_end)
  wide_master <- master
  master <- projection_destination_rows(wide_master)
  age_bands <- projection_age_bands
  age_colours <- projection_age_colours
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
  # One supplied row per pregnancy; no additional deduplication.
  cohort <- prepare_report_cohort(final_cohort_2326)
  episodes <- cohort |> dplyr::filter(Year == baseline_fy, Birth_recorded)
  # Location classification: a flagged home birth is never a delivery at a trust.
  target_episodes <- episodes |>
    dplyr::filter(Trust %in% cluster_trusts, Home == 0)

  # Fix the organisation list across both the forward projection and retrospective check.
  recorded_trusts <- cohort |>
    dplyr::filter(Year %in% projection_history_years, Birth_recorded, Home == 0) |>
    dplyr::pull(Trust)
  recorded_booking_trusts <- cohort |>
    dplyr::filter(Booking_year %in% as.integer(substr(projection_history_years, 1, 4))) |>
    dplyr::pull(Booking_Trust)
  trust_codes <- unique(c(names(trust_labels)[names(trust_labels) != "Other"],
    sort(intersect(c(recorded_trusts, recorded_booking_trusts), cluster_trusts))))
  trust_names <- projection_trust_lookup$Trust_name[match(trust_codes, projection_trust_lookup$Trust)]
  named_trusts <- trust_codes %in% names(trust_labels)
  trust_names[named_trusts] <- unname(trust_labels[trust_codes[named_trusts]])
  trust_names[is.na(trust_names)] <- paste0("Trust ", trust_codes[is.na(trust_names)])
  organisations <- dplyr::bind_rows(
    data.frame(Key = paste0("trust-", tolower(trust_codes)), Level = "Trust",
               Code = trust_codes, Organisation = trust_names),
    data.frame(Key = paste0("icb-", tolower(cluster_icbs)), Level = "ICB",
               Code = cluster_icbs, Organisation = unname(icb_labels[cluster_icbs])),
    data.frame(Key = "cluster", Level = "Cluster", Code = "Cluster", Organisation = "Combined cluster"))

  # Source counts and projected rows were prepared in the model steps above.
  resident_denominators <- attr(wide_master, "resident_counts") |>
    dplyr::rename(Denominator = MSDS_resident_count) |>
    dplyr::mutate(Age_band = report_age_band(Age))
  # Match feeder labels and accounting to the full three-year allocation period.
  feeder_las <- attr(wide_master, "allocation_tables")$delivery |>
    dplyr::filter(Destination %in% cluster_trusts, Count > 0) |>
    dplyr::pull(LA) |> unique() |> sort()
  ons_births <- master |> dplyr::distinct(LA, LA_name) |>
    dplyr::rename(area_code = LA, area_name = LA_name)
  geography_changes <- data.frame(LA = character(), LA_name = character(),
    ONS_source_codes = character(), Treatment = character())
  ons_la_names <- ons_births |>
    dplyr::distinct(area_code, area_name) |>
    dplyr::rename(LA = area_code, LA_name = area_name)
  la_names <- projection_la_lookup
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
  observed_totals <- dplyr::bind_rows(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, , drop = FALSE]
    data.frame(Key = org$Key, Observed_baseline = nrow(select_org(target_episodes, org)))
  }))
  # Describe MSDS records that cannot inform a matched LA/age allocation.
  target_status <- target_episodes |>
    dplyr::mutate(LA_usable = !is.na(LA) & !LA %in% c("", "Unknown LA", "L99999999"),
      Age_usable = Age_band != "Unknown", ONS_covered = LA %in% ons_la_names$LA,
      Reason = dplyr::case_when(
        !LA_usable & !Age_usable ~ "Missing/unusable LA and age",
        !LA_usable ~ "Missing/unusable resident LA",
        !Age_usable ~ "Missing/unusable maternal age",
        !ONS_covered ~ "No England ONS series for resident LA", TRUE ~ ""))
  coverage <- dplyr::bind_rows(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, , drop = FALSE]
    data <- select_org(target_status, org)
    provider_births <- select_org(dplyr::filter(episodes, Trust %in% cluster_trusts), org)
    data.frame(Key = org$Key, Level = org$Level, Organisation = org$Organisation,
      Recorded_births = nrow(provider_births),
      Home_births = sum(provider_births$Home %in% 1),
      Unknown_home_flag = sum(!provider_births$Home %in% c(0, 1)),
      Observed_baseline = nrow(data), Matched_allocation = sum(data$Reason == ""),
      Unmatched = sum(data$Reason != ""))
  }))
  coverage_details <- dplyr::bind_rows(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, , drop = FALSE]
    select_org(target_status, org) |>
      dplyr::filter(Reason != "") |>
      dplyr::mutate(LA = dplyr::if_else(is.na(LA) | LA == "", "Unknown LA", LA)) |>
      dplyr::count(LA, Age, Age_band, Reason, name = "N") |>
      dplyr::mutate(Key = org$Key, Level = org$Level, Organisation = org$Organisation) |>
      dplyr::left_join(la_names, by = "LA") |>
      dplyr::mutate(LA_name = dplyr::if_else(LA %in% c("Unknown LA", "L99999999"),
        "Unknown LA", dplyr::coalesce(LA_name, LA)))
  }))
  # Descriptive shares read pooled MSDS inputs, including LAs without ONS coverage.
  # Do not reconstruct observed counts from the successfully projected rows.
  pooled_deliveries <- attr(wide_master, "allocation_tables")$delivery |>
    dplyr::filter(Destination %in% cluster_trusts) |> dplyr::rename(Trust = Destination)
  feeder_shares <- dplyr::bind_rows(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, , drop = FALSE]
    counts <- select_org(pooled_deliveries, org) |>
      dplyr::group_by(LA, Age) |>
      dplyr::summarise(Numerator = sum(Count), .groups = "drop")
    # Include every known resident age for each contributing LA, even zero flows.
    # Do not invent zero denominators for ages absent from the supplied totals.
    org_las <- unique(counts$LA)
    age_cells <- dplyr::bind_rows(
      resident_denominators |> dplyr::filter(LA %in% org_las, !is.na(Age)) |>
        dplyr::select(LA, Age), counts |> dplyr::select(LA, Age)) |>
      dplyr::distinct(LA, Age)
    age_cells |>
      dplyr::left_join(counts, by = c("LA", "Age")) |>
      dplyr::mutate(Numerator = dplyr::coalesce(Numerator, 0L)) |>
      dplyr::left_join(resident_denominators |> dplyr::select(-Age_band), by = c("LA", "Age")) |>
      dplyr::mutate(Key = org$Key, Level = org$Level, Organisation = org$Organisation,
        Age_band = report_age_band(Age),
        Share = dplyr::if_else(Denominator > 0, Numerator / Denominator, NA_real_)) |>
      dplyr::left_join(feeder_names, by = "LA")
  }))

  # Keep home births and other destinations in the LA denominator.
  destination_accounting <- resident_denominators |>
    dplyr::filter(LA %in% feeder_las, !is.na(Age)) |>
    dplyr::left_join(feeder_shares |>
      dplyr::filter(Level == "Cluster") |>
      dplyr::select(LA, Age, Cluster_trusts = Numerator), by = c("LA", "Age")) |>
    dplyr::left_join(attr(wide_master, "allocation_tables")$delivery |>
      dplyr::filter(LA %in% feeder_las, Destination == "Home") |>
      dplyr::transmute(LA, Age, Recorded_home_births = Count), by = c("LA", "Age")) |>
    dplyr::mutate(Cluster_trusts = dplyr::coalesce(Cluster_trusts, 0L),
      Recorded_home_births = dplyr::coalesce(Recorded_home_births, 0L),
      Other_or_unrepresented = Denominator - Cluster_trusts - Recorded_home_births) |>
    dplyr::left_join(feeder_names, by = "LA")

  # Read completed destination columns. Aggregate only AFTER projection.
  modifiers <- wide_master |> dplyr::distinct(Age, Stillbirth_ratio, Stillbirth_factor, Multiplicity_factor, Maternity_modifier)
  projection_years <- sort(unique(master$Year))
  ons_maternity_projections <- wide_master |>
    dplyr::group_by(LA, ONS_age, Year) |>
    dplyr::summarise(ONS_maternities = sum(Adjusted_maternities), .groups = "drop") |>
    dplyr::group_by(LA, ONS_age) |>
    dplyr::mutate(ONS_reference = ONS_maternities[Year == ons_reference_year],
      Growth_factor = ifelse(ONS_reference > 0, ONS_maternities / ONS_reference, NA_real_),
      ONS_change = Growth_factor - 1) |> dplyr::ungroup()
  completed <- master |>
    dplyr::filter(Delivery_destination %in% cluster_trusts, Allocation_status == "Recorded delivery trust") |>
    dplyr::rename(Trust = Delivery_destination)
  # 3.9a Delivery summaries: aggregate completed individual-age counts ----
  # Retain Age (1) while summing LA(R) contributions to delivery trusts
  trust_age_totals <- completed |>
    dplyr::group_by(Trust, Age, Year) |>
    dplyr::summarise(Projected = sum(Projected_maternities), .groups = "drop")
  trust_age_totals$ICB <- unname(provider_icb[trust_age_totals$Trust])
  icb_age_totals <- trust_age_totals |>
    dplyr::group_by(ICB, Age, Year) |>
    dplyr::summarise(Projected = sum(Projected), .groups = "drop")
  cluster_age_totals <- icb_age_totals |>
    dplyr::group_by(Age, Year) |>
    dplyr::summarise(Projected = sum(Projected), .groups = "drop")

  feeder_projections <- dplyr::bind_rows(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, , drop = FALSE]
    select_org(completed, org) |>
      dplyr::group_by(LA, LA_name, Age, ONS_age, Year, Age_treatment) |>
      dplyr::summarise(Projected = sum(Projected_maternities), Numerator = sum(MSDS_destination_count),
        Denominator = dplyr::first(MSDS_resident_count), .groups = "drop") |>
      dplyr::mutate(Key = org$Key, Level = org$Level, Organisation = org$Organisation,
        Share = Numerator / Denominator, Age_band = report_age_band(Age),
        Treatment = Age_treatment, Year_label = paste0("Mid-", Year))
  })) |>
    dplyr::left_join(ons_maternity_projections, by = c("LA", "ONS_age", "Year"), relationship = "many-to-one")

  single_age_projections <- dplyr::bind_rows(
    dplyr::transmute(trust_age_totals, Key = paste0("trust-", tolower(Trust)), Age, Year, Projected),
    dplyr::transmute(icb_age_totals, Key = paste0("icb-", tolower(ICB)), Age, Year, Projected),
    dplyr::mutate(cluster_age_totals, Key = "cluster"))
  single_age_projections$Year_label <- paste0("Mid-", single_age_projections$Year)
  # Presentation only: sum the already projected exact-age contributions for charts.
  age_projections <- feeder_projections |>
    dplyr::group_by(Key, Age_band, Year, Year_label) |>
    dplyr::summarise(Projected = sum(Projected), .groups = "drop")
  # Descriptive change in the completed model counts; not an input multiplier.
  weighted_age_growth <- age_projections |>
    dplyr::group_by(Key, Age_band) |>
    dplyr::mutate(Baseline = Projected[Year == ons_reference_year],
      Growth_change = relative_change(Projected, Baseline)) |>
    dplyr::ungroup()
  maternity_projections <- tidyr::expand_grid(Key = organisations$Key, Year = projection_years) |>
    dplyr::left_join(single_age_projections |>
      dplyr::group_by(Key, Year) |>
      dplyr::summarise(Projected = sum(Projected), .groups = "drop"), by = c("Key", "Year")) |>
    dplyr::mutate(Projected = ifelse(!Key %in% feeder_projections$Key, 0, Projected),
      Year_label = paste0("Mid-", Year)) |>
    dplyr::left_join(observed_totals, by = "Key") |>
    dplyr::mutate(Change = Projected - Observed_baseline,
      Percent_change = relative_change(Projected, Observed_baseline)) |>
    dplyr::left_join(organisations |> dplyr::select(Key, Level, Organisation), by = "Key")
  # 3.9b Booking summaries: independent of delivery shares ----------------
  booking_rows <- projection_destination_rows(wide_master, "Booking")
  booking_trust_totals <- booking_rows |>
    dplyr::filter(Booking_destination %in% cluster_trusts) |>
    dplyr::transmute(Trust = Booking_destination, Age, Year, Projected = Projected_bookings) |>
    dplyr::group_by(Trust, Age, Year) |>
    dplyr::summarise(Projected = sum(Projected), .groups = "drop")
  booking_trust_totals$ICB <- unname(provider_icb[booking_trust_totals$Trust])
  booking_icb_totals <- booking_trust_totals |> dplyr::group_by(ICB, Age, Year) |>
    dplyr::summarise(Projected = sum(Projected), .groups = "drop")
  booking_cluster_totals <- booking_icb_totals |> dplyr::group_by(Age, Year) |>
    dplyr::summarise(Projected = sum(Projected), .groups = "drop")
  # Sum completed booking counts into the same final reporting age bands.
  booking_age_projections <- dplyr::bind_rows(
    dplyr::transmute(booking_trust_totals, Key = paste0("trust-", tolower(Trust)), Age, Year, Projected),
    dplyr::transmute(booking_icb_totals, Key = paste0("icb-", tolower(ICB)), Age, Year, Projected),
    dplyr::mutate(booking_cluster_totals, Key = "cluster")) |>
    dplyr::mutate(Age_band = report_age_band(Age)) |>
    dplyr::group_by(Key, Age_band, Year) |>
    dplyr::summarise(Projected = sum(Projected), .groups = "drop")
  booking_total_projections <- tidyr::expand_grid(Key = organisations$Key, Year = projection_years) |>
    dplyr::left_join(booking_age_projections |>
      dplyr::group_by(Key, Year) |> dplyr::summarise(Projected = sum(Projected), .groups = "drop"),
      by = c("Key", "Year")) |>
    dplyr::mutate(Projected = dplyr::coalesce(Projected, 0)) |>
    dplyr::group_by(Key) |>
    dplyr::mutate(Percent_change = relative_change(Projected, Projected[Year == ons_reference_year])) |>
    dplyr::ungroup() |>
    dplyr::left_join(organisations, by = "Key")
  projection_note <- paste("ONS births (L) × local exact-age share × [1 + mean annual national S/L]",
    "× mean annual national M/(L+S) × direct LA/age destination proportion.",
    "Delivery and booking proportions separately pool 2023/24--2025/26 counts; no booking uplift is applied.",
    "No rebasing to observed activity. Home, unknown and unallocated destinations remain explicit.")
  result <- list(calculation_version = "ons-shared-age-calculations-v8", organisations = organisations, feeder_shares = feeder_shares,
    master = wide_master, delivery_rows = master, booking_rows = booking_rows,
    booking_totals = list(Trust = booking_trust_totals, ICB = booking_icb_totals, Cluster = booking_cluster_totals),
    booking_age_projections = booking_age_projections, booking_total_projections = booking_total_projections,
    booking_proportions = allocation_table_wide(attr(wide_master, "allocation_tables")$booking, "Booking"),
    ons_modifications = attr(wide_master, "ons_modifications"),
    delivery_proportions = allocation_table_wide(attr(wide_master, "allocation_tables")$delivery),
    allocation_tables = attr(wide_master, "allocation_tables"), master_dictionary = projection_column_dictionary(wide_master),
    observed_totals = observed_totals, resident_denominators = resident_denominators, destination_accounting = destination_accounting,
    baseline_fy = baseline_fy, ons_reference_year = ons_reference_year,
    projection_years = projection_years, age_bands = age_bands,
    coverage = coverage, coverage_details = coverage_details, geography = geography_changes,
    modifiers = modifiers, ons_maternity_projections = ons_maternity_projections, feeder_projections = feeder_projections,
    single_age_projections = single_age_projections,
    age_projections = age_projections, weighted_age_growth = weighted_age_growth,
    maternity_projections = maternity_projections, note = projection_note)
  if (!prepare_outputs) return(result)

  # 3.9c Delivery charts and tables: display the completed counts ----------

  # Make charts and tables for the allocation and projection views.
  coverage_table <- make_projection_coverage_table(coverage, baseline_fy)
  coverage_details_table <- coverage_details |>
    dplyr::select(Level, Organisation, LA_name, LA, Age, Reason, N) |>
    base_table(paste(baseline_fy, "records without matched ONS counts")) |>
    gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age = "Maternal age",
                   Reason = "Reason", N = "Observed deliveries") |>
    gt::fmt_number(columns = "N", decimals = 0)
  geography_table <- geography_changes |>
    base_table("Feeder codes matched to the ONS geography") |>
    gt::cols_label(LA = "MSDS LA code", LA_name = "Resident LA", Treatment = "ONS treatment",
                   ONS_source_codes = "ONS source area codes")
  # Section 2.2.8: exact-age shares in tables; chart bands sum counts before division.
  # Rank LAs by actual deliveries here; keep cluster LAs and ten additional feeders.
  # The other-feeder share sums counts and denominators, including zero-flow ages.
  mapping_share_max <- max(1, feeder_shares$Share, na.rm = TRUE)
  mapping_outputs <- stats::setNames(lapply(organisations$Key, function(key) {
    org_shares <- feeder_shares[feeder_shares$Key == key, , drop = FALSE]
    if (!nrow(org_shares)) return(list(chart = htmltools::div("No recorded deliveries in the pooled calibration period"),
      table = base_table(data.frame(Note = "No recorded deliveries in the pooled calibration period"), "LA allocation")))
    la_volume <- org_shares |>
      dplyr::filter(!is.na(LA), LA != "", !LA %in% c("Unknown LA", "L99999999")) |>
      dplyr::group_by(LA, LA_name) |>
      dplyr::summarise(N = sum(Numerator), .groups = "drop") |>
      dplyr::filter(N > 0) |>
      dplyr::arrange(dplyr::desc(N), LA_name, LA)
    additional_las <- la_volume |>
      dplyr::filter(!LA %in% projection_catchment_las) |>
      dplyr::slice_head(n = projection_other_feeders) |>
      dplyr::pull(LA)
    visible_las <- la_volume$LA[la_volume$LA %in% c(projection_catchment_las, additional_las)]
    chart_bands <- c(age_bands, if (any(org_shares$Age_band == "Unknown" & org_shares$Numerator > 0)) "Unknown")
    org_shares <- org_shares |>
      dplyr::mutate(Age_band = factor(Age_band, levels = c(age_bands, "Unknown"))) |>
      dplyr::arrange(match(LA, c(la_volume$LA, "Unknown LA", "L99999999")), Age)
    flow <- org_shares |>
      dplyr::filter(Age_band %in% chart_bands) |>
      dplyr::mutate(Source_LA = dplyr::case_when(
        is.na(LA) | LA == "" | LA %in% c("Unknown LA", "L99999999") ~ "unknown-la",
        LA %in% visible_las ~ LA, TRUE ~ "other-feeder-las")) |>
      dplyr::group_by(Source_LA, Age_band) |>
      dplyr::summarise(N = sum(Numerator),
        Denominator = if (all(is.finite(Denominator) & Denominator >= 0)) sum(Denominator) else NA_real_,
        .groups = "drop") |>
      dplyr::mutate(Share = ifelse(Denominator > 0, N / Denominator, NA_real_))
    source_order <- c(visible_las,
      if ("other-feeder-las" %in% flow$Source_LA) "other-feeder-las",
      if ("unknown-la" %in% flow$Source_LA) "unknown-la")
    source_labels <- c(la_volume$LA_name[match(visible_las, la_volume$LA)],
      if ("other-feeder-las" %in% source_order) "All other feeder LAs",
      if ("unknown-la" %in% source_order) "Unknown LA")
    flow <- dplyr::arrange(flow, match(Source_LA, source_order), Age_band)
    # Unknown LA/age remains in the full table and count totals, without a plotted share.
    chart_data <- flow |>
      dplyr::filter(Source_LA != "unknown-la", Age_band %in% age_bands)
    chart_sources <- source_order[source_order %in% chart_data$Source_LA]
    chart_labels <- source_labels[match(chart_sources, source_order)]
    if (!nrow(chart_data)) {
      chart <- htmltools::div("No usable pooled LA/age allocation proportions")
    } else {
      org <- organisations[match(key, organisations$Key), , drop = FALSE]
      org_name <- org$Organisation[[1]]
      chart <- plotly::plot_ly(height = max(400, length(chart_sources) * 32 + 100))
      # Slight vertical offsets keep the age-band dots visible on each LA row.
      age_offsets <- seq(-0.28, 0.28, length.out = length(age_bands))
      for (i in seq_along(age_bands)) {
        points <- chart_data[chart_data$Age_band == age_bands[i], , drop = FALSE]
        if (!nrow(points)) next
        hover <- paste0(org_name, "<br>", source_labels[match(points$Source_LA, source_order)],
          "<br>Age: ", age_bands[i], "<br>Deliveries here: ", scales::comma(points$N, accuracy = 1),
          "<br>MSDS LA/age maternities: ", scales::comma(points$Denominator, accuracy = 1),
          "<br>% sent here: ", scales::percent(points$Share, accuracy = 0.1))
        chart <- plotly::add_trace(chart, x = points$Share,
          y = match(points$Source_LA, chart_sources) + age_offsets[i],
          type = "scatter", mode = "markers", name = age_bands[i],
          marker = list(color = age_colours[i], size = 8),
          text = hover, hovertemplate = "%{text}<extra></extra>")
      }
      chart <- chart |>
        plotly::layout(showlegend = FALSE,
          xaxis = list(title = "% of MSDS LA/age maternities sent here", tickformat = ".0%",
            range = c(0, mapping_share_max), zeroline = TRUE),
          yaxis = list(title = "", tickmode = "array", tickvals = seq_along(chart_sources),
            ticktext = chart_labels, range = c(length(chart_sources) + 0.6, 0.4),
            tickfont = list(size = 11), showgrid = TRUE, zeroline = FALSE),
          margin = list(l = 180, r = 20, t = 15, b = 65), font = list(family = "Arial", size = 11)) |>
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    }
    table <- org_shares |>
      dplyr::select(LA_name, LA, Age, Numerator, Denominator, Share) |>
      base_table("Pooled 2023/24--2025/26 LA and individual-age allocation") |>
      gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age = "Maternal age",
        Numerator = "Deliveries here", Denominator = "MSDS LA/age maternities", Share = "% sent here") |>
      gt::fmt_number(columns = c("Numerator", "Denominator"), decimals = 0) |>
      gt::fmt_percent(columns = "Share", decimals = 1)
    list(chart = chart, table = table, flow = flow, chart_data = chart_data, visible_las = visible_las)
  }), organisations$Key)
  accounting_table <- destination_accounting |>
    dplyr::select(LA_name, LA, Age, Denominator, Cluster_trusts,
                  Recorded_home_births, Other_or_unrepresented) |>
    base_table("Pooled 2023/24--2025/26 MSDS resident maternities by destination") |>
    gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age = "Maternal age",
      Denominator = "MSDS LA/age maternities", Cluster_trusts = "At cluster trusts",
      Recorded_home_births = "Recorded home maternities",
      Other_or_unrepresented = "Other / unrepresented destinations") |>
    gt::fmt_number(columns = c("Denominator", "Cluster_trusts", "Recorded_home_births",
                               "Other_or_unrepresented"), decimals = 0)
  style_line_chart <- function(chart, y_title, percent = FALSE, ons_only = FALSE) {
    tick_years <- sort(unique(c(ons_reference_year,
      projection_years[projection_years %% 5 == 0], max(projection_years))))
    plotly::layout(chart,
      xaxis = list(title = "ONS year ending mid-year (modelled counts)",
        tickmode = "array", tickvals = tick_years,
        ticktext = paste0("Mid-", tick_years)),
      yaxis = list(title = y_title, tickformat = if (percent) ".0%" else ",.0f",
                   rangemode = "tozero", zeroline = TRUE),
      legend = list(orientation = "h", x = 0, y = -0.25),
      margin = list(l = 85, r = 20, t = 20, b = 125), font = list(family = "Arial")) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }
  age_line_chart <- function(data, value, title, percent = FALSE) {
    # Set the plot and its container to the same compact height from the start.
    chart <- plotly::plot_ly(height = projection_age_grid_height)
    chart_bands <- c(age_bands, if ("Unknown" %in% data$Age_band) "Unknown")
    chart_colours <- c(age_colours, "#222222")
    for (i in seq_along(chart_bands)) {
      line <- data |> dplyr::filter(Age_band == chart_bands[i]) |> dplyr::arrange(Year)
      if (!nrow(line)) next
      values <- line[[value]]
      hover <- paste0(paste0("Mid-", line$Year),
        "<br>Age: ", chart_bands[i], "<br>", title, ": ",
        if (percent) scales::percent(values, accuracy = 0.1) else scales::comma(values, accuracy = 1))
      chart <- plotly::add_trace(chart, x = line$Year, y = values, text = hover,
        type = "scatter", mode = "lines+markers", name = chart_bands[i],
        line = list(color = chart_colours[i], width = 2), marker = list(color = chart_colours[i], size = 4),
        hovertemplate = "%{text}<extra></extra>", connectgaps = FALSE)
    }
    style_line_chart(chart, title, percent)
  }
  forecast_outputs <- stats::setNames(lapply(organisations$Key, function(key) {
    org_shares <- feeder_shares[feeder_shares$Key == key, , drop = FALSE]
    org_flows <- feeder_projections[feeder_projections$Key == key, , drop = FALSE]
    org_ages <- age_projections[age_projections$Key == key, , drop = FALSE]
    org_totals <- maternity_projections[maternity_projections$Key == key, , drop = FALSE]
    org_totals <- dplyr::arrange(org_totals, Year)
    # Sum completed counts, then calculate the chart-only age-band change.
    ons_age_growth <- weighted_age_growth |>
      dplyr::filter(Key == key)
    growth_chart <- age_line_chart(ons_age_growth, "Growth_change", "% change in incoming maternities", TRUE)
    incoming_chart <- age_line_chart(org_ages, "Projected", "Incoming maternities")
    # Tables retain every forecast year; their columns scroll horizontally.
    growth_table <- if (nrow(org_flows)) org_flows |>
      dplyr::mutate(ONS_year_label = paste0("Mid-", Year)) |>
      dplyr::select(LA_name, LA, Age, ONS_age, Numerator, Denominator, Share, Treatment, ONS_year_label, ONS_change) |>
      tidyr::pivot_wider(names_from = ONS_year_label, values_from = ONS_change) |>
      dplyr::arrange(LA_name, Age) |>
      base_table("ONS change by feeder LA and age", paste("Relative to the ONS", ons_reference_year, "reference")) |>
      gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age = "Maternal age",
        Numerator = "Pooled deliveries here", Denominator = "Pooled MSDS LA/age maternities", Share = "% sent here",
        ONS_age = "ONS model age", Treatment = "Projection treatment") |>
      gt::fmt_number(columns = c("Numerator", "Denominator"), decimals = 0) |>
      gt::fmt_percent(columns = c("Share", paste0("Mid-", projection_years)), decimals = 1) else
      base_table(data.frame(Note = "No matched feeder-LA flows in the pooled 2023/24--2025/26 period"), "ONS change")
    incoming_table <- if (nrow(org_flows)) org_flows |>
      dplyr::select(LA_name, LA, Age, ONS_age, Treatment, Year_label, Projected) |>
      tidyr::pivot_wider(names_from = Year_label, values_from = Projected) |>
      dplyr::arrange(LA_name, Age) |>
      base_table("Incoming maternities by feeder LA and age") |>
      gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age = "Maternal age",
                     ONS_age = "ONS model age", Treatment = "Projection treatment") |>
      gt::fmt_number(columns = paste0("Mid-", projection_years), decimals = 0) else
      base_table(data.frame(Note = "No matched feeder-LA flows in the pooled 2023/24--2025/26 period"), "Incoming maternities")
    if (!nrow(org_flows)) {
      growth_chart <- htmltools::div("No matched feeder-LA flows in the pooled 2023/24--2025/26 period")
      incoming_chart <- htmltools::div("No matched feeder-LA flows in the pooled 2023/24--2025/26 period")
    }
    if (nrow(org_flows) && !nrow(ons_age_growth)) {
      growth_chart <- htmltools::div("No matched ONS age-specific series; these delivery contributions are not projected.")
    }
    # Delivery charts: completed counts, compared with the recorded baseline.
    total_chart <- plotly::plot_ly(org_totals, height = projection_chart_height, x = ~Year, y = ~Projected,
      type = "scatter", mode = "lines+markers", line = list(color = "#005EB8", width = 2),
      text = ~paste0(Year_label, "<br>Maternities: ", scales::comma(Projected, accuracy = 1)),
      hovertemplate = "%{text}<extra></extra>", showlegend = FALSE) |>
      style_line_chart("Total maternities")
    change_chart <- plotly::plot_ly(org_totals, height = projection_chart_height, x = ~Year, y = ~Percent_change,
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

  # Keep the named trusts and every additional recorded delivery trust in the pool.
  age_groups <- list(
    Trust = organisations[organisations$Level == "Trust" &
      organisations$Code %in% c(setdiff(names(trust_labels), "Other"), recorded_trusts), , drop = FALSE],
    ICB_Cluster = organisations[organisations$Level %in% c("ICB", "Cluster"), , drop = FALSE])
  # Section 2.2.8: put the allocation dot plots together with one age-band legend.
  mapping_key <- htmltools::div(
    style = "display: flex; flex-wrap: wrap; gap: 12px 24px; margin-bottom: 20px; font-size: 14px;",
    htmltools::tagList(lapply(seq_along(age_bands), function(i) {
      htmltools::span(style = "display: inline-flex; align-items: center; gap: 6px;",
        htmltools::span(style = paste0("display: inline-block; width: 9px; height: 9px; border-radius: 50%; background: ",
          age_colours[i], ";"), `aria-hidden` = "true"), age_bands[i])
    })))
  mapping_grids <- lapply(age_groups, function(rows) {
    htmltools::tagList(mapping_key,
      htmltools::div(style = "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 24px;",
        htmltools::tagList(lapply(seq_len(nrow(rows)), function(i) {
          htmltools::div(style = "min-width: 0;",
            htmltools::h3(style = "font-size: 14px; margin: 0 0 8px;", rows$Organisation[i]),
            mapping_outputs[[rows$Key[i]]]$chart)
        }))))
  })
  mapping_table_rows <- dplyr::bind_rows(age_groups)
  mapping_tables <- htmltools::tagList(lapply(seq_len(nrow(mapping_table_rows)), function(i) {
    htmltools::tagList(htmltools::h3(mapping_table_rows$Organisation[i]),
      mapping_outputs[[mapping_table_rows$Key[i]]]$table)
  }))

  # Section 3.3.2: use the same organisation groups for the age-growth charts.
  age_grid <- function(rows, field) {
    bands <- age_bands
    if (any(age_projections$Key %in% rows$Key &
                                        age_projections$Age_band == "Unknown")) {
      bands <- c(bands, "Unknown")
    }
    colours <- c(age_colours, "#222222")
    key <- htmltools::div(
      style = "display: flex; flex-wrap: wrap; gap: 12px 24px; margin-bottom: 20px; font-size: 14px;",
      htmltools::tagList(lapply(seq_along(bands), function(i) {
        htmltools::span(style = "display: inline-flex; align-items: center; gap: 6px;",
          htmltools::span(style = paste0("display: inline-block; width: 18px; height: 3px; background: ",
            colours[i], ";"), `aria-hidden` = "true"),
          if (bands[i] == "Unknown") "Unknown age" else bands[i])
      })))
    htmltools::tagList(key,
      htmltools::div(style = "display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); column-gap: 24px; row-gap: 36px;",
        htmltools::tagList(lapply(seq_len(nrow(rows)), function(i) {
          chart <- forecast_outputs[[rows$Key[i]]][[field]]
          if (inherits(chart, "plotly")) {
            chart <- plotly::layout(chart, showlegend = FALSE,
              xaxis = list(title = "", tickangle = -45, tickfont = list(size = 10)),
              yaxis = list(title = if (field == "growth_chart") "% change" else "Maternities",
                           tickfont = list(size = 10)),
              margin = list(l = 65, r = 15, t = 15, b = 65))
          }
          htmltools::div(style = "min-width: 0;",
            htmltools::h3(style = "font-size: 14px; margin: 0 0 8px;", rows$Organisation[i]), chart)
        }))))
  }
  age_grids <- lapply(age_groups, function(rows) list(
    growth_chart = age_grid(rows, "growth_chart"),
    incoming_chart = age_grid(rows, "incoming_chart")))

  # Put the existing detailed tables together under organisation headings.
  # All feeder LAs, individual ages and forecast years stay in these final table tabs.
  table_rows <- dplyr::bind_rows(age_groups)
  age_tables <- stats::setNames(lapply(c("growth_table", "incoming_table"), function(field) {
    htmltools::tagList(lapply(seq_len(nrow(table_rows)), function(i) {
      htmltools::tagList(
        htmltools::h3(table_rows$Organisation[i]),
        forecast_outputs[[table_rows$Key[i]]][[field]])
    }))
  }), c("growth_table", "incoming_table"))

  # Section 3.3.1: trusts share one chart; the two ICBs and whole cluster share another.
  # Each line uses the existing totals across all feeder LAs and age bands.
  total_charts <- stats::setNames(lapply(c("Trust", "ICB_Cluster"), function(group) {
    rows <- age_groups[[group]]
    colours <- grDevices::hcl.colors(nrow(rows), palette = "Dark 3")
    chart <- plotly::plot_ly(height = projection_chart_height + 100)
    for (i in seq_len(nrow(rows))) {
      line <- maternity_projections |>
        dplyr::filter(Key == rows$Key[i]) |>
        dplyr::arrange(Year)
      chart <- plotly::add_trace(chart,
        x = line$Year, y = line$Projected, type = "scatter", mode = "lines+markers",
        name = rows$Organisation[i],
        line = list(color = colours[i], width = 2),
        marker = list(color = colours[i], size = 4),
        text = paste0(rows$Organisation[i], "<br>", line$Year_label,
          "<br>Maternities: ", scales::comma(line$Projected, accuracy = 1)),
        hovertemplate = "%{text}<extra></extra>", connectgaps = FALSE)
    }
    chart |>
      style_line_chart("Total maternities") |>
      plotly::layout(
        legend = list(orientation = "h", x = 0, y = 1.04,
                      xanchor = "left", yanchor = "bottom", font = list(size = 12)),
        margin = list(l = 85, r = 20, t = 130, b = 125))
  }), c("Trust", "ICB_Cluster"))

  # Section 3.3.3: compare percentage changes from the actual 2025/26 baseline.
  # Trusts share one chart; the two ICBs and whole cluster share the other.
  change_charts <- stats::setNames(lapply(c("Trust", "ICB_Cluster"), function(group) {
    rows <- age_groups[[group]]
    colours <- grDevices::hcl.colors(nrow(rows), palette = "Dark 3")
    chart <- plotly::plot_ly(height = projection_chart_height + 100)
    for (i in seq_len(nrow(rows))) {
      line <- maternity_projections |>
        dplyr::filter(Key == rows$Key[i]) |>
        dplyr::arrange(Year)
      chart <- plotly::add_trace(chart,
        x = line$Year, y = line$Percent_change, type = "scatter", mode = "lines+markers",
        name = rows$Organisation[i],
        line = list(color = colours[i], width = 2),
        marker = list(color = colours[i], size = 4),
        text = paste0(rows$Organisation[i], "<br>", line$Year_label,
          "<br>Change from ", baseline_fy, ": ",
          ifelse(is.na(line$Percent_change), "n.a.",
                 scales::percent(line$Percent_change, accuracy = 0.1))),
        hovertemplate = "%{text}<extra></extra>", connectgaps = FALSE)
    }
    chart |>
      style_line_chart(paste("% change from observed", baseline_fy), percent = TRUE) |>
      plotly::layout(
        legend = list(orientation = "h", x = 0, y = 1.04,
                      xanchor = "left", yanchor = "bottom", font = list(size = 12)),
        margin = list(l = 85, r = 20, t = 130, b = 125))
  }), c("Trust", "ICB_Cluster"))

  all_totals_table <- maternity_projections |>
    dplyr::select(Level, Organisation, Year_label, Projected) |>
    tidyr::pivot_wider(names_from = Year_label, values_from = Projected) |>
    gt::gt(groupname_col = "Level", rowname_col = "Organisation") |>
    gt::tab_header(title = "All organisation maternity projections") |>
    gt::fmt_number(columns = paste0("Mid-", projection_years), decimals = 0) |>
    gt::tab_options(container.width = "100%", container.overflow.x = TRUE,
                    table.font.names = c("Open Sans", "Arial", "sans-serif"))
  all_changes_table <- maternity_projections |>
    dplyr::select(Level, Organisation, Year_label, Percent_change) |>
    tidyr::pivot_wider(names_from = Year_label, values_from = Percent_change) |>
    gt::gt(groupname_col = "Level", rowname_col = "Organisation") |>
    gt::tab_header(title = paste("All organisation changes from observed", baseline_fy)) |>
    gt::fmt_percent(columns = paste0("Mid-", projection_years), decimals = 1) |>
    gt::sub_missing(missing_text = "n.a.") |>
    gt::tab_options(container.width = "100%", container.overflow.x = TRUE,
                    table.font.names = c("Open Sans", "Arial", "sans-serif"))
  result$allocation_accounting_table <- master |>
    dplyr::group_by(Year, Allocation_status) |>
    dplyr::summarise(Maternities = sum(Projected_maternities), .groups = "drop") |>
    tidyr::pivot_wider(names_from = Year, values_from = Maternities, values_fill = 0) |>
    base_table("Adjusted ONS maternities: all modelled LAs and destinations",
      "These categories reconcile to the delivery starting volume after ONS adjustments. Recorded delivery trusts include providers outside this cluster.") |>
    gt::fmt_number(columns = where(is.numeric), decimals = 1)
  result$pathway_table <- master |> dplyr::filter(Year == ons_reference_year,
      Delivery_destination %in% cluster_trusts) |>
    dplyr::select(LA, Age, Delivery_destination, Exact_age_live_births, Stillbirth_factor,
      Multiplicity_factor, Adjusted_maternities, Destination_proportion, Projected_maternities) |>
    base_table(paste("Mid-", ons_reference_year, " direct delivery allocations", sep = "")) |>
    gt::fmt_number(columns = c("Exact_age_live_births", "Stillbirth_factor", "Multiplicity_factor",
      "Adjusted_maternities", "Projected_maternities"), decimals = 3) |>
    gt::fmt_percent(columns = "Destination_proportion", decimals = 1)
  # 3.9d Booking charts: completed counts, grouped for presentation -------
  # Changes are relative to the MODELLED reference year, not observed bookings.
  booking_chart <- function(key, view) {
    data <- booking_total_projections[booking_total_projections$Key == key, ]
    chart <- plotly::plot_ly(height = projection_chart_height)
    if (view == "age") {
      data <- booking_age_projections[booking_age_projections$Key == key, ]
      for (band in age_bands) {
        band_data <- data[data$Age_band == band, ]
        if (!nrow(band_data)) next
        chart <- plotly::add_trace(chart, x = band_data$Year, y = band_data$Projected,
          type = "scatter", mode = "lines", name = band,
          line = list(color = age_colours[match(band, age_bands)], width = 2),
          text = paste0("Age ", band, "<br>Mid-", band_data$Year, "<br>Modelled bookings: ",
            scales::comma(band_data$Projected, accuracy = 1)),
          hovertemplate = "%{text}<extra></extra>")
      }
      if (!nrow(data)) chart <- plotly::add_trace(chart,
        x = projection_years, y = rep(0, length(projection_years)),
        type = "scatter", mode = "lines", name = "No allocated bookings",
        line = list(color = main_chart_colour), showlegend = FALSE, hoverinfo = "skip")
    } else {
      values <- if (view == "change") data$Percent_change else data$Projected
      chart <- plotly::add_trace(chart, x = data$Year, y = values,
        type = "scatter", mode = "lines+markers", name = "Modelled bookings",
        line = list(color = main_chart_colour, width = 2),
        text = paste0("Mid-", data$Year, "<br>", if (view == "change")
          paste0("Change from modelled Mid-", ons_reference_year, ": ", scales::percent(values, accuracy = 0.1)) else
          paste0("Modelled bookings: ", scales::comma(values, accuracy = 1))),
        hovertemplate = "%{text}<extra></extra>")
    }
    chart <- chart |> style_line_chart(
      if (view == "change") paste("% change from modelled Mid-", ons_reference_year, sep = "") else
        "Modelled bookings (no uplift)", percent = view == "change")
    if (view == "age") chart <- plotly::layout(chart, showlegend = TRUE,
      legend = list(orientation = "h", y = -0.25), margin = list(b = 100))
    if (!any(data$Projected > 0)) chart <- plotly::layout(chart,
      yaxis = list(range = c(0, 1), tickvals = 0), showlegend = FALSE,
      annotations = list(list(text = "No allocated bookings", x = 0.5, y = 0.5,
        xref = "paper", yref = "paper", showarrow = FALSE)))
    chart
  }
  booking_views <- c("total", "age", "change")
  booking_charts <- stats::setNames(lapply(booking_views, function(view) {
    stats::setNames(lapply(organisations$Key, function(key) booking_chart(key, view)), organisations$Key)
  }), booking_views)
  # Include providers with bookings even when they have no recorded deliveries.
  booking_groups <- age_groups
  booking_groups$Trust <- organisations |> dplyr::filter(Level == "Trust",
    Code %in% c(setdiff(names(trust_labels), "Other"),
      booking_trust_totals$Trust[booking_trust_totals$Projected > 0]))
  booking_grids <- lapply(booking_charts, function(charts) lapply(booking_groups, function(rows) {
    htmltools::div(style = "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 24px;",
      htmltools::tagList(lapply(seq_len(nrow(rows)), function(i) {
        htmltools::div(style = "min-width: 0;", htmltools::h3(rows$Organisation[i]), charts[[rows$Key[i]]])
      })))
  }))
  booking_table <- booking_total_projections |>
    dplyr::select(Level, Organisation, Year, Projected, Percent_change) |>
    base_table("Modelled bookings: no early-loss uplift", "Change from the modelled reference year") |>
    gt::cols_label(Projected = "Modelled bookings", Percent_change = "% change") |>
    gt::fmt_number(columns = "Projected", decimals = 0) |>
    gt::fmt_percent(columns = "Percent_change", decimals = 1)
  booking_age_table <- booking_age_projections |>
    dplyr::left_join(organisations, by = "Key") |>
    dplyr::select(Level, Organisation, Age_band, Year, Projected) |>
    base_table("Modelled bookings by age band: no early-loss uplift") |>
    gt::cols_label(Age_band = "Age (B)", Projected = "Modelled bookings") |>
    gt::fmt_number(columns = "Projected", decimals = 0)
  result$booking_charts <- booking_charts
  result$booking_grids <- booking_grids
  result$booking_chart_groups <- booking_groups
  result$booking_table <- booking_table
  result$booking_age_table <- booking_age_table
  result$mapping_outputs <- mapping_outputs
  result$mapping_grids <- mapping_grids
  result$mapping_tables <- mapping_tables
  result$accounting_table <- accounting_table
  result$coverage_table <- coverage_table
  result$coverage_details_table <- coverage_details_table
  result$geography_table <- geography_table
  result$forecast_outputs <- forecast_outputs
  result$delivery_chart_groups <- age_groups
  result$age_grids <- age_grids
  result$age_tables <- age_tables
  result$total_charts <- total_charts
  result$change_charts <- change_charts
  result$all_totals_table <- all_totals_table
  result$all_changes_table <- all_changes_table
  result$sankey_comparison <- build_projection_sankeys(
    target_episodes, completed, organisations, baseline_fy, max(projection_years))
  result
}

# 3.9e Delivery flows: observed year and final projection side by side -----
# CONTEXT_ONLY: sum completed counts; these charts do not change the model.
build_projection_sankeys <- function(observed, projected, organisations,
    observed_year, final_year, main_las = projection_catchment_las,
    other_las = projection_other_feeders) {
  # Keep every recorded non-home delivery, including an unknown resident LA.
  actual <- observed |> dplyr::count(LA, Trust, name = "Observed")
  future <- projected |> dplyr::filter(Year == final_year) |>
    dplyr::group_by(LA, Trust) |>
    dplyr::summarise(Projected = sum(Projected_maternities), .groups = "drop")
  raw_flows <- dplyr::full_join(actual, future, by = c("LA", "Trust")) |>
    dplyr::mutate(Observed = dplyr::coalesce(Observed, 0),
      Projected = dplyr::coalesce(Projected, 0))
  raw_flows$LA[raw_flows$LA %in% c("Unknown LA", "L99999999", "") | is.na(raw_flows$LA)] <- "Unknown LA"

  # Use the same LA groups in both panels. Rank extra feeders across BOTH years.
  ranking <- raw_flows |> dplyr::group_by(LA) |>
    dplyr::summarise(Volume = sum(Observed + Projected), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(Volume), LA)
  keep_las <- unique(c(main_las, head(ranking$LA[
    !ranking$LA %in% c(main_las, "Unknown LA")], other_las)))
  flows <- raw_flows |>
    dplyr::mutate(LA = ifelse(LA %in% c(keep_las, "Unknown LA"), LA, "All other feeder LAs")) |>
    dplyr::group_by(LA, Trust) |>
    dplyr::summarise(Observed = sum(Observed), Projected = sum(Projected), .groups = "drop") |>
    dplyr::filter(Observed > 0 | Projected > 0)
  # Calculate change AFTER summing the displayed flow, not by averaging percentages.
  flows$Percent_change <- ifelse(flows$Observed > 0,
    flows$Projected / flows$Observed - 1, NA_real_)
  origins <- unique(c(keep_las, "All other feeder LAs", "Unknown LA"))
  origins <- origins[origins %in% flows$LA]
  trusts <- organisations$Code[organisations$Level == "Trust"]
  trusts <- trusts[trusts %in% flows$Trust]
  la_labels <- projection_la_lookup$LA_name[match(origins, projection_la_lookup$LA)]
  la_labels[is.na(la_labels)] <- origins[is.na(la_labels)]
  trust_names <- organisations$Organisation[match(trusts, organisations$Code)]
  labels <- c(la_labels, paste0(trusts, " - ", trust_names))
  nodes <- dplyr::bind_rows(
    flows |> dplyr::group_by(Code = LA) |>
      dplyr::summarise(Observed = sum(Observed), Projected = sum(Projected), .groups = "drop") |>
      dplyr::slice(match(origins, Code)),
    flows |> dplyr::group_by(Code = Trust) |>
      dplyr::summarise(Observed = sum(Observed), Projected = sum(Projected), .groups = "drop") |>
      dplyr::slice(match(trusts, Code)))
  nodes$Percent_change <- ifelse(nodes$Observed > 0, nodes$Projected / nodes$Observed - 1, NA_real_)

  # One symmetric red-to-blue scale, centred on zero. Zero starting counts are grey.
  changes <- c(flows$Percent_change, nodes$Percent_change)
  colour_limit <- max(0.1, ceiling(max(c(0, abs(changes[is.finite(changes)]))) * 10) / 10)
  palette <- grDevices::colorRampPalette(c("#B2182B", "#F2F2F2", "#2166AC"))(101)
  change_colour <- function(x) {
    colour <- rep("#909090", length(x))
    valid <- is.finite(x)
    colour[valid] <- palette[1L + round(50 * (1 + x[valid] / colour_limit))]
    colour
  }
  change_label <- function(x) ifelse(is.finite(x), sprintf("%+.1f%%", 100 * x),
    "Not calculable: no recorded starting count")
  flow_text <- paste0(la_labels[match(flows$LA, origins)], " to ",
    trust_names[match(flows$Trust, trusts)],
    "<br>", observed_year, " recorded: ", scales::comma(flows$Observed, accuracy = 1),
    "<br>Mid-", final_year, " projected: ", scales::comma(flows$Projected, accuracy = 0.1),
    "<br>Change: ", change_label(flows$Percent_change))
  make_panel <- function(field) {
    visible <- flows[[field]] > 0
    if (!any(visible)) return(htmltools::p("No deliveries in this panel."))
    plotly::plot_ly(type = "sankey", arrangement = "snap",
      height = max(540, 32 * length(origins)),
      node = list(label = gsub("\n", "<br>", stringr::str_wrap(labels, 24)), pad = 16, thickness = 14,
        color = if (field == "Observed") rep("#75A6CE", nrow(nodes)) else change_colour(nodes$Percent_change),
        line = list(color = "#666666", width = 0.4),
        customdata = paste0(labels, "<br>", observed_year, ": ", scales::comma(nodes$Observed),
          "<br>Mid-", final_year, ": ", scales::comma(nodes$Projected, accuracy = 0.1),
          "<br>Change: ", change_label(nodes$Percent_change)),
        hovertemplate = "%{customdata}<extra></extra>"),
      link = list(source = match(flows$LA[visible], origins) - 1L,
        target = length(origins) + match(flows$Trust[visible], trusts) - 1L,
        value = flows[[field]][visible],
        color = if (field == "Observed") rep("#75A6CE", sum(visible)) else change_colour(flows$Percent_change[visible]),
        customdata = flow_text[visible], hovertemplate = "%{customdata}<extra></extra>")) |>
      plotly::layout(font = list(size = 11), margin = list(l = 8, r = 8, t = 20, b = 15)) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }
  panels <- list(observed = make_panel("Observed"), projected = make_panel("Projected"))
  display <- htmltools::tagList(
    htmltools::div(class = "projection-flow-pair",
      htmltools::div(htmltools::h4(paste(observed_year, "- recorded MSDS")),
        htmltools::p(paste(scales::comma(sum(flows$Observed)), "deliveries at cluster trusts")), panels$observed),
      htmltools::div(htmltools::h4(paste0("Mid-", final_year, " - projected")),
        htmltools::p(paste(scales::comma(sum(flows$Projected), accuracy = 1), "deliveries at cluster trusts")), panels$projected)),
    htmltools::div(class = "projection-change-key",
      htmltools::span(paste0(scales::percent(-colour_limit), " (decrease)")),
      htmltools::span(class = "projection-change-gradient", "0%"),
      htmltools::span(paste0("+", scales::percent(colour_limit), " (increase)"))),
    htmltools::p(class = "example-method-note",
      "Grey: no recorded starting count. Ribbon colour = flow change; node colour = total change. ",
      "Ribbon widths show volumes within each panel; panels scale independently. Hover for both counts."))
  list(raw_flows = raw_flows, flows = flows, nodes = nodes, panels = panels, display = display,
    observed_year = observed_year, final_year = final_year, colour_limit = colour_limit)
}

# 3.9f Variation around the three-year model inputs -----------------------
# CONTEXT_ONLY: annual comparisons; no rates or projections are replaced.
build_input_variation_review <- function(cohort, resident_totals, registration_counts,
    pooled, providers = cluster_trusts) {
  years <- pooled$years
  # Reuse the model's counting rules, taking one full year at a time.
  annual <- lapply(years, function(fy) {
    tables <- build_allocation_tables(cohort, resident_totals, calibration_years = fy)
    lapply(tables[c("residents", "delivery", "booking")], function(x) {
      x$FY <- rep(fy, nrow(x)); x
    })
  })
  annual_residents <- dplyr::bind_rows(lapply(annual, `[[`, "residents"))
  annual_delivery <- dplyr::bind_rows(lapply(annual, `[[`, "delivery"))
  annual_booking <- dplyr::bind_rows(lapply(annual, `[[`, "booking"))

  # Compare the SAME LA(R), Age (1) and destination with its pooled proportion.
  # Zero deliveries to a trust count as zero; a zero resident denominator is undefined.
  allocation_variation <- function(reference, yearly, denominator) {
    reference <- reference |> dplyr::filter(Destination %in% providers, Count > 0) |>
      dplyr::select(LA, Age, Destination, Pooled = Proportion)
    denominators <- yearly |> dplyr::distinct(LA, Age, FY, .data[[denominator]])
    names(denominators)[4] <- "Denominator"
    data <- merge(reference, data.frame(FY = years), by = NULL) |>
      dplyr::left_join(yearly |> dplyr::select(LA, Age, Destination, FY, Count),
        by = c("LA", "Age", "Destination", "FY")) |>
      dplyr::left_join(denominators, by = c("LA", "Age", "FY")) |>
      dplyr::mutate(Count = dplyr::coalesce(Count, 0),
        Annual = ifelse(Denominator > 0, Count / Denominator, NA_real_),
        Departure_pp = 100 * (Annual - Pooled))
    data
  }
  delivery <- allocation_variation(pooled$delivery, annual_delivery, "MSDS_resident_count")
  booking <- allocation_variation(pooled$booking, annual_booking, "MSDS_booking_total")

  # Boundary age mix: local exact-age share within <=15 or 44+, for each LA.
  boundary <- pooled$residents |>
    dplyr::mutate(ONS_age = report_ons_age(Age)) |>
    dplyr::filter(ONS_age %in% c(15, 44)) |>
    dplyr::group_by(LA, ONS_age) |>
    dplyr::mutate(Pooled = MSDS_resident_count / sum(MSDS_resident_count)) |>
    dplyr::ungroup() |> dplyr::select(LA, Age, ONS_age, Pooled)
  annual_boundary <- annual_residents |>
    dplyr::mutate(ONS_age = report_ons_age(Age)) |>
    dplyr::filter(ONS_age %in% c(15, 44))
  boundary_denominators <- annual_boundary |> dplyr::group_by(LA, ONS_age, FY) |>
    dplyr::summarise(Denominator = sum(MSDS_resident_count), .groups = "drop")
  age_mix <- merge(boundary, data.frame(FY = years), by = NULL) |>
    dplyr::left_join(annual_boundary |> dplyr::select(LA, Age, FY, MSDS_resident_count),
      by = c("LA", "Age", "FY")) |>
    dplyr::left_join(boundary_denominators, by = c("LA", "ONS_age", "FY")) |>
    dplyr::mutate(Count = dplyr::coalesce(MSDS_resident_count, 0),
      Annual = ifelse(Denominator > 0, Count / Denominator, NA_real_),
      Departure_pp = 100 * (Annual - Pooled))

  # MSDS boxes describe the spread ACROSS cells of each cell's annual departure.
  # Each cell has equal display weight; model proportions still pool the counts.
  departure_plot <- function(data, group, x_title) {
    data$Group <- as.character(data[[group]])
    groups <- if (group == "Age") as.character(sort(unique(data$Age))) else sort(unique(data$Group))
    plot_data <- data |> dplyr::filter(is.finite(Departure_pp))
    if (!nrow(plot_data)) return(htmltools::p("No annual denominators available for this comparison."))
    chart <- plotly::plot_ly(height = 440)
    colours <- c("#005EB8", "#D55E00", "#00856A")
    for (i in seq_along(years)) {
      rows <- plot_data[plot_data$FY == years[i], ]
      if (!nrow(rows)) next
      chart <- plotly::add_trace(chart, type = "box", x = rows$Group, y = rows$Departure_pp,
        name = years[i], boxpoints = "outliers", marker = list(color = colours[i]),
        line = list(color = colours[i]),
        text = paste0(rows$LA, ", Age (1) ", rows$Age,
          "<br>Annual count / total: ", rows$Count, " / ", rows$Denominator,
          "<br>Annual: ", scales::percent(rows$Annual, accuracy = 0.1),
          "; pooled: ", scales::percent(rows$Pooled, accuracy = 0.1)), hoverinfo = "text+y")
    }
    chart |> plotly::layout(boxmode = "group",
      xaxis = list(title = x_title, type = "category", categoryorder = "array", categoryarray = groups),
      yaxis = list(title = "Annual minus pooled (percentage points)", zeroline = TRUE, zerolinecolor = "#333333"),
      legend = list(orientation = "h", x = 0, y = 1.1), margin = list(l = 85, r = 20, t = 50, b = 75)) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }

  # ONS boxes contain the three annual ratios at EACH original published age.
  # Show every annual point and the exact arithmetic mean used by the model.
  ons <- prepare_projection_age_factors(registration_counts)
  age_order <- c("<14", as.character(14:49), "50+")
  ons_plot <- function(field, y_title) {
    chart <- plotly::plot_ly(height = 440)
    for (age in age_order) {
      rows <- ons$annual[ons$annual$age == age, ]
      chart <- plotly::add_trace(chart, type = "box", x = rep(age, nrow(rows)), y = rows[[field]],
        name = age, boxpoints = FALSE, showlegend = FALSE,
        line = list(color = "#9BA9B4"), fillcolor = "#E4ECF2")
    }
    for (i in seq_along(sort(unique(ons$annual$year)))) {
      year <- sort(unique(ons$annual$year))[i]
      rows <- ons$annual[ons$annual$year == year, ]
      chart <- plotly::add_trace(chart, type = "scatter", mode = "markers", x = rows$age, y = rows[[field]],
        name = as.character(year), marker = list(color = c("#005EB8", "#D55E00", "#00856A")[i], size = 6),
        text = paste0("Age ", rows$age, ", ", year, "<br>Births (L): ", rows$live_births,
          "; stillbirths: ", rows$stillbirths, "; maternities: ", rows$maternities), hoverinfo = "text+y")
    }
    chart <- plotly::add_trace(chart, type = "scatter", mode = "markers",
      x = ons$factors$age, y = ons$factors[[field]], name = "Three-year mean used",
      marker = list(color = "#222222", symbol = "diamond-open", size = 10))
    chart |> plotly::layout(xaxis = list(title = "ONS original maternal age", type = "category",
        categoryorder = "array", categoryarray = age_order, tickangle = -45),
      yaxis = list(title = y_title, tickformat = ".1%"),
      legend = list(orientation = "h", x = 0, y = 1.15), margin = list(l = 80, r = 20, t = 65, b = 85)) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }
  # Keep the underlying counts and denominators available for analyst inspection.
  coverage <- dplyr::bind_rows(lapply(c("Delivery", "Booking", "Boundary age mix"), function(label) {
    data <- switch(label, Delivery = delivery, Booking = booking, "Boundary age mix" = age_mix)
    data |> dplyr::group_by(FY) |> dplyr::summarise(
      Cells = dplyr::n(), Plotted = sum(is.finite(Departure_pp)),
      No_annual_denominator = sum(!is.finite(Annual)), .groups = "drop") |>
      dplyr::mutate(Input = label)
  }))
  list(ons_annual = ons$annual, ons_means = ons$factors,
    delivery = delivery, booking = booking, age_mix = age_mix, coverage = coverage,
    delivery_chart = departure_plot(delivery, "Destination", "Delivery trust"),
    booking_chart = departure_plot(booking, "Destination", "Booking trust"),
    age_mix_chart = departure_plot(age_mix, "Age", "Age (1) within the combined ONS boundary groups"),
    stillbirth_chart = ons_plot("Stillbirth_ratio", "Stillbirths / births (L)"),
    multiplicity_chart = ons_plot("Multiplicity_factor", "Maternities / births (L+S)"))
}

# 3.10 Retrospective comparison with recorded deliveries -----------------
# Pooled calibration includes the comparison years; not independent validation.
build_projection_backtest <- function() {
  # Section 3.5.2: compare with actual history.
  # Apply the same pooled three-year proportions to historical ONS counts.
  # This is a retrospective diagnostic using current ONS/registration data, not a vintage backtest.
  historical <- build_first_draft_projections(baseline_fy = "2023/24",
    ons_reference_year = 2024L, projection_end = 2026L, prepare_outputs = FALSE)
  organisations <- historical$organisations
  years <- projection_history_years
  # Count the actual deliveries separately for each financial year.
  observed <- prepare_report_cohort(final_cohort_2326) |>
    dplyr::filter(Year %in% years, Birth_recorded, Trust %in% cluster_trusts, Home == 0) |>
    dplyr::mutate(Year_label = Year)
  observed_totals <- dplyr::bind_rows(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, , drop = FALSE]
    keep <- if (org$Level == "Trust") observed$Trust == org$Code else
      if (org$Level == "ICB") unname(provider_icb[observed$Trust]) == org$Code else rep(TRUE, nrow(observed))
    data <- observed[keep, , drop = FALSE]
    data.frame(Key = org$Key, Year_label = years,
      Observed = vapply(years, function(year) sum(data$Year_label == year), integer(1)))
  }))
  # Put observed and modelled counts side by side and calculate the difference.
  comparison <- historical$maternity_projections |>
    dplyr::transmute(Key, Level, Organisation, Year,
      Year_label = paste0(Year - 1L, "/", substr(Year, 3, 4)), Modelled = Projected) |>
    dplyr::left_join(observed_totals, by = c("Key", "Year_label")) |>
    dplyr::mutate(Difference = Modelled - Observed,
      Percent_difference = ifelse(Observed > 0, Difference / Observed, NA_real_)) |>
    dplyr::arrange(match(Key, organisations$Key), Year)
  # Retrospective charts retain every recorded delivery trust across the three years.
  trust_organisations <- organisations |>
    dplyr::filter(Level == "Trust", Code %in% c(setdiff(names(trust_labels), "Other"), observed$Trust))
  icb_cluster_organisations <- organisations |>
    dplyr::filter(Level %in% c("ICB", "Cluster"))
  chart_organisations <- dplyr::bind_rows(trust_organisations, icb_cluster_organisations)
  outputs <- stats::setNames(lapply(chart_organisations$Key, function(key) {
    data <- comparison[comparison$Key == key, , drop = FALSE]
    chart <- plotly::plot_ly(height = projection_history_chart_height) |>
      plotly::add_trace(x = data$Year, y = data$Observed, type = "scatter", mode = "lines+markers",
        name = "MSDS observed", line = list(color = projection_history_colours[["MSDS observed"]], width = 2),
        marker = list(color = projection_history_colours[["MSDS observed"]], size = 5),
        text = paste0(data$Year_label, "<br>Observed: ", scales::comma(data$Observed, accuracy = 1)),
        hovertemplate = "%{text}<extra></extra>") |>
      plotly::add_trace(x = data$Year, y = data$Modelled, type = "scatter", mode = "lines+markers",
        name = "ONS-based model", line = list(color = projection_history_colours[["ONS-based model"]], width = 2),
        marker = list(color = projection_history_colours[["ONS-based model"]], size = 5),
        text = paste0("ONS year ending mid-", data$Year, "<br>Modelled: ", scales::comma(data$Modelled, accuracy = 1),
          "<br>Difference from observed: ", ifelse(is.na(data$Percent_difference), "n.a.",
                                                   scales::percent(data$Percent_difference, accuracy = 0.1))),
        hovertemplate = "%{text}<extra></extra>") |>
      plotly::layout(xaxis = list(title = "Comparison year",
        tickmode = "array", tickvals = 2024:2026, ticktext = years),
        yaxis = list(title = "Maternities", tickformat = ",.0f", rangemode = "tozero"),
        showlegend = FALSE,
        margin = list(l = 65, r = 12, t = 15, b = 55), font = list(family = "Arial", size = 11)) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
    list(chart = chart)
  }), chart_organisations$Key)

  # Put one shared colour key above each grid, as in the antenatal pathway charts.
  comparison_key <- htmltools::div(
    style = "display: flex; flex-wrap: wrap; gap: 12px 24px; margin-bottom: 20px; font-size: 14px;",
    htmltools::tagList(lapply(names(projection_history_colours), function(series) {
      htmltools::span(style = "display: inline-flex; align-items: center; gap: 6px;",
        htmltools::span(style = paste0("display: inline-block; width: 18px; height: 3px; background: ",
          projection_history_colours[[series]], ";"), `aria-hidden` = "true"), series)
    })))
  comparison_grid <- function(rows) {
    htmltools::tagList(comparison_key,
      htmltools::div(style = "display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 24px;",
        htmltools::tagList(lapply(seq_len(nrow(rows)), function(i) {
          htmltools::div(style = "min-width: 0;",
            htmltools::h3(style = "font-size: 14px; margin: 0 0 8px;", rows$Organisation[i]),
            outputs[[rows$Key[i]]]$chart)
        }))))
  }
  trust_grid <- comparison_grid(trust_organisations)
  icb_cluster_grid <- comparison_grid(icb_cluster_organisations)

  # Keep all comparison values together: one organisation per row and metrics under each year.
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
    gt::tab_source_note(source_note = "% difference = (modelled − observed) / observed × 100. Positive means modelled is higher; n.a. = zero observed count.") |>
    gt::tab_options(table.font.names = c("Open Sans", "Arial", "sans-serif"),
                    container.width = "100%", container.overflow.x = TRUE,
                    table.width = gt::px(1500), table.font.size = gt::px(12))
  for (year in years) all_table <- gt::tab_spanner(all_table, label = year,
    columns = paste0(metrics, "_", year), id = paste0("backtest-", year), gather = FALSE)
  coverage_table <- make_projection_coverage_table(historical$coverage, "2023/24")
  coverage_details_table <- historical$coverage_details |>
    dplyr::select(Level, Organisation, LA_name, LA, Age, Reason, N) |>
    gt::gt() |>
    gt::tab_header(title = "2023/24 records without matched ONS counts") |>
    gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age = "Maternal age",
      Reason = "Reason", N = "Observed deliveries") |>
    gt::fmt_number(columns = "N", decimals = 0)
  list(note = historical$note, organisations = organisations, baseline = historical,
       data = comparison, outputs = outputs, trust_grid = trust_grid,
       icb_cluster_grid = icb_cluster_grid, all_table = all_table,
       coverage = historical$coverage, coverage_details = historical$coverage_details,
       coverage_table = coverage_table, coverage_details_table = coverage_details_table)
}

# 3.11 Prepare and save the model report outputs -------------------------
# Save only after forward projections and the retrospective comparison finish.
first_draft_projections <- build_first_draft_projections()
projection_backtest <- build_projection_backtest()
first_draft_projections$backtest <- projection_backtest
first_draft_projections$age_boundary_review <- build_msds_age_review(
  final_cohort_2326, first_draft_projections, projection_backtest$baseline,
  trust_labels, cluster_trusts)
first_draft_projections$input_variation <- build_input_variation_review(
  final_cohort_2326, total_births_by_LA_2326, registration_counts,
  first_draft_projections$allocation_tables)
saveRDS(first_draft_projections, ".projections.rds")

