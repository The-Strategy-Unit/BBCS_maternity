# BBCS MATERNITY ANALYSIS ================================================
# Main editing file: general setup, ONS exploration, MSDS exploration, model
# Run with Run analysis.R from the project root
# The secure parquet reader supplies the input tables used below.

# TERMINOLOGY KEY --------------------------------------------------------
# Births (L)   = live-born babies
# Births (L+S) = live-born + stillborn babies
# Maternities = pregnancies entering the maternity pathway: booking and/or delivery
#               Count once per pregnancy, regardless of outcome
# ONS maternities = pregnancies reaching at least one live birth or stillbirth
# Age (SY)     = individual year of maternal age (34, 35, 36, ...)
# Age (B)     = age band used for descriptive charts / final summaries
# ONS age     = Age (SY) at 16-43, plus combined <=15 and 44+ source rows
# LA(R)       = local authority of residence; column LA in the model
# LA(B)       = LA of booking location, only if a location lookup supplies it
# LA(D)       = LA of delivery location, only if a location lookup supplies it
# LA(C)       = resident LAs contributing to a stated trust/ICB/cluster catchment
# Trust(B/D)  = booking / delivery provider, NOT a local authority
# ICB(R/B/D)  = residence / booking-provider / delivery-provider ICB
# This model uses LA(R), Trust(B/D), and separate booking/delivery-provider ICBs.
# It does not contain a booking-site or delivery-site LA lookup.
# A catchment is a set of contributing LA(R)s, not a new LA code.
# FY          = financial year, labelled 2025/26 for example
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
source("R/disclosure.R")
source("R/report_colours.R")
source("R/ons_data.R")
source("R/report_cohort.R")
source("R/age_boundary_review.R")

# Settings, labels and colours ------------------------------------------
# Settings: years, organisation labels and chart colours
analysis_years <- 2023:2025
latest_year <- max(analysis_years)
cluster_icbs <- c("QUA", "QHL")
cluster_label <- "Combined cluster"
outcome_columns <- c("Within", "Outside", "No_delivery", "Unknown")
outcome_labels <- c("Delivery at cluster trust", "Other trust or home birth",
                    "No recorded delivery", "Delivery location unknown")
outcome_colours <- c("#006747", "#ED8B00", "#2C2825", "#959595")
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

icb_source_order <- c("QJ2", "QWU", "QUA", "QHL", "QGH", "QOC", "QNC", "Other")
icb_target_order <- icb_source_order
trust_source_order <- c("RNA", "RXK", "RBK", "RL4", "RRK", "RQ3", "Other")
trust_target_order <- trust_source_order

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
projection_reference_year <- as.integer(substr(projection_baseline, 1, 4))
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

# Use the configured areas, age range and colours.
area_config <- read_area_config()
analysis_parameters <- read_analysis_parameters()
sources <- build_source_summary()
# MODEL_INPUT: ONS_BIRTHS_L
# Births (L), LA(R) x ONS source age x year
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
pooled_rates <- pooled_multiplicity_rates(multiplicity_data)
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
  pooled_rates = pooled_rates,
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
# final_cohort_2326: one supplied pregnancy row, with LA(R), Age (SY), Trust(B/D)
# MODEL_INPUT: MSDS_BOOKINGS
# All supplied pregnancies, including no recorded booking or delivery.
# MODEL_INPUT: MSDS_RESIDENTS
# Every LA: group identifies deliveries (birth only + antenatal+delivery) and
# antenatal only. Both use birth FY/age first, with EDD FY/booking-age fallback.
# Source names containing "births" do not change their maternity counting unit.
# 2.1 Latest-year patient flows (report section 2.2.4) ------------------------
# CONTEXT_ONLY: pregnancy-year charts and Age (B) summaries stay exploratory.
# Section 3 uses supplied rows aged 13--54 and one shared LA(R)/Age (SY) denominator.
# Check the required inputs once; data QA is in R/QA.R.
required_objects <- "final_cohort_2326"
missing_objects <- required_objects[!vapply(required_objects, function(name) exists(name, inherits = TRUE), logical(1))]
if (length(missing_objects)) stop("Run the preparation scripts first. Missing: ",
                                  paste(missing_objects, collapse = ", "), call. = FALSE)
report_episodes <- prepare_report_cohort(final_cohort_2326)

maternal_age_completeness_table <- function(counts) {
  dc_cells(counts, c("N", "Denominator"), list(Percent = c("N", "Denominator"))) |>
    dplyr::select(Measure, N, Percent, Percentage_of) |>
    gt::gt() |>
    gt::tab_header(title = "Maternal age completeness in the supplied cohort") |>
    gt::cols_label(Measure = "Measure", N = "Pregnancies (n)", Percent = "%",
      Percentage_of = "Percentage of") |>
    gt::fmt_number(columns = N, decimals = 0) |>
    gt::fmt_percent(columns = Percent, decimals = 1) |>
    gt::sub_missing(columns = Percent, missing_text = "Not applicable") |>
    gt::tab_source_note("All supplied pregnancy rows, before age or year filters. Missing means NA in the source field. Birth recorded: birth_flag = 1; no birth recorded: birth_flag = 0; other flags remain separate. The booking-age row overlaps the birth-status rows; these rows must not be added together.") |>
    gt::tab_options(table.width = gt::pct(100))
}
maternal_age_completeness <- summarise_maternal_age_completeness(final_cohort_2326)
maternal_age_completeness_display <- maternal_age_completeness_table(maternal_age_completeness)

booking_age_fallback_outputs <- function(counts) {
  shown <- dc_cells(counts, c("Booking_age_known", "Birth_age_missing"),
    list(Share = c("Birth_age_missing", "Booking_age_known")))
  # Empty display bands have no percentage; all source rows, including unknown
  # booking ages, remain represented. Keep zero-row bands in the companion table.
  chart_data <- shown[shown$Booking_age_known > 0, , drop = FALSE]
  chart <- plotly::plot_ly(chart_data, x = ~Age_band, y = ~Share, type = "bar",
    height = 380, marker = list(color = "#0072B2"),
    text = ~paste0("Booking age: ", Age_band,
      "<br>Birth age missing: ", scales::comma(Birth_age_missing, accuracy = 1),
      "<br>Records in this booking-age group: ", scales::comma(Booking_age_known, accuracy = 1),
      "<br>Missing birth age: ", ifelse(is.na(Share), "Not available", scales::percent(Share, accuracy = 0.1))),
    textposition = "none", hovertemplate = "%{text}<extra></extra>") |>
    plotly::layout(xaxis = list(title = "Age at booking", categoryorder = "array",
      categoryarray = counts$Age_band),
      yaxis = list(title = "Birth age missing (%)", tickformat = ".0%", rangemode = "tozero"),
      margin = list(l = 75, r = 20, t = 20, b = 60), showlegend = FALSE) |>
    plotly::config(responsive = TRUE, displaylogo = FALSE)
  if (all(is.na(shown$Share))) chart <- plotly::layout(chart,
    yaxis = list(range = c(0, 1)), annotations = list(list(
      text = "No pregnancy rows in the supplied cohort", x = .5, y = .5,
      xref = "paper", yref = "paper", showarrow = FALSE)))
  table <- shown |>
    gt::gt() |>
    gt::cols_label(Age_band = "Age at booking", Booking_age_known = "Records in age group (n)",
      Birth_age_missing = "Birth age missing (n)", Share = "% with birth age missing") |>
    gt::fmt_number(columns = c(Booking_age_known, Birth_age_missing), decimals = 0) |>
    gt::fmt_percent(columns = Share, decimals = 1) |>
    gt::sub_missing(columns = Share, missing_text = "Not available") |>
    gt::tab_options(table.width = gt::pct(100))
  list(chart = dc_widget(chart), table = table)
}
booking_age_fallback_counts <- summarise_booking_age_fallback(final_cohort_2326)
booking_age_fallback <- booking_age_fallback_outputs(booking_age_fallback_counts)

record_completeness_outputs <- function(counts) {
  sources <- c("Birth", "Booking fallback", "Unknown")
  colours <- c("Birth" = "#0072B2", "Booking fallback" = "#D55E00", "Unknown" = "#777777")
  by_scope <- lapply(unique(counts$age_sources$Scope), function(scope) {
    age <- counts$age_sources |> dplyr::filter(Scope == scope)
    shown <- dc_cells(age, c("N", "Total"), list(Share = c("N", "Total")))
    shown$Age_source <- factor(shown$Age_source, levels = sources)
    chart <- plotly::plot_ly(shown, y = ~Group, x = ~Share, color = ~Age_source,
      colors = colours, type = "bar", orientation = "h",
      height = max(260, 28 * dplyr::n_distinct(shown$Group) + 120),
      text = ~paste0(Group, "<br>Age source: ", Age_source,
        "<br>Pregnancies: ", scales::comma(N), " / ", scales::comma(Total),
        "<br>Share: ", scales::percent(Share, accuracy = .1)),
      hovertemplate = "%{text}<extra></extra>") |>
      plotly::layout(barmode = "stack",
        xaxis = list(title = "Share of all pregnancy rows in this group", tickformat = ".0%"),
        yaxis = list(title = scope, automargin = TRUE),
        legend = list(orientation = "h", y = -0.2), margin = list(l = 140, b = 100)) |>
      plotly::config(responsive = TRUE, displaylogo = FALSE)
    age_table <- shown |> dplyr::select(Group, Age_source, N, Total, Share) |>
      gt::gt() |> gt::cols_label(Age_source = "Age used", N = "Pregnancies",
        Total = "All rows in group", Share = "% of group") |>
      gt::fmt_number(columns = c(N, Total), decimals = 0) |>
      gt::fmt_percent(columns = Share, decimals = 1)
    field_table <- counts$fields |> dplyr::filter(Scope == scope) |>
      dc_cells(c("N", "Total"), list(Share = c("N", "Total"))) |>
      dplyr::select(Group, Field, Status, N, Total, Share) |>
      gt::gt(groupname_col = "Group") |>
      gt::cols_label(N = "Pregnancies", Total = "All rows in group", Share = "% of group") |>
      gt::fmt_number(columns = c(N, Total), decimals = 0) |>
      gt::fmt_percent(columns = Share, decimals = 1) |>
      gt::tab_source_note("Each field uses every pregnancy row in the group as its denominator. Missing fields are not filled from a different event. Supplied means present, not verified or conflict-free.")
    list(chart = dc_widget(chart), age_table = age_table, field_table = field_table)
  })
  names(by_scope) <- unique(counts$age_sources$Scope)
  list(by_scope = by_scope)
}
record_completeness_counts <- summarise_record_completeness(final_cohort_2326)
record_completeness <- record_completeness_outputs(record_completeness_counts)

# Section 2.2.4: patient flows in 2025/26
# Two-stage flows and the combined residence-to-booking-to-birth flow.
# Clean the supplied codes and group providers outside the labelled areas as Other.
group_flow_code <- function(x, keep_codes) {
  code <- report_code(x)
  code_pattern <- paste0("^", substr(keep_codes[1], 1, 1), "[A-Z0-9]{2}$")
  as.character(ifelse(code %in% keep_codes, code,
    ifelse(code == "OTHER" | grepl(code_pattern, code), "Other", "Unknown")))
}

# Booking/delivery stages, including missing events
flow_stage_icb <- function(provider, stage) {
  icb <- unname(provider_icb[provider])
  icb[is.na(icb)] <- ifelse(grepl("^R[A-Z0-9]{2}$", provider[is.na(icb)]), "Other", "Unknown")
  special <- stage %in% c("No booking", "No delivery", "Home")
  icb[special] <- stage[special]
  icb
}

classify_patient_flow_stages <- function(cohort) {
  keep <- union(setdiff(names(trust_labels), "Other"), cluster_trusts)
  cohort |>
    dplyr::mutate(
      Has_booking = Booking_provider_supplied | Booking_date_supplied,
      Booking = dplyr::if_else(Has_booking, group_flow_code(Booking_Trust, keep), "No booking"),
      Delivery = dplyr::case_when(
        !Birth_recorded ~ "No delivery", Home %in% 1 ~ "Home",
        TRUE ~ group_flow_code(Trust, keep)),
      Booking_ICB = flow_stage_icb(Booking_Trust, Booking),
      Delivery_ICB = flow_stage_icb(Trust, Delivery))
}

full_flow_trust_labels <- function() {
  extra <- setdiff(cluster_trusts, names(flow_trust_labels))
  c(flow_trust_labels, stats::setNames(extra, extra),
    `No booking` = "No recorded booking", `No delivery` = "No recorded delivery",
    Home = "Home birth")
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


# Use the same pregnancy records, flags and pregnancy-year definition in every year.
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
make_residence_booking_birth_flow <- function(cohort, all_pregnancies = FALSE) {
  if (all_pregnancies) cohort <- classify_patient_flow_stages(cohort)
  paths <- tibble::tibble(
    residence = group_flow_code(cohort$Resident_ICB, setdiff(names(icb_labels), "Other")),
    booking = if (all_pregnancies) cohort$Booking else
      group_flow_code(cohort$Booking_Trust, setdiff(names(trust_labels), "Other")),
    birth = if (all_pregnancies) cohort$Delivery else
      group_flow_code(cohort$Trust, setdiff(names(trust_labels), "Other"))) |>
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

# Section 2.2.4B: all pregnancies in the assigned year
make_full_cohort_year_flows <- function(start_year, episodes = report_episodes) {
  cohort <- episodes |> dplyr::filter(Pregnancy_year == start_year)
  stages <- classify_patient_flow_stages(cohort)
  three_stage <- make_residence_booking_birth_flow(cohort, all_pregnancies = TRUE)
  icb <- stages |>
    dplyr::count(source = group_flow_code(Resident_ICB, setdiff(names(icb_labels), "Other")),
      target = Delivery_ICB, name = "n")
  list(icb = list(links = icb), trust = list(links = three_stage$booking_birth),
    residence_booking_birth = three_stage)
}
full_patient_flows <- make_full_cohort_year_flows(latest_year)

# Visible inclusion accounting: these mutually exclusive delivery categories add up.
flow_coverage <- report_episodes |>
  dplyr::mutate(Period = dplyr::coalesce(Year, "Missing/invalid pregnancy FY")) |>
  dplyr::group_by(Period) |>
  dplyr::summarise(Supplied_rows = dplyr::n(),
    No_recorded_birth = sum(!Birth_recorded),
    Home_births = sum(Birth_recorded & Home %in% 1),
    Unknown_home_flag = sum(Birth_recorded & !Home %in% c(0, 1)),
    Non_home = sum(Birth_recorded & Home %in% 0),
    Unknown_birth_provider = sum(Birth_recorded & Home %in% 0 &
      !grepl("^R[A-Z0-9]{2}$", Trust)), .groups = "drop")
flow_coverage_table <- gt::gt(dc_counts(flow_coverage, setdiff(names(flow_coverage), "Period"))) |>
  gt::tab_header(title = "Which supplied pregnancy records enter the delivery flows?") |>
  gt::cols_label(Period = "Delivery year", Supplied_rows = "Supplied rows",
    No_recorded_birth = "Birth flag not 1", Home_births = "Home births",
    Unknown_home_flag = "Home flag unknown/invalid", Non_home = "Recorded non-home deliveries",
    Unknown_birth_provider = "Of non-home: provider unknown/invalid") |>
  gt::fmt_number(columns = -Period, decimals = 0) |>
  gt::tab_source_note("Only recorded non-home deliveries in 2023/24–2025/26 enter sections 2.2.4A and 2.2.5. Section 2.2.4B includes all supplied pregnancies assigned to 2025/26. Unknown providers remain separate. The four inclusion categories are mutually exclusive; the last column is a subset.") |>
  gt::tab_options(container.width = "100%", container.overflow.x = TRUE)

# Sankey charts

# Fixed category order; other, home and missing categories last
sankey_category_order <- function(preferred, available = preferred) {
  available <- unique(as.character(available))
  ordered <- unique(c(preferred, sort(setdiff(available[!is.na(available)], preferred))))
  ordered <- ordered[!is.na(ordered) & ordered %in% available]
  key <- tolower(trimws(ordered))
  last <- !nzchar(key) | key %in% c("na", "n/a", "null", "<na>", "none", "blank") |
    grepl("^(other($| trust| provider| icb| ethnic)|all other |home($| birth| delivery)|no (recorded |booking|delivery)|unknown|missing|not (stated|known|recorded)|two conflicting|l999)", key)
  # Shared order for trailing categories
  tail_rank <- rep(5L, length(key))
  tail_rank[grepl("^(other($| trust| provider| icb| ethnic)|all other )", key)] <- 1L
  tail_rank[grepl("^home($| birth| delivery)", key)] <- 2L
  tail_rank[grepl("^no (recorded )?booking", key)] <- 3L
  tail_rank[grepl("^no (recorded )?delivery", key)] <- 4L
  c(ordered[!last], ordered[last][order(tail_rank[last])], if (anyNA(available)) NA_character_)
}

# Node positions from controlled ribbon widths; matched in assets/report-charts.html
sankey_node_y <- function(x, source, target, value, height, pad) {
  incoming <- outgoing <- numeric(length(x))
  for (i in seq_along(value)) if (is.finite(value[i]) && value[i] > 0) {
    outgoing[source[i] + 1L] <- outgoing[source[i] + 1L] + value[i]
    incoming[target[i] + 1L] <- incoming[target[i] + 1L] + value[i]
  }
  weights <- pmax(incoming, outgoing)
  layers <- lapply(unique(x), function(stage) which(x == stage & weights > 0))
  layers <- Filter(length, layers)
  y <- rep(0.5, length(x))
  if (!length(layers)) return(y)
  gap <- min(pad, height / max(1L, max(lengths(layers)) - 1L))
  scale <- min(vapply(layers, function(ids)
    (height - gap * (length(ids) - 1L)) / sum(weights[ids]), numeric(1)))
  for (ids in layers) {
    sizes <- weights[ids] * scale
    spare <- height - sum(sizes) - gap * (length(ids) - 1L)
    y[ids] <- (spare / 2 + cumsum(sizes) - sizes / 2 + gap * (seq_along(ids) - 1L)) / height
  }
  y
}

make_sankey <- function(flow, labels, source_suffix, target_suffix,
                        source_order, target_order,
                        empty_message = "No recorded non-home deliveries in this year.") {
  links <- flow$links
  source_order <- sankey_category_order(source_order)
  target_order <- sankey_category_order(target_order)
  codes <- unique(c(source_order, target_order))
  colours <- identity_colours(codes)
  display_label <- function(x) unname(labels[x])
  total <- sum(links$n)
  if (!nrow(links)) return(htmltools::div(empty_message))

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
  total <- dc_count(total)
  node_totals <- dc_count(node_totals)
  source_totals <- dc_count(source_totals)
  links$n <- dc_count(links$n)
  chart_height <- max(500, length(node_codes) * 38)
  node_x <- c(rep(0.01, length(source_nodes)), rep(0.99, length(target_nodes)))
  hover <- paste0(display_label(links$source), " to ", display_label(links$target),
                  "<br>Records: ", scales::comma(links$n),
                  "<br>", scales::percent(links$n / source_totals, accuracy = 0.1),
                  " of records from this origin",
                  "<br>", scales::percent(links$n / total, accuracy = 0.1),
                  " of all records")

  plotly::plot_ly(
    type = "sankey", height = chart_height,
    orientation = "h", arrangement = "fixed",
    node = list(
      label = node_labels, color = unname(colours[node_codes]),
      x = node_x, y = sankey_node_y(node_x, from_id, to_id, links$n, chart_height - 36, 14),
      customdata = paste0(scales::comma(node_totals), "<br>", scales::percent(dc_divide(node_totals, total), accuracy = 0.1)),
      pad = 14, thickness = 18,
      line = list(color = "rgba(44,40,37,0.35)", width = 0.5),
      hovertemplate = "%{label}<br>Records: %{customdata} of all records<extra></extra>"
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
make_three_stage_sankey <- function(flow, all_pregnancies = FALSE) {
  total <- sum(flow$paths$n)
  if (!nrow(flow$paths)) return(htmltools::div(if (all_pregnancies)
    "No supplied pregnancies assigned to this year." else "No recorded non-home deliveries in this year."))

  provider_labels <- if (all_pregnancies) full_flow_trust_labels() else flow_trust_labels
  source_order <- sankey_category_order(trust_source_order, names(provider_labels))
  target_order <- sankey_category_order(trust_target_order, names(provider_labels))
  count_label <- if (all_pregnancies) "Pregnancies" else "Deliveries"
  count_noun <- tolower(count_label)
  stage_labels <- if (all_pregnancies) c("ICB of residence", "Booking trust / status", "Delivery trust / status") else
    c("ICB of residence", "Booking trust", "Delivery trust")

  residence <- icb_source_order[icb_source_order %in% flow$paths$residence]
  booking <- source_order[source_order %in% flow$paths$booking]
  birth <- target_order[target_order %in% flow$paths$birth]
  stage <- rep(stage_labels,
               c(length(residence), length(booking), length(birth)))
  full_labels <- unname(c(flow_icb_labels[residence], provider_labels[booking],
                         provider_labels[birth]))
  short_labels <- sub(" NHS.*$", "", sub(" ICB$", "", full_labels))
  short_labels <- vapply(short_labels, function(label)
    paste(strwrap(label, width = 24), collapse = "<br>"), character(1))
  icb_colours <- identity_colours(icb_source_order)
  trust_colours <- identity_colours(source_order)
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
  total <- dc_count(total)
  node_totals <- dc_count(node_totals)
  counts <- dc_count(counts)
  chart_height <- max(520, 58 * max(length(residence), length(booking), length(birth)))
  node_x <- c(rep(0.01, length(residence)), rep(0.5, length(booking)), rep(0.99, length(birth)))
  hover <- paste0(stage[from_id + 1L], ": ", full_labels[from_id + 1L], "<br>",
    stage[to_id + 1L], ": ", full_labels[to_id + 1L],
    "<br>", count_label, ": ", scales::comma(counts),
    "<br>", scales::percent(counts / node_totals[from_id + 1L], accuracy = 0.1),
    " of ", count_noun, " from this origin at this stage",
    "<br>", scales::percent(counts / total, accuracy = 0.1), " of all included ", count_noun)

  plotly::plot_ly(type = "sankey", height = chart_height,
    orientation = "h", arrangement = "fixed",
    node = list(label = short_labels, color = node_colours, pad = 22, thickness = 18,
      x = node_x, y = sankey_node_y(node_x, from_id, to_id, counts, chart_height - 54, 22),
      line = list(color = "rgba(44,40,37,0.35)", width = 0.5),
      customdata = paste0(stage, ": ", full_labels, "<br>", count_label, ": ", scales::comma(node_totals), "<br>",
        scales::percent(node_totals / total, accuracy = 0.1), " of all included ", count_noun),
      hovertemplate = "%{customdata}<extra></extra>"),
    link = list(source = from_id, target = to_id, value = counts,
      color = grDevices::adjustcolor(node_colours[from_id + 1L], alpha.f = 0.42),
      customdata = hover, hovertemplate = "%{customdata}<extra></extra>")) |>
    plotly::layout(font = list(family = "Arial", size = 11, color = "#2c2825"),
      margin = list(l = 12, r = 12, t = 42, b = 12),
      annotations = list(
        list(x = 0, y = 1.07, xref = "paper", yref = "paper", xanchor = "left",
             text = "<b>ICB of residence</b>", showarrow = FALSE),
        list(x = 0.5, y = 1.07, xref = "paper", yref = "paper", xanchor = "center",
             text = paste0("<b>", stage_labels[2], "</b>"), showarrow = FALSE),
        list(x = 1, y = 1.07, xref = "paper", yref = "paper", xanchor = "right",
             text = paste0("<b>", stage_labels[3], "</b>"), showarrow = FALSE)),
      paper_bgcolor = "white", plot_bgcolor = "white") |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

# Chart objects
icb_sankey <- make_sankey(
  icb_flow, flow_icb_labels, " (residence)", " (delivery)",
  icb_source_order, icb_target_order
)
trust_sankey <- make_sankey(
  trust_flow, flow_trust_labels, " (booking)", " (delivery)",
  trust_source_order, trust_target_order
)
residence_booking_birth_sankey <- make_three_stage_sankey(residence_booking_birth_flow)

full_patient_flows$icb_sankey <- make_sankey(full_patient_flows$icb,
  c(flow_icb_labels, Home = "Home birth", `No delivery` = "No recorded delivery"),
  " (residence)", " (delivery)", icb_source_order,
  c(icb_target_order, "Home", "No delivery"),
  empty_message = "No supplied pregnancies assigned to this year.")
full_patient_flows$trust_sankey <- make_sankey(full_patient_flows$trust,
  full_flow_trust_labels(), " (booking)", " (delivery)",
  sankey_category_order(trust_source_order, names(full_flow_trust_labels())),
  sankey_category_order(trust_target_order, names(full_flow_trust_labels())),
  empty_message = "No supplied pregnancies assigned to this year.")
full_patient_flows$residence_booking_birth_sankey <- make_three_stage_sankey(
  full_patient_flows$residence_booking_birth, all_pregnancies = TRUE)

# 2.2 Patient flows by financial year (report section 2.2.5) ------------------
# This bit repeats the two Sankeys for each year and makes their tables.

make_year_sankeys <- function(start_year) {
  flows <- make_cohort_year_flows(start_year)
  list(
    icb_flow = flows$icb, trust_flow = flows$trust,
    icb_sankey = make_sankey(flows$icb, flow_icb_labels, " (residence)", " (delivery)",
                             icb_source_order, icb_target_order),
    trust_sankey = make_sankey(flows$trust, flow_trust_labels, " (booking)", " (delivery)",
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
  origin_total <- dc_count(ave(flow_counts$n, flow_counts$source, FUN = sum))
  total <- dc_count(sum(flow_counts$n))
  flow_counts$n <- dc_count(flow_counts$n)
  flow_counts$origin_share <- dc_divide(flow_counts$n, origin_total)
  flow_counts$total_share <- dc_divide(flow_counts$n, total)
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
    by_year[[year]]$icb_flow, flow_icb_labels, "ICB of residence", "Delivery provider ICB",
    icb_source_order, icb_target_order, year
  )
  by_year[[year]]$trust_table <- make_flow_table(
    by_year[[year]]$trust_flow, flow_trust_labels, "Booking trust", "Delivery trust",
    trust_source_order, trust_target_order, year
  )
}

# 2.3 Cluster deliveries and booking starts (report section 2.2.2) ------------
# Use the same supplied pregnancy rows as the patient-flow, geography and projection views.
activity_episodes <- report_episodes |>
  dplyr::transmute(Row, person_id_mother = Person, uniq_preg_id = Pregnancy,
    delivery_date = Delivery_date, delivery_year = Delivery_year,
    birth_code = Trust, booking_code = Booking_Trust, residence_code = Resident_ICB,
    booking_date = Booking_date, booking_year = Pregnancy_year,
    pregnancy_fy = Pregnancy_FY, pregnancy_year_source = Pregnancy_year_source,
    birth_flag = Birth_flag, birth_recorded = Birth_recorded, home_birth = Home)

# Shared pathway definition for the latest-year and three-year outputs.
classify_pathway_outcome <- function(episodes) {
  # Assigned birth/due FY does not establish a recorded birth.
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
  note = "The combined cluster includes Black Country and Birmingham and Solihull. Delivery totals count each pregnancy with a recorded live birth or stillbirth at a cluster provider once, in its assigned pregnancy financial year. Home births and records with unknown home status are excluded from these hospital totals. Antenatal bookings count pregnancies by booking provider and assigned pregnancy financial year, retaining every recorded outcome, including home births, unknown delivery locations and no recorded delivery."
)

safe_share <- function(n, total) dc_ratio(n, total)
count_text <- function(x) scales::comma(dc_count(x), accuracy = 1)
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
cluster_activity$total_table <- gt::gt(dc_counts(residence_totals, "Episodes")) |>
  gt::tab_header(title = "Recorded deliveries within the cluster", subtitle = "2025/26") |>
  gt::cols_label(Episodes = "Deliveries", Share = "% of cluster deliveries") |>
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
cluster_activity$local_table <- gt::gt(dc_counts(local_deliveries, "Episodes")) |>
  gt::tab_header(title = "Cluster residents delivering within the cluster", subtitle = "2025/26") |>
  gt::cols_label(ICB = "ICB of residence", Episodes = "Deliveries", Share = "% of cluster deliveries") |>
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
cluster_activity$outside_table <- gt::gt(dc_counts(outside_summary, "Episodes")) |>
  gt::tab_header(title = "Outside-cluster residents delivering within the cluster", subtitle = "2025/26") |>
  gt::cols_label(ICB = "ICB of residence", Episodes = "Deliveries", Share = "% of outside-resident deliveries",
                 Cluster_share = "% of cluster deliveries") |>
  gt::fmt_number(columns = "Episodes", decimals = 0) |>
  gt::fmt_percent(columns = c("Share", "Cluster_share"), decimals = 1)
cluster_activity$outside_diagram <- plotly::plot_ly(
  dc_counts(outside_summary, "Episodes"), x = ~Episodes, y = ~ICB, type = "bar", orientation = "h", height = 420,
  marker = list(color = main_chart_colour),
  hovertemplate = "%{y}<br>Deliveries: %{x:,.0f}<extra></extra>"
) |>
  plotly::layout(xaxis = list(title = "Deliveries"),
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

# Same pregnancy-year definition across all three years
delivery_trend <- do.call(rbind, lapply(analysis_years, function(year) {
  year_deliveries <- deliveries[deliveries$delivery_year == year, ]
  data.frame(Year = paste0(year, "/", substr(year + 1L, 3, 4)),
    Total = nrow(year_deliveries), Local = sum(year_deliveries$residence_type == "Cluster residents"),
    Outside = sum(year_deliveries$residence_type == "Outside-cluster residents"),
    Unknown = sum(year_deliveries$residence_type == "Unknown residence"))
}))
delivery_trend$Outside_share <- safe_share(delivery_trend$Outside, delivery_trend$Total)
delivery_trend$Change <- c(NA_real_, diff(dc_count(delivery_trend$Total)))
cluster_activity$trend_table <- gt::gt(dc_counts(delivery_trend, c("Total", "Local", "Outside", "Unknown"))) |>
  gt::cols_label(Year = "Financial year", Total = "Cluster deliveries", Local = "Cluster residents",
    Outside = "Outside-cluster residents", Unknown = "Unknown residence",
    Outside_share = "Outside-resident share", Change = "Change in deliveries from previous year") |>
  gt::fmt_number(columns = c("Total", "Local", "Outside", "Unknown", "Change"), decimals = 0) |>
  gt::fmt_percent(columns = "Outside_share", decimals = 1) |>
  gt::sub_missing(missing_text = "—")
cluster_activity$trend_text <- paste0(
  "Cluster deliveries changed from ", count_text(delivery_trend$Total[1]), " in 2023/24 to ",
  count_text(delivery_trend$Total[3]), " in 2025/26 (",
  if (delivery_trend$Total[3] >= delivery_trend$Total[1]) "+" else "", scales::comma(dc_count(delivery_trend$Total[3]) - dc_count(delivery_trend$Total[1]), accuracy = 1),
  "). The share for women living outside the cluster changed from ", percent_text(delivery_trend$Outside_share[1]),
  " to ", percent_text(delivery_trend$Outside_share[3]), ".")

# Antenatal starts: assigned pregnancy year 2025/26; all recorded delivery dates
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
cluster_activity$pathway_table <- gt::gt(dc_counts(pathways, c("Starts", outcome_columns)), groupname_col = "Level") |>
  gt::tab_header(title = "Recorded outcomes of cluster antenatal bookings", subtitle = "Bookings in 2025/26") |>
  gt::cols_label(Organisation = "Booking organisation", Starts = "Antenatal bookings",
    Within = "Delivery at cluster trust", Outside = "Other trust or home birth",
    No_delivery = "No recorded delivery", Unknown = "Delivery location unknown") |>
  gt::fmt_number(columns = c("Starts", "Within", "Outside", "No_delivery", "Unknown"), decimals = 0)
cluster_activity$pathway_text <- paste0(
  count_text(nrow(starts)), " pregnancies were assigned to 2025/26 with a cluster booking provider; ",
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

make_pathway_sankey <- function(row, count_label = "Antenatal bookings", period = "2025/26") {
  organisation <- row$Organisation[[1]]
  # Provider code/name lookup
  code <- if ("Code" %in% names(row)) row$Code[[1]] else {
    labels <- c(trust_labels, icb_labels, Cluster = cluster_label)
    matched <- match(organisation, unname(labels))
    if (is.na(matched)) organisation else names(labels)[matched]
  }
  counts <- as.numeric(unlist(row[outcome_columns], use.names = FALSE))
  total <- dc_count(row$Starts[[1]])
  counts <- dc_count(counts)
  shares <- if (total > 0) counts / total else rep(NA_real_, 4)
  outcome_text <- paste0(outcome_labels, " (", organisation, ")")
  labels <- paste0(outcome_text, "<br>", count_text(counts), " (",
                   percent_text(shares), ")")
  chart <- plotly::plot_ly(type = "sankey", height = 170,
    arrangement = "fixed", orientation = "h",
    node = list(
      label = c(count_text(total), percent_text(shares)),
      color = c(unname(identity_colours(code)), outcome_colours),
      x = c(0.01, rep(0.99, 4)),
      y = sankey_node_y(c(0.01, rep(0.99, 4)), rep(0L, 4), 1:4, counts, 130, 18),
      pad = 18, thickness = 18,
      customdata = paste0(c(organisation, outcome_text), "<br>", count_label, ": ", c(total, counts)),
      hovertemplate = "%{customdata}<extra></extra>"
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
    return(htmltools::div(paste0("No ", tolower(count_label), " recorded for this organisation in ", period, ".")))
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
# Show the six named trusts; include any additional cluster booking trusts observed.
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
    "Trust, ICB and cluster totals group the same antenatal bookings." else
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

  raw_pathway_trends <- pathway_trends
  pathway_trends <- dc_cells(pathway_trends, c("Numerator", "Denominator"),
    list(Share = c("Numerator", "Denominator"))) |>
    dplyr::group_by(Key, Outcome) |> dplyr::arrange(Year_start, .by_group = TRUE) |>
    dplyr::mutate(YoY_change = dc_divide(Share, dplyr::lag(Share)) - 1,
      Base_change = dc_divide(Share, dplyr::first(Share)) - 1) |> dplyr::ungroup()

  # Charts: show each outcome share and its change from the first year.
  make_delivery_trend_chart <- function(key, measure = c("share", "base_change"), compact = FALSE) {
    measure <- match.arg(measure)
    show_change <- measure == "base_change"
    data <- pathway_trends[pathway_trends$Key == key, , drop = FALSE]
    chart <- plotly::plot_ly(height = if (compact) 300 else 480)
    for (i in seq_along(trend_labels)) {
      line <- data[data$Outcome == trend_labels[i], , drop = FALSE]
      hover <- paste0(line$Year, "<br>", line$Outcome,
        "<br>Pregnancies with this outcome: ", scales::comma(line$Numerator, accuracy = 1),
        "<br>All antenatal bookings: ", scales::comma(line$Denominator, accuracy = 1),
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
      xaxis = list(title = if (compact) "Pregnancy FY" else "Assigned pregnancy financial year", tickmode = "array",
        tickvals = pathway_years, ticktext = c("2023/24", "2024/25", "2025/26")),
      yaxis = if (show_change) {
        list(title = "% change in share from 2023/24", tickformat = ".0%",
             rangemode = "tozero", zeroline = TRUE, zerolinecolor = "#727272", zerolinewidth = 1)
      } else list(title = "% of antenatal bookings", range = c(0, 1), tickformat = ".0%"),
      showlegend = !compact,
      legend = list(orientation = "h", x = 0, y = -0.22),
      margin = if (compact) list(l = 65, r = 12, t = 15, b = 55) else
        list(l = 75, r = 20, t = 25, b = 125),
      font = list(family = "Arial", size = if (compact) 11 else 12),
      paper_bgcolor = "white", plot_bgcolor = "white") |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
    empty_years <- unique(data$Year[data$Denominator == 0])
    chart_notes <- if (length(empty_years)) paste0(
      "No antenatal bookings recorded: ", paste(empty_years, collapse = ", "), "; percentages unavailable.")
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
  delivery_over_time$data <- raw_pathway_trends
  delivery_over_time$display_data <- pathway_trends
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
                   subtitle = "Assigned pregnancy financial years 2023/24 to 2025/26") |>
    gt::cols_label(Year = "Pregnancy FY", Outcome = "Outcome",
      Numerator = "Pregnancies with this outcome", Denominator = "All antenatal bookings",
      Share = "% of antenatal bookings", YoY_change = "% change in share year on year",
      Base_change = "% change in share from 2023/24") |>
    gt::fmt_number(columns = c("Numerator", "Denominator"), decimals = 0, use_seps = TRUE) |>
    gt::fmt_percent(columns = c("Share", "YoY_change", "Base_change"), decimals = 1) |>
    gt::sub_missing(missing_text = "n.a.") |>
    gt::tab_source_note(source_note = paste0(
      "Share = pregnancies with this outcome / all antenatal bookings in the same ", booking_group_text, " and assigned pregnancy year. ",
      "Changes are relative changes in that share: (current share / reference share - 1) x 100, ",
      "not percentage-point differences. n.a. = no antenatal bookings, no prior year, or a zero/unavailable reference share. ",
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
                   subtitle = "Relative change from assigned pregnancy financial year 2023/24") |>
    gt::tab_stubhead(label = if ("Trust" %in% group_levels) "Booking trust / ICB / cluster" else
                      "Booking ICB / cluster") |>
    gt::cols_label(.list = stats::setNames(
      as.list(outcome_short_labels[change_column_key$Outcome_index]), change_columns)) |>
    gt::fmt_percent(columns = dplyr::all_of(change_columns), decimals = 1) |>
    gt::sub_missing(missing_text = "n.a.") |>
    gt::tab_source_note(source_note = paste0(
      "Every cell is (the year's outcome share / its 2023/24 share - 1) x 100. ",
      "Outcome share = pregnancies with this outcome / all antenatal bookings in that ", booking_group_text, " and year. ",
      "Positive values indicate a higher share; negative values indicate a lower share. ",
      "2023/24 is 0% where its share is positive; n.a. means no antenatal bookings or a zero/unavailable baseline share. ",
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

# Save the exploration outputs; QA runs separately through R/QA.R.
save_report_rds(
  list(icb_sankey = icb_sankey, trust_sankey = trust_sankey, by_year = by_year,
       full_patient_flows = full_patient_flows,
       country_coverage = report_country_coverage(report_episodes),
       maternal_age_completeness = maternal_age_completeness,
       maternal_age_completeness_table = maternal_age_completeness_display,
       booking_age_fallback_counts = booking_age_fallback_counts,
       booking_age_fallback = booking_age_fallback,
       record_completeness = record_completeness,
       record_completeness_counts = record_completeness_counts,
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
             "resident_icb", "la_of_mother", "age_at_birth_mother", "age_at_booking_mother", "birth_provider", "home_birth")
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
  age_bands <- unique(c(age_bands, delivery_episodes$Age_band))

  # Shared colours across geography charts; frequent organisations first
  colour_priority <- names(sort(table(c(delivery_episodes$LA, delivery_episodes$Destination)), decreasing = TRUE))
  geography_colours <- identity_colours(c(colour_priority, "All other feeder LAs", "All other trusts", "Unknown LA"),
    priority = colour_priority)

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
  matrix_table <- dc_counts(matrix_wide, c(matrix_columns, "Total")) |>
    table_style("2025/26 resident LA to delivery destination", rowname = "Resident_LA") |>
    gt::cols_label(LA = "LA code", .list = as.list(matrix_labels)) |>
    gt::fmt_number(columns = dplyr::all_of(c(matrix_columns, "Total")), decimals = 0) |>
    gt::tab_options(table.font.size = gt::px(11))
  matrix_values_table <- dc_cells(destination_matrix, c("N", "Origin_total"), list(Share = c("N", "Origin_total"))) |>
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
  origins <- sankey_category_order(cluster_las, sankey_links$Origin)
  origins <- origins[origins %in% sankey_links$Origin]
  origin_labels <- la_names$Resident_LA[match(origins, la_names$LA)]
  origin_labels[origins %in% c("Unknown LA", "All other feeder LAs")] <- origins[origins %in% c("Unknown LA", "All other feeder LAs")]
  destinations <- sankey_category_order(trust_target_order, sankey_links$Destination_group)
  destinations <- destinations[destinations %in% sankey_links$Destination_group]
  destination_labels <- destination_names$Delivery_destination[match(destinations, destination_names$Destination_group)]
  sankey_links <- sankey_links |>
    dplyr::group_by(Origin) |>
    dplyr::mutate(Origin_total = sum(N), Share = N / Origin_total) |>
    dplyr::ungroup()
  # Independent node totals use raw links, not a sum of rounded ribbons.
  node_totals <- dc_count(c(
    vapply(origins, function(z) sum(sankey_links$N[sankey_links$Origin == z]), numeric(1)),
    vapply(destinations, function(z) sum(sankey_links$N[sankey_links$Destination_group == z]), numeric(1))))
  raw_sankey_links <- sankey_links
  sankey_links <- dc_cells(sankey_links, c("N", "Origin_total"), list(Share = c("N", "Origin_total")))
  palette <- unname(geography_colours[origins])
  sankey_height <- max(340, max(length(origins), length(destinations)) * 18)
  node_x <- c(rep(0.01, length(origins)), rep(0.99, length(destinations)))
  sankey_chart <- plotly::plot_ly(type = "sankey", height = sankey_height,
    arrangement = "fixed", node = list(label = short_chart_name(c(origin_labels, destination_labels)), pad = 9, thickness = 12,
      x = node_x,
      y = sankey_node_y(node_x, match(sankey_links$Origin, origins) - 1L,
        length(origins) + match(sankey_links$Destination_group, destinations) - 1L,
        sankey_links$N, sankey_height - 20, 9),
      color = c(palette, unname(geography_colours[destinations])),
      customdata = paste0(c(origin_labels, destination_labels), "<br>Deliveries: ", node_totals),
      hovertemplate = "%{customdata}<extra></extra>"),
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
  origin_colours <- geography_colours[all_origins]

  # Short chart labels fit the compact panels; hover still gives the complete name.
  small_sankey <- function(links, origin_codes, destination_codes, percentage_label) {
    origin_order <- sankey_category_order(cluster_las, names(all_origin_labels))
    destination_order <- sankey_category_order(trust_target_order, names(all_destination_labels))
    origin_codes <- origin_order[origin_order %in% origin_codes]
    destination_codes <- destination_order[destination_order %in% destination_codes]
    source_names <- unname(all_origin_labels[origin_codes])
    target_names <- unname(all_destination_labels[destination_codes])
    source_counts <- vapply(origin_codes, function(code) sum(links$N[links$Origin == code]), numeric(1))
    target_counts <- vapply(destination_codes, function(code) sum(links$N[links$Destination_group == code]), numeric(1))
    total <- dc_count(sum(links$N))
    source_counts <- dc_count(source_counts)
    target_counts <- dc_count(target_counts)
    links$N <- dc_count(links$N)
    chart_height <- max(220, max(length(origin_codes), length(destination_codes)) * 21 + 40)
    node_x <- c(rep(0.02, length(origin_codes)), rep(0.98, length(destination_codes)))
    node_names <- c(source_names, target_names)
    node_counts <- c(source_counts, target_counts)
    plotly::plot_ly(type = "sankey", height = chart_height, arrangement = "fixed",
      node = list(
        label = short_chart_name(node_names),
        pad = 14, thickness = 12,
        x = node_x,
        y = sankey_node_y(node_x, match(links$Origin, origin_codes) - 1L,
          length(origin_codes) + match(links$Destination_group, destination_codes) - 1L,
          links$N, chart_height - 20, 14),
        color = c(unname(origin_colours[origin_codes]), unname(geography_colours[destination_codes])),
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
    data <- dc_geography_trend(data, c("Key", "LA"))
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
    if (is.null(colours)) colours <- geography_colours[groups]
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
    raw_grouped <- grouped
    grouped <- dc_geography_trend(grouped, "Origin_group")
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
      raw_plot_ages <- plot_ages
      plot_ages <- dc_cells(plot_ages, c("N", "LA_total"), list(Age_share = c("N", "LA_total")))
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
    origin_table <- dc_cells(origin, c("N", "Organisation_total"), list(Share = c("N", "Organisation_total"))) |>
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
    age_table <- table_style(dc_counts(age_wide, c(age_bands, "Total")), "2025/26 hospital delivery counts: LA by age band", rowname = "Resident_LA") |>
      gt::cols_label(LA = "LA code") |>
      gt::fmt_number(columns = dplyr::all_of(c(age_bands, "Total")), decimals = 0)
    list(origin_chart = origin_chart, age_chart = age_chart, table = origin_table, age_table = age_table,
      count_chart = line_chart(grouped, "N", "Deliveries", units = "count"),
      stability_chart = line_chart(grouped, "Share", "% of organisation deliveries from LA"),
      change_chart = line_chart(grouped, "Count_change", "% change in deliveries from 2023/24"),
      share_change_chart = line_chart(grouped, "Share_change_pp", "Change in LA contribution (percentage points)", units = "pp"),
      count_table = wide_trend_table(trend, "All resident LA contributions, 2023/24-2025/26"),
      change_table = wide_trend_table(trend, "Change in all resident LA contributions from 2023/24", changes = TRUE),
      trend_groups = raw_grouped, display_trend_groups = grouped, visible_las = visible_las,
      age_mix = if (nrow(origin)) raw_plot_ages else NULL,
      display_age_mix = if (nrow(origin)) plot_ages else NULL)
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
  trend_colours <- geography_colours[trend_key$Origin_group]
  trend_specs <- list(
    count = list(field = "N", title = "Deliveries", units = "count"),
    contribution = list(field = "Share", title = "% from LA", units = "percent"),
    count_change = list(field = "Count_change", title = "% change in deliveries", units = "percent"),
    share_change = list(field = "Share_change_pp", title = "Change in LA share (pp)", units = "pp"))
  trend_grid <- function(rows, spec) {
    grid_data <- lapply(rows$Key, function(key) organisation_plots[[key]]$display_trend_groups)
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
    sankey_chart = htmltools::div(style = "max-width: 760px;", sankey_chart),
    sankey_links = raw_sankey_links, display_sankey_links = sankey_links,
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
save_report_rds(delivery_geography, ".delivery_geography.rds")

# 3. COMBINED PROJECTION MODEL ===========================================
# Read the steps in order. Local shares retain LA(R) and Age (SY).
# National conversion factors vary by age and apply across all LAs.

# 3.1 Pool the supplied cohort and retain the wider resident denominator --
# Use supplied cohort rows aged 13--54; dates remain descriptive fields.
# All resident delivery and antenatal-only totals come from the expanded LA file.
# Each LA uses its own age-specific rate; there is no borrowed cluster rate.
projection_resident_las <- function(episodes, include_las = cluster_las) {
  parameters <- read_analysis_parameters()
  ages <- seq(parameters[["minimum_maternal_age"]], parameters[["maximum_maternal_age"]])
  feeds_cluster <- projection_delivery_destination(episodes) %in% cluster_trusts |
    projection_booking_destination(episodes) %in% cluster_trusts
  las <- c(include_las, episodes$LA[feeds_cluster & episodes$Age %in% ages])
  sort(unique(las[grepl("^[ENSW][0-9]{8}$", las)]))
}

build_allocation_tables <- function(cohort, resident_totals,
    calibration_years = c("2023/24", "2024/25", "2025/26"),
    cluster_resident_las = cluster_las) {
  episodes <- prepare_report_cohort(cohort)
  parameters <- read_analysis_parameters()
  model_ages <- seq(parameters[["minimum_maternal_age"]], parameters[["maximum_maternal_age"]])
  required_las <- projection_resident_las(episodes, cluster_resident_las)
  usable <- function(x) !x %in% c("Unknown LA", "L99999999", "") & !is.na(x)
  unknown_groups <- prepare_la_records(resident_totals) |>
    dplyr::filter(is.na(Pathway_group), Pregnancy_FY %in% calibration_years) |>
    dplyr::count(LA = report_la_code(la_of_mother), Age, name = "Resident_unclassified_rows")
  residents <- prepare_la_deliveries(resident_totals, missing_count_zero = TRUE) |>
    dplyr::filter(as.character(birth_yr) %in% calibration_years) |>
    dplyr::transmute(LA = report_la_code(la_of_mother),
      Age = report_age(age_at_birth_mother), MSDS_resident_count = report_number(count)) |>
    dplyr::filter(LA %in% required_las, Age %in% model_ages) |>
    dplyr::group_by(LA, Age) |>
    dplyr::summarise(MSDS_resident_count = sum(MSDS_resident_count), .groups = "drop")
  residents <- tidyr::expand_grid(LA = required_las, Age = model_ages) |>
    dplyr::left_join(residents, by = c("LA", "Age")) |>
    dplyr::mutate(MSDS_resident_count = dplyr::coalesce(MSDS_resident_count, 0))
  antenatal <- prepare_la_antenatal_only(resident_totals, missing_count_zero = TRUE) |>
    dplyr::filter(as.character(edd_yr) %in% calibration_years) |>
    dplyr::transmute(LA = report_la_code(la_of_mother),
      Age = report_age(age_at_booking_mother), Resident_antenatal_only = report_number(count)) |>
    dplyr::filter(LA %in% required_las, Age %in% model_ages) |>
    dplyr::group_by(LA, Age) |>
    dplyr::summarise(Resident_antenatal_only = sum(Resident_antenatal_only), .groups = "drop")
  episodes$Delivery_destination <- projection_delivery_destination(episodes)
  episodes$Booking_destination <- projection_booking_destination(episodes)
  counted <- episodes |> dplyr::filter(LA %in% required_las, Age %in% model_ages)
  cohort_counts <- counted |> dplyr::group_by(LA, Age) |>
    dplyr::summarise(Cohort_pregnancies = dplyr::n(),
      Cohort_recorded_births = sum(Birth_recorded),
      Cohort_no_recorded_delivery = sum(!Birth_recorded), .groups = "drop")
  calibration <- dplyr::full_join(residents, cohort_counts, by = c("LA", "Age")) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Cohort_"), ~ dplyr::coalesce(.x, 0))) |>
    dplyr::left_join(antenatal, by = c("LA", "Age")) |>
    dplyr::left_join(unknown_groups, by = c("LA", "Age")) |>
    dplyr::mutate(
    Resident_unclassified_rows = dplyr::coalesce(Resident_unclassified_rows, 0L),
    Resident_antenatal_only = dplyr::coalesce(Resident_antenatal_only, 0),
    Resident_pregnancies = MSDS_resident_count + Resident_antenatal_only,
    Bookings_uplift_modifier = dplyr::if_else(Resident_unclassified_rows > 0, NA_real_,
      dplyr::if_else(MSDS_resident_count > 0, Resident_pregnancies / MSDS_resident_count, 0)),
    Bookings_uplift_source = dplyr::case_when(
      Resident_unclassified_rows > 0 ~ "Unknown/missing resident pathway group",
      MSDS_resident_count > 0 ~ "Own LA and age: expanded resident extract",
      TRUE ~ "Zero: no resident deliveries at this LA and age"),
    Resident_birth_gap = MSDS_resident_count - Cohort_recorded_births,
    Shared_maternity_denominator = dplyr::if_else(MSDS_resident_count == 0,
      0, MSDS_resident_count * Bookings_uplift_modifier),
    Estimated_no_recorded_delivery = Shared_maternity_denominator - MSDS_resident_count,
    Booking_Denominator_Factor = Shared_maternity_denominator - Cohort_pregnancies,
    Calibration_issue = dplyr::case_when(
      Resident_unclassified_rows > 0 ~ "Unknown/missing resident pathway group",
      is.na(MSDS_resident_count) ~ "No matching resident birth count",
      Resident_birth_gap < -1e-8 ~ "Cohort births exceed resident births: check source periods/processing",
      Booking_Denominator_Factor < -1e-8 ~ "Cohort pregnancies exceed estimated resident total",
      Estimated_no_recorded_delivery < Cohort_no_recorded_delivery - 1e-8 ~
        "LA antenatal-only count below observed cohort no-delivery count",
      MSDS_resident_count <= 0 ~ "No positive resident birth denominator",
      TRUE ~ ""),
    Allocation_available = Calibration_issue == "",
    Birth_gap_review = dplyr::case_when(is.na(Resident_birth_gap) ~ "Missing resident count",
      Resident_birth_gap < -1e-8 ~ "Cohort deliveries exceed resident total",
      Resident_birth_gap > 1e-8 ~ "Resident deliveries outside the linked cohort",
      TRUE ~ "Counts match"))

  # Known destinations retain their observed counts. The residual birth count
  # stays separate from no-recorded-delivery and no-booking records.
  delivery_observed <- counted |> dplyr::filter(Birth_recorded) |>
    dplyr::count(LA, Age, Destination = Delivery_destination, name = "Count")
  delivery <- dplyr::bind_rows(delivery_observed,
    calibration |> dplyr::filter(Allocation_available, Resident_birth_gap > 1e-8) |>
      dplyr::transmute(LA, Age, Destination = "Not_represented", Count = Resident_birth_gap),
    calibration |> dplyr::filter(Allocation_available, Estimated_no_recorded_delivery > 0) |>
      dplyr::transmute(LA, Age, Destination = "No_recorded_delivery", Count = Estimated_no_recorded_delivery)) |>
    dplyr::left_join(calibration |> dplyr::select(LA, Age, MSDS_resident_count,
      Shared_maternity_denominator, Allocation_available), by = c("LA", "Age")) |>
    dplyr::filter(Allocation_available) |>
    dplyr::mutate(Proportion = Count / Shared_maternity_denominator)
  booking_observed <- counted |>
    dplyr::count(LA, Age, Destination = Booking_destination, name = "Count")
  booking <- dplyr::bind_rows(booking_observed,
    calibration |> dplyr::filter(Allocation_available, Booking_Denominator_Factor > 1e-8) |>
      dplyr::transmute(LA, Age, Destination = "Booking_Denominator_Factor", Count = Booking_Denominator_Factor)) |>
    dplyr::left_join(calibration |> dplyr::select(LA, Age, Shared_maternity_denominator,
      Cohort_pregnancies, Allocation_available), by = c("LA", "Age")) |>
    dplyr::filter(Allocation_available) |>
    dplyr::mutate(Proportion = Count / Shared_maternity_denominator)
  list(residents = residents, delivery = delivery, booking = booking,
    delivery_observed = delivery_observed, booking_observed = booking_observed,
    calibration = calibration, cohort_counts = cohort_counts,
    cluster_resident_las = cluster_resident_las, years = calibration_years, ages = model_ages,
    period_review = episodes |> dplyr::count(Year, Birth_FY, Due_FY, Pregnancy_year_source,
      Birth_recorded, name = "Pregnancies"),
    exclusions = episodes |> dplyr::mutate(LA_usable = usable(LA), Age_usable = Age %in% model_ages) |>
      dplyr::count(LA_usable, Age_usable, Birth_recorded, name = "Pregnancies"))
}

check_projection_la_sources <- function(cohort, resident_totals, ons_births,
    include_las = character(), calibration_years = c("2023/24", "2024/25", "2025/26"),
    start_year = 2025L, end_year = NULL) {
  # Only cluster LAs and areas contributing cohort activity need a projection.
  usable_codes <- function(x) {
    codes <- report_code(x, "Unknown LA")
    sort(unique(codes[!codes %in% c("Unknown LA", "L99999999", "") & !is.na(codes)]))
  }
  cohort_las <- usable_codes(report_la_code(cohort$la_of_mother))
  resident_deliveries <- prepare_la_deliveries(resident_totals)
  resident_las <- usable_codes(report_la_code(resident_deliveries$la_of_mother[
    as.character(resident_deliveries$birth_yr) %in% calibration_years]))
  ons_las <- usable_codes(ons_births$area_code[ons_births$age_group != "All ages" &
    ons_births$year >= start_year &
    ons_births$year <= if (is.null(end_year)) Inf else end_year])
  required_las <- projection_resident_las(prepare_report_cohort(cohort), include_las)
  data.frame(LA = required_las) |>
    dplyr::mutate(In_cohort = LA %in% cohort_las,
      In_resident_counts = LA %in% resident_las, In_ONS = LA %in% ons_las,
      Issue = dplyr::case_when(
        !In_resident_counts & !In_ONS ~ "Missing resident birth counts and ONS births",
        !In_resident_counts ~ "Missing resident birth counts",
        !In_ONS ~ "Missing ONS births", TRUE ~ ""))
}

build_projection_master <- function(cohort, resident_totals, ons_births, registration_counts,
    baseline_fy = "2025/26", reference_year = as.integer(substr(baseline_fy, 1, 4)), end_year = NULL,
    include_las = character(), calibration_years = c("2023/24", "2024/25", "2025/26"),
    start_year = reference_year, private_nhs_counts = read_private_nhs_counts()) {
  # MODEL_INPUT: MSDS_RESIDENTS, MSDS_DELIVERIES, MSDS_BOOKINGS
  allocation <- build_allocation_tables(cohort, resident_totals, calibration_years)
  residents <- allocation$residents
  selected_las <- unique(c(include_las, residents$LA, allocation$cohort_counts$LA))
  la_source_matching <- check_projection_la_sources(cohort, resident_totals, ons_births,
    include_las, calibration_years, start_year, end_year)

  # 3.2 ONS births (L): retain LA(R), source age and projection year --------
  # MODEL_INPUT: ONS_BIRTHS_L
  ons <- ons_births |>
    dplyr::filter(area_code %in% selected_las, age_group != "All ages",
      year >= start_year, year <= if (is.null(end_year)) Inf else end_year) |>
    dplyr::transmute(LA = area_code, LA_name = as.character(area_name),
      ONS_age = as.integer(age_group), Year = year, ONS_live_births = projected_births)

  # 3.3 Split combined ONS ages using the local maternity age mix ----------
  # MODEL_INPUT: MSDS_RESIDENTS (pooled 2023/24--2025/26 counts)
  # Ages 16--43: weight 1 (their own ONS count remains unchanged).
  # At the extremes: exact-age MSDS count / matching combined-group count.
  # Split only across the configured projection ages (13--54).
  model_ages <- allocation$ages
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
  age_split <- dplyr::bind_rows(age_split, unsplit)
  master <- merge(ons, age_split, by = c("LA", "ONS_age"), all.x = TRUE, sort = FALSE)
  master$Exact_age_live_births <- master$ONS_live_births * master$Age_weight

  # 3.4 Stillbirth uplift, then multiplicity conversion -------------------
  # MODEL_INPUT: ONS_STILLBIRTH_RATIO, ONS_MULTIPLICITY
  # Pool Table 5 counts across 2023--2025 at each published age, then divide.
  # Source age <14 applies equally below 14; source 50+ equally to ages 50+.
  ons_modifications <- prepare_projection_age_factors(registration_counts, model_ages)$modifications
  master <- dplyr::left_join(master, ons_modifications,
    by = c("Age", "ONS_age" = "ONS_source_age"))
  no_split <- is.na(master$Age)
  # 3.5 Private-NHS modifier: pooled England Table 4 setting proxy, by age band.
  # Apply once before the pathway adjustment; keep observed allocation denominators raw.
  private_nhs <- prepare_private_nhs_factors(private_nhs_counts, model_ages)
  master <- dplyr::left_join(master, private_nhs$modifications, by = "Age")
  # Fifth modifier: each LA's own expanded resident extract, at the same age.
  calibration <- allocation$calibration
  master <- dplyr::left_join(master, calibration |> dplyr::select(-MSDS_resident_count),
    by = c("LA", "Age"))
  no_local_cell <- is.na(master$Bookings_uplift_source)
  master$Bookings_uplift_modifier[no_local_cell] <- 0
  master$Bookings_uplift_source[no_local_cell] <- "Zero: no resident deliveries at this LA and age"
  master$Allocation_available[no_local_cell] <- FALSE
  master$Calibration_issue[no_local_cell] <- "No matching LA/age resident calibration"
  master$Maternity_modifier <- master$Stillbirth_factor * master$Multiplicity_factor *
    master$Private_NHS_factor * master$Bookings_uplift_modifier
  master$Adjusted_births_ls <- master$Exact_age_live_births * master$Stillbirth_factor
  master$Maternities_before_private <- master$Adjusted_births_ls * master$Multiplicity_factor
  master$Maternities_after_private <- master$Maternities_before_private * master$Private_NHS_factor
  master$Adjusted_maternities <- master$Exact_age_live_births * master$Maternity_modifier
  # A zero source allocation remains zero even when its uplift is unavailable.
  master$Adjusted_maternities[master$Exact_age_live_births == 0] <- 0
  master$Adjusted_maternities[master$Bookings_uplift_modifier == 0] <- 0
  master$Projection_available <- is.finite(master$Adjusted_maternities)
  master$Delivery_projection_base <- master$Adjusted_maternities
  master$Booking_projection_base <- master$Adjusted_maternities

  # 3.5 Independently allocate the same adjusted maternity total -----------
  add_allocations <- function(master, rates, prefix) {
    special <- if (prefix == "Delivery") c("Home", "Unknown_location", "No_recorded_delivery", "Not_represented") else
      c("No_recorded_booking", "Booking_provider_unknown", "Booking_Denominator_Factor")
    destinations <- sort(unique(c(as.character(rates$Destination), special, "Unallocated")))
    labels <- make.names(destinations)
    totals <- unique(rates[c("LA", "Age", "Shared_maternity_denominator")])
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
      master[[count_name]] <- ifelse(master[[share_name]] == 0, 0,
        master$Adjusted_maternities * master[[share_name]])
    }
    attr(master, paste0(prefix, "_destinations")) <- stats::setNames(destinations, labels)
    master
  }
  master <- add_allocations(master, allocation$delivery, "Delivery")
  delivery_names <- attr(master, "Delivery_destinations")
  master <- add_allocations(master, allocation$booking, "Booking")
  booking_names <- attr(master, "Booking_destinations")
  master$Age_treatment <- ifelse(no_split, "Unsplit: no local age distribution",
    ifelse(master$ONS_age %in% c(15, 44), "Pooled local MSDS age split", "Exact age"))
  master$Baseline_FY <- baseline_fy
  master$ONS_reference_year <- reference_year
  master$Source_cell_id <- paste(master$LA, master$ONS_age, master$Year, sep = "/")
  master <- master[order(master$LA, master$Age, master$Year), ]
  master$Master_row_id <- seq_len(nrow(master))
  # Put the three review columns beside the LA(R), Age (SY) and year keys.
  # Exact_age_live_births is births (L) after the age split ONLY.
  review_columns <- c("LA", "Age", "Year", "Exact_age_live_births",
    "Delivery_projection_base", "Booking_projection_base")
  master <- master[c(review_columns, setdiff(names(master), review_columns))]
  attr(master, "Delivery_destinations") <- delivery_names
  attr(master, "Booking_destinations") <- booking_names
  attr(master, "allocation_tables") <- allocation
  attr(master, "resident_counts") <- residents
  attr(master, "unmatched_las") <- setdiff(selected_las, unique(ons$LA))
  attr(master, "la_source_matching") <- la_source_matching
  # Cohort calibration uses all supplied rows, without a financial-year filter.
  # The conversion inventory must cover that same cohort, including older rows.
  attr(master, "geography_changes") <- report_applied_geography(
    cohort$la_of_mother,
    resident_totals$la_of_mother[
      report_pregnancy_fy(resident_totals$birth_yr, resident_totals$edd_yr) %in% calibration_years &
      report_la_code(resident_totals$la_of_mother) %in% residents$LA])
  attr(master, "registration_source") <- registration_counts
  attr(master, "ons_modifications") <- ons_modifications
  attr(master, "private_nhs") <- private_nhs
  attr(master, "calculation_version") <- "ons-shared-age-calculations-v24"
  master
}

# 3.6 Read completed columns for charts; no rates are recalculated here ----
projection_destination_rows <- function(master, type = "Delivery") {
  destinations <- attr(master, paste0(type, "_destinations"))
  rates <- attr(master, "allocation_tables")[[tolower(type)]]
  rows <- lapply(names(destinations), function(code) {
    result <- master[c("LA", "LA_name", "ONS_age", "Age", "Year", "Source_cell_id",
      "ONS_live_births", "Exact_age_live_births", "Age_weight", "Stillbirth_ratio",
      "Stillbirth_factor", "Multiplicity_factor", "Private_NHS_age_band", "Private_NHS_rate", "Private_NHS_factor",
      "Maternities_before_private", "Maternities_after_private", "Bookings_uplift_modifier", "Bookings_uplift_source", "Maternity_modifier", "Adjusted_maternities",
      "MSDS_resident_count", "Age_treatment", "Baseline_FY", "ONS_reference_year")]
    result$Allocation_volume <- master[[paste0(type, "_projection_base")]]
    result$Allocation_denominator <- master[[paste0(type, "_denominator")]]
    result$Delivery_destination <- destinations[[code]]
    result$Destination_proportion <- master[[paste0(type, "_proportion_", code)]]
    result$Projected_maternities <- master[[paste0(type, "_count_", code)]]
    result$Allocation_available <- destinations[[code]] != "Unallocated"
    result$Allocation_status <- if (grepl("^R[A-Z0-9]{2}$", destinations[[code]])) {
      if (type == "Booking") "Recorded booking trust" else "Recorded delivery trust"
    } else projection_destination_label(destinations[[code]])
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
      Column == "Age" ~ "Age (SY); NA only where a boundary group cannot be split.",
      Column == "ONS_age" ~ "ONS source row: 15 means <=15; 44 means >=44.",
      Column == "Year" ~ "ONS projection year.",
      Column == "ONS_live_births" ~ "Source births (L); repeated across ages within a combined group.",
      Column == "Age_weight" ~ "Pooled exact-age recorded resident deliveries / matching combined-age total; 1 for ages 16--43.",
      Column == "MSDS_resident_count" ~ "LA extract birth only + antenatal+delivery counts, by assigned pregnancy FY and resolved birth/booking age, pooled over calibration years for every LA. Missing counts are zero in the model.",
      Column == "Exact_age_live_births" ~ "ONS births (L) after the age split only: ONS_live_births * Age_weight; before stillbirth, multiplicity, Private-NHS or pathway adjustments.",
      Column %in% c("Pooled_live_births", "Pooled_stillbirths", "Pooled_total_births", "Pooled_maternities") ~ "National Table 5 count summed across 2023--2025 at the matching registration age; used to calculate the factors, not a projected count.",
      Column == "Calculation_basis" ~ "Pool national counts across 2023--2025 at each published age, then divide.",
      Column == "Stillbirth_ratio" ~ "Pooled Table 5 stillbirths / pooled live births, 2023--2025.",
      Column == "Stillbirth_factor" ~ "1 + Stillbirth_ratio; births (L) to births (L+S).",
      Column == "Multiplicity_factor" ~ "Pooled Table 5 maternities / pooled births (L+S), 2023--2025.",
      Column == "National_conversion_factor" ~ "Stillbirth_factor * Multiplicity_factor; the national baby-to-maternity conversion before the Private-NHS and maternity pathway adjustments.",
      Column == "Private_NHS_age_band" ~ "England Table 4 band: under 20, five-year bands through 40-44, then 45+; 2025 older groups combined to match 2023-2024.",
      Column == "Private_NHS_rate" ~ "Pooled 2023-2025 England-resident non-NHS-establishment maternities / all maternities in the same age band; delivery-setting proxy for private care.",
      Column == "Private_NHS_factor" ~ "Private-NHS modifier = 1 - Private_NHS_rate. Applied once after multiplicity and before the pathway factor; same assumed reduction for bookings and deliveries.",
      Column == "Maternities_before_private" ~ "Exact_age_live_births * Stillbirth_factor * Multiplicity_factor; pregnancies reaching delivery before the Private-NHS modifier.",
      Column == "Maternities_after_private" ~ "Maternities_before_private * Private_NHS_factor; the delivery-equivalent projection before pathway uplift.",
      Column == "Maternity_modifier" ~ "Stillbirth_factor * Multiplicity_factor * Private_NHS_factor * Bookings_uplift_modifier; apply after the local age split.",
      Column == "Adjusted_births_ls" ~ "Exact_age_live_births * Stillbirth_factor.",
      Column == "Bookings_uplift_modifier" ~ "(LA deliveries + LA antenatal-only pregnancies) / LA deliveries at the same age over the calibration years. group defines deliveries (birth only + antenatal+delivery) and antenatal only. Both use birth FY first, otherwise EDD, and birth age first, otherwise booking age when birth age is NA. Zero if the delivery denominator is zero or missing; no borrowed rate.",
      Column == "Bookings_uplift_source" ~ "Own LA/age evidence from the expanded resident extract, or the agreed zero-denominator treatment.",
      Column == "Resident_antenatal_only" ~ "LA extract antenatal only group in the assigned pregnancy-FY calibration period, grouped by resolved birth/booking age. Missing counts are zero in the model.",
      Column == "Resident_unclassified_rows" ~ "Source aggregate rows with unknown/missing pathway group in this LA/age and calibration period; any such row blocks allocation.",
      Column == "Resident_pregnancies" ~ "Resident deliveries plus antenatal-only pregnancies from the LA extract; each pregnancy counted once.",
      Column == "Cohort_pregnancies" ~ "All supplied pregnancy rows with this usable resident LA and maternal age; no additional date filter.",
      Column == "Cohort_recorded_births" ~ "Supplied pregnancies with birth_flag = 1: live birth or stillbirth, counted once per pregnancy, including home deliveries and unknown delivery locations.",
      Column == "Cohort_no_recorded_delivery" ~ "Supplied cohort rows without a recorded delivery, used to reconcile provider allocations against the independent LA antenatal-only total.",
      Column == "Resident_birth_gap" ~ "Recorded resident deliveries minus deliveries in the supplied cohort; positive differences remain a separate allocation category.",
      Column == "Birth_gap_review" ~ "Distinguishes resident deliveries outside the linked cohort from missing totals or cohort counts exceeding them.",
      Column == "Calibration_issue" ~ "Reason this LA/age cannot support an allocation, or blank when the calibration is usable.",
      Column == "Allocation_available" ~ "Whether the supplied resident count and selected uplift support nonnegative destination allocations.",
      Column == "Booking_Denominator_Factor" ~ "Estimated resident pregnancy denominator minus all supplied cohort pregnancies; a residual allocation group, not observed bookings.",
      Column == "Shared_maternity_denominator" ~ "Recorded resident deliveries * Bookings_uplift_modifier; common denominator for both allocation views.",
      Column == "Estimated_no_recorded_delivery" ~ "Shared maternity denominator minus resident deliveries, using the LA extract antenatal-only count where the delivery denominator is positive.",
      Column == "Projection_available" ~ "False where a positive ONS count has no same-age uplift evidence; the projection remains missing.",
      Column == "Adjusted_maternities" ~ "Exact_age_live_births * Stillbirth_factor * Multiplicity_factor * Private_NHS_factor * Bookings_uplift_modifier; shared allocation total.",
      Column == "Delivery_projection_base" ~ "Adjusted maternities available for delivery allocation.",
      Column == "Booking_projection_base" ~ "The same adjusted maternity total used for delivery allocation.",
      grepl("_proportion_", Column) ~ "Destination count / (recorded resident deliveries * maternity pathway adjustment) at the same LA and age, fixed across projection years.",
      grepl("_count_", Column) ~ "Corresponding Delivery_projection_base or Booking_projection_base * destination proportion.",
      grepl("denominator", Column) ~ "The shared estimated resident maternity denominator at this LA(R)/Age (SY).",
      TRUE ~ "Source identifier or intermediate count; see the numbered model steps."))
}

allocation_table_wide <- function(rates, prefix = "Delivery") {
  total <- "Shared_maternity_denominator"
  rates |>
    dplyr::mutate(Destination = make.names(Destination)) |>
    tidyr::pivot_wider(id_cols = dplyr::all_of(c("LA", "Age", total)),
      names_from = Destination, values_from = c(Count, Proportion),
      names_glue = paste0(prefix, "_{.value}_{Destination}"), values_fill = 0)
}

# 3.7 Build forward and retrospective master tables ----------------------
# Same calculations and pooled rates; only the projection years differ.
# Keep all source years for resident charts; forward destination charts start at 2025.
private_nhs_source <- read_private_nhs_counts()
projection_master_all_years <- build_projection_master(final_cohort_2326, total_births_by_LA_2326,
  snpp, registration_counts, include_las = area_config$area_code, start_year = min(snpp$year),
  private_nhs_counts = private_nhs_source)
projection_master <- projection_master_all_years[
  projection_master_all_years$Year >= projection_reference_year, ]
# Same pooled rates for the historical comparison: retrospective, not held-out validation.
projection_master_backtest <- build_projection_master(final_cohort_2326, total_births_by_LA_2326,
  snpp, registration_counts, baseline_fy = "2023/24", reference_year = 2023L,
  end_year = 2025L, include_las = read_area_config()$area_code, private_nhs_counts = private_nhs_source)
# 3.8 Expose the factors, proportions and column key for inspection -------
# These objects describe the inputs already used in projection_master.
ons_modifications <- attr(projection_master, "ons_modifications")
projection_master_dictionary <- projection_column_dictionary(projection_master)
delivery_proportions <- allocation_table_wide(attr(projection_master, "allocation_tables")$delivery)
booking_proportions <- allocation_table_wide(attr(projection_master, "allocation_tables")$booking, "Booking")
booking_modifications <- attr(projection_master, "allocation_tables")$calibration

# 3.8a Resident charts: sum the same adjusted counts before allocation ----
# CONTEXT_ONLY: report summaries; no additional conversion or rate applied.
resident_outputs <- summarise_resident_projections(projection_master_all_years, area_config)

# 3.8b Compare resident maternities from ONS, MSDS and the model -----------
  # CONTEXT_ONLY: recorded resident deliveries in the supplied LA extract.
# Use the supplied resident totals, including residents outside cluster trusts.
build_resident_comparison <- function(resident_totals, observed, projected,
    areas = read_area_config(), years = projection_history_years, ons_base = NULL) {
  # Sum ages AFTER reading all resident counts; unknown ages still count here.
  msds <- prepare_la_deliveries(resident_totals) |>
    dplyr::transmute(area_code = report_la_code(la_of_mother),
      Period = as.character(birth_yr), maternities = report_number(count)) |>
    dplyr::filter(area_code %in% areas$area_code, Period %in% years) |>
    dplyr::group_by(area_code, Period) |>
    dplyr::summarise(maternities = sum(maternities), .groups = "drop") |>
    dplyr::left_join(areas[c("area_code", "area_name")], by = "area_code") |>
    dplyr::mutate(Source = "MSDS recorded resident deliveries",
      Year = as.integer(substr(Period, 1, 4)))
  registered <- observed |>
    dplyr::transmute(area_name = as.character(area_name), maternities,
      Period = as.character(year), Source = "ONS published maternities",
      Year = year)
  estimates <- projected |>
    dplyr::transmute(area_name = as.character(area_name), maternities,
      Period = as.character(year), Source = "Adjusted maternity projections",
      Year = year)
  data <- dplyr::bind_rows(registered, msds |> dplyr::select(-area_code), estimates)
  series <- c("ONS published maternities", "MSDS recorded resident deliveries", "Adjusted maternity projections", "ONS projected live-birth base")
  if (!is.null(ons_base)) data <- dplyr::bind_rows(data, ons_base |> dplyr::transmute(
    area_name = as.character(area_name), Year = year, Period = as.character(year),
    maternities = projected_births, Source = "ONS projected live-birth base"))
  controlled <- !data$Source %in% c("ONS published maternities", "ONS projected live-birth base")
  data$maternities[controlled] <- dc_count(data$maternities[controlled])
  data$Source <- factor(data$Source, levels = series)
  data$area_name <- factor(data$area_name, levels = areas$area_name)
  data <- data |> dplyr::arrange(Source, area_name, Year)
  recorded <- data |> dplyr::filter(Source %in% c("ONS published maternities", "MSDS recorded resident deliveries"))
  latest_recorded <- max(recorded$Year)
  boundary <- latest_recorded + 0.5
  start <- min(data$Year)
  finish <- max(data$Year)
  shown <- data |> dplyr::filter(!Source %in% c("Adjusted maternity projections", "ONS projected live-birth base") |
    Year > latest_recorded) |>
    dplyr::mutate(Line_type = ifelse(grepl("^ONS", Source), "ONS", "MSDS / modelled"))

  chart <- ggplot2::ggplot(shown, ggplot2::aes(Year, maternities, colour = area_name,
      group = interaction(area_name, Source), linetype = Line_type)) +
    ggplot2::annotate("rect", xmin = start - 0.25, xmax = boundary, ymin = -Inf, ymax = Inf,
      fill = "#ECEFF2") +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_vline(xintercept = boundary, colour = "#66717E", linewidth = 0.5) +
    ggplot2::annotate("text", x = (start + boundary) / 2, y = Inf,
      label = "Historical", vjust = 1.3, size = 3.5) +
    ggplot2::annotate("text", x = (boundary + finish) / 2, y = Inf,
      label = "Future projections", vjust = 1.3, size = 3.5) +
    ggplot2::scale_colour_manual(values = area_colour_palette(areas)) +
    ggplot2::scale_linetype_manual(values = c("MSDS / modelled" = "solid", "ONS" = "dashed")) +
    ggplot2::scale_x_continuous(breaks = sort(unique(c(start, latest_recorded, seq(start, finish, by = 2)))),
      expand = ggplot2::expansion(mult = c(0.025, 0.015))) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
      expand = ggplot2::expansion(mult = c(0.02, 0.16))) +
    ggplot2::labs(x = "Year", y = "Resident maternities / ONS live-birth base", colour = "LA(R)", linetype = NULL) +
    ons_plot_theme() +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1, nrow = 1,
      override.aes = list(linetype = "solid")),
      linetype = ggplot2::guide_legend(order = 2, nrow = 1)) +
    ggplot2::theme(legend.box = "vertical")
  list(data = data, display_data = shown, msds = msds, latest_recorded_year = latest_recorded,
    chart = chart, comparison_style_version = "organisation-lines-v1")
}
ons_exploration <- c(ons_exploration, resident_outputs)
ons_exploration$ons_modifications <- ons_modifications
ons_exploration$resident_comparison <- build_resident_comparison(
  total_births_by_LA_2326,
  observed_trajectory, resident_outputs$projected_trajectory, area_config,
  ons_base = snpp |> dplyr::filter(area_code %in% area_config$area_code, age_group != "All ages") |>
    dplyr::group_by(area_name, year) |>
    dplyr::summarise(projected_births = sum(projected_births), .groups = "drop"))
save_report_rds(ons_exploration, ".ons_exploration.rds")

# 3.9 Summaries and report outputs ---------------------------------------
# All model multipliers have already been applied at LA(R) x Age (SY).
# Check the inputs are available before starting. Counting checks belong in QA.R.
required_objects <- c("projection_master", "projection_master_backtest", "projection_master_dictionary", "final_cohort_2326", "total_births_by_LA_2326",
  "trust_labels", "icb_labels", "provider_icb", "cluster_icbs", "cluster_trusts")
absent <- required_objects[!vapply(required_objects,
  function(name) exists(name, inherits = TRUE), logical(1))]
if (length(absent)) stop("Missing objects: ", paste(absent, collapse = ", "),
  ". Use Run analysis.R.", call. = FALSE)
required_columns <- list(
  final_cohort_2326 = c("person_id_mother", "uniq_preg_id", "birth_yr", "birth_flag",
    "la_of_mother", "age_at_birth_mother", "age_at_booking_mother", "birth_provider", "home_birth"),
  total_births_by_LA_2326 = c("birth_yr", "edd_yr", "la_of_mother",
    "age_at_birth_mother", "age_at_booking_mother", "group", "count"))
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
  dc_counts(coverage, c("Recorded_births", "Home_births", "Unknown_home_flag", "Observed_baseline", "Matched_allocation", "Unmatched")) |>
    dplyr::select(Level, Organisation, Recorded_births, Home_births, Unknown_home_flag,
      Observed_baseline, Matched_allocation, Unmatched) |>
    gt::gt(groupname_col = "Level", rowname_col = "Organisation") |>
    gt::tab_header(title = paste(baseline_fy, "delivery inclusion and projection coverage")) |>
    gt::cols_label(Recorded_births = "Recorded maternities with this provider",
      Home_births = "Home: excluded", Unknown_home_flag = "Unknown location: separate",
      Observed_baseline = "Non-home baseline", Matched_allocation = "Matched LA/age",
      Unmatched = "Not allocated") |>
    gt::fmt_number(columns = c("Recorded_births", "Home_births", "Unknown_home_flag",
      "Observed_baseline", "Matched_allocation", "Unmatched"), decimals = 0) |>
    gt::tab_source_note("One pregnancy per row; birth flag = 1 and the assigned pregnancy FY. Recorded maternities = home + unknown home flag + non-home baseline. Baseline = matched LA/age + not allocated, including unresolved calibration issues. Home births can involve NHS care; they are excluded only from these trust-location totals.") |>
    gt::tab_options(container.width = "100%", container.overflow.x = TRUE)
}

# Compare the same organisations without changing the projection calculation.
# ONS base uses the existing destination shares before any volume modifiers.
build_provider_total_comparison <- function(master, cohort, organisations, model, type,
    history_years = projection_history_years, history_master = NULL) {
  booking <- type == "Booking"
  provider <- if (booking) projection_booking_destination(cohort) else projection_delivery_destination(cohort)
  event_year <- cohort$Pregnancy_year
  historical <- as.integer(substr(history_years, 1, 4))
  # Earlier ONS bases use the already-calculated master, with the same shares.
  # Do not manufacture observed ONS provider counts or blend source years.
  base_master <- master
  if (!is.null(history_master)) base_master <- dplyr::bind_rows(as.data.frame(master),
    as.data.frame(history_master[history_master$Year %in% historical &
      !history_master$Year %in% master$Year, , drop = FALSE]))
  dplyr::bind_rows(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, ]
    codes <- if (org$Level == "Trust") org$Code else if (org$Level == "ICB")
      names(provider_icb)[provider_icb == org$Code] else cluster_trusts
    columns <- intersect(paste0(type, "_proportion_", make.names(codes)), names(base_master))
    base <- data.frame(Year = base_master$Year,
      Value = base_master$Exact_age_live_births * rowSums(base_master[columns])) |>
      dplyr::group_by(Year) |> dplyr::summarise(Value = sum_available_projection(Value), .groups = "drop")
    projected <- model[model$Key == org$Key, ] |> dplyr::arrange(Year)
    base <- base |> dplyr::left_join(projected |> dplyr::select(Year, Coverage_status, Coverage_note), by = "Year") |>
      dplyr::mutate(Coverage_status = dplyr::coalesce(Coverage_status, projected$Coverage_status[1]),
        Coverage_note = dplyr::coalesce(Coverage_note, projected$Coverage_note[1])) |>
      dplyr::mutate(Value = dplyr::if_else(Coverage_status == "No usable estimate", NA_real_, Value))
    observed <- data.frame(Year = historical,
      Value = vapply(historical, function(year)
        sum(provider %in% codes & event_year %in% year), numeric(1)))
    dplyr::bind_rows(
      projected |> dplyr::transmute(Year, Value = Projected, Series = "Modelled projection", Period = as.character(Year), Coverage_note),
      base |> dplyr::transmute(Year, Value, Series = "ONS base", Period = as.character(Year), Coverage_note),
      observed |> dplyr::mutate(Series = "MSDS recorded", Period = history_years,
        Coverage_note = "All recorded provider activity; may extend beyond model coverage")) |>
      dplyr::mutate(Key = org$Key, Level = org$Level, Organisation = org$Organisation)
  }))
}

provider_total_chart <- function(data, title) {
  data <- dc_coverage_note(dc_counts(data, "Value"))
  keys <- unique(data$Key)
  colours <- organisation_line_colours(keys)
  combined <- length(keys) > 1L
  latest_recorded <- max(data$Year[data$Series == "MSDS recorded"])
  boundary <- latest_recorded + 0.5
  # Historical model estimates remain in tables/backtests, not the future line.
  shown <- data[data$Series != "Modelled projection" | data$Year > latest_recorded, ]
  series_order <- c("Modelled projection", "MSDS recorded", "ONS base")
  chart <- plotly::plot_ly(height = if (combined) 640 else 440)
  for (key in keys) {
    first <- TRUE
    for (series in series_order) {
      line <- shown[shown$Key == key & shown$Series == series, ] |> dplyr::arrange(Year)
      if (!nrow(line)) next
      label <- gsub(" NHS.*$", "", line$Organisation[1])
      chart <- plotly::add_trace(chart, x = line$Year, y = line$Value,
        type = "scatter", mode = "lines", name = label, showlegend = first,
        legendgroup = key, meta = list(Key = key, Series = series),
        line = list(color = unname(colours[key]), width = 2.2,
          dash = if (series == "ONS base") "dash" else "solid"),
        text = paste0(line$Organisation, "<br>", series, "<br>", line$Period,
          "<br>", ifelse(is.na(line$Value), "Unavailable", scales::comma(line$Value, accuracy = 1)),
          "<br>", line$Coverage_note),
        hovertemplate = "%{text}<extra></extra>", connectgaps = FALSE)
      first <- FALSE
    }
  }
  years <- sort(unique(data$Year))
  left <- min(years) - 0.25
  right <- max(years) + 0.25
  ticks <- sort(unique(c(min(years), latest_recorded, years[years %% 5 == 0], max(years))))
  notes <- list(
    list(text = "Historical", x = (left + boundary) / 2, y = 1.06, xref = "x", yref = "paper", showarrow = FALSE),
    list(text = "Future projections", x = (boundary + right) / 2, y = 1.06, xref = "x", yref = "paper", showarrow = FALSE),
    list(text = "Solid: MSDS / modelled activity     Dashed: ONS base",
      x = 0, y = -0.18, xref = "paper", yref = "paper", xanchor = "left", showarrow = FALSE,
      font = list(size = 11)))
  if (any(grepl("^Partial", data$Coverage_note))) notes <- c(notes, list(list(
    text = "Partial projections show available allocations; recorded totals retain all activity.",
    x = 0, y = 1.14, xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "left", font = list(size = 10))))
  chart |> plotly::layout(
    xaxis = list(title = "Year (MSDS financial year starts)", tickvals = ticks,
      ticktext = as.character(ticks), range = c(left, right)),
    yaxis = list(title = title, tickformat = ",.0f", rangemode = "tozero"),
    shapes = list(
      list(type = "rect", xref = "x", yref = "paper", x0 = left, x1 = boundary,
        y0 = 0, y1 = 1, fillcolor = "#ECEFF2", line = list(width = 0), layer = "below"),
      list(type = "line", xref = "x", yref = "paper", x0 = boundary, x1 = boundary,
        y0 = 0, y1 = 1, line = list(color = "#66717E", width = 1.3))),
    annotations = notes,
    legend = list(orientation = "h", x = 0, y = -0.29, font = list(size = 11), groupclick = "togglegroup"),
    margin = list(l = 75, r = 20, t = 65, b = if (combined) 165 else 115),
    font = list(family = "Arial")) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}
build_first_draft_projections <- function(baseline_fy = projection_baseline,
  ons_reference_year = projection_reference_year, projection_end = projection_end_year,
  prepare_outputs = TRUE, master = if (baseline_fy == "2023/24") projection_master_backtest else projection_master,
  history_master = NULL) {
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
    dplyr::filter(Birth_recorded, Home == 0) |>
    dplyr::pull(Trust)
  recorded_booking_trusts <- cohort$Booking_Trust
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
  resident_denominators <- attr(wide_master, "allocation_tables")$calibration |>
    dplyr::transmute(LA, Age, Denominator = Shared_maternity_denominator,
      Allocation_available, Calibration_issue, Age_band = report_age_band(Age))
  # Retain observed feeders even when calibration cannot support allocation.
  feeder_las <- attr(wide_master, "allocation_tables")$delivery_observed |>
    dplyr::filter(Destination %in% cluster_trusts, Count > 0) |>
    dplyr::pull(LA) |> unique() |> sort()
  ons_births <- master |> dplyr::distinct(LA, LA_name) |>
    dplyr::rename(area_code = LA, area_name = LA_name)
  geography_changes <- attr(wide_master, "geography_changes")
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
    dplyr::left_join(attr(wide_master, "allocation_tables")$calibration |>
      dplyr::select(LA, Age, Calibration_issue), by = c("LA", "Age")) |>
    dplyr::mutate(LA_usable = !is.na(LA) & !LA %in% c("", "Unknown LA", "L99999999"),
      Age_usable = Age_band != "Unknown", ONS_covered = LA %in% ons_la_names$LA,
      Reason = dplyr::case_when(
        !LA_usable & !Age_usable ~ "Missing/unusable LA and age",
        !LA_usable ~ "Missing/unusable resident LA",
        !Age_usable ~ "Missing/unusable maternal age",
        !Age %in% attr(wide_master, "allocation_tables")$ages ~ "Outside projection age range",
        !ONS_covered ~ "No ONS series for resident LA in supplied projections",
        is.na(Calibration_issue) ~ "No matching LA/age calibration",
        Calibration_issue != "" ~ Calibration_issue, TRUE ~ ""))
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
  pooled_deliveries <- attr(wide_master, "allocation_tables")$delivery_observed |>
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
        Share = dplyr::if_else(Allocation_available & Denominator > 0, Numerator / Denominator, NA_real_)) |>
      dplyr::left_join(feeder_names, by = "LA")
  }))

  # Keep each destination visible, including source gaps and invalid cells.
  recorded_destinations <- attr(wide_master, "allocation_tables")$delivery_observed |>
    dplyr::group_by(LA, Age) |>
    dplyr::summarise(Cluster_trusts = sum(Count[Destination %in% cluster_trusts]),
      Other_trusts = sum(Count[grepl("^R[A-Z0-9]{2}$", Destination) & !Destination %in% cluster_trusts]),
      Recorded_home_births = sum(Count[Destination == "Home"]),
      Unknown_location = sum(Count[Destination == "Unknown_location"]), .groups = "drop")
  destination_accounting <- attr(wide_master, "allocation_tables")$calibration |>
    dplyr::filter(LA %in% feeder_las, !is.na(Age)) |>
    dplyr::transmute(LA, Age, Denominator = Shared_maternity_denominator,
      No_recorded_delivery = Estimated_no_recorded_delivery,
      Not_represented = Resident_birth_gap, Calibration_issue) |>
    dplyr::left_join(recorded_destinations, by = c("LA", "Age")) |>
    dplyr::mutate(dplyr::across(c("Cluster_trusts", "Other_trusts", "Recorded_home_births",
      "Unknown_location"), ~ dplyr::coalesce(.x, 0))) |>
    dplyr::left_join(feeder_names, by = "LA")

  # Follow provider records within the projection age range, retaining unknown
  # ages for coverage checks. Records at known ages outside the range are out of scope.
  model_cells <- wide_master |> dplyr::group_by(LA, Age) |>
    dplyr::summarise(In_master = TRUE, Allocation_available = all(Allocation_available),
      Projection_available = all(Projection_available),
      Calibration_issue = dplyr::first(Calibration_issue), .groups = "drop")
  provider_status <- function(type) {
    records <- cohort
    records$Trust <- if (type == "Delivery") projection_delivery_destination(records) else
      projection_booking_destination(records)
    records <- records |> dplyr::filter(Trust %in% cluster_trusts) |>
      dplyr::left_join(model_cells, by = c("LA", "Age")) |>
      dplyr::mutate(Reason = dplyr::case_when(
        LA %in% c("Unknown LA", "L99999999", "") ~ "Missing/unusable resident LA",
        is.na(Age) ~ "Missing/unusable maternal age",
        !Age %in% attr(wide_master, "allocation_tables")$ages ~ "Outside projection age range",
        is.na(In_master) ~ "No matching ONS LA/age projection",
        !Allocation_available ~ Calibration_issue,
        !Projection_available ~ "No same-age uplift estimate", TRUE ~ "")) |>
      dplyr::select(LA, Age, Trust, Reason, Booking_year, Delivery_year)
    dplyr::bind_rows(lapply(seq_len(nrow(organisations)), function(i) {
      org <- organisations[i, , drop = FALSE]
      select_org(records, org) |>
        dplyr::mutate(Key = org$Key, Level = org$Level, Organisation = org$Organisation,
          Age_band = report_age_band(Age), Allocation = type)
    }))
  }
  delivery_status <- provider_status("Delivery")
  booking_status <- provider_status("Booking")
  provider_records <- dplyr::bind_rows(delivery_status, booking_status)
  provider_coverage <- provider_records |>
    dplyr::group_by(Allocation, Key, Level, Organisation) |>
    dplyr::summarise(Cohort_rows = dplyr::n(), Matched_rows = sum(Reason == ""),
      Unmatched_rows = sum(Reason != ""), .groups = "drop") |>
    dplyr::mutate(Coverage = Matched_rows / Cohort_rows)
  # These annual counts use exactly the LA/age support of the model. They are
  # comparators only; neither missing ages nor source counts are redistributed.
  matched_history <- provider_records |>
    dplyr::mutate(Event_year = ifelse(Allocation == "Booking", Booking_year, Delivery_year)) |>
    dplyr::group_by(Allocation, Key, Event_year) |>
    dplyr::summarise(Observed_all = dplyr::n(), Observed_matched = sum(Reason == ""), .groups = "drop")
  summarise_gaps <- function(records) records |>
    dplyr::filter(Reason != "", is.na(Age) | Age %in% attr(wide_master, "allocation_tables")$ages) |>
    dplyr::count(LA, Age, Trust, Reason, Key, Level, Organisation, Age_band, Allocation, name = "Cohort_rows")
  delivery_gaps <- summarise_gaps(delivery_status)
  booking_gaps <- summarise_gaps(booking_status)
  mark_projection_coverage <- function(data, gaps, records) {
    # An unknown source age can affect any displayed age; known ages affect
    # their own age/band. Unknown-age counts flag every band, but are counted
    # once in provider totals. Available_projected is an accounting subtotal.
    keys <- c("Key", intersect(c("Age", "Age_band"), names(data)))
    unknown <- if (length(keys) > 1L) gaps |> dplyr::filter(is.na(Age)) else gaps[0, ]
    specific <- if (length(keys) > 1L) gaps |> dplyr::filter(!is.na(Age)) else gaps
    counts <- specific |> dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
      dplyr::summarise(Unmatched_cohort_rows = sum(Cohort_rows), .groups = "drop")
    unknown <- unknown |> dplyr::group_by(Key) |>
      dplyr::summarise(Unknown_age_rows = sum(Cohort_rows), .groups = "drop")
    matched <- records |> dplyr::filter(Reason == "") |>
      dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
      dplyr::summarise(Matched_cohort_rows = dplyr::n(), .groups = "drop")
    data |> dplyr::left_join(counts, by = keys) |>
      dplyr::left_join(unknown, by = "Key") |>
      dplyr::left_join(matched, by = keys) |>
      dplyr::mutate(Unmatched_cohort_rows = dplyr::coalesce(Unmatched_cohort_rows, 0L) +
        dplyr::coalesce(Unknown_age_rows, 0L),
        Matched_cohort_rows = dplyr::coalesce(Matched_cohort_rows, 0L),
        Available_projected = dplyr::coalesce(Projected, 0),
        Projected = dplyr::if_else(Matched_cohort_rows == 0 & Unmatched_cohort_rows > 0,
          NA_real_, Projected),
        Projection_complete = Unmatched_cohort_rows == 0 & is.finite(Projected),
        Coverage_status = dplyr::case_when(!is.finite(Projected) ~ "No usable estimate",
          Projection_complete ~ "Complete", TRUE ~ "Partial"),
        Coverage_note = paste0(Coverage_status, ": ", Matched_cohort_rows,
          " matched cohort records; ", Unmatched_cohort_rows,
          " unmatched (ages 13-54 and unknown age). Partial values show available allocations only.")) |>
      dplyr::select(-Unknown_age_rows)
  }

  # Read completed destination columns. Aggregate only AFTER projection.
  modifiers <- wide_master |> dplyr::distinct(LA, Age, Stillbirth_ratio, Stillbirth_factor, Multiplicity_factor,
    Private_NHS_age_band, Private_NHS_rate, Private_NHS_factor, Bookings_uplift_modifier, Bookings_uplift_source, Maternity_modifier)
  projection_years <- sort(unique(master$Year))
  ons_maternity_projections <- wide_master |>
    dplyr::group_by(LA, ONS_age, Year) |>
    # Source births repeat across split ages; use the source count once.
    dplyr::summarise(ONS_source_births = dplyr::first(ONS_live_births),
      ONS_maternities = sum(Adjusted_maternities), .groups = "drop") |>
    dplyr::group_by(LA, ONS_age) |>
    dplyr::mutate(ONS_reference = ONS_maternities[Year == ons_reference_year],
      ONS_births_reference = ONS_source_births[Year == ons_reference_year],
      Growth_factor = dplyr::if_else(ONS_births_reference > 0,
        ONS_source_births / ONS_births_reference, NA_real_),
      ONS_change = Growth_factor - 1) |> dplyr::ungroup()
  completed <- master |>
    dplyr::filter(Delivery_destination %in% cluster_trusts, Allocation_status == "Recorded delivery trust") |>
    dplyr::rename(Trust = Delivery_destination)
  # 3.9a Delivery summaries: aggregate completed individual-age counts ----
  # Retain Age (SY) while summing LA(R) contributions to delivery trusts
  trust_age_totals <- completed |>
    dplyr::group_by(Trust, Age, Year) |>
    dplyr::summarise(Projected = sum_available_projection(Projected_maternities), .groups = "drop")
  trust_age_totals$ICB <- unname(provider_icb[trust_age_totals$Trust])
  icb_age_totals <- trust_age_totals |>
    dplyr::group_by(ICB, Age, Year) |>
    dplyr::summarise(Projected = sum_available_projection(Projected), .groups = "drop")
  cluster_age_totals <- icb_age_totals |>
    dplyr::group_by(Age, Year) |>
    dplyr::summarise(Projected = sum_available_projection(Projected), .groups = "drop")

  feeder_projections <- dplyr::bind_rows(lapply(seq_len(nrow(organisations)), function(i) {
    org <- organisations[i, , drop = FALSE]
    select_org(completed, org) |>
      dplyr::group_by(LA, LA_name, Age, ONS_age, Year, Age_treatment) |>
      dplyr::summarise(Projected = sum_available_projection(Projected_maternities), Numerator = sum(MSDS_destination_count),
        Denominator = dplyr::first(Allocation_denominator), .groups = "drop") |>
      dplyr::mutate(Key = org$Key, Level = org$Level, Organisation = org$Organisation,
        Share = Numerator / Denominator, Age_band = report_age_band(Age),
        Treatment = Age_treatment, Year_label = as.character(Year))
  })) |>
    dplyr::left_join(ons_maternity_projections, by = c("LA", "ONS_age", "Year"), relationship = "many-to-one")

  single_age_projections <- dplyr::bind_rows(
    dplyr::transmute(trust_age_totals, Key = paste0("trust-", tolower(Trust)), Age, Year, Projected),
    dplyr::transmute(icb_age_totals, Key = paste0("icb-", tolower(ICB)), Age, Year, Projected),
    dplyr::mutate(cluster_age_totals, Key = "cluster"))
  single_age_projections$Year_label <- as.character(single_age_projections$Year)
  # Presentation only: sum the already projected exact-age contributions for charts.
  age_projections <- feeder_projections |>
    dplyr::group_by(Key, Age_band, Year, Year_label) |>
    dplyr::summarise(Projected = sum_available_projection(Projected), .groups = "drop")
  # Descriptive change in the completed model counts; not an input multiplier.
  weighted_age_growth <- age_projections |>
    dplyr::group_by(Key, Age_band) |>
    dplyr::mutate(Baseline = Projected[Year == ons_reference_year],
      Growth_change = relative_change(Projected, Baseline)) |>
    dplyr::ungroup()
  maternity_projections <- tidyr::expand_grid(Key = organisations$Key, Year = projection_years) |>
    dplyr::left_join(single_age_projections |>
      dplyr::group_by(Key, Year) |>
      dplyr::summarise(Projected = sum_available_projection(Projected), .groups = "drop"), by = c("Key", "Year")) |>
    dplyr::mutate(Projected = ifelse(!Key %in% feeder_projections$Key, 0, Projected),
      Year_label = as.character(Year)) |>
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
    dplyr::summarise(Projected = sum_available_projection(Projected), .groups = "drop")
  booking_trust_totals$ICB <- unname(provider_icb[booking_trust_totals$Trust])
  booking_icb_totals <- booking_trust_totals |> dplyr::group_by(ICB, Age, Year) |>
    dplyr::summarise(Projected = sum_available_projection(Projected), .groups = "drop")
  booking_cluster_totals <- booking_icb_totals |> dplyr::group_by(Age, Year) |>
    dplyr::summarise(Projected = sum_available_projection(Projected), .groups = "drop")
  # Sum completed booking counts into the same final reporting age bands.
  booking_age_projections <- dplyr::bind_rows(
    dplyr::transmute(booking_trust_totals, Key = paste0("trust-", tolower(Trust)), Age, Year, Projected),
    dplyr::transmute(booking_icb_totals, Key = paste0("icb-", tolower(ICB)), Age, Year, Projected),
    dplyr::mutate(booking_cluster_totals, Key = "cluster")) |>
    dplyr::mutate(Age_band = report_age_band(Age)) |>
    dplyr::group_by(Key, Age_band, Year) |>
    dplyr::summarise(Projected = sum_available_projection(Projected), .groups = "drop")
  booking_total_projections <- tidyr::expand_grid(Key = organisations$Key, Year = projection_years) |>
    dplyr::left_join(booking_age_projections |>
      dplyr::group_by(Key, Year) |> dplyr::summarise(Projected = sum_available_projection(Projected), .groups = "drop"),
      by = c("Key", "Year")) |>
    dplyr::mutate(Projected = dplyr::if_else(!Key %in% booking_age_projections$Key, 0, Projected)) |>
    dplyr::group_by(Key) |>
    dplyr::mutate(Percent_change = relative_change(Projected, Projected[Year == ons_reference_year])) |>
    dplyr::ungroup() |>
    dplyr::left_join(organisations, by = "Key")
  # Keep valid allocations visible and label partial coverage. Compare changes
  # to matched recorded deliveries, or to the same model support for bookings.
  single_age_projections <- mark_projection_coverage(single_age_projections, delivery_gaps, delivery_status)
  age_projections <- mark_projection_coverage(age_projections, delivery_gaps, delivery_status)
  weighted_age_growth <- mark_projection_coverage(weighted_age_growth, delivery_gaps, delivery_status) |>
    dplyr::group_by(Key, Age_band) |>
    dplyr::mutate(Growth_change = relative_change(Projected, Projected[Year == ons_reference_year])) |>
    dplyr::ungroup()
  maternity_projections <- mark_projection_coverage(maternity_projections, delivery_gaps, delivery_status) |>
    dplyr::left_join(matched_history |> dplyr::filter(Allocation == "Delivery", Event_year == ons_reference_year) |>
      dplyr::select(Key, Observed_matched_baseline = Observed_matched), by = "Key") |>
    dplyr::mutate(Observed_matched_baseline = dplyr::coalesce(Observed_matched_baseline, 0L),
      Change = Projected - Observed_matched_baseline,
      Percent_change = relative_change(Projected, Observed_matched_baseline))
  booking_age_projections <- mark_projection_coverage(booking_age_projections, booking_gaps, booking_status)
  booking_total_projections <- mark_projection_coverage(booking_total_projections, booking_gaps, booking_status) |>
    dplyr::group_by(Key) |>
    dplyr::mutate(Percent_change = relative_change(Projected, Projected[Year == ons_reference_year])) |>
    dplyr::ungroup()
  projection_note <- paste("ONS births (L) \u00d7 local exact-age share \u00d7 [1 + S/L]",
    "\u00d7 M/(L+S) \u00d7 Private\u2013NHS modifier \u00d7 maternity pathway adjustment \u00d7 LA/age destination proportion.",
    "National Table 5 counts pool 2023--2025; MSDS calibration uses supplied cohort rows aged 13--54.",
    "Historical bookings and recorded deliveries share pregnancy FY: birth_yr first, otherwise edd_yr; appointments do not assign FY.",
    "Private\u2013NHS factors use pooled 2023--2025 England Table 4 age bands; the same setting-proxy reduction is assumed for bookings and deliveries.",
    "Every LA uses its own resident deliveries and antenatal-only counts for 2023/24--2025/26; a zero/missing delivery denominator gives a zero pathway factor.",
    "Both allocations use one adjusted total. Home, no recorded delivery/booking, unknowns",
    "and the Booking Denominator Factor remain separate. No rebasing to observed activity.")
  result <- list(calculation_version = "ons-shared-age-calculations-v24", organisations = organisations, feeder_shares = feeder_shares,
    master = wide_master, delivery_rows = master, booking_rows = booking_rows,
    booking_totals = list(Trust = booking_trust_totals, ICB = booking_icb_totals, Cluster = booking_cluster_totals),
    booking_age_projections = booking_age_projections, booking_total_projections = booking_total_projections,
    booking_proportions = allocation_table_wide(attr(wide_master, "allocation_tables")$booking, "Booking"),
    ons_modifications = attr(wide_master, "ons_modifications"), private_nhs = attr(wide_master, "private_nhs"),
    delivery_proportions = allocation_table_wide(attr(wide_master, "allocation_tables")$delivery),
    allocation_tables = attr(wide_master, "allocation_tables"), master_dictionary = projection_column_dictionary(wide_master),
    provider_projection_gaps = dplyr::bind_rows(delivery_gaps, booking_gaps),
    provider_coverage = provider_coverage, matched_history = matched_history,
    la_source_matching = attr(wide_master, "la_source_matching"),
    observed_totals = observed_totals, resident_denominators = resident_denominators, destination_accounting = destination_accounting,
    baseline_fy = baseline_fy, ons_reference_year = ons_reference_year,
    projection_years = projection_years, age_bands = age_bands,
    coverage = coverage, coverage_details = coverage_details, geography = geography_changes,
    modifiers = modifiers, ons_maternity_projections = ons_maternity_projections, feeder_projections = feeder_projections,
    single_age_projections = single_age_projections,
    age_projections = age_projections, weighted_age_growth = weighted_age_growth,
    maternity_projections = maternity_projections, note = projection_note)
  result$total_comparison <- list(
    Delivery = build_provider_total_comparison(wide_master, cohort, organisations, maternity_projections, "Delivery", history_master = history_master),
    Booking = build_provider_total_comparison(wide_master, cohort, organisations, booking_total_projections, "Booking", history_master = history_master))
  result$comparison_style_version <- "organisation-lines-v1"
  review_las <- union(cluster_las, attr(wide_master, "allocation_tables")$cohort_counts$LA)
  result$booking_calibration <- attr(wide_master, "allocation_tables")$calibration |>
    dplyr::filter(LA %in% review_las) |>
    dplyr::left_join(la_names, by = "LA") |>
    dplyr::mutate(LA_name = dplyr::coalesce(LA_name, LA))
  result$booking_denominator_summary <- result$booking_calibration |>
    dplyr::group_by(LA, LA_name) |>
    dplyr::summarise(Resident_births = sum(MSDS_resident_count),
      Cohort_births = sum(Cohort_recorded_births), Birth_gap = sum(Resident_birth_gap),
      Cohort_pregnancies = sum(Cohort_pregnancies),
      Estimated_maternities = sum(Shared_maternity_denominator),
      Booking_Denominator_Factor = sum(Booking_Denominator_Factor),
      Ages_with_birth_gap = sum(abs(Resident_birth_gap) > 1e-8, na.rm = TRUE),
      Ages_requiring_review = sum(!Allocation_available), .groups = "drop")
  result$booking_gap_projections <- wide_master |>
    dplyr::filter(LA %in% review_las) |>
    dplyr::transmute(LA, LA_name, Age, ONS_age, Age_band = report_age_band(Age), Year,
      Adjusted_maternities,
      # Zero source contributions stay zero; an unresolved positive gap is unknown.
      Booking_Denominator_Factor = dplyr::if_else(Exact_age_live_births == 0 |
        (Projection_available & Allocation_available), Booking_count_Booking_Denominator_Factor, NA_real_),
      Unallocated = Booking_count_Unallocated)
  result$booking_gap_by_la <- result$booking_gap_projections |>
    dplyr::group_by(LA, LA_name, Year) |>
    dplyr::summarise(Adjusted_maternities = sum(Adjusted_maternities),
      Booking_Denominator_Factor = sum(Booking_Denominator_Factor),
      Unallocated = sum(Unallocated), .groups = "drop") |>
    dplyr::mutate(Gap_share = dplyr::if_else(Adjusted_maternities > 0,
      Booking_Denominator_Factor / Adjusted_maternities, NA_real_))
  result$unestimated_cells <- wide_master |>
    dplyr::filter(!Projection_available | Bookings_uplift_modifier == 0, Exact_age_live_births > 0) |>
    dplyr::distinct(LA, LA_name, Age, ONS_age, Bookings_uplift_source)
  result$la_source_mismatches <- result$la_source_matching |>
    dplyr::filter(Issue != "") |>
    dplyr::left_join(la_names, by = "LA") |>
    dplyr::mutate(LA_name = dplyr::coalesce(LA_name, LA))
  if (!prepare_outputs) return(result)

  # Publication copies only: result's model tables above retain raw precision.
  maternity_projections <- dc_projection(maternity_projections, ons_reference_year)
  age_projections <- dc_projection(age_projections, ons_reference_year)
  weighted_age_growth <- dc_projection(weighted_age_growth, ons_reference_year)
  booking_total_projections <- dc_projection(booking_total_projections, ons_reference_year, booking = TRUE)
  booking_age_projections <- dc_projection(booking_age_projections, ons_reference_year, booking = TRUE)

  result$provider_coverage_table <- dc_cells(provider_coverage, c("Cohort_rows", "Matched_rows", "Unmatched_rows"),
    list(Coverage = c("Matched_rows", "Cohort_rows"))) |>
    dplyr::select(Allocation, Level, Organisation, Cohort_rows, Matched_rows, Unmatched_rows, Coverage) |>
    base_table("Provider projection coverage in the supplied cohort",
      "Matched means a usable LA/age allocation across the model years. All ages are counted here; unknown and out-of-range ages remain unmatched.") |>
    gt::fmt_number(columns = c(Cohort_rows, Matched_rows, Unmatched_rows), decimals = 0) |>
    gt::fmt_percent(columns = Coverage, decimals = 1)

  result$la_source_mismatches_table <- result$la_source_mismatches |>
    dplyr::select(LA, LA_name, In_cohort, In_resident_counts, In_ONS, Issue) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("In_"), ~ ifelse(.x, "Yes", "No"))) |>
    base_table("LA source mismatches requiring review",
      "Configured BBCS LAs and contributing outside LAs, matched to their resident delivery counts and ONS births") |>
    gt::cols_label(LA = "LA code", LA_name = "Resident LA", In_cohort = "Supplied cohort",
      In_resident_counts = "Resident birth counts", In_ONS = "ONS births", Issue = "Source to check")
  result$provider_projection_gaps_table <- dc_counts(result$provider_projection_gaps, "Cohort_rows") |>
    dplyr::left_join(la_names, by = "LA") |>
    dplyr::mutate(LA_name = dplyr::coalesce(LA_name, LA)) |>
    dplyr::select(Allocation, Level, Organisation, LA_name, Age, Reason, Cohort_rows) |>
    base_table("Provider records without complete projections",
      "Recorded provider activity at ages 13–54, plus records with an unknown age.") |>
    gt::cols_label(LA_name = "Resident LA", Age = "Age (SY)", Cohort_rows = "Cohort records") |>
    gt::fmt_number(columns = "Cohort_rows", decimals = 0)
  denominator_display <- dc_counts(result$booking_denominator_summary,
    c("Resident_births", "Cohort_births", "Cohort_pregnancies", "Estimated_maternities"))
  denominator_display$Birth_gap <- denominator_display$Resident_births - denominator_display$Cohort_births
  denominator_display$Booking_Denominator_Factor <- denominator_display$Estimated_maternities - denominator_display$Cohort_pregnancies
  result$booking_denominator_summary_table <- denominator_display |>
    base_table("Resident and cohort counts", "All resident totals come from the expanded LA extract; provider counts come from the linked cohort") |>
    gt::cols_label(LA_name = "Resident LA", LA = "LA code", Resident_births = "Resident deliveries",
      Cohort_births = "Cohort deliveries", Birth_gap = "Delivery gap", Cohort_pregnancies = "Cohort pregnancies",
      Estimated_maternities = "Estimated maternities", Booking_Denominator_Factor = "Booking Denominator Factor",
      Ages_with_birth_gap = "Ages with a birth gap", Ages_requiring_review = "Ages requiring review") |>
    gt::fmt_number(columns = where(is.numeric), decimals = 1)
  result$booking_period_table <- dc_counts(attr(wide_master, "allocation_tables")$period_review, "Pregnancies") |>
    base_table("Source periods in the full supplied cohort", "These counts are retained without an additional calibration date filter") |>
    gt::cols_label(Year = "Assigned pregnancy FY", Birth_FY = "Supplied birth FY", Due_FY = "Supplied EDD FY",
      Pregnancy_year_source = "FY source", Birth_recorded = "Birth recorded")
  result$booking_exclusions_table <- dc_counts(attr(wide_master, "allocation_tables")$exclusions, "Pregnancies") |>
    base_table("LA and age availability in the supplied cohort",
      "Records without a usable LA or age remain in this source audit but cannot inform an LA/age modifier or allocation") |>
    gt::cols_label(LA_usable = "Usable resident LA", Age_usable = "Usable maternal age",
      Birth_recorded = "Birth recorded")
  result$booking_calibration_table <- dc_calibration(result$booking_calibration) |>
    dplyr::select(LA_name, LA, Age, MSDS_resident_count, Cohort_recorded_births,
      Resident_birth_gap, Resident_antenatal_only, Resident_pregnancies,
      Cohort_no_recorded_delivery, Bookings_uplift_modifier,
      Bookings_uplift_source, Shared_maternity_denominator, Booking_Denominator_Factor,
      Birth_gap_review, Calibration_issue) |>
    base_table("Maternity pathway adjustment and denominator reconciliation by LA and age") |>
    gt::fmt_number(columns = c("MSDS_resident_count", "Cohort_recorded_births", "Resident_birth_gap",
      "Resident_antenatal_only", "Resident_pregnancies", "Cohort_no_recorded_delivery",
      "Shared_maternity_denominator", "Booking_Denominator_Factor"), decimals = 1) |>
    gt::fmt_number(columns = "Bookings_uplift_modifier", decimals = 3) |>
    gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age = "Age (SY)",
      MSDS_resident_count = "Resident deliveries", Cohort_recorded_births = "Cohort deliveries",
      Resident_antenatal_only = "LA antenatal-only pregnancies", Resident_pregnancies = "LA pregnancies",
      Resident_birth_gap = "Delivery gap", Cohort_no_recorded_delivery = "Cohort: no recorded delivery",
      Bookings_uplift_modifier = "Maternity pathway adjustment", Bookings_uplift_source = "Pathway factor source",
      Shared_maternity_denominator = "Shared denominator", Booking_Denominator_Factor = "Booking Denominator Factor",
      Birth_gap_review = "Delivery-count check", Calibration_issue = "Allocation check")
  result$unestimated_cells_table <- result$unestimated_cells |>
    base_table("LA/age cells with zero pathway factors or unavailable projections",
      "Zero or missing resident deliveries give a zero pathway factor. Rates are never borrowed from other LAs or ages.") |>
    gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age = "Age (SY)",
      ONS_age = "ONS source age", Bookings_uplift_source = "Pathway factor evidence")
  result$booking_gap_table <- dc_cells(result$booking_gap_by_la,
    c("Adjusted_maternities", "Booking_Denominator_Factor", "Unallocated"),
    list(Gap_share = c("Booking_Denominator_Factor", "Adjusted_maternities"))) |>
    dplyr::filter(Year %in% c(ons_reference_year, max(projection_years))) |>
    base_table("Booking Denominator Factor: projected volume", "Estimated residual group; unavailable where a positive source contribution cannot be estimated or allocated") |>
    gt::fmt_number(columns = c("Adjusted_maternities", "Booking_Denominator_Factor", "Unallocated"), decimals = 1) |>
    gt::fmt_percent(columns = "Gap_share", decimals = 1) |>
    gt::cols_label(LA_name = "Resident LA", LA = "LA code", Adjusted_maternities = "Adjusted maternities",
      Booking_Denominator_Factor = "Booking Denominator Factor", Gap_share = "Share of adjusted maternities") |>
    gt::sub_missing(missing_text = "Unavailable")
  result$booking_gap_age_table <- result$booking_gap_projections |>
    dplyr::filter(Year %in% c(ons_reference_year, max(projection_years))) |>
    dplyr::group_by(LA_name, Age_band, Year) |>
    dplyr::summarise(Booking_Denominator_Factor = sum(Booking_Denominator_Factor),
      Unallocated = sum(Unallocated), .groups = "drop") |>
    dc_counts(c("Booking_Denominator_Factor", "Unallocated")) |>
    base_table("Booking Denominator Factor by LA and age band", "Unavailable where a positive source contribution cannot be estimated or allocated") |>
    gt::fmt_number(columns = c("Booking_Denominator_Factor", "Unallocated"), decimals = 1) |>
    gt::cols_label(LA_name = "Resident LA", Age_band = "Age band", Booking_Denominator_Factor = "Booking Denominator Factor") |>
    gt::sub_missing(missing_text = "Unavailable")

  # 3.9c Delivery charts and tables: display the completed counts ----------

  # Make charts and tables for the allocation and projection views.
  coverage_table <- make_projection_coverage_table(coverage, baseline_fy)
  coverage_details_table <- dc_counts(coverage_details, "N") |>
    dplyr::select(Level, Organisation, LA_name, LA, Age, Reason, N) |>
    base_table(paste(baseline_fy, "records without an available allocation")) |>
    gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age = "Maternal age",
                   Reason = "Reason", N = "Observed deliveries") |>
    gt::fmt_number(columns = "N", decimals = 0)
  geography_table <- if (nrow(geography_changes)) geography_changes |>
    base_table("Residence code conversions applied in this run") |>
    gt::cols_label(Source_code = "Supplied MSDS code", Source_name = "Supplied area",
      Target_code = "2023 ONS code", Target_name = "2023 area", Treatment = "Treatment") else
    base_table(data.frame(Note = "No listed residence code conversions were needed in this run."),
      "Residence code conversions")
  # Section 2.2.8: exact-age shares in tables; chart bands sum counts before division.
  # Rank LAs by actual deliveries here; keep cluster LAs and ten additional feeders.
  # The other-feeder share sums counts and denominators, including zero-flow ages.
  mapping_share_max <- max(1, ceiling(dc_ratio(feeder_shares$Numerator, feeder_shares$Denominator)), na.rm = TRUE)
  mapping_outputs <- stats::setNames(lapply(organisations$Key, function(key) {
    org_shares <- feeder_shares[feeder_shares$Key == key, , drop = FALSE]
    if (!nrow(org_shares)) return(list(chart = htmltools::div("No recorded deliveries in the supplied cohort"),
      table = base_table(data.frame(Note = "No recorded deliveries in the supplied cohort"), "LA allocation")))
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
        Denominator = if (all(Allocation_available & is.finite(Denominator) & Denominator >= 0)) sum(Denominator) else NA_real_,
        .groups = "drop") |>
      dplyr::mutate(Share = dplyr::if_else(Denominator > 0, N / Denominator, NA_real_))
    flow <- dc_cells(flow, c("N", "Denominator"), list(Share = c("N", "Denominator")))
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
          "<br>Estimated LA/age maternities: ", scales::comma(points$Denominator, accuracy = 1),
          "<br>% sent here: ", scales::percent(points$Share, accuracy = 0.1))
        chart <- plotly::add_trace(chart, x = points$Share,
          y = match(points$Source_LA, chart_sources) + age_offsets[i],
          type = "scatter", mode = "markers", name = age_bands[i],
          marker = list(color = age_colours[i], size = 8),
          text = hover, hovertemplate = "%{text}<extra></extra>")
      }
      chart <- chart |>
        plotly::layout(showlegend = FALSE,
          xaxis = list(title = "% of Estimated LA/age maternities sent here", tickformat = ".0%",
            range = c(0, mapping_share_max), zeroline = TRUE),
          yaxis = list(title = "", tickmode = "array", tickvals = seq_along(chart_sources),
            ticktext = chart_labels, range = c(length(chart_sources) + 0.6, 0.4),
            tickfont = list(size = 11), showgrid = TRUE, zeroline = FALSE),
          margin = list(l = 180, r = 20, t = 15, b = 65), font = list(family = "Arial", size = 11)) |>
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    }
    table <- dc_cells(org_shares, c("Numerator", "Denominator"), list(Share = c("Numerator", "Denominator"))) |>
      dplyr::select(LA_name, LA, Age, Numerator, Denominator, Share) |>
      base_table("Delivery shares by resident LA and age") |>
      gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age = "Maternal age",
        Numerator = "Deliveries here", Denominator = "Estimated LA/age maternities", Share = "% sent here") |>
      gt::fmt_number(columns = c("Numerator", "Denominator"), decimals = 0) |>
      gt::fmt_percent(columns = "Share", decimals = 1)
    list(chart = chart, table = table, flow = flow, chart_data = chart_data, visible_las = visible_las)
  }), organisations$Key)
  accounting_display <- destination_accounting |>
    dplyr::left_join(attr(wide_master, "allocation_tables")$calibration |>
      dplyr::transmute(LA, Age, Display_gap = dc_count(MSDS_resident_count) - dc_count(Cohort_recorded_births)),
      by = c("LA", "Age")) |>
    dplyr::mutate(Not_represented = Display_gap) |> dplyr::select(-Display_gap)
  accounting_table <- dc_counts(accounting_display, c("Denominator", "Cluster_trusts", "Other_trusts",
    "Recorded_home_births", "Unknown_location", "No_recorded_delivery")) |>
    dplyr::select(LA_name, LA, Age, Denominator, Cluster_trusts,
      Other_trusts, Recorded_home_births, Unknown_location,
      No_recorded_delivery, Not_represented, Calibration_issue) |>
    base_table("Estimated resident maternity denominator by destination") |>
    gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age = "Maternal age",
      Denominator = "Estimated LA/age maternities", Cluster_trusts = "At cluster trusts",
      Other_trusts = "At other trusts", Recorded_home_births = "Home birth",
      Unknown_location = "Delivery location unknown", No_recorded_delivery = "No recorded delivery: estimated",
      Not_represented = "Births not represented in cohort", Calibration_issue = "Allocation check") |>
    gt::fmt_number(columns = c("Denominator", "Cluster_trusts", "Other_trusts", "Recorded_home_births",
      "Unknown_location", "No_recorded_delivery", "Not_represented"), decimals = 1)
  style_line_chart <- function(chart, y_title, percent = FALSE, ons_only = FALSE) {
    tick_years <- sort(unique(c(ons_reference_year,
      projection_years[projection_years %% 5 == 0], max(projection_years))))
    plotly::layout(chart,
      xaxis = list(title = "Year",
        tickmode = "array", tickvals = tick_years,
        ticktext = as.character(tick_years)),
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
      hover <- paste0(as.character(line$Year),
        "<br>Age: ", chart_bands[i], "<br>", title, ": ",
        if (percent) scales::percent(values, accuracy = 0.1) else scales::comma(values, accuracy = 1),
        "<br>", line$Coverage_note)
      chart <- plotly::add_trace(chart, x = line$Year, y = values, text = hover,
        type = "scatter", mode = "lines+markers", name = chart_bands[i],
        line = list(color = chart_colours[i], width = 2), marker = list(color = chart_colours[i], size = 4),
        hovertemplate = "%{text}<extra></extra>", connectgaps = FALSE)
    }
    style_line_chart(chart, title, percent)
  }
  forecast_outputs <- stats::setNames(lapply(organisations$Key, function(key) {
    org_shares <- feeder_shares[feeder_shares$Key == key, , drop = FALSE]
    org_flows <- dc_cells(feeder_projections[feeder_projections$Key == key, , drop = FALSE],
      c("Projected", "Numerator", "Denominator"), list(Share = c("Numerator", "Denominator")))
    org_ages <- age_projections[age_projections$Key == key, , drop = FALSE]
    org_totals <- maternity_projections[maternity_projections$Key == key, , drop = FALSE]
    org_totals <- dplyr::arrange(org_totals, Year)
    # Sum completed counts, then calculate the chart-only age-band change.
    ons_age_growth <- weighted_age_growth |>
      dplyr::filter(Key == key)
    growth_chart <- age_line_chart(ons_age_growth, "Growth_change", paste("% change from modelled", ons_reference_year), TRUE)
    incoming_chart <- age_line_chart(org_ages, "Projected", "Projected deliveries")
    # Tables retain every forecast year; their columns scroll horizontally.
    growth_table <- if (nrow(org_flows)) org_flows |>
      dplyr::mutate(ONS_year_label = as.character(Year)) |>
      dplyr::select(LA_name, LA, Age, ONS_age, Numerator, Denominator, Share, Treatment, ONS_year_label, ONS_change) |>
      tidyr::pivot_wider(names_from = ONS_year_label, values_from = ONS_change) |>
      dplyr::arrange(LA_name, Age) |>
      base_table("Change in ONS projected births by resident LA and age", paste("Relative to", ons_reference_year, "births (L); combined boundary ages share their source growth")) |>
      gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age = "Maternal age",
        Numerator = "Recorded deliveries here", Denominator = "Shared MSDS denominator", Share = "Allocation share",
        ONS_age = "ONS model age", Treatment = "Projection treatment") |>
      gt::fmt_number(columns = c("Numerator", "Denominator"), decimals = 0) |>
      gt::fmt_percent(columns = c("Share", as.character(projection_years)), decimals = 1) else
      base_table(data.frame(Note = "No available feeder-LA allocations from the supplied cohort"), "ONS change")
    incoming_table <- if (nrow(org_flows)) org_flows |>
      dplyr::select(LA_name, LA, Age, ONS_age, Treatment, Year_label, Projected) |>
      tidyr::pivot_wider(names_from = Year_label, values_from = Projected) |>
      dplyr::arrange(LA_name, Age) |>
      base_table("Projected deliveries by resident LA and age") |>
      gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age = "Maternal age",
                     ONS_age = "ONS model age", Treatment = "Projection treatment") |>
      gt::fmt_number(columns = as.character(projection_years), decimals = 0) else
      base_table(data.frame(Note = "No available feeder-LA allocations from the supplied cohort"), "Projected deliveries")
    if (!nrow(org_flows)) {
      growth_chart <- htmltools::div("No available feeder-LA allocations from the supplied cohort")
      incoming_chart <- htmltools::div("No available feeder-LA allocations from the supplied cohort")
    }
    if (nrow(org_flows) && !nrow(ons_age_growth)) {
      growth_chart <- htmltools::div("No matched ONS age-specific series; these delivery contributions are not projected.")
    }
    # Delivery charts: completed counts, compared with the recorded baseline.
    total_chart <- provider_total_chart(result$total_comparison$Delivery[
      result$total_comparison$Delivery$Key == key, ], "Deliveries / ONS live-birth base")
    change_chart <- plotly::plot_ly(org_totals, height = projection_chart_height, x = ~Year, y = ~Percent_change,
      type = "scatter", mode = "lines+markers", line = list(color = unname(organisation_line_colours(key)), width = 2),
      marker = list(color = unname(organisation_line_colours(key)), size = 5),
      text = ~paste0(Year_label, "<br>Change from matched recorded ", baseline_fy, ": ",
        ifelse(is.na(Percent_change), "n.a.", scales::percent(Percent_change, accuracy = 0.1)), "<br>", Coverage_note),
      hovertemplate = "%{text}<extra></extra>", showlegend = FALSE) |>
      style_line_chart(paste("% change from matched recorded", baseline_fy), TRUE)
    if (org_totals$Observed_matched_baseline[1] == 0) change_chart <- plotly::layout(change_chart,
      annotations = list(list(text = paste("No matched observed deliveries in", baseline_fy, "; relative change unavailable."),
        x = 0, y = 1.05, xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "left")),
      margin = list(t = 45))
    total_table <- org_totals |>
      dplyr::select(Year_label, Coverage_status, Projected) |>
      base_table("Projected deliveries") |>
      gt::cols_label(Year_label = "Year", Projected = "Projected deliveries") |>
      gt::fmt_number(columns = "Projected", decimals = 0)
    change_table <- org_totals |>
      dplyr::select(Year_label, Observed_baseline, Observed_matched_baseline, Coverage_status, Projected, Change, Percent_change) |>
      base_table(paste("Delivery change from matched recorded", baseline_fy)) |>
      gt::cols_label(Year_label = "Year", Observed_baseline = paste("Recorded", baseline_fy),
        Observed_matched_baseline = "Matched recorded", Coverage_status = "Coverage",
        Projected = "Projected deliveries", Change = "Count change", Percent_change = "% change") |>
      gt::fmt_number(columns = c("Observed_baseline", "Observed_matched_baseline", "Projected", "Change"), decimals = 0) |>
      gt::fmt_percent(columns = "Percent_change", decimals = 1)
    list(growth_chart = growth_chart, incoming_chart = incoming_chart,
      growth_table = growth_table, incoming_table = incoming_table,
      total_chart = total_chart, total_table = total_table,
      change_chart = change_chart, change_table = change_table)
  }), organisations$Key)

  # Use the same named and additional recorded providers in both chart sections.
  age_groups <- list(
    Trust = organisations[organisations$Level == "Trust", , drop = FALSE],
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
              yaxis = list(title = if (field == "growth_chart") paste("% change from modelled", ons_reference_year) else "Projected deliveries",
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

  # Shared total comparisons and change charts for both allocation views.
  total_charts <- lapply(age_groups, function(rows) provider_total_chart(
    result$total_comparison$Delivery[result$total_comparison$Delivery$Key %in% rows$Key, ],
    "Deliveries / ONS live-birth base"))
  combined_change_chart <- function(data, rows, title) {
    colours <- organisation_line_colours(rows$Key)
    chart <- plotly::plot_ly(height = projection_chart_height + 100)
    for (i in seq_len(nrow(rows))) {
      line <- data[data$Key == rows$Key[i], ] |> dplyr::arrange(Year)
      chart <- plotly::add_trace(chart, x = line$Year, y = line$Percent_change,
        type = "scatter", mode = "lines", name = rows$Organisation[i],
        line = list(color = colours[i], width = 2), connectgaps = FALSE,
        text = paste0(rows$Organisation[i], "<br>", line$Year, "<br>", title, ": ",
          ifelse(is.na(line$Percent_change), "Unavailable", scales::percent(line$Percent_change, accuracy = 0.1)),
          "<br>", line$Coverage_note),
        hovertemplate = "%{text}<extra></extra>")
    }
    style_line_chart(chart, title, percent = TRUE)
  }
  change_charts <- lapply(age_groups, function(rows) combined_change_chart(
    maternity_projections, rows, paste("% change from matched recorded", baseline_fy)))
  organisation_grid <- function(charts, rows) {
    htmltools::div(style = "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 24px;",
      htmltools::tagList(lapply(seq_len(nrow(rows)), function(i) {
        htmltools::div(style = "min-width: 0;", htmltools::h3(rows$Organisation[i]), charts[[rows$Key[i]]])
      })))
  }
  result$delivery_total_grids <- lapply(age_groups, function(rows) organisation_grid(
    lapply(forecast_outputs, function(x) x$total_chart), rows))
  result$delivery_change_grids <- lapply(age_groups, function(rows) organisation_grid(
    lapply(forecast_outputs, function(x) x$change_chart), rows))
  all_totals_table <- maternity_projections |>
    dplyr::select(Level, Organisation, Coverage_status, Year_label, Projected) |>
    tidyr::pivot_wider(names_from = Year_label, values_from = Projected) |>
    gt::gt(groupname_col = "Level", rowname_col = "Organisation") |>
    gt::tab_header(title = "Projected deliveries by organisation") |>
    gt::fmt_number(columns = as.character(projection_years), decimals = 0) |>
    gt::sub_missing(missing_text = "Unavailable") |>
    gt::tab_source_note(source_note = "Partial estimates retain available allocations; see provider coverage. Entirely unsupported estimates remain unavailable.") |>
    gt::tab_options(container.width = "100%", container.overflow.x = TRUE,
                    table.font.names = c("Open Sans", "Arial", "sans-serif"))
  all_changes_table <- maternity_projections |>
    dplyr::select(Level, Organisation, Coverage_status, Year_label, Percent_change) |>
    tidyr::pivot_wider(names_from = Year_label, values_from = Percent_change) |>
    gt::gt(groupname_col = "Level", rowname_col = "Organisation") |>
    gt::tab_header(title = paste("Delivery changes from matched recorded", baseline_fy)) |>
    gt::fmt_percent(columns = as.character(projection_years), decimals = 1) |>
    gt::sub_missing(missing_text = "n.a.") |>
    gt::tab_options(container.width = "100%", container.overflow.x = TRUE,
                    table.font.names = c("Open Sans", "Arial", "sans-serif"))
  result$allocation_accounting_table <- master |>
    dplyr::group_by(Year, Allocation_status) |>
    dplyr::summarise(Maternities = sum(Projected_maternities), .groups = "drop") |>
    dc_counts("Maternities") |>
    tidyr::pivot_wider(names_from = Year, values_from = Maternities, values_fill = 0) |>
    base_table("Modelled maternities: all modelled LAs and delivery destinations",
      "These categories reconcile to the shared adjusted total after all five modifiers. Trusts include providers outside this cluster.") |>
    gt::fmt_number(columns = where(is.numeric), decimals = 1)
  result$booking_allocation_accounting_table <- booking_rows |>
    dplyr::group_by(Year, Allocation_status) |>
    dplyr::summarise(Maternities = sum(Projected_bookings), .groups = "drop") |>
    dc_counts("Maternities") |>
    tidyr::pivot_wider(names_from = Year, values_from = Maternities, values_fill = 0) |>
    base_table("Modelled maternities: all antenatal booking destinations",
      "All modelled LAs. Providers, no recorded booking, unknown providers, the Booking Denominator Factor and unallocated volumes retain their own categories.") |>
    gt::cols_label(Allocation_status = "Booking category") |>
    gt::fmt_number(columns = where(is.numeric), decimals = 1)
  pathway_display <- master |>
    dplyr::left_join(wide_master |> dplyr::select(LA, Age, Year, MSDS_ONS_age_count), by = c("LA", "Age", "Year")) |>
    dplyr::left_join(dc_calibration(attr(wide_master, "allocation_tables")$calibration) |>
      dplyr::select(LA, Age, Display_pathway = Bookings_uplift_modifier), by = c("LA", "Age"))
  pathway_display$Age_weight <- ifelse(pathway_display$ONS_age %in% c(15, 44) & !is.na(pathway_display$Age),
    dc_ratio(pathway_display$MSDS_resident_count, pathway_display$MSDS_ONS_age_count), 1)
  pathway_display$Bookings_uplift_modifier <- pathway_display$Display_pathway
  pathway_display$Destination_proportion <- dc_ratio(pathway_display$MSDS_destination_count, pathway_display$Allocation_denominator)
  boundary <- pathway_display$ONS_age %in% c(15, 44) & !is.na(pathway_display$Age)
  pathway_display$Exact_age_live_births[boundary] <- dc_count(pathway_display$Exact_age_live_births[boundary])
  pathway_display <- dc_counts(pathway_display, c("Adjusted_maternities", "Projected_maternities"))
  result$pathway_table <- pathway_display |> dplyr::filter(Year == ons_reference_year,
      Delivery_destination %in% cluster_trusts) |>
    dplyr::select(LA, ONS_age, Age, Delivery_destination, ONS_live_births, Age_weight, Exact_age_live_births, Stillbirth_factor,
      Multiplicity_factor, Private_NHS_factor, Bookings_uplift_modifier, Adjusted_maternities, Destination_proportion, Projected_maternities) |>
    base_table(paste(ons_reference_year, "direct delivery allocations")) |>
    gt::cols_label(LA = "LA(R)", ONS_age = "ONS source age", Age = "Age (SY)",
      Delivery_destination = "Delivery trust", ONS_live_births = "ONS live births",
      Age_weight = "Age weight", Exact_age_live_births = "Live births after age split",
      Stillbirth_factor = "Stillbirth factor", Multiplicity_factor = "Multiplicity factor",
      Private_NHS_factor = "Private\u2013NHS modifier",
      Bookings_uplift_modifier = "Maternity pathway adjustment", Adjusted_maternities = "Adjusted maternities",
      Destination_proportion = "Delivery share", Projected_maternities = "Projected deliveries") |>
    gt::fmt_number(columns = c("ONS_live_births", "Age_weight", "Exact_age_live_births", "Stillbirth_factor", "Multiplicity_factor",
      "Bookings_uplift_modifier", "Adjusted_maternities", "Projected_maternities"), decimals = 3) |>
    gt::fmt_number(columns = "Private_NHS_factor", decimals = 6) |>
    gt::fmt_percent(columns = "Destination_proportion", decimals = 1)
  # 3.9d Booking charts: completed counts, grouped for presentation -------
  # Changes are relative to the MODELLED reference year, not observed bookings.
  booking_chart <- function(key, view) {
    if (view == "total") return(provider_total_chart(result$total_comparison$Booking[
      result$total_comparison$Booking$Key == key, ], "Antenatal bookings / ONS live-birth base"))
    data <- booking_total_projections[booking_total_projections$Key == key, ]
    incomplete <- key %in% booking_gaps$Key || anyNA(data$Projected)
    chart <- plotly::plot_ly(height = projection_chart_height)
    if (view %in% c("age", "age_change")) {
      data <- booking_age_projections[booking_age_projections$Key == key, ]
      for (band in age_bands) {
        band_data <- data[data$Age_band == band, ]
        if (!nrow(band_data)) next
        values <- if (view == "age_change") relative_change(band_data$Projected,
          band_data$Projected[band_data$Year == ons_reference_year]) else band_data$Projected
        chart <- plotly::add_trace(chart, x = band_data$Year, y = values,
          type = "scatter", mode = "lines", name = band,
          line = list(color = age_colours[match(band, age_bands)], width = 2),
          text = paste0("Age ", band, "<br>", band_data$Year, "<br>",
            if (view == "age_change") scales::percent(values, accuracy = 0.1) else scales::comma(values, accuracy = 1),
            "<br>", band_data$Coverage_note),
          hovertemplate = "%{text}<extra></extra>")
      }
      if (!nrow(data)) chart <- plotly::add_trace(chart,
        x = projection_years, y = rep(if (incomplete) NA_real_ else 0, length(projection_years)),
        type = "scatter", mode = "lines", name = if (incomplete) "Unavailable" else "No allocated bookings",
        line = list(color = main_chart_colour), showlegend = FALSE, hoverinfo = "skip")
    } else {
      values <- if (view == "change") data$Percent_change else data$Projected
      chart <- plotly::add_trace(chart, x = data$Year, y = values,
        type = "scatter", mode = "lines+markers", name = "Projected antenatal bookings",
        line = list(color = unname(organisation_line_colours(key)), width = 2),
        marker = list(color = unname(organisation_line_colours(key)), size = 5),
        text = paste0(data$Year, "<br>", if (view == "change")
          paste0("Change from modelled ", ons_reference_year, ": ", scales::percent(values, accuracy = 0.1)) else
          paste0("Projected antenatal bookings: ", scales::comma(values, accuracy = 1)), "<br>", data$Coverage_note),
        hovertemplate = "%{text}<extra></extra>")
    }
    chart <- chart |> style_line_chart(
      if (view %in% c("change", "age_change")) paste("% change from modelled ", ons_reference_year, sep = "") else
        "Projected antenatal bookings", percent = view %in% c("change", "age_change"))
    if (view %in% c("age", "age_change")) chart <- plotly::layout(chart, showlegend = TRUE,
      legend = list(orientation = "h", y = -0.25), margin = list(b = 100))
    if (incomplete) chart <- plotly::layout(chart,
      annotations = list(list(text = "Partial coverage: available allocations shown; see coverage table",
        x = 0, y = 1.05, xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "left")),
      margin = list(t = 50))
    if (!incomplete && !any(data$Projected > 0)) chart <- plotly::layout(chart,
      yaxis = list(range = c(0, 1), tickvals = 0), showlegend = FALSE,
      annotations = list(list(text = "No allocated bookings", x = 0.5, y = 0.5,
        xref = "paper", yref = "paper", showarrow = FALSE)))
    chart
  }
  booking_views <- c("total", "age", "change", "age_change")
  booking_charts <- stats::setNames(lapply(booking_views, function(view) {
    stats::setNames(lapply(organisations$Key, function(key) booking_chart(key, view)), organisations$Key)
  }), booking_views)
  # Include providers with bookings even when they have no recorded deliveries.
  booking_groups <- age_groups
  booking_grids <- lapply(booking_charts, function(charts)
    lapply(booking_groups, function(rows) organisation_grid(charts, rows)))
  result$booking_total_charts <- lapply(booking_groups, function(rows) provider_total_chart(
    result$total_comparison$Booking[result$total_comparison$Booking$Key %in% rows$Key, ],
    "Antenatal bookings / ONS live-birth base"))
  result$booking_change_charts <- lapply(booking_groups, function(rows) combined_change_chart(
    booking_total_projections, rows, paste("% change from modelled", ons_reference_year)))
  booking_table <- booking_total_projections |>
    dplyr::select(Level, Organisation, Coverage_status, Year, Projected, Percent_change) |>
    base_table("Projected antenatal bookings", paste("Change from modelled", ons_reference_year, "bookings")) |>
    gt::cols_label(Projected = "Projected antenatal bookings", Percent_change = paste("% change from modelled", ons_reference_year)) |>
    gt::fmt_number(columns = "Projected", decimals = 0) |>
    gt::fmt_percent(columns = "Percent_change", decimals = 1) |>
    gt::sub_missing(missing_text = "Unavailable")
  booking_age_table <- booking_age_projections |>
    dplyr::left_join(organisations, by = "Key") |>
    dplyr::select(Level, Organisation, Coverage_status, Age_band, Year, Projected) |>
    base_table("Projected bookings by age band") |>
    gt::cols_label(Age_band = "Age (B)", Projected = "Projected antenatal bookings") |>
    gt::fmt_number(columns = "Projected", decimals = 0) |>
    gt::sub_missing(missing_text = "Unavailable")
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
    target_episodes, completed, organisations, baseline_fy, max(projection_years),
    unavailable_flows = delivery_gaps |> dplyr::filter(Level == "Trust") |> dplyr::distinct(LA, Trust))
  result
}

# 3.9e Delivery flows: observed year and final projection side by side -----
# CONTEXT_ONLY: sum completed counts; these charts do not change the model.
build_projection_sankeys <- function(observed, projected, organisations,
    observed_year, final_year, main_las = projection_catchment_las,
    other_las = projection_other_feeders, unavailable_flows = NULL) {
  # Keep every recorded non-home delivery, including an unknown resident LA.
  actual <- observed |> dplyr::count(LA, Trust, name = "Observed")
  future <- projected |> dplyr::filter(Year == final_year) |>
    dplyr::group_by(LA, Trust) |>
    dplyr::summarise(Projected = sum_available_projection(Projected_maternities), .groups = "drop")
  raw_flows <- dplyr::full_join(actual, future, by = c("LA", "Trust")) |>
    dplyr::mutate(Projection_complete = !is.na(Projected),
      Observed = dplyr::coalesce(Observed, 0),
      Projected = dplyr::coalesce(Projected, 0))
  if (!is.null(unavailable_flows) && nrow(unavailable_flows)) {
    missing <- unavailable_flows |> dplyr::distinct(LA, Trust) |>
      dplyr::mutate(Incomplete_source = TRUE)
    raw_flows <- raw_flows |> dplyr::full_join(missing, by = c("LA", "Trust")) |>
      dplyr::mutate(Projection_complete = dplyr::coalesce(Projection_complete, FALSE) &
        !dplyr::coalesce(Incomplete_source, FALSE),
        Observed = dplyr::coalesce(Observed, 0), Projected = dplyr::coalesce(Projected, 0)) |>
      dplyr::select(-Incomplete_source)
  }
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
    dplyr::summarise(Observed = sum(Observed), Projected = sum(Projected),
      Projection_complete = all(Projection_complete), .groups = "drop") |>
    dplyr::filter(Observed > 0 | Projected > 0 | !Projection_complete)
  # Calculate change AFTER summing the displayed flow, not by averaging percentages.
  flows$Percent_change <- ifelse(flows$Projection_complete & flows$Observed > 0,
    flows$Projected / flows$Observed - 1, NA_real_)
  # Largest flows; fixed LA display order
  origins <- sankey_category_order(main_las, c(keep_las, "All other feeder LAs", "Unknown LA"))
  origins <- origins[origins %in% flows$LA]
  trusts <- organisations$Code[organisations$Level == "Trust"]
  trusts <- sankey_category_order(trust_target_order, trusts)
  trusts <- trusts[trusts %in% flows$Trust]
  la_labels <- projection_la_lookup$LA_name[match(origins, projection_la_lookup$LA)]
  la_labels[is.na(la_labels)] <- origins[is.na(la_labels)]
  trust_names <- organisations$Organisation[match(trusts, organisations$Code)]
  labels <- c(la_labels, paste0(trusts, " - ", trust_names))
  nodes <- dplyr::bind_rows(
    flows |> dplyr::group_by(Code = LA) |>
      dplyr::summarise(Observed = sum(Observed), Projected = sum(Projected),
        Projection_complete = all(Projection_complete), .groups = "drop") |>
      dplyr::slice(match(origins, Code)),
    flows |> dplyr::group_by(Code = Trust) |>
      dplyr::summarise(Observed = sum(Observed), Projected = sum(Projected),
        Projection_complete = all(Projection_complete), .groups = "drop") |>
      dplyr::slice(match(trusts, Code)))
  displayed_totals <- dc_count(c(sum(flows$Observed), sum(flows$Projected)))
  raw_display_flows <- flows
  raw_display_nodes <- nodes
  raw_display_nodes$Percent_change <- ifelse(nodes$Projection_complete & nodes$Observed > 0,
    nodes$Projected / nodes$Observed - 1, NA_real_)
  flows <- dc_counts(flows, c("Observed", "Projected"))
  flows$Percent_change <- ifelse(flows$Projection_complete, dc_divide(flows$Projected, flows$Observed) - 1, NA_real_)
  nodes <- dc_counts(nodes, c("Observed", "Projected"))
  nodes$Percent_change <- ifelse(nodes$Projection_complete & nodes$Observed > 0,
    nodes$Projected / nodes$Observed - 1, NA_real_)

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
  change_label <- function(x, complete) ifelse(!complete, "Unavailable: incomplete allocation",
    ifelse(is.finite(x), sprintf("%+.1f%%", 100 * x), "Not calculable: no recorded starting count"))
  flow_text <- paste0(la_labels[match(flows$LA, origins)], " to ",
    trust_names[match(flows$Trust, trusts)],
    "<br>", observed_year, " recorded: ", scales::comma(flows$Observed, accuracy = 1),
    "<br>", final_year, " allocated: ", scales::comma(flows$Projected, accuracy = 0.1),
    ifelse(flows$Projection_complete, "", " (incomplete)"),
    "<br>Change: ", change_label(flows$Percent_change, flows$Projection_complete))
  make_panel <- function(field) {
    visible <- flows[[field]] > 0
    if (!any(visible)) return(htmltools::p(if (field == "Projected" && any(!flows$Projection_complete))
      "No available projected delivery flows; allocation is incomplete." else "No deliveries in this panel."))
    chart_height <- max(540, 32 * length(origins))
    node_x <- c(rep(0.01, length(origins)), rep(0.99, length(trusts)))
    plotly::plot_ly(type = "sankey", arrangement = "fixed",
      height = chart_height,
      node = list(label = gsub("\n", "<br>", stringr::str_wrap(labels, 24)), pad = 16, thickness = 14,
        x = node_x,
        y = sankey_node_y(node_x, match(flows$LA[visible], origins) - 1L,
          length(origins) + match(flows$Trust[visible], trusts) - 1L,
          flows[[field]][visible], chart_height - 35, 16),
        color = if (field == "Observed") rep("#75A6CE", nrow(nodes)) else change_colour(nodes$Percent_change),
        line = list(color = "#666666", width = 0.4),
        customdata = paste0(labels, "<br>", observed_year, ": ", scales::comma(nodes$Observed),
          "<br>", final_year, " allocated: ", scales::comma(nodes$Projected, accuracy = 0.1),
          ifelse(nodes$Projection_complete, "", " (incomplete)"),
          "<br>Change: ", change_label(nodes$Percent_change, nodes$Projection_complete)),
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
        htmltools::p(paste(scales::comma(displayed_totals[1]), "deliveries at cluster trusts")), panels$observed),
      htmltools::div(htmltools::h4(paste0(final_year, " - available allocations")),
        htmltools::p(paste(scales::comma(displayed_totals[2], accuracy = 1), "deliveries allocated to cluster trusts",
          if (any(!flows$Projection_complete)) "(incomplete coverage)" else "")), panels$projected)),
    htmltools::div(class = "projection-change-key",
      htmltools::span(paste0(scales::percent(-colour_limit), " (decrease)")),
      htmltools::span(class = "projection-change-gradient", "0%"),
      htmltools::span(paste0("+", scales::percent(colour_limit), " (increase)"))),
    htmltools::p(class = "example-method-note",
      "Grey: incomplete allocation or no recorded starting count. Incomplete counts show available allocations only; no change is calculated. Ribbon colour = flow change; node colour = total change. ",
      "Ribbon widths show volumes within each panel; panels scale independently. Hover for both counts."))
  list(raw_flows = raw_flows, flows = raw_display_flows, nodes = raw_display_nodes,
    display_flows = flows, display_nodes = nodes, panels = panels, display = display,
    observed_year = observed_year, final_year = final_year, colour_limit = colour_limit)
}

# 3.9f Variation around the three-year model inputs -----------------------
# CONTEXT_ONLY: annual comparisons; no rates or projections are replaced.
build_input_variation_review <- function(cohort, resident_totals, registration_counts,
    pooled, providers = cluster_trusts) {
  years <- pooled$years
  # Descriptive annual shares use the shared pregnancy FY. They do not
  # change the model's full-cohort calibration or allocate the residual group.
  episodes <- prepare_report_cohort(cohort)
  episodes$Delivery_destination <- projection_delivery_destination(episodes)
  episodes$Booking_destination <- projection_booking_destination(episodes)
  valid <- episodes |> dplyr::filter(!LA %in% c("Unknown LA", "L99999999", ""), Age %in% pooled$ages)
  annual_residents <- prepare_la_deliveries(resident_totals, missing_count_zero = TRUE) |>
    dplyr::filter(as.character(birth_yr) %in% years) |>
    dplyr::transmute(LA = report_la_code(la_of_mother), Age = report_age(age_at_birth_mother),
      FY = as.character(birth_yr), MSDS_resident_count = report_number(count)) |>
    dplyr::filter(Age %in% pooled$ages, LA %in% pooled$residents$LA) |>
    dplyr::group_by(LA, Age, FY) |>
    dplyr::summarise(MSDS_resident_count = sum(MSDS_resident_count), .groups = "drop")
  annual_delivery <- valid |> dplyr::filter(Year %in% years, Birth_recorded) |>
    dplyr::count(LA, Age, FY = Year, Destination = Delivery_destination, name = "Count") |>
    dplyr::full_join(annual_residents, by = c("LA", "Age", "FY"))
  annual_booking <- valid |> dplyr::filter(Pregnancy_FY %in% years) |>
    dplyr::mutate(FY = Pregnancy_FY) |>
    dplyr::count(LA, Age, FY, Destination = Booking_destination, name = "Count") |>
    dplyr::group_by(LA, Age, FY) |> dplyr::mutate(MSDS_booking_total = sum(Count)) |>
    dplyr::ungroup()
  pooled_delivery <- pooled$delivery_observed |>
    dplyr::left_join(pooled$residents, by = c("LA", "Age")) |>
    dplyr::mutate(Proportion = Count / MSDS_resident_count, Pooled_denominator = MSDS_resident_count)
  pooled_booking <- pooled$booking_observed |>
    dplyr::left_join(pooled$cohort_counts, by = c("LA", "Age")) |>
    dplyr::mutate(Proportion = Count / Cohort_pregnancies, Pooled_denominator = Cohort_pregnancies)

  # Compare the SAME LA(R), Age (SY) and destination with its pooled proportion.
  # Zero deliveries to a trust count as zero; a zero resident denominator is undefined.
  allocation_variation <- function(reference, yearly, denominator) {
    reference <- reference |> dplyr::filter(Destination %in% providers, Count > 0) |>
      dplyr::select(LA, Age, Destination, Pooled = Proportion, Pooled_count = Count, Pooled_denominator)
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
  delivery <- allocation_variation(pooled_delivery, annual_delivery, "MSDS_resident_count")
  booking <- allocation_variation(pooled_booking, annual_booking, "MSDS_booking_total")

  # Boundary age mix: local exact-age share within <=15 or 44+, for each LA.
  boundary <- pooled$residents |>
    dplyr::mutate(ONS_age = report_ons_age(Age)) |>
    dplyr::filter(ONS_age %in% c(15, 44)) |>
    dplyr::group_by(LA, ONS_age) |>
    dplyr::mutate(Pooled_count = MSDS_resident_count, Pooled_denominator = sum(MSDS_resident_count),
      Pooled = MSDS_resident_count / Pooled_denominator) |>
    dplyr::ungroup() |> dplyr::select(LA, Age, ONS_age, Pooled, Pooled_count, Pooled_denominator)
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
    data <- dc_cells(data, c("Count", "Denominator", "Pooled_count", "Pooled_denominator"),
      list(Annual = c("Count", "Denominator"), Pooled = c("Pooled_count", "Pooled_denominator")))
    data$Departure_pp <- 100 * (data$Annual - data$Pooled)
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
        text = paste0(rows$LA, ", Age (SY) ", rows$Age,
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
  # Show every annual point and the ratio from pooled counts used by the model.
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
      x = ons$factors$age, y = ons$factors[[field]], name = "Pooled ratio used",
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
  list(ons_annual = ons$annual, ons_pooled = ons$factors,
    delivery = delivery, booking = booking, age_mix = age_mix, coverage = coverage,
    delivery_chart = departure_plot(delivery, "Destination", "Delivery trust"),
    booking_chart = departure_plot(booking, "Destination", "Booking trust"),
    age_mix_chart = departure_plot(age_mix, "Age", "Age (SY) within the combined ONS boundary groups"),
    stillbirth_chart = ons_plot("Stillbirth_ratio", "Stillbirths / births (L)"),
    multiplicity_chart = ons_plot("Multiplicity_factor", "Maternities / births (L+S)"))
}

# 3.10 Retrospective comparison with recorded deliveries -----------------
# Pooled calibration includes the comparison years; not independent validation.
build_projection_backtest <- function() {
  # Section 3.5.2: compare with actual history.
  # Apply the same full-cohort proportions to historical ONS counts.
  # This is a retrospective diagnostic using current ONS/registration data, not a vintage backtest.
  historical <- build_first_draft_projections(baseline_fy = "2023/24",
    ons_reference_year = 2023L, projection_end = 2025L, prepare_outputs = FALSE)
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
      Year_label = paste0(Year, "/", substr(Year + 1L, 3, 4)), Modelled = Projected, Coverage_status, Coverage_note) |>
    dplyr::left_join(observed_totals, by = c("Key", "Year_label")) |>
    dplyr::left_join(historical$matched_history |> dplyr::filter(Allocation == "Delivery") |>
      dplyr::select(Key, Year = Event_year, Observed_matched), by = c("Key", "Year")) |>
    dplyr::mutate(Observed_matched = dplyr::coalesce(Observed_matched, 0L),
      Difference = Modelled - Observed_matched,
      Percent_difference = ifelse(Observed_matched > 0, Difference / Observed_matched, NA_real_)) |>
    dplyr::arrange(match(Key, organisations$Key), Year)
  # Retrospective charts retain every recorded delivery trust across the three years.
  raw_comparison <- comparison
  comparison <- dc_coverage_note(dc_counts(comparison, c("Observed", "Observed_matched", "Modelled")))
  comparison$Difference <- comparison$Modelled - comparison$Observed_matched
  comparison$Percent_difference <- dc_divide(comparison$Modelled, comparison$Observed_matched) - 1
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
        text = paste0("ONS ", data$Year, "<br>Modelled: ", scales::comma(data$Modelled, accuracy = 1),
          "<br>", data$Coverage_note, "<br>Matched observed: ", data$Observed_matched,
          "<br>Difference from matched observed: ", ifelse(is.na(data$Percent_difference), "n.a.",
                                                   scales::percent(data$Percent_difference, accuracy = 0.1))),
        hovertemplate = "%{text}<extra></extra>") |>
      plotly::layout(xaxis = list(title = "Comparison year",
        tickmode = "array", tickvals = as.integer(substr(years, 1, 4)), ticktext = years),
        yaxis = list(title = "Deliveries", tickformat = ",.0f", rangemode = "tozero"),
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
  metrics <- c("Observed", "Observed_matched", "Modelled", "Difference", "Percent_difference")
  wide <- comparison |>
    dplyr::select(Level, Organisation, Coverage_status, Year_label, dplyr::all_of(metrics)) |>
    tidyr::pivot_wider(names_from = Year_label, values_from = dplyr::all_of(metrics), names_sep = "_")
  column_order <- unlist(lapply(years, function(year) paste0(metrics, "_", year)), use.names = FALSE)
  wide <- dplyr::select(wide, Level, Organisation, Coverage_status, dplyr::all_of(column_order))
  percent_columns <- paste0("Percent_difference_", years)
  count_columns <- setdiff(column_order, percent_columns)
  labels <- stats::setNames(rep(c("All MSDS observed", "Matched observed", "ONS-based model", "Model − matched", "% difference"),
                                  length(years)), column_order)
  all_table <- gt::gt(wide, groupname_col = "Level", rowname_col = "Organisation") |>
    gt::tab_header(title = "Recorded versus projected deliveries: all organisations") |>
    gt::cols_label(.list = as.list(labels)) |>
    gt::fmt_number(columns = dplyr::all_of(count_columns), decimals = 0) |>
    gt::fmt_percent(columns = dplyr::all_of(percent_columns), decimals = 1) |>
    gt::sub_missing(missing_text = "n.a.") |>
    gt::tab_source_note(source_note = "% difference = (modelled − matched observed) / matched observed × 100. Observed retains all recorded deliveries; Observed_matched uses the model's usable LA/age cells. Partial modelled counts retain available allocations. n.a. identifies a zero matched observed count or no usable estimate.") |>
    gt::tab_options(table.font.names = c("Open Sans", "Arial", "sans-serif"),
                    container.width = "100%", container.overflow.x = TRUE,
                    table.width = gt::px(1500), table.font.size = gt::px(12))
  for (year in years) all_table <- gt::tab_spanner(all_table, label = year,
    columns = paste0(metrics, "_", year), id = paste0("backtest-", year), gather = FALSE)
  coverage_table <- make_projection_coverage_table(historical$coverage, "2023/24")
  coverage_details_table <- dc_counts(historical$coverage_details, "N") |>
    dplyr::select(Level, Organisation, LA_name, LA, Age, Reason, N) |>
    gt::gt() |>
    gt::tab_header(title = "2023/24 records without an available allocation") |>
    gt::cols_label(LA_name = "Resident LA", LA = "LA code", Age = "Maternal age",
      Reason = "Reason", N = "Observed deliveries") |>
    gt::fmt_number(columns = "N", decimals = 0)
  list(note = historical$note, organisations = organisations, baseline = historical,
       data = raw_comparison, display_data = comparison, outputs = outputs, trust_grid = trust_grid,
       icb_cluster_grid = icb_cluster_grid, all_table = all_table,
       coverage = historical$coverage, coverage_details = historical$coverage_details,
       coverage_table = coverage_table, coverage_details_table = coverage_details_table)
}

# 3.11 Prepare and save the model report outputs -------------------------
# Save only after forward projections and the retrospective comparison finish.
first_draft_projections <- build_first_draft_projections(history_master = projection_master_all_years)
projection_backtest <- build_projection_backtest()
first_draft_projections$backtest <- projection_backtest
first_draft_projections$age_boundary_review <- build_msds_age_review(
  final_cohort_2326, first_draft_projections, projection_backtest$baseline,
  trust_labels, cluster_trusts)
first_draft_projections$input_variation <- build_input_variation_review(
  final_cohort_2326, total_births_by_LA_2326, registration_counts,
  first_draft_projections$allocation_tables)
save_report_rds(first_draft_projections, ".projections.rds")

