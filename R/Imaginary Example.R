# Imaginary Example: show the projection method with small fictional numbers.
# The only real input is public ONS growth, rounded to the nearest five percentage points.

# Settings: reference year, future years and rounding
example_reference_year <- 2026L
example_horizon <- 10L
example_rounding <- 0.05
example_colours <- c(Observed = "#005EB8", Projected = "#D55E00")

# Fictional inputs: past totals, delivery destinations and the LA/age shares
observed_deliveries <- data.frame(Year = c("2023/24", "2024/25", "2025/26"), Maternities = c(100, 105, 110))
example_destinations <- data.frame(
  LA = c("B: catchment", "C: outside catchment"), Resident_maternities = c(100, 100),
  Trust_A = c(80, 30), Home = c(5, 5), Other_trusts = c(15, 65))
example_feeder_inputs <- data.frame(
  Cell = c("b-younger", "b-older", "c-younger", "c-older"),
  LA = c("B", "B", "C", "C"), Age = c("25-29", "30-34", "25-29", "30-34"),
  All_LA_maternities = c(50, 50, 50, 50), At_Trust_A = c(40, 40, 20, 10),
  Modifier = c(0.5, 1, 0.5, 1), Reference_births = c(100, 50, 100, 50))
example_modifiers <- data.frame(Age = c("25-29", "30-34"),
                        Babies_per_maternity = c(2, 1), Modifier = c(0.5, 1))

make_imaginary_example <- function() {
  # Step 1: get Method 1 ONS maternity totals for the six catchment LAs.
  source("R/ons_data.R", local = TRUE)
  areas <- read_area_config()
  public_modifiers <- read_national_multiplicity() |>
    dplyr::mutate(age_band = ifelse(as.character(age_band) %in% c("40-44", "45+"),
                                   "40+", as.character(age_band))) |>
    births_to_maternities_modifiers()
  public_totals <- project_maternities(read_snpp_births(), public_modifiers,
                                      local_authorities = areas$area_name) |>
    dplyr::filter(year %in% c(example_reference_year, example_reference_year + example_horizon)) |>
    dplyr::group_by(year) |>
    dplyr::summarise(Maternities = sum(projected_maternities), .groups = "drop") |>
    dplyr::arrange(year)

  # Step 2: round the public ten-year change to the nearest five percentage points.
  ons_change <- public_totals$Maternities[2] / public_totals$Maternities[1] - 1
  ten_year_change <- round(ons_change / example_rounding) * example_rounding

  table_style <- function(data) {
    gt::gt(data) |>
      gt::tab_options(table.font.names = c("Open Sans", "Arial", "sans-serif"),
                      container.width = "100%", container.overflow.x = TRUE)
  }

  # Step 3: find each LA/age group's share sent to Trust A.
  feeder_shares <- example_feeder_inputs
  feeder_shares$Share <- feeder_shares$At_Trust_A / feeder_shares$All_LA_maternities

  # Step 4: convert fictional ONS births to maternities, then calculate growth.
  # Apply that growth to the actual Trust A count in each LA/age group.
  projection_steps <- do.call(rbind, lapply(0:example_horizon, function(year) {
    age_flows <- feeder_shares
    age_flows$Future_year <- year
    age_flows$Year <- if (year == 0) "2025/26: actual" else paste("Year", year)
    # Use one simple linear path; its last year reaches the rounded ONS change.
    age_flows$Future_multiplier <- 1 + ten_year_change * year / example_horizon
    age_flows$ONS_births <- age_flows$Reference_births * age_flows$Future_multiplier
    age_flows$ONS_maternities <- age_flows$ONS_births * age_flows$Modifier
    age_flows$Reference_maternities <- age_flows$Reference_births * age_flows$Modifier
    age_flows$Growth <- age_flows$ONS_maternities / age_flows$Reference_maternities
    age_flows$Projected_flow <- age_flows$At_Trust_A * age_flows$Growth
    age_flows
  }))

  # Step 5: add the four LA/age contributions to get Trust A's total each year.
  projected_totals <- do.call(rbind, lapply(0:example_horizon, function(year) {
    year_flows <- projection_steps[projection_steps$Future_year == year, ]
    data.frame(Future_year = year, Year = year_flows$Year[1], Future_multiplier = year_flows$Future_multiplier[1],
      B_younger = year_flows$Projected_flow[year_flows$Cell == "b-younger"],
      B_older = year_flows$Projected_flow[year_flows$Cell == "b-older"],
      C_younger = year_flows$Projected_flow[year_flows$Cell == "c-younger"],
      C_older = year_flows$Projected_flow[year_flows$Cell == "c-older"], Total = sum(year_flows$Projected_flow))
  }))
  projected_totals$Percent_change <- projected_totals$Total / 110 - 1

  # Step 6: make the tables and chart used by the worked example.
  projection_table <- projected_totals[, c("Year", "Future_multiplier", "B_younger", "B_older", "C_younger", "C_older", "Total", "Percent_change")] |>
    table_style() |>
    gt::cols_label(B_younger = "25-29", B_older = "30-34", C_younger = "25-29", C_older = "30-34",
      Future_multiplier = "Common future multiplier", Total = "Trust A total", Percent_change = "% change from 110") |>
    gt::tab_spanner(label = "Catchment LA B", columns = c("B_younger", "B_older")) |>
    gt::tab_spanner(label = "Outside LA C", columns = c("C_younger", "C_older")) |>
    gt::fmt_number(columns = c("B_younger", "B_older", "C_younger", "C_older", "Total"), decimals = 0) |>
    gt::fmt_number(columns = "Future_multiplier", decimals = 3) |>
    gt::fmt_percent(columns = "Percent_change", decimals = 1)
  ons_wide <- data.frame(Year = ifelse(projected_totals$Future_year == 0,
    paste0("Reference: mid-", example_reference_year),
    paste0("Year ", projected_totals$Future_year, ": mid-", example_reference_year + projected_totals$Future_year)))
  for (cell in example_feeder_inputs$Cell) ons_wide[[cell]] <- projection_steps$ONS_births[projection_steps$Cell == cell]
  ons_table <- ons_wide |>
    table_style() |>
    gt::cols_label(.list = list(`b-younger` = "25-29", `b-older` = "30-34",
                                `c-younger` = "25-29", `c-older` = "30-34")) |>
    gt::tab_spanner(label = "Catchment LA B: ONS births", columns = c("b-younger", "b-older")) |>
    gt::tab_spanner(label = "Outside LA C: ONS births", columns = c("c-younger", "c-older")) |>
    gt::fmt_number(columns = example_feeder_inputs$Cell, decimals = 1)

  chart <- plotly::plot_ly(height = 430) |>
    plotly::add_trace(x = seq_len(nrow(observed_deliveries)) - nrow(observed_deliveries), y = observed_deliveries$Maternities, type = "scatter", mode = "lines+markers",
      name = "Observed", line = list(color = unname(example_colours["Observed"]), width = 3),
      text = paste0(observed_deliveries$Year, "<br>Actual maternities: ", observed_deliveries$Maternities),
      hovertemplate = "%{text}<extra></extra>") |>
    plotly::add_trace(x = projected_totals$Future_year, y = projected_totals$Total, type = "scatter", mode = "lines+markers",
      name = "Projected", line = list(color = unname(example_colours["Projected"]), width = 3, dash = "dash"),
      text = paste0(projected_totals$Year, "<br>Maternities: ", projected_totals$Total,
        "<br>Change from 110: ", scales::percent(projected_totals$Percent_change, accuracy = 0.1)),
      hovertemplate = "%{text}<extra></extra>") |>
    plotly::layout(xaxis = list(title = "Observed financial year / fictional future year",
      tickmode = "array", tickvals = -2:example_horizon,
      ticktext = c("23/24", "24/25", "25/26", as.character(seq_len(example_horizon)))),
      yaxis = list(title = "Maternities at Trust A", tickformat = ",.0f", rangemode = "tozero"),
      legend = list(orientation = "h", x = 0, y = -0.2),
      margin = list(l = 80, r = 20, t = 20, b = 100), font = list(family = "Arial")) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)

  list(
    observed = observed_deliveries, mapping = feeder_shares, calculations = projection_steps, totals = projected_totals,
    ons_change = ons_change, ten_year_change = ten_year_change, public_totals = public_totals,
    public_areas = areas$area_name,
    observed_table = observed_deliveries |> table_style(),
    destinations_table = example_destinations |>
      table_style() |>
      gt::cols_label(LA = "Mother lives in", Resident_maternities = "MSDS-recorded resident maternities",
                     Trust_A = "At Trust A", Home = "Home births", Other_trusts = "At other trusts"),
    mapping_table = feeder_shares[, c("LA", "Age", "All_LA_maternities", "At_Trust_A", "Share")] |>
      table_style() |>
      gt::cols_label(LA = "Resident LA", Age = "Age band", All_LA_maternities = "MSDS LA/age maternities",
                     At_Trust_A = "At Trust A", Share = "Share sent to Trust A") |>
      gt::fmt_percent(columns = "Share", decimals = 0),
    modifiers_table = example_modifiers |>
      table_style() |>
      gt::cols_label(Age = "Age band", Babies_per_maternity = "Imaginary babies per maternity",
                     Modifier = "Multiply ONS births by") |>
      gt::fmt_number(columns = "Modifier", decimals = 1),
    projection_table = projection_table, ons_table = ons_table, chart = chart)
}

imaginary_example <- make_imaginary_example()
# Save the finished example so rendering does not repeat the ONS calculations.
saveRDS(imaginary_example, ".imaginary_example.rds")
