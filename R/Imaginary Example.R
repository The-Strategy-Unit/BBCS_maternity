# Standalone fictional example: no MSDS, Azure or real ONS data is used.
# The report shows every input and calculation needed to follow the example.

make_imaginary_example <- function() {
  table_style <- function(data) {
    gt::gt(data) |>
      gt::tab_options(table.font.names = c("Open Sans", "Arial", "sans-serif"),
                      container.width = "100%", container.overflow.x = TRUE)
  }
  observed <- data.frame(Year = c("2023/24", "2024/25", "2025/26"), Maternities = c(100, 105, 110))
  destinations <- data.frame(
    LA = c("B: catchment", "C: outside catchment"), Resident_maternities = c(100, 100),
    Trust_A = c(80, 30), Home = c(5, 5), Other_trusts = c(15, 65))
  mapping <- data.frame(
    Cell = c("b-younger", "b-older", "c-younger", "c-older"),
    LA = c("B", "B", "C", "C"), Age = c("25-29", "30-34", "25-29", "30-34"),
    All_LA_maternities = c(50, 50, 50, 50), At_Trust_A = c(40, 40, 20, 10),
    Modifier = c(0.5, 1, 0.5, 1), Reference_births = c(100, 50, 100, 50),
    Added_births_per_year = c(10, 10, 0, 5))
  mapping$Share <- mapping$At_Trust_A / mapping$All_LA_maternities
  modifiers <- data.frame(Age = c("25-29", "30-34"),
                          Babies_per_maternity = c(2, 1), Modifier = c(0.5, 1))

  calculations <- do.call(rbind, lapply(0:10, function(year) {
    data <- mapping
    data$Future_year <- year
    data$Year <- if (year == 0) "2025/26: actual" else paste("Year", year)
    # Fixed additions create easy, invented ONS trajectories, not compounded rates.
    data$ONS_births <- data$Reference_births + year * data$Added_births_per_year
    data$ONS_maternities <- data$ONS_births * data$Modifier
    data$Reference_maternities <- data$Reference_births * data$Modifier
    data$Growth <- data$ONS_maternities / data$Reference_maternities
    data$Projected_flow <- data$At_Trust_A * data$Growth
    data
  }))
  totals <- do.call(rbind, lapply(0:10, function(year) {
    data <- calculations[calculations$Future_year == year, ]
    data.frame(Future_year = year, Year = data$Year[1],
      B_younger = data$Projected_flow[data$Cell == "b-younger"],
      B_older = data$Projected_flow[data$Cell == "b-older"],
      C_younger = data$Projected_flow[data$Cell == "c-younger"],
      C_older = data$Projected_flow[data$Cell == "c-older"], Total = sum(data$Projected_flow))
  }))
  totals$Percent_change <- totals$Total / 110 - 1
  first_year <- calculations[calculations$Future_year == 1, ]
  first_year_display <- data.frame(
    LA = first_year$LA, Age = first_year$Age,
    Births = paste(first_year$Reference_births, "to", first_year$ONS_births),
    Multiply_by = first_year$Modifier,
    Maternities = paste(first_year$Reference_maternities, "to", first_year$ONS_maternities),
    Growth = first_year$Growth - 1,
    Trust_A = paste(first_year$At_Trust_A, "to", first_year$Projected_flow))

  projection_table <- totals[, c("Year", "B_younger", "B_older", "C_younger", "C_older", "Total", "Percent_change")] |>
    table_style() |>
    gt::cols_label(B_younger = "25-29", B_older = "30-34", C_younger = "25-29", C_older = "30-34",
      Total = "Trust A total", Percent_change = "% change from 110") |>
    gt::tab_spanner(label = "Catchment LA B", columns = c("B_younger", "B_older")) |>
    gt::tab_spanner(label = "Outside LA C", columns = c("C_younger", "C_older")) |>
    gt::fmt_number(columns = c("B_younger", "B_older", "C_younger", "C_older", "Total"), decimals = 0) |>
    gt::fmt_percent(columns = "Percent_change", decimals = 1)
  ons_wide <- data.frame(Year = ifelse(totals$Future_year == 0, "Reference: mid-2026",
    paste0("Year ", totals$Future_year, ": mid-", 2026 + totals$Future_year)))
  for (cell in mapping$Cell) ons_wide[[cell]] <- calculations$ONS_births[calculations$Cell == cell]
  ons_table <- ons_wide |>
    table_style() |>
    gt::cols_label(.list = list(`b-younger` = "25-29", `b-older` = "30-34",
                                `c-younger` = "25-29", `c-older` = "30-34")) |>
    gt::tab_spanner(label = "Catchment LA B: ONS births", columns = c("b-younger", "b-older")) |>
    gt::tab_spanner(label = "Outside LA C: ONS births", columns = c("c-younger", "c-older")) |>
    gt::fmt_number(columns = mapping$Cell, decimals = 0)

  chart <- plotly::plot_ly(height = 430) |>
    plotly::add_trace(x = -2:0, y = observed$Maternities, type = "scatter", mode = "lines+markers",
      name = "Observed", line = list(color = "#005EB8", width = 3),
      text = paste0(observed$Year, "<br>Actual maternities: ", observed$Maternities),
      hovertemplate = "%{text}<extra></extra>") |>
    plotly::add_trace(x = totals$Future_year, y = totals$Total, type = "scatter", mode = "lines+markers",
      name = "Projected", line = list(color = "#D55E00", width = 3, dash = "dash"),
      text = paste0(totals$Year, "<br>Maternities: ", totals$Total,
        "<br>Change from 110: ", scales::percent(totals$Percent_change, accuracy = 0.1)),
      hovertemplate = "%{text}<extra></extra>") |>
    plotly::layout(xaxis = list(title = "Observed financial year / fictional future year",
      tickmode = "array", tickvals = -2:10,
      ticktext = c("23/24", "24/25", "25/26", as.character(1:10))),
      yaxis = list(title = "Maternities at Trust A", tickformat = ",.0f", rangemode = "tozero"),
      legend = list(orientation = "h", x = 0, y = -0.2),
      margin = list(l = 80, r = 20, t = 20, b = 100), font = list(family = "Arial")) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)

  list(
    observed = observed, mapping = mapping, calculations = calculations, totals = totals,
    observed_table = observed |> table_style(),
    destinations_table = destinations |>
      table_style() |>
      gt::cols_label(LA = "Mother lives in", Resident_maternities = "All resident maternities",
                     Trust_A = "At Trust A", Home = "Home births", Other_trusts = "At other trusts"),
    mapping_table = mapping[, c("LA", "Age", "All_LA_maternities", "At_Trust_A", "Share")] |>
      table_style() |>
      gt::cols_label(LA = "Resident LA", Age = "Age band", All_LA_maternities = "All LA/age maternities",
                     At_Trust_A = "At Trust A", Share = "Share sent to Trust A") |>
      gt::fmt_percent(columns = "Share", decimals = 0),
    modifiers_table = modifiers |>
      table_style() |>
      gt::cols_label(Age = "Age band", Babies_per_maternity = "Imaginary babies per maternity",
                     Modifier = "Multiply ONS births by") |>
      gt::fmt_number(columns = "Modifier", decimals = 1),
    first_year_table = first_year_display |>
      table_style() |>
      gt::cols_label(LA = "Resident LA", Age = "Age band", Births = "ONS births: base to Year 1",
        Multiply_by = "x modifier", Maternities = "ONS maternities: base to Year 1",
        Growth = "Growth", Trust_A = "Trust A flow: actual to projected") |>
      gt::fmt_number(columns = "Multiply_by", decimals = 1) |>
      gt::fmt_percent(columns = "Growth", decimals = 0),
    projection_table = projection_table, ons_table = ons_table, chart = chart)
}

imaginary_example <- make_imaginary_example()
