# Compare the disclosure-controlled modelled and observed lines shown in the charts.
retrospective_error_data <- function(display_data) {
  stopifnot(!anyDuplicated(display_data[c("Key", "Year")]))
  display_data |>
    dplyr::mutate(error = dplyr::if_else(is.finite(Modelled) & is.finite(Observed),
        Modelled - Observed, NA_real_),
      absolute_error = abs(error),
      pct_error = dc_divide(error, Observed), ape = abs(pct_error)) |>
    dplyr::arrange(match(Level, c("Cluster", "ICB", "Trust")), Organisation, Year)
}

retrospective_error_summary <- function(annual) {
  annual |>
    dplyr::group_by(Key, Level, Organisation) |>
    dplyr::summarise(
      mae = dc_divide(sum(absolute_error, na.rm = TRUE), sum(is.finite(absolute_error))),
      mape = dc_divide(sum(ape, na.rm = TRUE), sum(is.finite(ape))),
      .groups = "drop") |>
    dplyr::arrange(match(Level, c("Cluster", "ICB", "Trust")), Organisation)
}

retrospective_error_table <- function(title, headings, rows) {
  htmltools::div(class = "retro-error-scroll",
    htmltools::tags$table(class = "retro-error-table",
      htmltools::tags$caption(title),
      htmltools::tags$thead(htmltools::tags$tr(lapply(headings, function(x)
        htmltools::tags$th(scope = "col", x)))),
      htmltools::tags$tbody(rows)))
}

retrospective_annual_errors <- function(annual, activity) {
  years <- unique(annual[c("Year", "Year_label")]) |> dplyr::arrange(Year)
  cell <- function(row) {
    if (!nrow(row) || !is.finite(row$error))
      return(htmltools::tags$td(class = "retro-error-missing", title = "No comparison available", "\u2014"))
    difference <- if (row$error == 0) "Exact match" else paste(
      scales::comma(row$absolute_error, accuracy = 1), if (row$error > 0) "over" else "under")
    state <- if (is.na(row$ape)) "missing" else if (row$error > 0) "over" else if (row$error < 0) "under" else "exact"
    sign <- switch(state, over = "+", under = "\u2212", "")
    percentage <- if (is.na(row$ape)) "\u2014" else paste0(sign, scales::percent(row$ape, accuracy = 0.1))
    details <- c(paste("Observed:", scales::comma(row$Observed, accuracy = 1)),
      paste("Modelled:", scales::comma(row$Modelled, accuracy = 1)), difference,
      if (is.na(row$ape)) "Percentage undefined")
    htmltools::tags$td(class = paste("retro-error-cell", paste0("retro-error-", state)), tabindex = 0,
      `aria-label` = paste(c(row$Organisation, row$Year_label, percentage, details), collapse = ", "),
      htmltools::tags$strong(percentage),
      htmltools::span(class = "retro-error-tooltip", `aria-hidden` = "true",
        lapply(details, htmltools::span)))
  }
  rows <- lapply(unique(annual$Key), function(key) {
    org <- annual[annual$Key == key, ]
    htmltools::tags$tr(htmltools::tags$th(scope = "row", org$Organisation[1]),
      lapply(years$Year, function(y) cell(org[org$Year == y, ])))
  })
  retrospective_error_table(paste("Modelled", activity, "above or below observed (%)"),
    c("Organisation", years$Year_label), rows)
}

retrospective_period_errors <- function(annual, activity) {
  summary <- retrospective_error_summary(annual)
  pct <- function(x) if (is.finite(x)) scales::percent(x, accuracy = 0.1) else "\u2014"
  rows <- lapply(seq_len(nrow(summary)), function(i) {
    row <- summary[i, ]
    htmltools::tags$tr(htmltools::tags$th(scope = "row", row$Organisation),
      htmltools::tags$td(if (is.na(row$mae)) "\u2014" else scales::comma(row$mae, accuracy = .1)),
      htmltools::tags$td(pct(row$mape)))
  })
  retrospective_error_table(paste("Average annual error in", activity),
    c("Organisation", "Average absolute error (pregnancies)", "Average absolute error (%)"), rows)
}

retrospective_error_trends <- function(annual, bookings = NULL) {
  combined <- !is.null(bookings)
  all_rows <- dplyr::bind_rows(annual, bookings)
  years <- unique(all_rows[c("Year", "Year_label")]) |> dplyr::arrange(Year)
  colours <- organisation_line_colours(unique(all_rows$Key))
  limit <- max(abs(all_rows$pct_error), 0, na.rm = TRUE)
  limit <- if (limit > 0) 1.08 * limit else 0.01
  panel <- function(rows, title, activity = NULL) {
    plot <- plotly::plot_ly(height = if (combined) 235 else 430)
    for (key in unique(rows$Key)) {
      org <- rows[rows$Key == key, ]
      line <- org[match(years$Year, org$Year), ]
      plot <- plotly::add_trace(plot, type = "scatter", mode = "lines+markers",
        x = years$Year, y = line$pct_error, name = org$Organisation[1], connectgaps = FALSE,
        line = list(color = unname(colours[key]), width = 2.5),
        marker = list(color = unname(colours[key]), size = 7),
        text = paste0(org$Organisation[1], "<br>", years$Year_label,
          "<br>Observed: ", scales::comma(line$Observed, accuracy = 1),
          "<br>Modelled: ", scales::comma(line$Modelled, accuracy = 1)),
        hovertemplate = "%{text}<br>Error: %{y:+.1%}<extra></extra>")
    }
    plot <- plot |> plotly::layout(
      xaxis = list(title = "Financial year", tickmode = "array", tickvals = years$Year, ticktext = years$Year_label),
      yaxis = list(title = "Model error (%)", tickformat = ".1%", range = c(-limit, limit),
        zeroline = TRUE, zerolinecolor = "#555555", zerolinewidth = 1.5),
      showlegend = !combined, paper_bgcolor = "white", plot_bgcolor = "white",
      margin = list(l = 75, r = 20, t = if (combined) 12 else 65, b = if (combined) 52 else 85)) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
    if (!combined) return(dc_widget(report_line_chart(plot, legend_position = "bottom", title = title)))
    htmltools::div(class = "retro-grid-panel",
      htmltools::tags$h5(htmltools::tags$strong(activity), htmltools::span(title)), dc_widget(plot))
  }
  if (combined) {
    organisations <- unique(all_rows[c("Key", "Level", "Organisation")]) |>
      dplyr::arrange(match(Level, c("Trust", "ICB", "Cluster")), Organisation)
    key <- function(rows, label) htmltools::div(class = "retro-grid-key",
      htmltools::tags$strong(label),
      report_colour_key(stats::setNames(unname(colours[rows$Key]), rows$Organisation), lines = TRUE))
    return(htmltools::div(class = "retro-combined-grid",
      panel(bookings[bookings$Level == "Trust", ], "Trusts", "Antenatal bookings"),
      panel(annual[annual$Level == "Trust", ], "Trusts", "Deliveries"),
      panel(bookings[bookings$Level %in% c("ICB", "Cluster"), ], "ICBs and cluster", "Antenatal bookings"),
      panel(annual[annual$Level %in% c("ICB", "Cluster"), ], "ICBs and cluster", "Deliveries"),
      key(organisations[organisations$Level == "Trust", ], "Trusts (top row)"),
      key(organisations[organisations$Level %in% c("ICB", "Cluster"), ], "ICBs and cluster (bottom row)")))
  }
  htmltools::div(class = "report-chart-grid report-change-pair",
    style = "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 24px;",
    htmltools::div(panel(annual[annual$Level == "Trust", ], "Trusts")),
    htmltools::div(panel(annual[annual$Level %in% c("ICB", "Cluster"), ], "ICBs and BBCS providers combined")))
}
