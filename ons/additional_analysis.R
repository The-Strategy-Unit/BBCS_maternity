# Run order: from project root, run the two highlighted R scripts; then source("ons/additional_analysis.R")
# Render: quarto render "ons/heather_scratch view.qmd" after this script finishes
# Secure data: use Sarah's objects in memory; save only chart outputs for Quarto

## 1. Patient flows ----------------------------------------------------------
# Inputs: Sarah's Azure script creates cohort_IDs and trust_booking_and_birth_location_matrix
if (!exists("cohort_IDs") || !exists("trust_booking_and_birth_location_matrix")) {
  stop("Run 'Read parquet files from azure.R' first; its two table objects are needed.")
}

# Column aliases: add the secure table's actual column name here if a check fails
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
    links <- aggregate(count[valid],
                       by = list(source = source_code, target = target_code), sum)
    names(links)[3] <- "n"
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
  QJ2 = "Leicester, Leicestershire and Rutland ICB", Other = "Other ICBs"
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
  list(cohort_IDs = cohort_IDs,
       trust_booking_and_birth_location_matrix = trust_booking_and_birth_location_matrix),
  c("resident_icb_interest", "resident_icb", "icb_residence"),
  c("birth_icb_interest", "birth_icb", "icb_birth"),
  names(icb_labels)[names(icb_labels) != "Other"]
)
trust_flow <- make_flows(
  list(trust_booking_and_birth_location_matrix = trust_booking_and_birth_location_matrix,
       cohort_IDs = cohort_IDs),
  c("booking_provider_grouped", "trust_booking", "booking_provider"),
  c("birth_provider_grouped", "trust_birth_spell", "birth_provider"),
  names(trust_labels)[names(trust_labels) != "Other"]
)

# Chart: same labels, colours and hover measures as the ONS index Sankeys
make_sankey <- function(flow, labels, source_suffix, target_suffix) {
  links <- flow$links
  codes <- unique(c(links$source, links$target))
  palette <- c("#005EB8", "#D55E00", "#00856A", "#8E5EA2",
               "#B07300", "#C44E52", "#4477AA", "#727272")
  colours <- setNames(palette[seq_along(codes)], codes)
  display_label <- function(x) unname(labels[x])
  total <- sum(links$n)

  source_nodes <- unique(links$source)
  target_nodes <- unique(links$target)
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

# Outputs: ready for the scratch view; add later analyses below this section
icb_sankey <- make_sankey(icb_flow, icb_labels, " (residence)", " (birth)")
trust_sankey <- make_sankey(trust_flow, trust_labels, " (booking)", " (birth)")
patient_flow_details <- data.frame(
  Chart = c("ICB residence to birth", "Booking to birth trust"),
  `Source table` = c(icb_flow$table, trust_flow$table),
  `Count basis` = c(icb_flow$count_basis, trust_flow$count_basis),
  Records = c(sum(icb_flow$links$n), sum(trust_flow$links$n)),
  Links = c(nrow(icb_flow$links), nrow(trust_flow$links)),
  check.names = FALSE
)

# Quarto uses a new R session: hand over finished outputs, not raw patient records
scratch_dir <- if (file.exists("ons/additional_analysis.R")) "ons" else "."
saveRDS(
  list(icb_sankey = icb_sankey, trust_sankey = trust_sankey,
       patient_flow_details = patient_flow_details),
  file.path(scratch_dir, ".heather_scratch_objects.rds")
)
