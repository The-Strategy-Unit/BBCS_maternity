# Publication controls only. Never call these helpers from model/calibration code.
# NHS England, MSDS monthly statistics, 17 September 2026:
# https://digital.nhs.uk/data-and-information/publications/statistical/maternity-services-monthly-statistics/final-june-2026-provisional-july-2026#publication-data-files-and-suppression-methodology
msds_disclosure_version <- "msds-activity-v1"

dc_count <- function(x) {
  # Modelled counts can be fractional: retain genuine zero, protect positive
  # values below 8, and round remaining counts to the nearest five (ties up).
  # Absorb floating-point summation noise at an exact half-five boundary.
  out <- 5 * floor(x / 5 + 0.5 + 16 * .Machine$double.eps * pmax(1, abs(x / 5)))
  out[is.finite(x) & x > 0 & x < 8] <- 5
  out[!is.finite(x)] <- NA_real_
  out
}

dc_divide <- function(n, d) {
  # Divide first so a single denominator retains every numerator.
  # ifelse() with a scalar denominator test would return only the first ratio.
  ratio <- n / d
  usable <- rep_len(is.finite(d) & d > 0, length(ratio))
  ratio[!usable | !is.finite(ratio)] <- NA_real_
  ratio
}
dc_ratio <- function(n, d) dc_divide(dc_count(n), dc_count(d))

dc_counts <- function(data, columns) {
  data[columns] <- lapply(data[columns], dc_count)
  data
}

# Apply to a completed display aggregation. Each rate specifies count columns;
# denominators must already have been summed from RAW cells at this display level.
dc_cells <- function(data, counts, rates = list()) {
  data <- dc_counts(data, counts)
  for (field in names(rates)) {
    pair <- rates[[field]]
    data[[field]] <- dc_divide(data[[pair[1]]], data[[pair[2]]])
  }
  data
}

dc_coverage_note <- function(data) {
  if ("Coverage_note" %in% names(data)) {
    status <- if ("Coverage_status" %in% names(data)) data$Coverage_status else
      ifelse(grepl("^Partial:", data$Coverage_note), "Partial",
        ifelse(grepl("^Complete:", data$Coverage_note), "Complete",
          ifelse(grepl("^No usable estimate:", data$Coverage_note), "No usable estimate",
            "All recorded activity; may extend beyond model coverage")))
    data$Coverage_note <- paste0(status, "; see provider coverage table")
  }
  data
}

dc_calibration <- function(data) {
  data <- dc_counts(data, c("MSDS_resident_count", "Cohort_pregnancies",
    "Cohort_recorded_births", "Cohort_no_recorded_delivery", "Resident_antenatal_only",
    "Resident_pregnancies", "Shared_maternity_denominator"))
  data$Bookings_uplift_modifier <- ifelse(data$MSDS_resident_count > 0,
    dc_divide(data$Resident_pregnancies, data$MSDS_resident_count), 0)
  data$Resident_birth_gap <- data$MSDS_resident_count - data$Cohort_recorded_births
  data$Booking_Denominator_Factor <- data$Shared_maternity_denominator - data$Cohort_pregnancies
  data
}

dc_projection <- function(data, reference_year, booking = FALSE) {
  fields <- intersect(c("Projected", "Baseline", "Modelled_baseline", "Observed_baseline", "Observed_matched_baseline"), names(data))
  data <- dc_coverage_note(dc_counts(data, fields))
  # Delivery and booking growth both use their own modelled reference year.
  # Divide controlled counts, including the denominator, so the base is 0%.
  if (any(c("Change", "Percent_change", "Growth_change") %in% names(data))) {
    keys <- intersect(c("Key", "Age_band", "Age"), names(data))
    data <- data |> dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
      dplyr::mutate(Modelled_baseline = Projected[match(reference_year, Year)]) |> dplyr::ungroup()
    if ("Change" %in% names(data)) data$Change <- data$Projected - data$Modelled_baseline
    if ("Percent_change" %in% names(data)) data$Percent_change <- dc_divide(data$Projected, data$Modelled_baseline) - 1
    if ("Growth_change" %in% names(data)) data$Growth_change <- dc_divide(data$Projected, data$Modelled_baseline) - 1
  }
  data
}

dc_geography_trend <- function(data, keys) {
  data <- dc_cells(data, c("N", "Organisation_total"), list(Share = c("N", "Organisation_total")))
  data |> dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::mutate(Base_count = N[Year == "2023/24"], Base_share = Share[Year == "2023/24"],
      Count_change = dc_divide(N, Base_count) - 1, Share_change = Share - Base_share,
      Share_change_pp = 100 * Share_change, Relative_share_change = dc_divide(Share, Base_share) - 1) |>
    dplyr::ungroup()
}

# Plotly normally serialises both built traces and its original construction
# attributes. Keep only the built presentation object in the HTML payload.
dc_widget <- function(widget) {
  hook <- function(w) {
    if (is.function(previous)) w <- previous(w)
    w$x$attrs <- NULL
    w$x$visdat <- NULL
    w$x$cur_data <- NULL
    w
  }
  environment(hook) <- list2env(list(previous = widget$preRenderHook), parent = baseenv())
  widget$preRenderHook <- hook
  widget
}

dc_report_objects <- function(x) {
  if (inherits(x, "plotly")) return(dc_widget(x))
  if (is.data.frame(x) || inherits(x, "ggplot") || inherits(x, "gt_tbl")) return(x)
  if (is.list(x)) for (i in seq_along(x)) x[i] <- list(dc_report_objects(x[[i]]))
  x
}

save_report_rds <- function(object, file) {
  object <- dc_report_objects(object)
  attr(object, "disclosure_version") <- msds_disclosure_version
  attr(object, "run_id") <- getOption("maternity.run_id")
  saveRDS(object, file)
}

read_report_rds <- function(file, run_id) {
  object <- readRDS(file)
  if (!identical(attr(object, "disclosure_version"), msds_disclosure_version) ||
      is.null(run_id) || !identical(attr(object, "run_id"), run_id))
    stop("Outdated or incompatible report data: ", file,
      ". Run R/Run analysis.R to prepare the controlled report outputs.", call. = FALSE)
  dc_report_objects(object)
}

# Also runs on knitted QMD output, before it can become a shareable report.
audit_disclosure_widgets <- function(html) {
  html <- paste(html, collapse = "\n")
  scripts <- regmatches(html, gregexpr(
    '(?s)<script\\b[^>]*type="application/json"[^>]*>.*?</script>', html, perl = TRUE))[[1]]
  checked <- c(widgets = 0L, count_traces = 0L, sankeys = 0L)
  multiples <- function(values) {
    x <- unlist(values, use.names = FALSE)
    if (!length(x)) return(TRUE)
    if (!is.numeric(x)) return(FALSE)
    all(is.na(x) | abs(x / 5 - round(x / 5)) < 1e-8)
  }
  axis_title <- function(axis) {
    title <- axis$title
    if (is.list(title)) title <- title$text
    if (is.null(title)) "" else title
  }
  for (script in scripts) {
    payload <- sub('(?s)^<script[^>]*>(.*?)</script>$', '\\1', script, perl = TRUE)
    x <- jsonlite::fromJSON(payload, simplifyVector = FALSE)$x
    if (is.null(x$data)) next
    checked["widgets"] <- checked["widgets"] + 1L
    if (any(c("attrs", "visdat", "cur_data") %in% names(x)))
      stop("Original Plotly construction data remains in the HTML")
    if (grepl('"(person_id_mother|uniq_preg_id|private_unused_sentinel)"', payload))
      stop("A raw record field remains in a widget payload")
    y_count <- grepl('^Pregnancies$|^Projected (deliveries|antenatal bookings)$|^Deliveries$|^Deliveries /|^Antenatal bookings($| /)|^Recorded (non-home|/ modelled non-home) deliveries$',
      axis_title(x$layout$yaxis))
    x_count <- axis_title(x$layout$xaxis) %in% c("Deliveries", "Pregnancies")
    for (trace in x$data) {
      if (identical(trace$type, "sankey")) {
        checked["sankeys"] <- checked["sankeys"] + 1L
        if (is.list(trace$meta) && identical(trace$meta$msds_sankey_scale, "normalised_categories")) {
          counts <- unlist(trace$meta$display_counts, use.names = FALSE)
          denominators <- unlist(trace$meta$display_denominators, use.names = FALSE)
          sources <- unlist(trace$link$source, use.names = FALSE)
          values <- unlist(trace$link$value, use.names = FALSE)
          if (!length(counts) || length(counts) != length(values) ||
              length(counts) != length(sources) || length(counts) != length(denominators) ||
              !multiples(counts) || !multiples(denominators) ||
              any(!is.finite(counts) | counts <= 0) || any(!is.finite(denominators) | denominators <= 0) ||
              any(!is.finite(values)) || any(!is.finite(sources)))
            stop("Uncontrolled proportion Sankey inputs in the HTML")
          expected <- 100 * counts / ave(counts, sources, FUN = sum)
          if (any(abs(values - expected) > 1e-6))
            stop("Proportion Sankey widths do not use controlled counts")
        } else if (!multiples(trace$link$value)) stop("Uncontrolled Sankey counts in the HTML")
      }
      if (y_count || x_count) {
        checked["count_traces"] <- checked["count_traces"] + 1L
        if (!multiples(if (y_count) trace$y else trace$x))
          stop("Uncontrolled activity count coordinates in the HTML")
      }
    }
  }
  if (any(checked == 0L)) stop("The HTML disclosure audit found incomplete chart output")
  message("Embedded disclosure audit passed: ", checked["widgets"], " widgets; ",
    checked["count_traces"], " count traces; ", checked["sankeys"], " Sankeys.")
  invisible(checked)
}

audit_disclosure_html <- function(path) {
  html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (!grepl('id="disclosure-control"', html, fixed = TRUE))
    stop("Disclosure method note is absent from the rendered report")
  audit_disclosure_widgets(html)
}
