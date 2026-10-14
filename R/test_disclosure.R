# Invented counts only. Run from the project root with Rscript.
source("R/disclosure.R")
suppressPackageStartupMessages(library(dplyr))
near <- function(x, y) stopifnot(isTRUE(all.equal(as.numeric(x), as.numeric(y))))
near(dc_count(c(0, 1:13, 17, 18, 22, 23, NA)),
  c(0, rep(5, 7), rep(10, 5), 15, 15, 20, 20, 25, NA))
near(dc_count(c(0, 0.01, 7.5, 8, 12.49, 12.5)), c(0, 5, 5, 10, 10, 15))
stopifnot(is.na(dc_ratio(0, 0)), is.na(dc_ratio(1, NA)), identical(dc_ratio(0, 10), 0))
near(dc_ratio(c(1, 7, 8), c(13, 13, 13)), c(5/15, 5/15, 10/15))

# Sum raw cells first. A total is not the sum of protected component counts.
raw <- data.frame(group = c("a", "a"), n = c(1, 1))
original <- raw
total <- raw |> summarise(n = sum(n)) |> dc_counts("n")
near(total$n, 5)
near(sum(dc_count(raw$n)), 10)
stopifnot(identical(raw, original))

raw <- data.frame(Key = "a", Year = 2025:2027, Projected = c(12, 13, 17),
  Observed_baseline = 11, Observed_matched_baseline = 9,
  Change = c(3, 4, 8), Percent_change = c(3, 4, 8)/9,
  Coverage_status = "Partial", Coverage_note = "11 raw records, 9 matched")
original <- raw
shown <- dc_projection(raw, 2025)
near(shown$Projected, c(10, 15, 15))
near(shown$Change, c(0, 5, 5))
near(shown$Percent_change, c(0, .5, .5))
stopifnot(!any(grepl("11|9", shown$Coverage_note)), identical(raw, original))
booked <- dc_projection(raw, 2025, booking = TRUE)
near(booked$Percent_change, c(0, .5, .5))

# Independent raw denominator, not the sum of rounded children.
mix <- data.frame(N = c(1, 2, 10), Denominator = 13)
shown <- dc_cells(mix, c("N", "Denominator"), list(Share = c("N", "Denominator")))
near(shown$Share, c(5, 5, 10)/15)
stopifnot(sum(shown$Share) != 1)

# Plotly's original construction attributes must not accompany safe traces.
# The unused sentinel simulates a raw column carried in a presentation frame.
frame <- data.frame(year = 2025:2027, count = dc_count(c(1, 13, 18)),
  private_unused_sentinel = c(913127, 913129, 913133))
widget <- plotly::plot_ly(frame, x = ~year, y = ~count, type = "scatter", mode = "lines")
payload <- htmlwidgets:::createPayload(dc_widget(widget))
encoded <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", force = TRUE)
stopifnot(!grepl("private_unused_sentinel|913127|913129|913133", encoded),
  is.null(payload$x$attrs), is.null(payload$x$visdat), is.null(payload$x$cur_data))
near(payload$x$data[[1]]$y, c(5, 15, 20))

# Fail closed on a raw chart coordinate or a retained construction payload.
# ONS-only counts are exempt from MSDS rounding.
html_path <- tempfile(fileext = ".html")
controlled <- list(x = list(data = list(
  list(type = "scatter", y = c(5, 10)),
  list(type = "sankey", link = list(value = c(5, 15)))),
  layout = list(yaxis = list(title = "Deliveries"))))
ons_only <- list(x = list(data = list(list(type = "scatter", y = c(11, 17))),
  layout = list(yaxis = list(title = "ONS published births"))))
write_payloads <- function() writeLines(paste0('<section id="disclosure-control"></section>',
  '<script type="application/json">', jsonlite::toJSON(controlled, auto_unbox = TRUE), '</script>',
  '<script type="application/json">', jsonlite::toJSON(ons_only, auto_unbox = TRUE), '</script>'), html_path)
write_payloads()
audit_disclosure_html(html_path)
controlled$x$data[[1]]$y[1] <- 11
write_payloads()
stopifnot(inherits(try(audit_disclosure_html(html_path), silent = TRUE), "try-error"))
controlled$x$data[[1]]$y[1] <- 5
controlled$x$attrs <- list(raw = 11)
write_payloads()
stopifnot(inherits(try(audit_disclosure_html(html_path), silent = TRUE), "try-error"))
unlink(html_path)
cat("Disclosure boundary, raw aggregation, rates, change and widget payload tests passed.\n")
