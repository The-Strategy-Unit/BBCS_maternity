# Synthetic section 4 publication QA; never reads Azure or replaces report outputs.
run_demo_disclosure_test <- function() {
  root <- normalizePath(".", winslash = "/")
  previous_options <- options(maternity.run_id = "demo-disclosure-test")
  on.exit({ setwd(root); options(previous_options) }, add = TRUE)
  code <- parse("R/Maternity analysis.R")
  for (statement in code) {
    if (is.call(statement) && identical(statement[[1]], as.name("<-")) &&
        identical(statement[[2]], as.name("required_inputs"))) break
    eval(statement)
  }
  for (statement in code) {
    if (is.call(statement) && identical(statement[[1]], as.name("<-")) &&
        is.call(statement[[3]]) && identical(statement[[3]][[1]], as.name("function"))) eval(statement)
  }
  for (statement in code) {
    if (is.call(statement) && identical(statement[[1]], as.name("<-")) &&
        identical(statement[[2]], as.name("pathway_legend"))) eval(statement)
  }
  imd <- read.csv("data/lookups/imd2019_lsoa2011.csv")
  lsoas <- imd$lsoa_2011[match(1:10, imd$imd_decile)]
  groups <- rep(1:20, times = 0:19)
  n <- length(groups)
  final_cohort_2326 <- data.frame(
    person_id_mother = paste0("QA_MOTHER_SENTINEL_", groups),
    uniq_preg_id = paste0("QA_PREGNANCY_SENTINEL_", seq_len(n)),
    birth_yr = "2025/26", edd_yr = "2025/26", birth_flag = as.integer(groups %% 4 != 0),
    birth_date = "2025-12-01", la_of_mother = "E08000031",
    age_at_birth_mother = c(NA, 19, 24, 29, 34, 39, 44, 49)[1 + groups %% 8],
    age_at_booking_mother = NA_real_,
    birth_provider = c("RL4", "RQ3", "RZZ", NA)[1 + groups %% 4],
    home_birth = c(0L, 0L, 1L, NA_integer_)[1 + groups %% 4], resident_icb = "QUA",
    booking_provider = c("RL4", "RQ3", "RZZ", NA)[1 + groups %% 4],
    antenatal_app_date = ifelse(groups %% 4 == 3, NA, "2025-06-01"))
  demographics <- data.frame(person_id_mother = paste0("QA_MOTHER_SENTINEL_", 1:20),
    lsoa_2011 = c(lsoas, NA_character_)[1 + (1:20) %% 11],
    ethnicity = rep(c("A", "N", "Z", "99", "A|H"), 4))
  report_episodes <- prepare_report_cohort(final_cohort_2326)
  folder <- tempfile("qa_demo_disclosure_", tmpdir = root)
  dir.create(folder)
  dir.create(file.path(folder, "R"))
  dir.create(file.path(folder, "data", "lookups"), recursive = TRUE)
  stopifnot(all(file.copy(c("R/Demo Sankeys.R", "R/demographics.R"), file.path(folder, "R"))),
    all(file.copy(c("data/lookups/imd2019_lsoa2011.csv", "data/lookups/ethnic_category.csv",
      "data/lookups/ethnicity_pair_rules.csv"), file.path(folder, "data", "lookups"))))
  setwd(folder)
  source("R/Demo Sankeys.R", local = environment())
  raw <- `Demo Sankeys`
  stopifnot(sum(raw$N) == n, !any(grepl("person|preg|lsoa|raw", names(raw))))
  protect <- function(x) ifelse(x == 0, 0, ifelse(x < 8, 5, 5 * floor(x / 5 + .5)))
  stopifnot(demo_sankey_charts$cohort_total == protect(n))
  for (field in c("imd_quintile", "Age_band", "ethnicity_broad", "ethnicity_description")) {
    displayed <- demo_outcome_counts(raw, field)
    for (i in seq_len(nrow(displayed))) {
      category <- displayed$Category[i]
      selected <- if (is.na(category)) is.na(raw[[field]]) else
        !is.na(raw[[field]]) & as.character(raw[[field]]) == category
      numerator <- protect(sum(raw$N[selected & raw$Outcome == displayed$Outcome[i]]))
      denominator <- protect(sum(raw$N[selected]))
      stopifnot(displayed$N[i] == numerator, displayed$Category_total[i] == denominator,
        displayed$Organisation_total[i] == protect(n),
        abs(displayed$Category_share[i] - numerator / denominator) < 1e-10,
        abs(displayed$Organisation_share[i] - numerator / protect(n)) < 1e-10)
    }
  }
  charts <- read_report_rds(".demo_sankeys.rds", "demo-disclosure-test")
  widgets <- function(x) {
    if (inherits(x, "plotly")) return(list(x))
    if (!is.list(x) || is.data.frame(x)) return(list())
    unlist(lapply(x, widgets), recursive = FALSE)
  }
  payloads <- lapply(widgets(charts), function(widget) htmlwidgets:::createPayload(widget)$x)
  text_counts <- 0L
  for (payload in payloads) {
    stopifnot(is.null(payload$attrs), is.null(payload$visdat), is.null(payload$cur_data))
    for (trace in payload$data) {
      labels <- unlist(c(trace$text, trace$node$label, trace$node$customdata, trace$link$customdata))
      matches <- regmatches(labels, gregexpr(
        "(?:Pregnancies: |Records: |^|<br>)([0-9][0-9,]*)(?= ?(?:<br>| \\(|$))", labels, perl = TRUE))
      counts <- suppressWarnings(as.numeric(gsub("[^0-9]", "", unlist(matches))))
      stopifnot(all(counts %% 5 == 0))
      text_counts <- text_counts + length(counts)
    }
  }
  render_tree <- function(x) {
    if (inherits(x, c("shiny.tag", "shiny.tag.list", "htmlwidget"))) return(x)
    if (is.list(x)) return(lapply(x, render_tree))
    NULL
  }
  page <- htmltools::tagList(htmltools::tags$h1("Synthetic section 4 disclosure QA"),
    htmltools::tags$p(paste(charts$cohort_total, "pregnancies")), render_tree(charts))
  htmltools::save_html(page, "section4.html")
  html <- paste(readLines("section4.html", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  stopifnot(!grepl("QA_MOTHER_SENTINEL_|QA_PREGNANCY_SENTINEL_|IMD decile", html),
    grepl("IMD quintile 5", html, fixed = TRUE), grepl("Unknown (NA)", html, fixed = TRUE),
    text_counts > 0L, identical(raw, `Demo Sankeys`))
  checked <- audit_disclosure_widgets(html)
  stopifnot(checked[["widgets"]] == length(payloads))
  cat("PASS: saved/reloaded section 4, independent rounded denominators and all demographic splits.\n")
  cat("PASS:", text_counts, "count labels/hover entries protected; no identifiers or raw construction data.\n")
  cat("Synthetic output:", file.path(folder, "section4.html"), "\n")
}
run_demo_disclosure_test()
