# ANALYSIS RUNNER ========================================================
# Use Run analysis.R with the secure parquet inputs.
# A clean child R process handles inputs, analysis, QA and rendering.

run_maternity_project <- function() {
  if (!file.exists("R/Maternity analysis.R") || !file.exists("index.qmd"))
    stop("Open BBCS_maternity and run from the project root.")

  # Remove previous inspection objects; the analysis itself runs in a clean process.
  inspection_names <- c("projection_master", "projection_master_backtest",
    "projection_master_dictionary", "delivery_proportions", "booking_proportions", "ons_modifications", "qa_results", "qa_details")
  present <- intersect(inspection_names, ls(envir = .GlobalEnv))
  if (length(present)) rm(list = present, envir = .GlobalEnv)
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  status <- system2(rscript, c("--vanilla", shQuote("R/Analysis runner.R")))
  if (status != 0L)
    stop("Analysis failed. No current report is available. See the error above.", call. = FALSE)

  # Return the completed master and QA results for View() in RStudio.
  projections <- readRDS(".projections.rds")
  checks <- readRDS(".qa.rds")
  projection_master <<- projections$master
  projection_master_backtest <<- projections$backtest$baseline$master
  projection_master_dictionary <<- projections$master_dictionary
  delivery_proportions <<- projections$delivery_proportions
  booking_proportions <<- projections$booking_proportions
  ons_modifications <<- projections$ons_modifications
  qa_results <<- checks$results
  qa_details <<- checks$details
  message("Report ready: _site/index.html")
  invisible(NULL)
}

prepare_and_render <- function() {
  # Print warnings when they happen, keeping any file error visible in the console.
  options(warn = 1)
  project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
  if (!file.exists("R/Maternity analysis.R") || !file.exists("index.qmd"))
    stop("Run from the BBCS_maternity project root.")

  output_files <- c(".ons_exploration.rds", ".patient_flows.rds",
    ".delivery_geography.rds", ".projections.rds", ".qa.rds")
  generated_paths <- c(output_files, ".analysis_run.rds", "_site")

  # Check exact destinations before removing generated output directories.
  # Refuse paths that resolve outside this checkout, including directory links.
  for (item in generated_paths[file.exists(generated_paths)]) {
    resolved <- normalizePath(item, winslash = "/", mustWork = TRUE)
    if (!startsWith(tolower(resolved), paste0(tolower(project_root), "/")))
      stop("Generated output is outside the project: ", item)
  }
  unlink(generated_paths, recursive = TRUE)
  if (any(file.exists(generated_paths))) stop("Could not clear previous outputs. Close open report files and retry.")
  completed <- FALSE
  on.exit({
    # Failure cannot leave partial outputs that look like a completed run.
    if (!completed) unlink(generated_paths, recursive = TRUE)
  }, add = TRUE)
  started_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  run_id <- paste("analysis", format(Sys.time(), "%Y%m%dT%H%M%OS6"), Sys.getpid(), sep = "-")
  run_info <- list(run_id = run_id, status = "running",
    started_at = started_at, output_files = output_files)
  saveRDS(run_info, ".analysis_run.rds")
  message("Preparing data and analysis...")

  # 1. Read Sarah's prepared parquet inputs from the secure environment.
  source("R/Read parquet files from azure.R", local = .GlobalEnv)

  # 2. Prepare exploration and projection results.
  suppressPackageStartupMessages(source("R/Maternity analysis.R", local = .GlobalEnv))

  # 3. Existing QA: review flags remain visible; calculation errors stop.
  suppressPackageStartupMessages(source("R/QA.R", local = .GlobalEnv))
  saveRDS(list(results = qa_results, details = qa_details), ".qa.rds")
  for (output_file in output_files) {
    output <- readRDS(output_file)
    attr(output, "run_id") <- run_id
    saveRDS(output, output_file)
  }
  run_info$status <- "prepared"
  run_info$qa_checks <- sum(qa_results$Result == "Check")

  # 4. Build in a temporary folder: intermediate files belong to this run only.
  # Avoid OneDrive locks and collisions with an editor rendering index.qmd.
  # Only the finished report is copied back to the usual _site folder.
  quarto <- Sys.which("quarto")
  if (!nzchar(quarto)) stop("Quarto was not found. Install/configure Quarto before running.")
  render_folder <- tempfile("maternity-report-")
  dir.create(render_folder)
  render_folder <- normalizePath(render_folder, winslash = "/", mustWork = TRUE)
  temporary_root <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  if (!startsWith(tolower(render_folder), paste0(tolower(temporary_root), "/")))
    stop("Unexpected temporary report folder.")
  on.exit(unlink(render_folder, recursive = TRUE), add = TRUE)
  render_inputs <- c("index.qmd", "_quarto.yml", "_brand.yml", "styles.css",
    "R", "data", "config", "assets", output_files, ".analysis_run.rds")
  if (!all(file.copy(render_inputs, render_folder, recursive = TRUE)))
    stop("Could not copy the report inputs into the temporary folder.")
  # The private render has complete inputs; project previews wait until publication.
  saveRDS(run_info, file.path(render_folder, ".analysis_run.rds"))
  message("Rendering report...")
  setwd(render_folder)
  status <- tryCatch(system2(quarto, c("render", "index.qmd", "--quiet")),
    finally = setwd(project_root))
  rendered_site <- file.path(render_folder, "_site")
  if (status != 0L || !file.exists(file.path(rendered_site, "index.html")))
    stop("Report rendering failed.")
  if (!file.copy(rendered_site, project_root, recursive = TRUE) ||
      !file.exists("_site/index.html")) stop("Could not save the finished report.")
  saveRDS(run_info, ".analysis_run.rds")
  completed <- TRUE
  invisible(NULL)
}

# A preview opened during preparation waits for the same run to finish.
read_completed_analysis <- function() {
  status_file <- ".analysis_run.rds"
  if (!file.exists(status_file))
    stop("No prepared analysis is available. Run Run analysis.R in the secure environment; it creates the report automatically.", call. = FALSE)
  report_run <- readRDS(status_file)
  if (identical(report_run$status, "running")) {
    message("Analysis is running; waiting for the completed report data...")
    expected_run <- report_run$run_id
    for (second in seq_len(1800L)) {
      Sys.sleep(1)
      if (!file.exists(status_file))
        stop("The analysis stopped before completion. See the error in the analysis console.", call. = FALSE)
      report_run <- tryCatch(readRDS(status_file), error = function(e) NULL)
      if (!is.null(report_run) && !identical(report_run$run_id, expected_run))
        stop("A different analysis run has started. Wait for its finished report.", call. = FALSE)
      if (identical(report_run$status, "prepared")) break
    }
  }
  if (!identical(report_run$status, "prepared"))
    stop("Analysis has not finished. Its runner will create the report automatically.", call. = FALSE)
  report_run
}

# Executed only by Rscript, not when a launcher sources this file.
if (sys.nframe() == 0L) {
  prepare_and_render()
}
