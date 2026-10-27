# RUN THE ANALYSIS ======================================================
# Reuse loaded parquet objects -> analyse -> render -> open report.
# Run with: source("R/Run analysis.R")
# Refresh changed extracts explicitly: run_maternity_project(refresh_data = TRUE)
# Run investigations separately: source("R/QA.R")

run_maternity_project <- function(refresh_data = FALSE) {
  # GENERAL SETUP -------------------------------------------------------
  if (!file.exists("R/Maternity analysis.R") || !file.exists("index.qmd"))
    stop("Open BBCS_maternity and run from the project root.")
  project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
  previous_options <- options(warn = 1, maternity.run_id = NULL)
  on.exit(options(previous_options), add = TRUE)

  # Clear previous inspection objects before preparing this run's results.
  inspection_names <- c("projection_master", "projection_master_backtest",
    "projection_master_dictionary", "delivery_proportions", "booking_proportions",
    "ons_modifications", "private_nhs_modifications", "booking_modifications")
  present <- intersect(inspection_names, ls(envir = .GlobalEnv))
  if (length(present)) rm(list = present, envir = .GlobalEnv)

  # Replace saved outputs so failed runs don't break things
  output_files <- c(".ons_exploration.rds", ".patient_flows.rds",
    ".delivery_geography.rds", ".projections.rds", ".demo_sankeys.rds")
  generated_paths <- c(output_files, ".analysis_run.rds", "_site")
  for (item in generated_paths[file.exists(generated_paths)]) {
    resolved <- normalizePath(item, winslash = "/", mustWork = TRUE)
    if (!startsWith(tolower(resolved), paste0(tolower(project_root), "/")))
      stop("Generated output is outside the project: ", item)
  }
  unlink(generated_paths, recursive = TRUE)
  if (any(file.exists(generated_paths)))
    stop("Could not clear previous outputs. Close open report files and retry.")
  prepared <- FALSE
  on.exit({
    if (!prepared) unlink(generated_paths, recursive = TRUE)
  }, add = TRUE)
  started_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  run_id <- paste("analysis", format(Sys.time(), "%Y%m%dT%H%M%OS6"), Sys.getpid(), sep = "-")
  options(maternity.run_id = run_id)
  run_info <- list(run_id = run_id, status = "running",
    started_at = started_at, output_files = output_files)
  saveRDS(run_info, ".analysis_run.rds")

  # 1. LOAD PARQUETS ONCE PER SESSION, OR WHEN EXPLICITLY REFRESHED --------
  parquet_objects <- c("final_cohort_2326", "total_births_by_LA_2326",
    "trust_matrix_data", "icb_matrix_data", "demographics")
  inputs_loaded <- all(vapply(parquet_objects, exists, logical(1),
    envir = .GlobalEnv, inherits = FALSE))
  if (refresh_data || !inputs_loaded) {
    message("Reading Sarah's parquet files...")
    source("R/Read parquet files from azure.R", local = .GlobalEnv)
  } else {
    message("Using parquet objects already loaded in this R session.")
  }

  # 2. PREPARE THE ANALYSIS ----------------------------------------------
  # ONS / MSDS exploration, projections, charts and tables.
  message("Preparing analysis, charts and tables...")
  suppressPackageStartupMessages(source("R/Maternity analysis.R", local = .GlobalEnv, encoding = "UTF-8"))
  source("R/Demo Sankeys.R", local = .GlobalEnv, encoding = "UTF-8")

  # 3. MARK THE SAVED ANALYSIS READY -------------------------------------
  # save_report_rds attaches run metadata during the original write.
  run_info$status <- "prepared"
  saveRDS(run_info, ".analysis_run.rds")
  prepared <- TRUE

  # 4. RENDER THE REPORT -------------------------------------------------
  # Build in a temporary folder and report is put in  _site.
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
  saveRDS(run_info, file.path(render_folder, ".analysis_run.rds"))
  message("Rendering report...")
  # Stream Quarto output to the console; a locked project log must not prevent
  # rendering or mask the original error when reporting a failed render.
  setwd(render_folder)
  status <- tryCatch(system2(quarto, c("render", "index.qmd"),
    stdout = "", stderr = "", wait = TRUE),
    error = function(e) stop("Could not run Quarto: ", conditionMessage(e),
      ". Prepared results are saved.", call. = FALSE),
    finally = setwd(project_root))
  rendered_site <- file.path(render_folder, "_site")
  if (status != 0L || !file.exists(file.path(rendered_site, "index.html"))) {
    stop('Report rendering failed; see the Quarto messages above in the console. Prepared results are saved. ',
      'To retry rendering only, run system2(Sys.which("quarto"), c("render", "index.qmd")).',
      call. = FALSE)
  }
  audit_disclosure_html(file.path(rendered_site, "index.html"))
  if (!file.copy(rendered_site, project_root, recursive = TRUE) ||
      !file.exists("_site/index.html")) stop("Could not save the finished report.")
  saveRDS(run_info, ".analysis_run.rds")

  # 5. SHOW THE COMPLETED OBJECTS IN RSTUDIO ------------------------------
  projections <- get("first_draft_projections", envir = .GlobalEnv)
  projection_master <<- projections$master
  projection_master_backtest <<- projections$backtest$baseline$master
  projection_master_dictionary <<- projections$master_dictionary
  delivery_proportions <<- projections$delivery_proportions
  booking_proportions <<- projections$booking_proportions
  ons_modifications <<- projections$ons_modifications
  private_nhs_modifications <<- projections$private_nhs$pooled
  booking_modifications <<- projections$allocation_tables$calibration
  message("Report ready: _site/index.html")

  # 6. OPEN THE FINISHED REPORT ------------------------------------------
  # Use the current RStudio / default browser setting 
  report_file <- normalizePath("_site/index.html", winslash = "/", mustWork = TRUE)
  tryCatch(
    utils::browseURL(report_file),
    error = function(e) message("Report saved; browser could not open. Open _site/index.html.")
  )
  invisible(NULL)
}

# RUN ALL STEPS ---------------------------------------------------------
run_maternity_project()
