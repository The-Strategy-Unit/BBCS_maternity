# Read the Azure data and make the saved charts and tables for sections 4-10.
# From the project folder: source("R/Run analysis.R")
# Run R/QA.R separately when you want to check the data and calculations.

local({
  scripts <- c(
    "R/Read parquet files from azure.R",
    "R/matrix of booking appts and births.R",
    "R/additional_analysis.R",
    "R/Deliveries by LA and Age Bands.R",
    "R/Projections.R",
    "R/Imaginary Example.R"
  )

  missing_scripts <- scripts[!file.exists(scripts)]
  if (length(missing_scripts)) {
    stop(
      "Run this script from the BBCS_maternity project root. ",
      "Open BBCS_maternity.Rproj first. Missing scripts: ",
      paste(missing_scripts, collapse = ", "),
      call. = FALSE
    )
  }

  # Clear old report data before starting, so a failed run cannot leave stale charts.
  unlink(c(".patient_flows.rds", ".delivery_geography.rds", ".projections.rds"))

  # Run each script in order; an error stops preparation at the problem.
  for (i in seq_along(scripts)) {
    message("[", i, "/", length(scripts), "] Running ", scripts[i])
    source(scripts[i], local = .GlobalEnv, echo = FALSE)
  }

  message("Finished. Report data, charts and tables are refreshed. Render index.qmd next.")
})
