# Read the Azure data and make the saved charts and tables for sections 4-10.
# Section 11 is a static worked example in index.qmd.
# From the project folder: source("R/Run analysis.R")
# Run R/QA.R separately when you want to check the data and calculations.

local({
  scripts <- c(
    "R/Read parquet files from azure.R",
    "R/additional_analysis.R",
    "R/Deliveries by LA and Age Bands.R",
    "R/Projections.R"
  )

  # Clear old report data before starting, so a failed run cannot leave stale charts.
  unlink(c(".patient_flows.rds", ".delivery_geography.rds", ".projections.rds"))

  # Run each script in order; an error stops preparation at the problem.
  for (i in seq_along(scripts)) {
    message("[", i, "/", length(scripts), "] Running ", scripts[i])
    source(scripts[i], local = .GlobalEnv, echo = FALSE)
  }

  message("Finished. Report data, charts and tables are refreshed. Render index.qmd next.")
})
