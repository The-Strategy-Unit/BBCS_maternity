# Open BBCS_maternity.Rproj and run this script in the secure R session.
# Or, from the project root:
# source("R/Run these to get needed objects.R")
# Creates the data/analysis objects in the Global Environment and refreshes
# .patient_flows.rds for index.qmd. Azure authentication may prompt as usual.

local({
  scripts <- c(
    "R/Read parquet files from azure.R",
    "R/matrix of booking appts and births.R",
    "R/additional_analysis.R"
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

  for (i in seq_along(scripts)) {
    message("[", i, "/", length(scripts), "] Running ", scripts[i])
    # Share objects between scripts and retain them for interactive analysis.
    # An error stops the runner before subsequent scripts are attempted.
    source(scripts[i], local = .GlobalEnv, echo = FALSE)
  }

  message("Finished. Objects are loaded and .patient_flows.rds is refreshed. ",
          "You can now render index.qmd.")
})
