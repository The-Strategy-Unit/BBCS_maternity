# RUN THE ANALYSIS ======================================================
# Run from the project root in RStudio, with the usual secure Azure access.
# Each run reads the parquet files afresh in this R session.
source("R/Analysis runner.R")

run_maternity_project(c(
  # 1. Read Sarah's prepared parquet files using her existing Azure setup.
  "R/Read parquet files from azure.R",
  # 2. Prepare ONS / MSDS exploration, projections, charts and tables.
  "R/Maternity analysis.R",
  # 3. Check the completed analysis before rendering the report.
  "R/QA.R"
))
# The runner then renders index.qmd and saves _site/index.html.
