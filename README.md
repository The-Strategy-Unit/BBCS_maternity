# BBCS maternity: ONS report

This repository contains the source and public ONS inputs for one report comparing
two age-specific methods of converting projected live births into maternities.

## Run

Install R and Quarto. In R, install these packages once:

```r
install.packages(c("dplyr", "janitor", "purrr", "readr", "readxl", "tidyr",
                   "gt", "ggplot2", "plotly", "scales", "knitr", "rmarkdown"))
```

From the repository root run:

```sh
quarto render
```

Open `_site/index.html` to see the result. Generated files are ignored by Git.
The ONS calculations run on every render; freezing is disabled. Edit
`index.qmd` for report changes and `R/ons_data.R` for ONS data
preparation. Area and population-age settings are in `config/`.
The Quarto project runs its R chunks from this repository root, alongside the
`data/` folder. Open `BBCS_maternity.Rproj` when working interactively in RStudio.

`index.qmd` is the report source. Files such as `index.rmarkdown`, `index_files/`
and `_site/` are generated output; regenerate them in the secure environment.
Historical setup and QA notes are preserved in `ONS-analysis-notes.md`.

## Patient flows

From the repository root, run these in the secure R session before rendering:

```r
source("R/Read parquet files from azure.R")
source("R/matrix of booking appts and births.R")
source("R/additional_analysis.R")
```

Keep the working directory at the project root when sourcing these scripts.
All R scripts live in `R/`; their data paths use the project root.

`R/additional_analysis.R` saves `.patient_flows.rds` in the project root for the
Sankeys and cluster-activity tables in `index.qmd`. Without this file, those
outputs are skipped. The generated RDS and HTML, and the separately supplied
`sankey data/` folder, are ignored by Git.

## ONS inputs

The six files in `data/` are the three birth-registration workbooks (2023–2025),
the historic local-authority live-birth extract (2013–2025), the 2022-based
migration-category birth projection and its matching female-population extract.
The birth CSV was downloaded from the corrected ONS migration-category edition on
25 September 2026. The population extract matches that variant to whole-person
rounding. Definitions and linked sources are in the report.

Git tracks only the explicitly listed source, configuration and ONS input files.
Additional files stay local unless the allowlist in `.gitignore` is updated.
