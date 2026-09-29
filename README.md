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
quarto render ons
```

Open `ons/_site/index.html` to see the result. Generated files are ignored by Git.
The ONS calculations run on every render; freezing is disabled. Edit
`ons/index.qmd` for report changes and `ons/R/ons_data.R` for ONS data
preparation. Area and population-age settings are in `ons/config/`.

## Patient flows

From the repository root, run these in the secure R session before rendering:

```r
source("Read parquet files from azure.R")
source("matrix of booking appts and births.R")
source("ons/additional_analysis.R")
```

`additional_analysis.R` saves `ons/.patient_flows.rds` for the Sankeys in
`ons/index.qmd`. Without this file, those charts are skipped. The generated RDS
and HTML are ignored by Git.

## ONS inputs

The six files in `data/` are the three birth-registration workbooks (2023–2025),
the historic local-authority live-birth extract (2013–2025), the 2022-based
migration-category birth projection and its matching female-population extract.
The birth CSV was downloaded from the corrected ONS migration-category edition on
25 September 2026. The population extract matches that variant to whole-person
rounding. Definitions and linked sources are in the report.

Git tracks only the explicitly listed source, configuration and ONS input files.
Additional files stay local unless the allowlist in `.gitignore` is updated.
