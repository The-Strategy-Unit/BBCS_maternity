# BBCS maternity analysis

This project produces the BBCS maternity report, covering recorded bookings and deliveries, patient flows, population demographics and demand projections using MSDS and ONS data. The methodology and disclosure rules are explained in the report.

## Run the report

Open the project in RStudio with the project root as the working directory. R, Quarto, the required R packages and access to the MSDS data must be available in the secure environment.

```r
source("R/Run analysis.R")
```

This reuses loaded data where available, prepares the analysis, checks disclosure controls, and opens the finished report at `_site/index.html`.

If an input is missing, it reads the parquets once using the existing Azure setup.

To reload changed source data after loading the run script:

```r
run_maternity_project(refresh_data = TRUE)
```

Rerun the analysis after changing data or chart calculations. Rendering alone uses the previously saved results. Rendering progress and any errors appear in the R console.

## Requirements and inputs

R and Quarto must be on the PATH. Required R packages:

```r
install.packages(c("dplyr", "tidyr", "readr", "readxl", "janitor", "purrr",
  "ggplot2", "gt", "plotly", "scales", "htmltools", "knitr", "rmarkdown", "digest"))
```

The Azure reader also uses `arrow`, `AzureKeyVault`, `AzureStor`, `cli` and `glue`.
It expects `container`, `lake`, `folder` and `azure_key_vault` environment variables,
with access to the configured storage and Key Vault secret.

| Azure dataset | R object |
|---|---|
| `final_cohort_2326.parquet` | `final_cohort_2326` |
| `total_births_by_LA_2326.parquet` | `total_births_by_LA_2326` |
| `demographics.parquet` | `demographics` |
| `trust_matrix.parquet` | `trust_matrix_data` |
| `icb_matrix.parquet` | `icb_matrix_data` |

Public inputs in `data/`:

- `2023birthregistrations.xlsx`, `2024birthregistrations.xlsx`, `2025birthregistrations.xlsx`
- `2022 SNPP Births persons.csv`
- `ons_2022_based_female_population_projections_2023_geography.csv`
- `ons_live_births_la_2013_2025.csv`

Keep `data/lookups/`, `config/` and `assets/` with the project. Lookups are read
locally. Birth and population projections use the 2022-based migration-category
variant on 2023 LA geography.

## Main files

| File | Purpose |
|---|---|
| `index.qmd` | Report text, sections and tabs |
| `R/Run analysis.R` | Run the analysis and render the report |
| `R/Read parquet files from azure.R` | Load the secure inputs |
| `R/matrix of booking appts and births.R` | Sarah's standalone matrix tables, retained for separate use |
| `R/Maternity analysis.R` | Main calculations, tables and charts |
| `R/Demo Sankeys.R` | Demographic flows and cohort profiles |
| `R/demographics.R` | Demographic cleaning and lookups |
| `R/ons_data.R` | ONS readers and shared calculations |
| `R/report_cohort.R` | Pregnancy, age and geography definitions |
| `R/age_boundary_review.R` | Descriptive age comparisons |
| `R/report_colours.R` | Organisation and category colours |
| `R/disclosure.R` | Publication disclosure controls |
| `config/analysis_parameters.csv` | Model settings |
| `config/local_authorities.csv` | Local authority configuration |
| `data/` | Public ONS inputs and reference lookups |

Further calculation notes are in [ONS-analysis-notes.md](ONS-analysis-notes.md).

## Results

The runner returns `projection_master`, `projection_master_backtest`,
`projection_master_dictionary`, `delivery_proportions`, `booking_proportions`,
`ons_modifications`, `private_nhs_modifications` and `booking_modifications`
to the RStudio Environment. Use `View()` to inspect them.

Prepared report data are saved as `.ons_exploration.rds`, `.patient_flows.rds`,
`.delivery_geography.rds`, `.projections.rds` and `.demo_sankeys.rds`.
`.analysis_run.rds` identifies the completed run. These files are generated in
the secure environment and are not transferred through Git.

## Disclosure and outputs

MSDS-derived report counts are disclosure controlled: zero stays zero, counts from 1–7 become 5, and other counts round to the nearest 5. Percentages use controlled numerators and denominators, so displayed categories may not add exactly to totals. See the report's Disclosure control section for the guidance and examples.

The HTML report is the output to share. Intermediate RDS files can contain unrounded data and must remain in the secure environment.
