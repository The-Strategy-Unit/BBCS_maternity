# BBCS maternity analysis

`index.qmd` is the report. It compares two ways of converting ONS projected live
births into maternities, describes recorded patient flows and delivery activity,
and makes first-draft projections for trusts, ICBs and the combined cluster.
Method 1 is used for the service projections; Method 2 remains for comparison.

## Run the report

Run the full report on the computer with access to the Azure data. Install R and
Quarto, and work from the project folder containing `index.qmd`, `R/` and `data/`.
Open `BBCS_maternity.Rproj` in RStudio if it is available; keep the R working
directory at the project root.

These packages are used by the current scripts and report. Install them once in R:

```r
install.packages(c(
  "arrow", "AzureKeyVault", "AzureStor", "cli", "dplyr", "flextable",
  "ggplot2", "glue", "gt", "htmltools", "janitor", "knitr", "plotly",
  "purrr", "readr", "readxl", "rmarkdown", "scales", "stringr", "tibble", "tidyr"
))
```

First, run this in the secure R session:

```r
source("R/Run these to get needed objects.R")
```

This reads the Azure inputs and runs the analysis scripts in the required order.
Check any **need attention** messages before using the report; sections 8 and 9
can save an unavailable result even when the runner reaches **Finished**.

Then run this in the terminal, from the same project folder:

```sh
quarto render index.qmd
```

Open `_site/index.html`. Rendering recalculates the public ONS methods and the
imaginary example, but reads saved outputs for sections 4–9. It does not rerun the
Azure preparation. Run the preparation script again after changing those analysis
scripts or their data, then render again.

## Which files do what?

| File | Purpose |
|---|---|
| `index.qmd` | Report text, headings, tabs and calls to display charts and tables. |
| `R/Run these to get needed objects.R` | Runs Sarah's reader and matrix scripts, then the report preparation and imaginary example. |
| `R/ons_data.R` | Reads public ONS inputs and prepares the two births-to-maternities methods. |
| `R/additional_analysis.R` | Prepares sections 4–7: patient flows, cluster activity and delivery outcomes over time. |
| `R/Deliveries by LA and Age Bands.R` | Prepares section 8: LA-to-destination flows, hospital LA/age contributions and changes over time. |
| `R/Projections.R` | Prepares section 9: feeder shares, age-specific growth, total projections, percentage changes and the retrospective comparison. |
| `R/Imaginary Example.R` | Creates the fictional worked example in section 10. |
| `R/scratch area.R` | Optional column/type and flag checks; the runner does not source it. |

Keep new calculations, charts and tables in the R scripts; use the QMD to label
and organise their outputs. The runner first calls Sarah's
`R/Read parquet files from azure.R` and `R/matrix of booking appts and births.R`.

## Inputs needed

The active public ONS files must be in `data/`:

- `2023birthregistrations.xlsx`, `2024birthregistrations.xlsx` and `2025birthregistrations.xlsx`
- `2022 SNPP Births persons.csv`
- `ons_2022_based_female_population_projections_2023_geography.csv`
- `ons_live_births_la_2013_2025.csv`

Birth and population projections use the 2022-based migration-category variant
on 2023 LA geography. The older
`ons_2022_based_births_migration_category.csv` and
`ons_2022_based_female_population_projections.csv` are retained but are not selected
by the current readers. Sources and methods are in the report and
`ONS-analysis-notes.md`.

Sarah's reader loads these Azure parquet datasets into R:

| Dataset | R object used by the analysis |
|---|---|
| `final_cohort_2326.parquet` | `final_cohort_2326` |
| `total_births_by_LA_2326.parquet` | `total_births_by_LA_2326` |
| `trust_matrix.parquet` | `trust_matrix_data` |
| `icb_matrix.parquet` | `icb_matrix_data` |

The reader expects the environment variables `container`, `lake`, `folder` and
`azure_key_vault`, and access to the configured Azure storage and Key Vault secret.

### Azure access and connection setup

## Change areas and settings

- `config/local_authorities.csv` sets the LAs, display order and colours for the
  public ONS sections. Adding an LA also requires its data in the historic births
  and female-population extracts.
- `config/analysis_parameters.csv` sets the female-population age range, currently
  13–55. This does not change the service projection age bands.
- Trust and ICB labels, provider membership and the service cluster are defined in
  `R/additional_analysis.R`. The current cluster combines Black Country and
  Birmingham and Solihull. These settings are separate from the ONS LA configuration.

Section 8's hospital percentages describe where an organisation's deliveries came
from. Section 9's feeder percentages describe how much of each resident LA/age
group delivered at that organisation. Home births stay in the LA totals but are
not counted as hospital-location deliveries.

Projections start from actual 2025/26 counts and apply Method 1 ONS age-specific
growth afterwards, relative to mid-2026. Section 9's coverage tables show records
held constant and any boundary proxies used.

## Saved outputs and missing sections

The preparation scripts save these files in the project root:

| Output | Used for |
|---|---|
| `.patient_flows.rds` | Sections 4–7 |
| `.delivery_geography.rds` | Section 8 |
| `.projections.rds` | Section 9, including the retrospective comparison |

If a section is missing or unavailable, read its message and the preparation
console output. Correct the reported input issue, rerun preparation and render
again. Re-rendering alone will not refresh these files.

Generated RDS files and HTML stay local and are ignored by Git. Prepare the RDS
files on the data computer; they are not transferred with code changes. Current
and future R scripts under `R/` are allowed by `.gitignore`.
