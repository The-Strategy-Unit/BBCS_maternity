# BBCS maternity analysis

This project produces the BBCS maternity report, covering recorded bookings and deliveries, patient flows, population demographics and demand projections using MSDS and ONS data. The methodology and disclosure rules are explained in the report.

## Run the report

Open the project in RStudio with the project root as the working directory. R, Quarto, the required R packages and access to the MSDS data must be available in the secure environment.

```r
source("R/Run analysis.R")
```

This reuses loaded data where available, prepares the analysis, checks disclosure controls, and opens the finished report at `_site/index.html`.

To reload changed source data after loading the run script:

```r
run_maternity_project(refresh_data = TRUE)
```

Rerun the analysis after changing data or chart calculations. Rendering alone uses the previously saved results. If rendering fails, check `render.log`.

## Main files

| File | Purpose |
|---|---|
| `index.qmd` | Report text, sections and tabs |
| `R/Run analysis.R` | Run the analysis and render the report |
| `R/Maternity analysis.R` | Main calculations, tables and charts |
| `R/Demo Sankeys.R` | Demographic flows and cohort profiles |
| `R/disclosure.R` | Publication disclosure controls |
| `config/analysis_parameters.csv` | Model settings |
| `config/local_authorities.csv` | Local authority configuration |
| `data/` | Public ONS inputs and reference lookups |

Further calculation notes are in [ONS-analysis-notes.md](ONS-analysis-notes.md).

## Disclosure and outputs

MSDS-derived report counts are disclosure controlled: zero stays zero, counts from 1–7 become 5, and other counts round to the nearest 5. Percentages use controlled numerators and denominators, so displayed categories may not add exactly to totals. See the report's Disclosure control section for the guidance and examples.

The HTML report is the output to share. Intermediate RDS files can contain unrounded data and must remain in the secure environment.
