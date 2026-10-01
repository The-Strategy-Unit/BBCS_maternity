# BBCS maternity analysis

`index.qmd` is the report. It compares two ways of converting ONS projected live
births into maternities, describes recorded patient flows and delivery activity,
and makes first-draft projections for trusts, ICBs and the combined cluster.
Method 1 is used for the service projections; Method 2 remains for comparison.

## Run the report

Run the full report in UDAL since it needs to be able to access Azure data. 

Install R and Quarto, and work from the project folder containing `index.qmd`, `R/` and `data/`.
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
source("R/Run analysis.R")
```

This reads the Azure inputs and runs the analysis scripts in the required order.
An error stops preparation at the problem. Old saved outputs are cleared before
preparation starts, so a failed run cannot leave old charts looking current.

Then run this in the terminal, from the same project folder:

```sh
quarto render index.qmd
```

Open `_site/index.html`. Rendering recalculates the public ONS methods, but reads
saved outputs for sections 4–10. It does not rerun the
Azure preparation. Run the preparation script again after changing those analysis
scripts or their data, then render again.

To view an individual chart or table in RStudio, run the preparation script first,
then use the green Run button on that chart or table's chunk. Section setup chunks
only load saved objects and do not display a chart.

## Which files do what?

| File | Purpose |
|---|---|
| `index.qmd` | Report text, headings, tabs and calls to display charts and tables. |
| `R/Run analysis.R` | Runs Sarah's reader and matrix scripts, then the report preparation and imaginary example. |
| `R/ons_data.R` | Reads public ONS inputs and prepares the two births-to-maternities methods. |
| `R/additional_analysis.R` | Prepares sections 4–7: patient flows, cluster activity and delivery outcomes over time. |
| `R/Deliveries by LA and Age Bands.R` | Prepares section 8: LA-to-destination flows, hospital LA/age contributions and changes over time. |
| `R/Projections.R` | Prepares section 9: feeder shares, age-specific growth, total projections, percentage changes and the retrospective comparison. |
| `R/Imaginary Example.R` | Creates the fictional worked example in section 10. |
| `R/QA.R` | Checks the data and counts when you choose to run it; the runner does not source it. |
| `R/scratch area.R` | Optional column/type and flag checks; the runner does not source it. |
| `data/lookups/la_names.csv` | Public UK LA codes and names, including older codes used in the comparison years. |
| `data/lookups/trust_names.csv` | Public NHS trust codes and names, including closed trusts. |

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

The two small files in `data/lookups/` supply chart and table labels. Keep them
with the code when moving the project into the secure environment; the analysis
reads these local copies and does not download lookups when it runs. They change
names only, not LA boundaries, trust membership or projection calculations.

Both were downloaded on 1 October 2026. `la_names.csv` combines the ONS
[April 2025 LA names and codes](https://www.data.gov.uk/dataset/b2c91962-58e7-40f1-ad56-7aa2473a93fd/local-authority-districts-april-2025-names-and-codes-in-the-uk-v21)
with codes present only in the
[April 2023 list](https://www.data.gov.uk/dataset/fb6edb97-5fff-4bce-a74c-3aa4f167d307/local-authority-districts-april-2023-names-and-codes-in-the-uk).
`trust_names.csv` contains the code and name columns from the
[NHS ODS trusts download](https://digital.nhs.uk/services/organisation-data-service/data-search-and-export/csv-downloads/other-nhs-organisations).

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
- Sections 8 and 9.1 keep the configured cluster LAs visible, then show the ten
  largest additional feeders and group the rest as "All other feeder LAs".
  Section 8.1 also shows the six main trusts and ten largest other delivery
  organisations, then "All other trusts". Full tables keep every flow.

Organisation and age-band tabs are written explicitly in `index.qmd`. If you
change those groups, update the corresponding tab headings and output calls too.

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
| `.imaginary_example.rds` | Section 10: imaginary worked example |

The QMD reads these files directly. If one is missing, R reports an error rather
than skipping its section. Run preparation again to refresh the saved outputs;
re-rendering alone will not refresh these files.

Generated RDS files and HTML stay local and are ignored by Git. Prepare the RDS
files on the data computer; they are not transferred with code changes. Current
and future R scripts under `R/` are allowed by `.gitignore`.

## Run the QA checks separately

After preparing the analysis, run this in the same secure R session when you want
to check the inputs, counts and projections:

```r
source("R/QA.R")
```

The QA script reports plain-language results and lists any records or totals that
need attention. It is not run by `Run analysis.R` or when rendering the report.
Run it after changing data, organisation membership or calculations, and before
using a new version of the results.

## Terminology key

**The main output of this analysis is estimated maternities: pregnancies reaching a live birth or stillbirth.** Babies, individual women and appointments are different units.

### Core counting terms

| Term | Plain-English meaning | Use in this analysis or pathway model |
|---|---|---|
| **Live birth** | One baby born showing signs of life. | The unit in the ONS birth projections used here. Two live-born twins count as two. |
| **Stillbirth** | One baby born without signs of life at 24 or more completed weeks. | Excluded from live-birth counts; included in the definition of a maternity. |
| **Total births** | Live births plus stillbirths: a count of babies. | Always specify “live births” or “total births”; the projected input here is live births. |
| **Maternity** | One pregnancy resulting in at least one live birth or stillbirth. | Our main counting unit. Twins or triplets from one pregnancy count as one maternity. |
| **All pregnancies** | Pregnancies regardless of outcome. | Broader than maternities. Early losses and terminations without a registrable birth require additional data for pathway modelling. |
| **Unique women / mothers** | Individual people counted once during a defined period. | Not supplied by the maternity total: one woman can have more than one maternity during that period. |
| **Singleton / multiple maternity** | One baby / two or more babies from one pregnancy. | “Multiplicity” describes how many babies are involved. It explains why baby and maternity counts differ. |
| **Contact, appointment or episode** | A unit of recorded healthcare activity, defined by the service dataset. | One maternity can generate many contacts. These are not counted by ONS maternity totals. |

The ONS definitions above follow the [birth statistics user guide](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/methodologies/userguidetobirthstatistics). The final column explains how each measure is used in this analysis or could inform pathway modelling.

### Terms used in the calculations

| Term | Meaning here |
|---|---|
| **Observed / published count** | An ONS figure derived from registered events that have already happened. |
| **Projected live births** | Future baby counts from the ONS demographic model, conditional on its assumptions. |
| **Estimated / projected maternities** | Counts calculated by this analysis from observed or projected live births using a maternity modifier. |
| **Maternity modifier** | Method 1: reciprocal of expected babies per maternity from age-specific multiplicity proportions. Method 2: pooled maternities divided by live births in each age band. Both multiply projected births to estimate maternities. |
| **General fertility rate (GFR)** | Live births per 1,000 women aged 15–44 in this report. It describes frequency relative to population size, not the number needing care. |
| **Age-specific fertility rate (ASFR)** | Live births per 1,000 women in the relevant age group. |
| **Total fertility rate (TFR)** | Children per woman if the year's age-specific fertility rates applied throughout childbearing life. Not an annual maternity count. |

**The conversion in one line:** projected live-born babies × maternity modifier = estimated maternities. Calculate this within each maternal age group, then add the results. Birth rates are a separate contextual measure and are not used instead of this conversion.

For illustration only, a modifier of **0.99** means an estimated **99 maternities for every 100 live-born babies**. A projection of 10,000 live births would therefore become 9,900 maternities if that modifier applied throughout. The actual analysis uses different age-specific modifiers. Method 1 adjusts for multiplicity without a separate stillbirth adjustment. Method 2 includes all maternities, including stillbirth-only maternities, in its numerator.

### Using these figures for pathway demand and capacity

Use **maternities as the starting volume for pathways associated with pregnancies reaching birth**. To turn that volume into service activity, specify the proportion using each pathway and the contacts per maternity. For baby-based services, retain baby counts. To model all antenatal or early-pregnancy demand, also account for pregnancies that do not reach an ONS-defined maternity.

Three further distinctions matter:

- **Residence versus provider:** these ONS figures concern mothers living in an area. Allocating demand to trusts requires patient-flow information.
- **Birth year versus care year:** antenatal care may occur before the year of birth and postnatal care afterwards. Annual maternity counts do not directly give appointments in the same year.
- **Demand versus capacity:** activity estimates still need pathway mix, staff time and other resource requirements before they become capacity requirements.
