# BBCS maternity analysis

`index.qmd` is the report. It converts ONS projected live births into maternities
using individual-age stillbirth/multiplicity conversions, describes recorded patient flows and delivery activity,
and makes first-draft projections for trusts, ICBs and the combined cluster.
Method 1 is used throughout; section 3 explains the ONS boundary-age conversion and local age weights.

## Run the report

Open the project in RStudio, with the project root as the working directory.
Run this command in the secure environment:

```r
source("R/Run analysis.R")
```

The command reads Sarah's prepared parquet inputs, prepares the analysis, runs
QA, renders `_site/index.html` and opens the finished report in the configured
browser. Reading and analysis run in the current
RStudio session, keeping its existing Azure setup. The command replaces the
previous outputs. If preparation or rendering fails, partial outputs are removed.
Rendering uses a temporary folder to avoid clashes with an editor render.
A preview opened during preparation waits for that run to finish.
The console shows a short QA summary, details of items requiring review, and
the finished report path; no separate QA script is needed.

The completed `projection_master`, `projection_master_backtest`, `ons_modifications`,
`delivery_proportions`, `booking_proportions`, column dictionary,
`qa_results` and `qa_details` are returned to the RStudio session for inspection.
Check `subset(qa_results, Result == "Check")` before using real results.
The runner requires the existing Azure access and packages in the secure
environment. The project requires R and Quarto on the PATH. Main analysis packages:

```r
install.packages(c("dplyr", "tidyr", "readr", "readxl", "janitor", "purrr",
  "ggplot2", "gt", "plotly", "scales", "htmltools", "knitr", "rmarkdown"))
# The parquet reader additionally uses arrow, AzureKeyVault, AzureStor, cli and glue.
```

## Where to edit

| File | Purpose |
|---|---|
| `R/Maternity analysis.R` | Main editing file: general settings/labels, ONS exploration, MSDS exploration, projection model, charts and tables. |
| `R/Run analysis.R` | Single run script: replace previous outputs, read secure data, analyse, check, render and return the completed objects to RStudio. |
| `R/Read parquet files from azure.R` | Sarah's unchanged Azure reader, run first on every analysis run. |
| `R/matrix of booking appts and births.R` | Sarah's original standalone matrix tables. Retained for separate use; the report builds its tables in the main analysis. |
| `R/ons_data.R` | Shared public-source readers and ONS chart/table functions. |
| `R/report_cohort.R` | Shared pregnancy-counting and field definitions. |
| `R/age_boundary_review.R` | Shared descriptive age-review functions. |
| `R/QA.R` | Existing count/reconciliation checks, run automatically by the launcher. |
| `index.qmd` | Existing report text, tabs and display chunks. Reads prepared ONS/MSDS outputs. |

The former activity, delivery-geography, projection-master and projection-chart
scripts are consolidated in `R/Maternity analysis.R`. The report's existing views
and saved-object names are retained.

The terminology key is at the top of the main script. Search `MODEL_INPUT:` for
data taken forward and `CONTEXT_ONLY` for exploration. `Age (1)` means individual
age and `Age (B)` a reporting band. ONS <=15 and 44+ remain explicit combined
source boundaries. `LA(R)` means residence. Booking/delivery provider codes are
`Trust(B/D)`; they are not booking/delivery LA codes. `LA(C)` denotes the set of
resident LAs contributing to a stated catchment, not a new administrative LA.

Model factors and allocation shares apply at LA(R) and the most detailed available
age before any chart bands. Final delivery and booking counts sum separately to
their provider trusts, then provider ICBs, then the cluster. Home, unknown and
unallocated contributions remain in the master.

## Inputs needed

The active public ONS files must be in `data/`:

- `2023birthregistrations.xlsx`, `2024birthregistrations.xlsx` and `2025birthregistrations.xlsx`
- `2022 SNPP Births persons.csv`
- `ons_2022_based_female_population_projections_2023_geography.csv`
- `ons_live_births_la_2013_2025.csv`

Birth and population projections use the 2022-based migration-category variant
on 2023 LA geography. Sources and methods are in the report and
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
| `trust_matrix.parquet` | `trust_matrix_data` (Sarah's standalone tables) |
| `icb_matrix.parquet` | `icb_matrix_data` (Sarah's standalone tables) |

The reader expects the environment variables `container`, `lake`, `folder` and
`azure_key_vault`, and access to the configured Azure storage and Key Vault secret.

## Change areas and settings

- `config/local_authorities.csv` sets the LAs, display order and colours for the
  public ONS sections. Adding an LA also requires its data in the historic births
  and female-population extracts.
- `config/analysis_parameters.csv` sets the female-population age range, currently
  13–55. This does not restrict the ages in the service projection.
- Trust and ICB labels, provider membership and the service cluster are defined in
  `R/Maternity analysis.R`. The current cluster combines Black Country and
  Birmingham and Solihull. These settings are separate from the ONS LA configuration.
- Sections 9.2, 9.3 and 10.1 keep the configured cluster LAs visible, then show the ten
  largest additional feeders and group the rest as "All other feeder LAs".
  Section 9.1 uses five additional flows, selected separately within each chart.
  Main cluster areas, home births and unknowns stay separate. The grids show the
  main areas, ten largest others overall and an all-other chart. Full tables keep every flow.

Section 9 groups compact charts into trust and ICB/cluster grids, with separate
tabs for each measure and the full tables. The grids are built from the organisation
list; the public-data age-band tabs remain explicit in `index.qmd`.

Section 9's hospital percentages describe where an organisation's deliveries came
from. Section 10's feeder percentages describe how much of each resident LA/age
group delivered at that organisation. Home births stay in the LA totals but are
not counted as hospital-location deliveries.

Projections start with ONS births (L). The runner returns `projection_master`,
`ons_modifications`, `delivery_proportions` and `booking_proportions` to RStudio.
Use `View()` to inspect them. The master has one LA(R)/Age (1)/year row and separate
destination proportion/count columns. An unsplit boundary remains an explicit
source-group row where local age information is unavailable.

Calculation steps:

1. Pool 2023/24–2025/26 resident counts. Split combined ONS ages using each LA's maternity age mix; preserve each ONS births (L) total.
2. Join `ons_modifications`: at each published age, pool 2023–2025 Table 5 counts, then divide stillbirths by live births; multiply births (L) by **1 + that ratio** to obtain births (L+S).
3. Multiply by pooled maternities/pooled births (L+S), using the same Table 5 counts for 2023–2025. Ages below 14 share the under-14 factor; ages 50+ share its published factor. Other ages use their own factors.
4. Delivery proportions = pooled destination deliveries / resident maternities. Booking proportions = pooled destination bookings / all bookings, including those without a recorded delivery. Both use LA(R)/Age (1) and remain fixed over future years.
5. Multiply each starting-volume column by its own destination proportions. Aggregate completed delivery and booking counts separately into age bands, trusts, ICBs and cluster.

The master starts with LA, Age, Year and three review columns:
`Exact_age_live_births` (births (L), age split only), `Delivery_projection_base`
and `Booking_projection_base` (separate adjusted allocation volumes).
The two allocation volumes currently equal adjusted maternities. No early-loss uplift is applied.
Any future booking uplift belongs before booking allocation and must not alter deliveries.
Section 10.6 shows booking totals, age bands and change from the modelled reference year.
Home, unknown, unrepresented and unallocated destinations remain explicit.
ONS source counts repeat across split ages; `Exact_age_live_births` is additive.

`projection_master_backtest` applies the same pooled proportions to historical
ONS counts. It is an in-sample retrospective comparison, not independent validation.
No modelled reference is forced to match observations. Section 10.4 retains the
observed 2025/26 comparator. Resident charts use the same adjusted master values
before destination allocation, retaining 2023–2047. All final reporting uses
Under 20, 20–24, 25–29, 30–34, 35–39, 40–44 and 45+; grouping follows calculation.
Historical banded births (L) and published maternities are displayed directly,
without a second estimated-maternity conversion.

The report requires calculation version `ons-shared-age-calculations-v10`.
The three ONS registration workbooks were checked against the public downloads
on 6 October 2026 and matched exactly. Table 5 supplies both modifiers; Table 7
remains descriptive evidence about multiplicity.

Sarah's supplied cohort is already deduplicated. The report counts each supplied
pregnancy row once, including rows with missing identifiers. Delivery-year totals
require `birth_flag = 1` and a valid `birth_yr`; hospital totals additionally require
`home_birth = 0` and a cluster delivery provider. Booking-year pathway denominators
retain all recorded cluster starts and follow their supplied outcomes, including
later deliveries. Section 4 shows inclusion/exclusion counts. Method 1 contains
the main-LA code comparison; section 11 illustrates the actual calculation.

Section 6.e shows the two ICBs and the combined cluster together. The cluster
counts combine the same booking starts shown in the two ICB charts.

Section 8 repeats section 7 with five outcome lines: the within-cluster delivery
group is split into the same trust as booking and a different cluster trust.
The trust-comparison panels retain this split when grouped into ICB and cluster totals.
Separate ICB-comparison tabs in 8.1 and 8.2 compare the booking trust's ICB with
the delivery trust's ICB: a transfer between trusts in the same ICB stays in the
same-ICB group. The combined cluster pools these comparisons across both ICBs.
Both views retain the same all-starts booking-year denominators, follow-up rules
and relative-percentage-change calculation; they do not use residence ICB.

## Saved outputs and missing sections

The preparation scripts save these files in the project root:

| Output | Used for |
|---|---|
| `.ons_exploration.rds` | ONS source exploration, shared resident projections and age-review objects |
| `.patient_flows.rds` | Sections 4–8 |
| `.delivery_geography.rds` | Section 9 |
| `.projections.rds` | Section 10, master and retrospective comparison |
| `.qa.rds` | QA results and detailed checks |
| `.analysis_run.rds` | Completion status and run identity for all current outputs |

The QMD reads these files directly. If one is missing, R reports an error rather
than skipping its section. Run preparation again to refresh the saved outputs;
re-rendering alone will not refresh these files.

Generated RDS files and HTML stay local and are ignored by Git. Prepare the RDS
files on the data computer; they are not transferred with code changes. Current
and future R scripts under `R/` are allowed by `.gitignore`.

## Review the QA results

The entry script runs the QA and returns the results. To inspect items needing review:

```r
subset(qa_results, Result == "Check")
```

The QA script reports plain-language results and lists any records or totals that
need attention. It runs automatically before the runner renders the report.
Run `R/Run analysis.R` after changing inputs or calculations, and review QA before
using a new version of the results.

The checks now reconcile annual cluster totals across sections 5, 6, 9 and 10,
recorded activity across sections 9 and 10, master-table source conservation, and chart
percentages against incoming counts. These comparisons are available in
`qa_details`. To see only items needing attention, use
`subset(qa_results, Result == "Check")`. The QA script does not deduplicate inputs.

For a local check without MSDS or Azure, run `Rscript R/test_report_consistency.R`.
It uses invented pregnancy records and public ONS files, keeps its outputs in
memory, and does not read or overwrite the secure report's saved RDS files.

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
| **Estimated / projected maternities** | Counts calculated from projected births (L), after the local age split and age-specific stillbirth/multiplicity adjustments. |
| **Maternity modifier** | [1 + S/L] × M/(L+S), with S, L and M pooled from 2023–2025 Table 5 counts at each published age. Multiplies projected live births to estimate maternities. |
| **General fertility rate (GFR)** | Live births per 1,000 women aged 15–44 in this report. It describes frequency relative to population size, not the number needing care. |
| **Age-specific fertility rate (ASFR)** | Live births per 1,000 women in the relevant age group. |
| **Total fertility rate (TFR)** | Children per woman if the year's age-specific fertility rates applied throughout childbearing life. Not an annual maternity count. |

**The conversion in one line:** projected live-born babies × maternity modifier = estimated maternities. Split combined ONS ages first, apply each published-age factor, then add completed results. Birth rates are a separate contextual measure and are not used instead of this conversion.

For illustration only, a modifier of **0.99** means an estimated **99 maternities for every 100 live-born babies**. A projection of 10,000 live births would therefore become 9,900 maternities if that modifier applied throughout. The actual analysis uses different age-specific modifiers. Method 1 includes both stillbirths and multiplicity. Its factors change the absolute ONS counts allocated to trusts. MSDS already includes live-birth and stillbirth maternities and supplies allocation rates without an uplift. Each factor uses pooled counts before division; their product is applied to births (L).

### Using these figures for pathway demand and capacity

Use **maternities as the starting volume for pathways associated with pregnancies reaching birth**. To turn that volume into service activity, specify the proportion using each pathway and the contacts per maternity. For baby-based services, retain baby counts. To model all antenatal or early-pregnancy demand, also account for pregnancies that do not reach an ONS-defined maternity.

Three further distinctions matter:

- **Residence versus provider:** these ONS figures concern mothers living in an area. Allocating demand to trusts requires patient-flow information.
- **Birth year versus care year:** antenatal care may occur before the year of birth and postnatal care afterwards. Annual maternity counts do not directly give appointments in the same year.
- **Demand versus capacity:** activity estimates still need pathway mix, staff time and other resource requirements before they become capacity requirements.
