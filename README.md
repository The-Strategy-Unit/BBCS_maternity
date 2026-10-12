# BBCS maternity analysis

`index.qmd` is the report. It converts ONS projected live births into maternities
using age distribution, stillbirth, multiplicity and Bookings uplift modifiers, describes recorded patient flows and delivery activity,
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
`delivery_proportions`, `booking_proportions`, `booking_modifications`, column dictionary,
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
data taken forward and `CONTEXT_ONLY` for exploration. `Age (SY)` means individual
age and `Age (B)` a reporting band. ONS <=15 and 44+ remain explicit combined
source boundaries. `LA(R)` means residence. Booking/delivery provider codes are
`Trust(B/D)`; they are not booking/delivery LA codes. `LA(C)` denotes the set of
resident LAs contributing to a stated catchment, not a new administrative LA.

Model factors and allocation shares apply at LA(R) and the most detailed available
age before any chart bands. Final delivery and booking counts sum separately to
their provider trusts, then provider ICBs, then the cluster. Home, unknown and
unallocated contributions remain in the master.

## Inputs needed

The analysis reads the following public ONS files from `data/`:

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
- `config/analysis_parameters.csv` sets ages **13–54** for the model, its MSDS
  rates and age weights, and the female-population context charts. The model
  does not expand this range to include other ages found in the supplied data.
- Trust and ICB labels, provider membership and the service cluster are defined in
  `R/Maternity analysis.R`. The current cluster combines Black Country and
  Birmingham and Solihull. These settings are separate from the ONS LA configuration.
- The hospital-delivery and pooled-allocation charts keep the configured cluster LAs visible, then show the ten
  largest additional feeders and group the rest as "All other feeder LAs".
  The resident-LA destination charts use five additional flows, selected separately within each chart.
  Main cluster areas, home births and unknowns stay separate. The grids show the
  main areas, ten largest others overall and an all-other chart. Full tables keep every flow.

The delivery-geography section groups compact charts into trust and ICB/cluster grids, with separate
tabs for each measure and the full tables. The grids are built from the organisation
list; the public-data age-band tabs remain explicit in `index.qmd`.

The hospital-delivery percentages describe where an organisation's deliveries came
from. The pooled-allocation percentages describe how much of each resident LA/age
group delivered at that organisation. Home births stay in the LA totals but are
not counted as hospital-location deliveries.

Projections start with ONS births (L). The runner returns `projection_master`,
`ons_modifications`, `booking_modifications`, `delivery_proportions` and `booking_proportions` to RStudio.
Use `View()` to inspect them. The master has one LA(R)/Age (SY)/year row and separate
destination proportion/count columns. An unsplit boundary remains an explicit
source-group row where local age information is unavailable.

Calculation steps:

1. Use pooled 2023/24–2025/26 resident-LA deliveries to split ONS ≤15 across
   ages 13–15 and ONS 44+ across ages 44–54; preserve each source live-birth total.
   Ages with no recorded activity have zero counts and zero boundary-age shares.
2. Apply the stillbirth factor, 1 + pooled S/L, from national Table 5 counts for
   2023–2025 at each published age.
3. Apply pooled M/(L+S) from the same national counts.
4. Apply the Bookings uplift: all supplied cohort pregnancies / recorded cohort deliveries for each cluster LA/age. Outside LAs use the pooled same-age rate among
   cluster residents as a proxy; use that proxy for missing local evidence too.
5. Divide each delivery or booking category count by the shared denominator:
   resident-LA births × Bookings uplift. Apply these independent proportions to
   the same adjusted projection. Sum finished counts to age bands and providers.

Use supplied cohort rows aged 13–54 without an extra date filter. Only No recorded
delivery drives the fourth modifier. Home birth and Delivery location unknown
remain recorded births. Where no same-age cluster ratio exists, flag the
projection as unestimated; do not substitute a ratio pooled across ages.

The master retains `Exact_age_live_births`, `Bookings_uplift_modifier`, its
source, the shared denominator, source counts and destination proportions/counts.
`Delivery_projection_base` and `Booking_projection_base` both equal
`Adjusted_maternities` after all four modifiers.

`National_conversion_factor` is the product of the stillbirth and multiplicity
factors. The master's `Maternity_modifier` also includes the Bookings uplift;
it is applied to `Exact_age_live_births`, after the age split.

**Booking Denominator Factor** is the shared denominator minus supplied cohort
pregnancies. It is an estimated residual group shown in the booking-projection tables. It remains separate from observed booking
providers, No recorded booking (provider and date both absent), and Booking
provider unknown. Resident deliveries absent from the linked cohort remain a separate
delivery category. Negative residuals and conflicting source counts are flagged
and left unallocated, not silently clamped or assigned to a provider.

Every configured or supplied MSDS LA is checked against the recorded resident delivery counts
and ONS births. Missing source codes are QA issues: the console lists
the affected LAs and the source to review. The complete check is retained as
`la_source_matching` in the projection outputs and QA details. An outside LA without linked
cohort rows can use the agreed same-age cluster proxy; missing resident or ONS
source data requires investigation.

Provider coverage follows supplied records aged 13–54, including rows without a usable
LA or ONS match, and retains unknown-age records for checks. Known ages outside
13–54 do not count as missing projection coverage. If a known provider flow cannot be projected, its reported
total and percentage change remain unavailable. Known-age gaps affect that age
band; missing-age records can affect every band. `Available_projected` retains
the successfully allocated portion for accounting, and `Projection_complete`
identifies whether the reported estimate is usable. Unmatched provider flows are
QA issues requiring investigation. Flow diagrams label partial
allocations and do not present missing coverage as a fall in demand.

Projected Booking Denominator Factor totals remain unavailable where a positive
source contribution cannot be estimated or allocated. Zero source contributions
remain zero. The detailed ONS growth tables use the source birth counts once per
LA/source age/year, independently of the availability of individual-age modifiers.

Review `booking_modifications` for differences between resident and cohort
births; check the Databricks processing when these differ. Cluster proxy rates
are explicitly assumed for outside-LA flows. These are not national MSDS ratios.
If available locally, `R/check_projection_cohort.R` provides additional aggregate
checks in the secure R session. This optional script is not distributed in Git.

`projection_master_backtest` applies the same pooled proportions to historical
ONS counts. It is an in-sample retrospective comparison, not independent validation.
No modelled reference is forced to match observations. The delivery-change section retains the
observed 2025/26 comparator. Resident charts use the same adjusted master values
before destination allocation, retaining 2023–2047. All final reporting uses
Under 20, 20–24, 25–29, 30–34, 35–39, 40–44 and 45+; grouping follows calculation.
Historical banded births (L) and published maternities are displayed directly,
without a second estimated-maternity conversion.

Sections 3.3 and 3.4 both show combined organisation charts and separate panels
for trusts, ICBs and the cluster. Total charts compare blue modelled projections,
orange ONS base projections and green recorded MSDS counts. The ONS line is
`Exact_age_live_births × destination proportion`, before stillbirth, multiplicity
and booking adjustments, using the same provider shares as the model. It remains
a live-birth baseline, not an ONS provider or booking forecast. Historical points
use the existing delivery-year or booking-year counts for 2023/24–2025/26; their
x-axis position is the financial year's start. Missing projection coverage stays
unavailable, while recorded historical counts remain visible.

Both sections retain age-band charts and total-change charts, with age-band change
also available for bookings. Existing percentage-change references are unchanged:
recorded 2025/26 for delivery totals; modelled 2025 for booking totals and age bands.
Run `Rscript R/test_projection_scale.R` to check the three chart series with
invented data and confirm that outside-LA bookings use the shared resident
denominator rather than being scaled up to the whole LA population.

The report requires calculation version `ons-shared-age-calculations-v16`.
The three ONS registration workbooks were checked against the public downloads
on 6 October 2026 and matched exactly. Table 5 supplies the two national modifiers; Table 7
remains descriptive evidence about multiplicity.

Sarah's supplied cohort is already deduplicated. The report counts each supplied
pregnancy row once, including rows with missing identifiers. Delivery-year totals
require `birth_flag = 1` and a valid `birth_yr`; hospital totals additionally require
`home_birth = 0` and a cluster delivery provider. Booking-year pathway denominators
retain all recorded cluster starts and follow their supplied outcomes, including
later deliveries. The MSDS input section shows inclusion/exclusion counts. ONS
exploration contains the main-LA code comparison; the projection method section
illustrates the calculations.

The booking-ICB flow charts show the two ICBs and the combined cluster together. The cluster
counts combine the same antenatal bookings shown in the two ICB charts.

The booking-organisation charts use five outcome lines: the within-cluster delivery
group is split into the same trust as booking and a different cluster trust.
The trust-comparison panels retain this split when grouped into ICB and cluster totals.
Separate ICB-comparison tabs compare the booking trust's ICB with
the delivery trust's ICB: a transfer between trusts in the same ICB stays in the
same-ICB group. The combined cluster pools these comparisons across both ICBs.
Both views use all antenatal bookings in the booking year as their denominator, with the same follow-up rules
and relative-percentage-change calculation; they do not use residence ICB.

## Saved outputs and missing sections

The preparation scripts save these files in the project root:

| Output | Used for |
|---|---|
| `.ons_exploration.rds` | ONS source exploration, shared resident projections and age-review objects |
| `.patient_flows.rds` | MSDS activity and patient-flow exploration |
| `.delivery_geography.rds` | Deliveries by LA(R) and age |
| `.projections.rds` | Projections, master and retrospective comparison |
| `.qa.rds` | QA results and detailed checks |
| `.analysis_run.rds` | Completion status and run identity for all current outputs |

The QMD reads these files directly. If one is missing, R reports an error rather
than skipping its section. Run preparation again to refresh the saved outputs;
re-rendering alone will not refresh these files.

Generated RDS files and HTML stay local and are ignored by Git. Prepare the RDS
files on the data computer; they are not transferred with code changes.
`.gitignore` explicitly includes the seven R scripts required by the report and
Sarah's optional standalone matrix script. Other R scripts, including
`R/test_report_consistency.R` and `R/check_projection_cohort.R`, stay local and are
not needed to run the report. New checking scripts are ignored automatically;
add any new runtime dependency to the `.gitignore` allowlist before sharing it.

## Review the QA results

The entry script runs the QA and returns the results. To inspect items needing review:

```r
subset(qa_results, Result == "Check")
```

The QA script reports plain-language results and lists any records or totals that
need attention. It runs automatically before rendering. QA results stay in the console and saved analysis objects; they are not report sections.
Run `R/Run analysis.R` after changing inputs or calculations, and review QA before
using a new version of the results.

The checks reconcile annual cluster totals across activity and patient-flow views,
recorded delivery comparators, master-table source conservation, pooled ONS
modifiers, and chart percentages against incoming counts. These comparisons are available in
`qa_details`. To see only items needing attention, use
`subset(qa_results, Result == "Check")`. The QA script does not deduplicate inputs.

If you have the optional local checking scripts, run
`Rscript R/test_report_consistency.R` for a check without MSDS or Azure.
It also requires the local `R/check_projection_cohort.R` script. These scripts
are excluded from Git. The check uses invented pregnancy records and public ONS
files, keeps its outputs in memory, and does not read or overwrite the secure
report's saved RDS files.

## Terminology key

**For this project, a maternity is one pregnancy entering maternity care through booking and/or delivery.** This includes live births, stillbirths and booked pregnancies without a recorded delivery.

### Core counting terms

| Term | Meaning | Use in this analysis or pathway model |
|---|---|---|
| **Live birth** | One baby born showing signs of life. | The unit in the ONS birth projections used here. Two live-born twins count as two. |
| **Stillbirth** | One baby born without signs of life at 24 or more completed weeks. | Excluded from live-birth counts; included in the definition of a maternity. |
| **Total births** | Live births plus stillbirths: a count of babies. | Always specify “live births” or “total births”; the projected input here is live births. |
| **Maternity — this project** | One pregnancy entering maternity care through booking and/or delivery, regardless of pregnancy outcome. | Includes live births, stillbirths and booked pregnancies without a recorded delivery. Counted once per pregnancy: a booking followed by a delivery is the same maternity, and twins or triplets also count as one. |
| **Maternity — ONS** | One pregnancy resulting in at least one live birth or stillbirth. | The source definition used in ONS maternity tables and conversion factors. |
| **Delivery** | One pregnancy resulting in at least one live birth or stillbirth, counted once even when there are twins or triplets. | MSDS resident delivery totals include home deliveries and deliveries outside the cluster. Trust delivery totals require a recorded non-home delivery at that provider. Pregnancies without a recorded delivery are excluded from delivery counts. |
| **Antenatal booking** | A pregnancy with recorded antenatal booking information, counted once rather than once per appointment. | Provider counts use the recorded booking provider, including pregnancies without a recorded delivery. Historical counts also need a booking date to identify the financial year. “Bookings” is used as a short form of “antenatal bookings”. |
| **All pregnancies** | Pregnancies regardless of outcome or contact with maternity services. | The project cohort covers those entering the maternity pathway. |
| **Unique women / mothers** | Individual people counted once during a defined period. | Not supplied by the maternity total: one woman can have more than one maternity during that period. |
| **Singleton / multiple maternity** | One baby / two or more babies from one pregnancy. | “Multiplicity” describes how many babies are involved. It explains why baby and maternity counts differ. |
| **Contact, appointment or episode** | A unit of recorded healthcare activity, defined by the service dataset. | One maternity can generate many contacts. These are not counted by ONS maternity totals. |

ONS source definitions follow the [birth statistics user guide](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/methodologies/userguidetobirthstatistics). The table distinguishes these from our project use of “maternity”.

### Terms used in the calculations

| Term | Meaning here |
|---|---|
| **Recorded / observed / published count** | MSDS recorded activity or ONS registered events that have already happened. The source and counting unit are identified alongside the figure. |
| **Projected live births** | Future baby counts from the ONS demographic model, conditional on its assumptions. |
| **Estimated / projected maternities** | Counts calculated from projected births (L), after all four modifiers: age distribution, stillbirth, multiplicity and Bookings uplift. |
| **Maternity modifier** | [1 + S/L] × M/(L+S) × Bookings uplift, applied after the age split. S, L and M are national Table 5 counts pooled across 2023–2025. |
| **General fertility rate (GFR)** | Live births per 1,000 women aged 15–44 in this report. It describes frequency relative to population size, not the number needing care. |
| **Age-specific fertility rate (ASFR)** | Live births per 1,000 women in the relevant age group. |
| **Total fertility rate (TFR)** | Children per woman if the year's age-specific fertility rates applied throughout childbearing life. Not an annual maternity count. |

**The conversion in one line:** projected live-born babies × age weight × [1 + S/L] × M/(L+S) × Bookings uplift = adjusted maternities. Split combined ONS ages first, apply the factors at each LA/age, then add completed results. Birth rates are a separate contextual measure and are not used instead of this conversion.

For illustration, national factors with a product of 0.99 and a Bookings uplift
of 1.25 give 123.75 adjusted maternities per 100 live births at an age with weight
1. The model applies each LA/age's factors separately before aggregation.

### Using these figures for pathway demand and capacity

Use the shared adjusted maternity volume and the relevant booking or delivery allocation to estimate pathway demand. To turn that volume into service activity, specify the proportion using each pathway and the contacts per maternity. For baby-based services, retain baby counts. The Bookings uplift uses no recorded delivery as a proxy; it does not establish clinical outcomes or capture pregnancies absent from maternity services.

Three further distinctions matter:

- **Residence versus provider:** these ONS figures concern mothers living in an area. Allocating demand to trusts requires patient-flow information.
- **Birth year versus care year:** antenatal care may occur before the year of birth and postnatal care afterwards. Annual maternity counts do not directly give appointments in the same year.
- **Demand versus capacity:** activity estimates still need pathway mix, staff time and other resource requirements before they become capacity requirements.
