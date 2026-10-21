# BBCS maternity analysis

`index.qmd` is the report. It converts ONS projected live births into maternities
using age distribution, stillbirth, multiplicity, Private–NHS and maternity pathway adjustments, describes recorded patient flows and delivery activity,
and makes first-draft projections for trusts, ICBs and the combined cluster.
Method 1 is used throughout; section 3 explains the ONS boundary-age conversion and local age weights.

## Run the report

Open the project in RStudio, with the project root as the working directory.
Run this command in the secure environment:

```r
source("R/Run analysis.R")
```

The command prepares the analysis, renders `_site/index.html` and opens the
finished report. It reuses Sarah's parquet objects already loaded in the current
RStudio session. If an input is missing, it reads the parquets once using the
existing Azure setup. It does not run QA, regression tests or the HTML audit.
To reload changed extracts explicitly, run `run_maternity_project(refresh_data = TRUE)`
after loading the run script, or run Sarah's parquet reader before your next analysis.
There is no automatic source-change detection. The command replaces the
previous outputs. If preparation fails, partial outputs are removed. If rendering
fails, the prepared results are retained and the error is saved in `render.log`.
After fixing the reported error, retry rendering without reading the data again:
`system2(Sys.which("quarto"), c("render", "index.qmd"))`.
Rendering uses a temporary folder to avoid clashes with an editor render.
A preview opened during preparation waits for that run to finish.
The console reports data loading/reuse, analysis preparation and rendering progress.

The completed `projection_master`, `projection_master_backtest`, `ons_modifications`,
`delivery_proportions`, `booking_proportions`, `booking_modifications`, column dictionary,
`private_nhs_modifications` are returned to the RStudio session for inspection.
QA is a separate, explicit command: `source("R/QA.R")`.
For small console investigations, run `source("R/ONS to MSDS Matching QA.R")`
after loading the two MSDS inputs, or run its numbered sections individually.
It prints raw totals, LA differences in both directions before/after geography
mapping, age support, and provider mismatch reasons. Sections 4–5 use
`first_draft_projections` from an analysis run with those same inputs to trace
trust/ICB/cluster coverage and compare raw resident counts with model denominators.
The `matching_*` objects remain available for inspection; NA and non-finite
results are retained. These are internal unrounded counts, with no files written.
Existing related checks are in `R/QA.R` section 5 and the prepared objects
`first_draft_projections$provider_projection_gaps`, `$la_source_matching` and
`$allocation_tables$calibration`. The gap object excludes known out-of-range
ages; the new script includes them when reconciling the full coverage table.

Section **1.2, Country of residence by financial year** contains three-row
England/Wales/Scotland tables with FY counts and percentages, in separate whole-
cohort, booking and delivery tabs. Report values are disclosure-controlled;
Northern Ireland and unknown residence remain in each percentage denominator.

Run `source("R/HH scratch area.R")` with the cohort loaded for compact country
counts and percentages: by FY and pooled, with separate cohort, booking and
delivery columns. Older scratch investigations are skipped unless selected
manually. For the additional age/provider breakdowns, run
`source("R/Country matching overview.R")` for country counts and percentages by
assigned FY, plus pooled non-English age-band and destination trust/ICB splits.
It produces overview tables only, without individual-LA lists or a report build.
Counts are pregnancies per activity, not distinct mothers. Unknown residence
and FY remain visible; percentages use all bookings or deliveries in the stated
FY, age band or organisation. Section 4 of the matching script
adds country contributions to trust/ICB/cluster gaps. The local name lookup has
22 Welsh and 32 Scottish LA codes; the supplied ONS births projection CSV has
296 English LAs and no Welsh or Scottish LAs. Both countries remain in MSDS
observed counts. The model accepts their codes and uses their projections if
matching rows are supplied. The merger crosswalk lists changes, not all valid LAs.
The six-LA female-population and historic-birth extracts serve BBCS resident
context charts. The England Table 4 private-care factor is an explicit national
assumption, not a filter on MSDS residence. English IMD leaves non-English
LSOAs unmatched (NA decile) while preserving their pregnancy rows.

The runner requires the existing Azure access and packages in the secure
environment. The project requires R and Quarto on the PATH. Main analysis packages:

```r
install.packages(c("dplyr", "tidyr", "readr", "readxl", "janitor", "purrr",
  "ggplot2", "gt", "plotly", "scales", "htmltools", "knitr", "rmarkdown", "digest"))
# The parquet reader additionally uses arrow, AzureKeyVault, AzureStor, cli and glue.
```

## Disclosure controls

The final report follows the [MSDS activity reporting rules](https://digital.nhs.uk/data-and-information/publications/statistical/maternity-services-monthly-statistics/final-june-2026-provisional-july-2026#publication-data-files-and-suppression-methodology): zero stays zero, counts 1–7 become 5, and other counts round to the nearest 5. Percentages use the controlled numerator and denominator. Positive fractional model outputs below 8 also become 5; exact half-five ties round upwards. Missing estimates stay missing. ONS-only figures are unchanged.

`R/disclosure.R` applies these rules to publication copies after each final aggregation. The master, calibration and internal reconciliation data retain raw precision. Independently controlled totals may differ from sums of displayed components. The QMD checks the saved results' disclosure version and run ID, removes Plotly construction data again on loading, and audits embedded activity counts (including demographic bars) during rendering. The runner also checks the finished HTML before copying or opening it. These publication checks stop rendering on failure; wider model QA remains optional in `R/QA.R`. Rerun the analysis after changing chart preparation code. The HTML is the report artifact; the intermediate RDS files and RStudio inspection objects retain raw data and are not part of it.

Regression checks: `Rscript R/test_disclosure.R` and `Rscript R/test_projection_scale.R`.

## Where to edit

| File | Purpose |
|---|---|
| `R/Maternity analysis.R` | Main editing file: general settings/labels, ONS exploration, MSDS exploration, projection model, charts and tables. |
| `R/Run analysis.R` | Single run script: reuse loaded data (or explicitly refresh), analyse, render and return the completed objects to RStudio. |
| `R/Read parquet files from azure.R` | Sarah's Azure reader; run on first use or when explicitly refreshing extracts. |
| `R/matrix of booking appts and births.R` | Sarah's original standalone matrix tables. Retained for separate use; the report builds its tables in the main analysis. |
| `R/ons_data.R` | Shared public-source readers and ONS chart/table functions. |
| `R/report_cohort.R` | Shared pregnancy-counting and field definitions. |
| `R/age_boundary_review.R` | Shared descriptive age-review functions. |
| `R/QA.R` | Manual source checks, reconciliation and HTML audit; prints labelled console sections when sourced explicitly. |
| `R/HH scratch area.R` | Latest direct-source diagnostic chunks at the top; older investigations retained underneath. Run selected chunks in the secure R console. |
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

The name files in `data/lookups/` supply chart and table labels. Keep them
with the code when moving the project into the secure environment; the analysis
reads these local copies and does not download lookups when it runs. They change
names only, not LA boundaries, trust membership or projection calculations.

Both were downloaded on 1 October 2026. `la_names.csv` combines the ONS
[April 2025 LA names and codes](https://www.data.gov.uk/dataset/b2c91962-58e7-40f1-ad56-7aa2473a93fd/local-authority-districts-april-2025-names-and-codes-in-the-uk-v21)
with codes present only in the
[April 2023 list](https://www.data.gov.uk/dataset/fb6edb97-5fff-4bce-a74c-3aa4f167d307/local-authority-districts-april-2023-names-and-codes-in-the-uk).
`trust_names.csv` contains the code and name columns from the
[NHS ODS trusts download](https://digital.nhs.uk/services/organisation-data-service/data-search-and-export/csv-downloads/other-nhs-organisations).

These are report-side display lookups, separate from Sarah's parquet reader.
Residence harmonisation is separate, using `data/lookups/la_to_2023.csv` and
`report_la_code()` in `R/report_cohort.R`. The crosswalk is derived from the
[versioned ONS code-change list](https://github.com/ONSdigital/uk-topojson/blob/1fcecd26ccf6abf84bb46204ae32179be480b46d/input/changes.csv)
(retrieved 14 October 2026). It maps 42 former English district codes through
whole-district mergers in 2019–2023 to their 2023 successors, including successive
Somerset mergers. It also maps the two 2025 codes back to their 2023 predecessors.
ONS confirms that
[Barnsley](https://www.ons.gov.uk/explore-local-statistics/areas/E08000038-barnsley)
and [Sheffield](https://www.ons.gov.uk/explore-local-statistics/areas/E08000039-sheffield)
changed boundaries/codes in 2025: E08000016/E08000019 became E08000038/E08000039.
The supplied 2023-geography projection correctly retains the earlier codes.
For Barnsley and Sheffield this is an explicit boundary approximation, because
the 2025 change transferred land between them. LA-level counts cannot identify
the affected residents, so they remain assigned to the named authority.
The same mapping is applied to MSDS cohort and resident totals before filtering
or aggregation, across descriptions, denominators, allocations and QA. Each
source record/count contributes once; `LA_source` retains the original code in
the prepared cohort, and the parquet-backed inputs are unchanged. ONS inputs,
Welsh codes and codes absent from the crosswalk are unchanged. Unresolved gaps
remain in coverage checks. The report geography table lists conversions actually
used in the run. Keep this crosswalk with the project; no runtime download is needed.
Run `Rscript R/test_geography.R` for the invented-data geography regression.

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
4. Apply the Private–NHS modifier: 1 minus pooled 2023–2025 England Table 4
   non-NHS maternities / all maternities, within each available age band.
5. Apply the maternity pathway adjustment from the expanded LA file:
   (deliveries + antenatal-only pregnancies) / deliveries at each LA and age.
   `group` identifies deliveries (`birth only` + `antenatal+delivery`) and
   `antenatal only`. Both use birth FY first, otherwise EDD FY; birth age first,
   otherwise booking age when birth age is NA. Pool 2023/24–2025/26 for both.
   Missing counts are zero; zero/missing delivery denominators give a zero factor.
   Every LA uses its own counts, with no borrowed cluster rate.
6. Divide each delivery or booking category count by the shared denominator:
   resident-LA births × maternity pathway adjustment. Apply these independent proportions to
   the same adjusted projection. Sum finished counts to age bands and providers.

Provider allocations use supplied cohort rows aged 13–54 without an extra date
filter. Home birth and Delivery location unknown remain recorded births. Raw
diagnostics retain missing counts, years and ages; the zero treatment applies
only to model counts. With a zero pathway factor, the adjusted projection is zero.

Resident delivery counts and age distributions for every LA come from
`total_births_by_LA_2326`, summed by LA, assigned pregnancy FY and resolved age.
Its required columns are `la_of_mother`, `birth_yr`, `edd_yr`, `age_at_birth_mother`,
`age_at_booking_mother`, `group` and `count`. The three disjoint group values are
`antenatal+delivery`, `antenatal only` and `birth only`. Deliveries include both
groups with birth/delivery; bookings include both groups with antenatal activity.
The pathway numerator includes all three groups once. Unknown groups remain in
raw QA and prevent allocation for the affected LA/age; they never become a
delivery or antenatal-only record based on dates. Cohort provider counts come from `final_cohort_2326`,
using `age_at_birth_mother`, with `age_at_booking_mother` used only where birth age is `NA`.
Both age columns are required in the updated cohort. The same resolved age feeds all
cohort-based charts, calibration and allocations. Booking age is used directly, without
adding a year; it may be younger than delivery age. If both ages are NA, the row
remains unknown. `prepare_report_cohort()` retains the selected label in `Age_value`
and its `Age_source`, alongside a numeric copy for projection arithmetic.
Section 2.2.1 shows birth age, booking-age fallback and unknown age by supplied
delivery year and provider, including missing year/provider groups. Original
field tables retain supplied age values as labels, including negative values
and text. Only NA uses the missing-age category. Numeric ages use the usual
bands; values outside those band definitions keep their supplied labels.
All pregnancy rows in each group contribute to its denominator.
Raw console counts are in `record_completeness_counts`; report copies use the
usual disclosure rounding. These are descriptive outputs, not automatic QA gates.
The pathway factor uses the LA extract's disjoint delivery and antenatal-only
counts; provider counts come from the pregnancy cohort.
Each pregnancy is assigned one FY: `birth_yr` first; if NA or blank, use cohort
`edd_yr` in either extract. Both booking and recorded delivery
use that assigned FY, even when the appointment was in an earlier FY. A supplied
but invalid birth FY remains unresolved rather than being replaced by EDD.
`Pregnancy_FY` / `Pregnancy_year` retain the shared year and `Pregnancy_year_source`
identifies birth/EDD/missing. `Year`, `Booking_year` and `Delivery_year` are
compatibility fields for the same assignment. `Birth_FY` and `Due_FY` retain
the separate source years; original appointment dates remain available. An absent EDD
column leaves the fallback unavailable; it never triggers appointment-year dating.
Unknown assigned years stay in pooled model shares but not annual points.
No missing day or month is invented. Delivery evidence still requires
`birth_flag`; a due year alone does not establish either a delivery or booking.
The MSDS inputs section includes an age-completeness table for the whole supplied
cohort: missing birth ages by recorded-birth status, recovery from booking age,
and both ages missing. Percentages explicitly identify their denominator. These
are pregnancy rows, not distinct mothers; missing birth evidence is not proof
that a delivery did not occur.
An adjacent chart compares missing birth age within booking-age bands, using
all records in each group as the denominator, including an explicit Unknown group
for NA booking ages. Other supplied values retain their own labels. Its table
and hover text include counts; this diagnostic does not alter the model age rule.

The master retains `Exact_age_live_births`, `Bookings_uplift_modifier`, its
source, the shared denominator, source counts and destination proportions/counts.
`Delivery_projection_base` and `Booking_projection_base` both equal
`Adjusted_maternities` after all five modifiers.

`National_conversion_factor` is the product of the stillbirth and multiplicity
factors. The master's `Maternity_modifier` also includes the Private–NHS modifier and maternity pathway adjustment;
it is applied to `Exact_age_live_births`, after the age split.

**Booking Denominator Factor** is the shared denominator minus supplied cohort
pregnancies. It is an estimated residual group shown in the booking-projection tables. It remains separate from observed booking
providers, No recorded booking (provider and date both absent), and Booking
provider unknown. Resident deliveries absent from the linked cohort remain a separate
delivery category. Negative residuals and conflicting source counts are flagged
and left unallocated, not silently clamped or assigned to a provider.

Source matching covers the six cluster LAs and other LAs with recorded bookings
or deliveries at cluster trusts, at ages 13–54. Areas appearing only in the wider
totals file do not enter the model. The console summarises missing source matches
once; `la_source_matching` and `qa_details` retain the individual rows. A larger
resident delivery total than the linked cohort is expected for incoming LAs and
is represented by the residual allocation categories.

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
births; check the extraction processing when these differ. All LAs use their own
resident totals and pathway factors, including outside-LA flows.
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
for trusts, ICBs and the cluster. Each organisation keeps one colour across total
and combined change charts. Total charts use solid lines for historical MSDS and
future modelled activity, and dashed lines for the ONS base. The historical side
is shaded, with a divider between 2025 and 2026 (MSDS 2025/26 and 2026/27).
There are no organisation-specific marker shapes. The ONS line is
`Exact_age_live_births × destination proportion`, before stillbirth, multiplicity, Private–NHS
and booking adjustments, using the same provider shares as the model. It remains
a live-birth baseline, not an ONS provider or booking forecast; earlier values
come from the same existing master and remain projection-derived. Historical MSDS values
use the shared pregnancy-year counts for 2023/24–2025/26; their
x-axis position is the financial year's start. Partial projections retain their
available allocations with coverage labels; wholly unsupported estimates remain
unavailable. Recorded historical totals retain all activity for context. Overlapping
historical model estimates remain in the tables and retrospective checks; the
forward comparison charts display the model only after the historical period.

Interactive charts resize to their container and the window height, including
after tab changes. On narrow layouts, chart grids stack and long horizontal
legend labels wrap. The resizing script is embedded in the standalone HTML;
chart panels have no internal scrollbars. Wide tables retain horizontal scrolling.

Both sections retain age-band charts and total-change charts, with age-band change
also available for bookings. Delivery-total percentage changes use recorded
2025/26 deliveries from the same usable LA/age cells; booking totals and age bands
use modelled 2025 on the same support. Retrospective differences likewise use
matched recorded deliveries; the full recorded totals are retained separately.
Coverage counts and percentages describe the supplied cohort, not an estimated
percentage of future demand. Unknown ages are not redistributed. A missing
destination within valid source coverage is zero. Missing source geographies and
undefined percentages remain unavailable. The pathway factor follows its separately
agreed convention: a zero or missing resident-delivery denominator gives zero.
`R/HH scratch area.R` holds optional snippets to select and run against Sarah's
loaded parquet objects, including missing-age checks and raw/harmonised LA codes.
It is not a report step and does not require a particular saved output.
Run `Rscript R/test_projection_scale.R` to check the three chart series with
invented data and confirm that outside-LA bookings use the shared resident
denominator rather than being scaled up to the whole LA population.
Run `Rscript R/test_age_fallback.R` to check birth-age precedence, the booking-age
fallback, remaining unknown ages, age-band boundaries and downstream allocations,
plus shared birth/EDD financial years and completeness charts that retain unknown groups.

The current calculation version is `ons-shared-age-calculations-v24`.
Run `Rscript R/test_pregnancy_year.R` for the birth-first/EDD-fallback rule and
the resident-file partition. Rerun `source("R/Run analysis.R")` with the loaded
secure inputs to regenerate all annual views and projection comparisons.
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
Both views use all antenatal bookings in the assigned pregnancy year as their denominator, with the same follow-up rules
and relative-percentage-change calculation; they do not use residence ICB.

## Saved outputs and missing sections

The preparation scripts save these files in the project root:

| Output | Used for |
|---|---|
| `.ons_exploration.rds` | ONS source exploration, shared resident projections and age-review objects |
| `.patient_flows.rds` | MSDS activity and patient-flow exploration |
| `.delivery_geography.rds` | Deliveries by LA(R) and age |
| `.projections.rds` | Projections, master and retrospective comparison |
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

QA never runs as part of the normal analysis or render. Run it separately when
investigating changed data or calculations:

```r
source("R/QA.R")
```

The script prints labelled sections for source records, ONS coverage, pathway
totals, geography, projections, cross-section totals and saved-report/HTML checks.
It uses loaded parquet objects and existing saved outputs. It does not read Azure,
rebuild charts or write a `.qa.rds` file. `qa_results`, `qa_issues` and `qa_details`
remain in the R session for inspection. Missing saved outputs are reported;
source checks can run without a rendered report. Saved-output comparisons refer
to the last analysis run, so rerun the analysis first when comparing new inputs.
The scratch file remains separate and unchanged by either workflow.

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
| **Antenatal booking** | A pregnancy with recorded antenatal booking information, counted once rather than once per appointment. | Provider counts use the recorded booking provider, including pregnancies without a recorded delivery. Historical counts use birth financial year first, otherwise estimated due financial year; an appointment date is not required. “Bookings” is used as a short form of “antenatal bookings”. |
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
| **Estimated / projected maternities** | Counts calculated from projected births (L), after all five modifiers: age distribution, stillbirth, multiplicity, Private–NHS and maternity pathway adjustment. |
| **Maternity modifier** | [1 + S/L] × M/(L+S) × Private–NHS modifier × maternity pathway adjustment, applied after the age split. S, L and M are national Table 5 counts pooled across 2023–2025. |
| **General fertility rate (GFR)** | Live births per 1,000 women aged 15–44 in this report. It describes frequency relative to population size, not the number needing care. |
| **Age-specific fertility rate (ASFR)** | Live births per 1,000 women in the relevant age group. |
| **Total fertility rate (TFR)** | Children per woman if the year's age-specific fertility rates applied throughout childbearing life. Not an annual maternity count. |

**The conversion in one line:** projected live-born babies × age weight × [1 + S/L] × M/(L+S) × Private–NHS modifier × maternity pathway adjustment = adjusted maternities. Split combined ONS ages first, apply the factors at each LA/age, then add completed results. Birth rates are a separate contextual measure and are not used instead of this conversion.

For illustration, national factors with a product of 0.99 and a maternity pathway adjustment
of 1.25 give 123.75 adjusted maternities per 100 live births at an age with weight
1. The model applies each LA/age's factors separately before aggregation.

### Using these figures for pathway demand and capacity

Use the shared adjusted maternity volume and the relevant booking or delivery allocation to estimate pathway demand. To turn that volume into service activity, specify the proportion using each pathway and the contacts per maternity. For baby-based services, retain baby counts. The maternity pathway adjustment uses no recorded delivery as a proxy; it does not establish clinical outcomes or capture pregnancies absent from maternity services.

Three further distinctions matter:

- **Residence versus provider:** these ONS figures concern mothers living in an area. Allocating demand to trusts requires patient-flow information.
- **Birth year versus care year:** antenatal care may occur before the year of birth and postnatal care afterwards. Annual maternity counts do not directly give appointments in the same year.
- **Demand versus capacity:** activity estimates still need pathway mix, staff time and other resource requirements before they become capacity requirements.


## Private–NHS modifier

Table 4 of the [ONS birth-registration workbooks](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/datasets/birthsinenglandandwalesbirthregistrations)
supplies England-resident maternities by delivery setting. Pool 2023–2025 counts
before calculating non-NHS establishments / all maternities. The multiplier is
one minus that share, applied once after multiplicity and before pathway uplift.
The model uses under 20, 20–24, 25–29, 30–34, 35–39, 40–44 and 45+; combine 2025
45–49 and 50+ to match earlier editions. Apply each band to its individual model
ages (13–54). Unknown age is a derived reconciliation row, not a substitute rate
for known ages or an unsplit ONS boundary. All-age totals are descriptive only.

This delivery-setting proxy is assumed to represent private care equally for
bookings and deliveries, in all modelled LAs and projection years. It does not
measure private funding within NHS establishments or partial private pathways.
Home/elsewhere births remain outside the proxy numerator. Recorded activity,
allocation denominators, provider shares and the pathway factor are unchanged.
Do not apply the reduction again after allocation or change pure ONS comparators.
Keep full precision through all calculations; existing disclosure controls apply
only to final MSDS-derived or combined outputs. Public ONS source tables retain
their published counts and the ratios calculated from them.

`private_nhs` retains annual and pooled evidence; `Private_NHS_age_band`,
`Private_NHS_rate` and `Private_NHS_factor` identify each master row's factor.
`Maternities_before_private` and `Maternities_after_private` expose its effect.
`National_conversion_factor` remains the Table 5 baby-to-maternity conversion.
The total `Maternity_modifier` includes the new reduction and pathway uplift.

## Demographic Sankeys

`R/Run analysis.R` runs `R/Demo Sankeys.R` after the main analysis. The RStudio
object `Demo Sankeys` is one aggregate table for the whole supplied three-year
BBCS cohort. It contains `Booking`, `Delivery`, `Booking_ICB`, `Delivery_ICB`,
`Pathway`, `Outcome`, `ethnicity_broad`, `ethnicity_description`, `imd_quintile`,
`Age_band` and `N`. There are no mother/pregnancy IDs, repeated event views or
additional year filters. ``sum(`Demo Sankeys`$N)`` equals the source pregnancy-row count.

Section 4.1 shows stacked demographic bars with separate delivery-provider and
booking-provider views. Both retain all cohort rows: no recorded event and
unknown/other providers remain separate. ICB comes from that event's provider,
without borrowing the other provider or residence. Whole BBCS cohort is the
sum of all ICB/event groups, not just the two named ICBs. Section 4.2 shows all
booking-to-delivery paths, including no-booking, no-delivery and home-birth
nodes. Section 4.3 groups all cohort pregnancies by booking provider/ICB and
uses the existing four delivery outcomes, including delivery-only pregnancies
in their own no-booking group. Each organisation has one Sankey per variable
in each of two tabs: **Between category** uses controlled pregnancy counts;
**Within category** normalises the controlled outcome counts to equal-height
category bars. Its outcome widths give equal weight to categories, not patients.
Hover percentages use independently controlled category/organisation totals in
both views, so within-category widths can differ from hover percentages after
rounding. Every demographic split reconciles to the same
whole-cohort population before display controls. All section 4 IMD views use
quintiles: source deciles 1–2, 3–4, 5–6, 7–8 and 9–10 form quintiles 1–5.
Quintile 1 is most deprived; quintile 5 is least deprived. Unknowns stay separate.
Raw counts are aggregated into quintiles before disclosure controls; the cleaned
demographics lookup retains its source decile for audit.

Run `Rscript R/test_demo_disclosure.R` for the section 4 publication check. It
generates all demographic charts from invented records in an isolated folder,
checks saved/reloaded outputs, rounded counts and independent percentage
denominators, and audits chart coordinates, hover counts and embedded HTML data.
It does not load Azure data or replace the production report.

Booking evidence is a supplied provider or appointment date. Delivery evidence
is birth flag 1. The three requested pathway statuses remain separate, plus
neither event recorded where present. Each supplied pregnancy counts once;
separate pregnancies for one mother are not collapsed. The period label comes
from the supplied extract. Annual views use birth FY first, otherwise due FY;
appointment dates remain evidence/audit fields rather than the model-year basis.

The unchanged `demographics.parquet` filename now supplies `person_id_mother`,
`lsoa_2011` and `ethnicity`. `R/demographics.R` prepares `demographics_cleaned`
before the pregnancy join. Its first four columns are mother ID, broad ethnicity,
ethnicity description and IMD decile; audit columns retain both raw strings,
the chosen LSOA/position, seed, source descriptions and cleaning status.
Missing/blank mother IDs never match. Identical source rows collapse; conflicting
rows for one mother stop the join rather than multiplying pregnancies.

For two pipe-separated LSOAs, a seeded pseudo-random first/second choice is made
once per mother. Seed `20261021` and a hash of the mother ID make it repeatable
across reruns and row reordering without altering R's random-number state.
The choice is made before lookup, so an unmatched choice is not replaced with
the other option. IMD is **2019 on 2011 LSOAs**, not a 2011 deprivation release:
`data/lookups/imd2019_lsoa2011.csv` retains the LSOA code and overall IMD decile
from [official IoD 2019 File 7](https://assets.publishing.service.gov.uk/media/5dc407b440f0b6379a7acc8d/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv),
retrieved 21 October 2026 (32,844 English LSOAs). Decile 1 is most deprived;
10 is least deprived. Missing, malformed, more-than-two and unmatched LSOAs
retain NA deciles and a review status. Non-English LSOAs have no English IMD decile.
The local lookup is bundled, so report runs do not download anything.

Ethnicity labels follow the [NHS ethnic category dictionary](https://www.datadictionary.nhs.uk/data_elements/ethnic_category.html)
(verified against the [official archived dictionary](https://archive.datadictionary.nhs.uk/DD%20Release%20November%202021/data_elements/ethnic_category.html)
because the current page denied automated access). `data/lookups/ethnic_category.csv`
maps the 17 supplied codes to seven groups: White; Mixed; Asian or Asian British;
Black or Black British; Other ethnic groups - Chinese; Other ethnic groups - Any
other; Not stated. `ethnicity_pair_rules.csv` transcribes Heather's supplied grid:
rows are the second code and columns the first. `OOC` and the key's `OC` mean
Chinese; `TC` stays **Two conflicting**, a separate unresolved category.
For example, S|N becomes Black or Black British, A|H is Two conflicting, and
L|R becomes Chinese. Mixed takes precedence wherever shown in the grid.
Single categories retain their full NHS description. Resolved pairs use the
broad result as their description because the grid does not assign a detailed
subtype; both original descriptions remain in `ethnicity_source_description`.
Whitespace/case are normalised and repeated identical codes act as one category.
NA, blanks, code 99 (Not known), unrecognised codes and more than two supplied
values stay unresolved (NA broad group) with their source value/status retained;
they are not silently converted to Z (Not stated).

Inspect aggregate cleaning results with `demographics_qa(demographics_cleaned)`;
these are internal, unrounded counts. Run `Rscript R/test_demographics.R` for the
synthetic grid, selection and join checks. Age follows the shared birth-age-first
rule. Existing disclosure controls apply after aggregation. Chart objects are
saved to the ignored `.demo_sankeys.rds`; no mother IDs or raw LSOAs enter them.
No QA or audit is added to the normal run.
