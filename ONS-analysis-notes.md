# ONS analysis notes

These notes record the ONS component's chosen source editions and methods QA.
Its files live in the project root. Use `README.md` for the current run order and
`index.qmd` for the current report, including both methods and patient flows.

## Structure

- `index.qmd`: the HTML report entry point.
- `R/ons_data.R`: imports and prepares ONS source data.
- `R/Read parquet files from azure.R` and `R/matrix of booking appts and births.R`: Sara's secure-data scripts, with their contents unchanged.
- `R/additional_analysis.R`: creates the patient-flow and cluster-activity report hand-off in the project root.
- `config/analysis_parameters.csv`: sets the displayed female-population ages (13 and 55); it does not extend the birth projection.
- `config/local_authorities.csv`: the selected local authorities, display order and plot colours.
- `_brand.yml`: Strategy Unit brand settings.
- `styles.css`: small report-specific style layer.
- `assets/`: local Strategy Unit logo files.
- `_site/`: rendered HTML output (generated).

## Render

From the repository root:

```bash
quarto render
```

The current report documents source coverage and preparation. Add analysis modules to `R/` and report sections to `index.qmd` as requirements are defined.

The female population projection snapshot is extracted directly from the official
ONS 2022-based migration-category variant on April 2023 local-authority geography.
It retains female single ages 0-89 for 2023-2047, using the archive's published
decimal values rather than whole-person Nomis rounding. This allows the displayed
range to be changed in `config/analysis_parameters.csv` without replacing the
snapshot. Adding an authority requires refreshing the extract to include its
GSS code; the report checks for this. This matching extract has its own file name:
`data/ons_2022_based_female_population_projections_2023_geography.csv`.

The historic live-birth snapshot is `data/ons_live_births_la_2013_2025.csv`, downloaded
from Nomis dataset `NM_205_1` (API reference `LEBIRTHSLA`). It contains total live births
and the published maternal age bands for 2013-2025 for the configured authorities. The
report uses the total for the live-birth chart and applies the age-specific multiplicity
modifiers to the age-band rows before summing estimated maternities. If an authority is
added to the configuration, refresh this extract with its GSS code as well as refreshing
the female population projection extract.


To change the areas shown throughout the report, edit `config/local_authorities.csv`. Add or remove rows and supply the ONS area code and name. Plot colours may be left blank; the report assigns additional colours automatically.

## Source and methods QA, 1 October 2026

Both Method 1 and Method 2 now use the 2022-based migration-category variant on
April 2023 local-authority geography for projected births and female population.
This replaces the 2021-geography setup documented on 25 September 2026; the
conversion calculations for both methods are unchanged.

The report reads the existing supplied `data/2022 SNPP Births persons.csv` directly.
That file is unchanged and matches the persons entry byte-for-byte in the official
2023-geography birth archive:
[ONS 2023-geography birth projections ZIP](https://www.ons.gov.uk/file?uri=%2Fpeoplepopulationandcommunity%2Fpopulationandmigration%2Fpopulationprojections%2Fdatasets%2Fbirthsbyageofmotherz3%2F2022basedmigrationcategoryvarianton2023localauthoritygeographies%2F2022snppbirthsmigcat23.zip).

The new, separately named female snapshot uses `2022 SNPP Population females.csv` from the matching
[ONS 2023-geography population projections ZIP](https://www.ons.gov.uk/file?uri=%2Fpeoplepopulationandcommunity%2Fpopulationandmigration%2Fpopulationprojections%2Fdatasets%2Flocalauthoritiesinenglandz1%2F2022basedmigrationcategoryvarianton2023localauthoritygeographies%2F2022snpppopulationsyoamigcat23.zip).
Archive SHA-256:
`c452e4e8d04747d4f05484efcdefe100e7d6117147c3d2b4051007a71e14ee82`.

Snapshot QA found 13,500 complete, unique, positive cells: six configured
authorities x 90 ages (0-89) x 25 years (2023-2047). Values are copied directly
from ONS; metadata identifies female sex, the 2022 base year, April 2023 geography,
migration-category variant and source archive. The five columns required by
`read_female_population_single_age()` are retained, with direct ONS metadata instead
of Nomis-specific fields. The maximum absolute difference from the older
whole-person snapshot is 4.493 people, so the earlier within-0.5 QA applies only
to the 2021-geography source, not this edition.

The older `data/ons_2022_based_births_migration_category.csv` and
`data/ons_2022_based_female_population_projections.csv` are preserved unchanged.
The report no longer selects them by default; no existing input CSV has been
replaced. Only the new matching female extract is added.

Public-data execution checks passed for both methods: each produces 900 complete
LA/age/year maternity cells (six LAs x six age bands x 25 years), with 150 complete
LA/year population denominators for the projected fertility ratios. The imaginary
example recalculates its growth from the selected birth file: Method 1 changes
by -0.6172% from 2026 to 2036, rounding to 0% at five-percentage-point intervals.
Its ten-year total therefore remains 110. Secure MSDS blocks were not run here.

Maternity estimates use the original age-specific multiplicity method: average
the 2023–2025 annual singleton/twin/triplet/four-plus shares, calculate expected
babies per maternity and take the reciprocal. Four-plus is represented as four.
These modifiers are applied to projected live births; no separate stillbirth
adjustment is made. Published maternities include stillbirth-only pregnancies.
The projection uses under-20 through 40+ bands because ONS assigns outlying ages
to model ages 15 and 44. For 40+, combine annual category counts before calculating
shares. No extra boundary-age births are added. Published calendar-year counts
and calculated mid-year series remain separate lines on the same chart.

For the original matrix-based patient-flow sections, reporting periods and counting
units are not established in the supplied matrices. Those values are labelled records
pending confirmation from their producer.
Execution freezing remains disabled in `_quarto.yml`.

## Combined report

At the time of these notes, `index.qmd` combined Comparison, Method 1 report and
Method 2 report as three level-one headings. The current version also includes
patient-flow and cluster-activity sections. Method details sit underneath as level-two headings.
The project renders only this file to `_site/index.html`. Both methods are calculated
on each render; freeze remains disabled. Edit `index.qmd` for future report changes.
The previous standalone method QMD files have been removed to avoid conflicting
versions of the method descriptions.
