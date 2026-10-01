# Historical ONS analysis notes

These notes preserve the ONS component's earlier setup and methods QA. Its files
now live in the project root. Use `README.md` for the current run order and
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

The female population projection snapshot was downloaded from Nomis dataset `NM_2006_1`
(ONS 2022-based subnational population projections). It retains female single ages
0-89 for 2023-2047, allowing the displayed range to be changed in
`config/analysis_parameters.csv` without replacing the snapshot. Adding an authority
requires refreshing the extract to include its GSS code; the report checks for this.

The historic live-birth snapshot is `data/ons_live_births_la_2013_2025.csv`, downloaded
from Nomis dataset `NM_205_1` (API reference `LEBIRTHSLA`). It contains total live births
and the published maternal age bands for 2013-2025 for the configured authorities. The
report uses the total for the live-birth chart and applies the age-specific multiplicity
modifiers to the age-band rows before summing estimated maternities. If an authority is
added to the configuration, refresh this extract with its GSS code as well as refreshing
the female population projection extract.


To change the areas shown throughout the report, edit `config/local_authorities.csv`. Add or remove rows and supply the ONS area code and name. Plot colours may be left blank; the report assigns additional colours automatically.

## Methods QA, 25 September 2026

The report reads `data/ons_2022_based_births_migration_category.csv`, extracted from
the corrected ONS migration-category ZIP on 25 September 2026:
https://www.ons.gov.uk/file?uri=%2Fpeoplepopulationandcommunity%2Fpopulationandmigration%2Fpopulationprojections%2Fdatasets%2Fbirthsbyageofmotherz3%2F2022basedmigrationcategoryvariant%2F2022snppbirthsmigcat.zip

This is the 2021 geography edition. The saved Nomis female-population extract was
checked against the matching corrected ONS population archive: all 13,500 values
agree to whole-person rounding (maximum difference 0.5). The previous supplied
birth file had undocumented variant/geography and differing values; it is retained
as an input record but is no longer read by the report. Its differences do not by
themselves establish that the original ONS edition was wrong.

Maternity estimates use the original age-specific multiplicity method: average
the 2023–2025 annual singleton/twin/triplet/four-plus shares, calculate expected
babies per maternity and take the reciprocal. Four-plus is represented as four.
These modifiers are applied to projected live births; no separate stillbirth
adjustment is made. Published maternities include stillbirth-only pregnancies.
The projection uses under-20 through 40+ bands because ONS assigns outlying ages
to model ages 15 and 44. For 40+, combine annual category counts before calculating
shares. No extra boundary-age births are added. Published calendar-year counts
and calculated mid-year series remain separate lines on the same chart.

Patient-flow reporting periods and counting units are not established in the
supplied matrices. They are labelled records pending confirmation from their producer.
Execution freezing remains disabled in `_quarto.yml`.

## Combined report

At the time of these notes, `index.qmd` combined Comparison, Method 1 report and
Method 2 report as three level-one headings. The current version also includes
patient-flow and cluster-activity sections. Method details sit underneath as level-two headings.
The project renders only this file to `_site/index.html`. Both methods are calculated
on each render; freeze remains disabled. Edit `index.qmd` for future report changes.
The previous standalone method QMD files have been removed to avoid conflicting
versions of the method descriptions.
