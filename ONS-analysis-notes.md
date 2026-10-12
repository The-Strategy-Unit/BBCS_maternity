# ONS analysis notes

## Current calculation, 8 October 2026

All adjusted projections use one LA(R) × Age (SY) × year master in
`R/Maternity analysis.R`. Apply four modifiers, then allocate:

1. Split combined ≤15 and 44+ ONS live births using resident-LA maternity age
   counts pooled across 2023/24–2025/26. Ages 16–43 retain their own ONS count.
2. Apply 1 + S/L from national Table 5 counts pooled across 2023–2025 at each
   published age.
3. Apply M/(L+S) from those same national counts.
4. Apply the Bookings uplift: all cohort pregnancies / recorded cohort births
   for each cluster LA/age. Only No recorded delivery drives the uplift;
   home births and Delivery location unknown remain recorded births.

Use every supplied cohort row, without another date filter. Birth counts and
age weights still come from `total_births_by_LA_2326`. Outside LAs use the
same-age ratio pooled among residents of the configured cluster LAs. The same
proxy applies where a cluster LA lacks local recorded-birth evidence. Do not
borrow a rate from another age. A positive source count without same-age
uplift evidence remains unestimated, including an unsplit boundary with no
exact age. Zero-weight source contributions remain zero.

## Shared denominator and residual groups

At each LA/age, estimated resident maternities = resident births × Bookings
uplift. Both delivery and booking shares divide by this same denominator.
Both allocation bases equal the completed adjusted maternity projection.

Retain observed trust, home and unknown-location counts. Resident births
missing from the cohort remain Not represented in linked cohort. The estimated
No recorded delivery count is the shared denominator minus resident births.

Every cohort row has a booking category. No recorded booking means both raw
provider and date are absent. A supplied date without a usable provider is
Booking provider unknown; a supplied but invalid date is still recorded
booking evidence. A valid provider can be allocated without a booking date.

**Booking Denominator Factor = estimated resident maternities − cohort
pregnancy count.** This is a separate residual group, not observed bookings
or known no-booking records. The cluster proxy is an explicit assumption for
unobserved flows, not a national MSDS ratio.

Example: 100 resident births, 60 cohort births and 15 no-delivery records give
an uplift of 75/60 = 1.25, denominator 125, birth gap 40 and booking residual 50.
If the two sources agree on recorded births and the local ratio is used, the
residual is zero. Outside-LA proxy rates can also create differences from their
observed cohort mix; these stay visible in the review.

## Review and reporting

The method tables compare resident births with recorded cohort births by LA
and age, show the uplift source, and calculate the booking residual. Review
any source difference against the Databricks processing and period coverage.
Negative gaps or a proxy no-delivery estimate below the observed cohort count
leave provider allocations Unallocated; negative residuals are never silently
clamped. Missing uplift evidence is shown explicitly. QA checks coverage as
well as totals. The booking section shows projected residual counts and shares
by LA and age band, separately from provider activity.
Its projected residual and share remain unavailable where a positive source
contribution cannot be estimated or allocated. Zero source contributions stay zero.

The LA source check expects every configured or supplied MSDS LA to match both
resident birth counts and ONS births. The console QA and report identify missing
codes and the source to review; `la_source_matching` retains the full check.
Outside LAs without cohort rows can use the agreed same-age proxy, provided
their resident and ONS sources match. Unmatched provider flows are QA issues.

Provider coverage uses all supplied rows, including those without a usable LA,
age or ONS match. Incomplete allocations leave the affected provider totals and
changes unavailable. The summaries retain `Available_projected` for reconciliation
and `Projection_complete` for display. Age-specific gaps affect their band;
unknown-age gaps can affect any band. The flow diagrams show available allocations
explicitly and suppress changes for incomplete flows. Missing coverage leaves
the affected estimate unavailable.

Inspect `projection_master`, `booking_modifications`, `ons_modifications`,
`delivery_proportions`, `booking_proportions` and the column dictionary.
`Exact_age_live_births` is additive; source `ONS_live_births` repeats across
split ages. `Delivery_projection_base` and `Booking_projection_base` are two
references to the same `Adjusted_maternities` value. Do not add their totals.
`National_conversion_factor` contains the two national factors only;
the master's `Maternity_modifier` also includes the Bookings uplift and is
applied after the age split.
Calculation version: `ons-shared-age-calculations-v15`.

Resident charts sum the master before allocation; trust charts sum completed
provider contributions. The historical comparison is in-sample, not independent
validation. All final age bands are applied after the calculations. Descriptive
annual delivery/booking-share charts retain their event-year definitions;
they do not rerun the full-cohort model on mismatched annual populations.
The detailed ONS growth tables compare source birth counts directly, taking the
repeated source count once per LA/source age/year. An unavailable modifier at
one split age does not remove the known source growth for another age.

The optional local `R/check_projection_cohort.R` prints aggregate source checks
from loaded parquet objects. It is excluded from Git and is not needed to run
the report. Secure-data execution remains necessary to inspect the actual gaps.

## Source editions and checks

The three registration workbooks (2023, 2024 and 2025) matched the ONS downloads
checked on 6 October 2026. Table 5 supplies the stillbirth and multiplicity
modifiers; the resident file supplies age weights and the cohort supplies the
Bookings uplift.

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

The historical birth snapshot is `data/ons_live_births_la_2013_2025.csv`, from
Nomis dataset `NM_205_1` (`LEBIRTHSLA`). It includes total births (L) and published
maternal age bands for the configured authorities. Adding an authority requires
refreshing this extract and the female-population extract with its GSS code.

## Run and review

Run `source("R/Run analysis.R")` from the project root in the secure
environment. The launcher reads Sarah's parquet inputs, prepares all outputs,
runs QA and renders the report. See `README.md` for files and settings.
Arithmetic and chart checks can run with the optional local
`R/test_report_consistency.R` and `R/check_projection_cohort.R` scripts, which are
excluded from Git. They use small counted examples, not an alternative MSDS
project dataset.
Full secure-data execution is still required to verify the actual cohort results.
