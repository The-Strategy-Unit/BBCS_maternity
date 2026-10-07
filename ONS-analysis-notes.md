# ONS analysis notes

## Current calculation, 7 October 2026

All adjusted projections use the same LA(R) × Age (1) × year master, prepared in
`R/Maternity analysis.R`. There is no separate ONS-only maternity conversion.

1. Retain the original ONS source age. Split combined ≤15 and 44+ births (L)
   using local resident maternity counts pooled across 2023/24–2025/26.
2. Pool national Table 5 counts across 2023–2025 at each published age.
   Calculate 1 + S/L, then M/(L+S), from those pooled totals.
3. Keep separate delivery and booking starting volumes. Both currently equal
   adjusted maternities; a booking early-loss uplift remains undecided.
4. Apply independent destination proportions at LA(R) × Age (1). Pool counts
   before dividing: delivery denominator = resident maternities; booking
   denominator = all bookings, including those without a recorded delivery.
5. Aggregate completed counts into Under 20, 20–24, 25–29, 30–34, 35–39, 40–44
   and 45+, then trust, ICB and cluster summaries.

`ons_modifications` is the table actually joined into the master. Ages below 14
share the under-14 national factors; ages 50+ share the published 50+ factors.
When a boundary group has no local age evidence, its count stays explicitly
unsplit and unallocated, using matching grouped national factors only for that
exception. It remains visible in resident totals and the unsplit-age review.

## Report views and audit columns

Resident charts sum adjusted maternities before destination allocation and retain
all 2023–2047 source years. Trust charts sum the allocated counts. The historical
MSDS comparison uses the same pooled proportions and is in-sample, not independent
validation. No projection is forced to match observed MSDS or ONS maternity counts.

The first review columns are `Exact_age_live_births` (age split only),
`Delivery_projection_base` and `Booking_projection_base`. Source `ONS_live_births`
repeats across split ages; `Exact_age_live_births` preserves each source total.
Delivery and booking columns each reconcile to their own starting volume;
the two views are separate rather than additive. Calculation version: `ons-shared-age-calculations-v10`.

Historical births (L), supplied in bands, are shown directly from Nomis. Observed
maternities come directly from ONS Table 3. Historical banded births are not
converted into a competing estimated-maternity series. Table 7 and the detailed
older-age evidence panels remain descriptive context, not model multipliers.

## Source editions and checks

The three registration workbooks (2023, 2024 and 2025) matched the ONS downloads
checked on 6 October 2026. Table 5 supplies both active modifiers.

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
Arithmetic and chart checks can run locally with `R/test_report_consistency.R`;
they are small counted examples, not an alternative MSDS project dataset.
Full secure-data execution is still required to verify the actual cohort results.
