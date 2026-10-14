# ONS analysis notes

## Current calculation

All adjusted projections use one LA(R) × Age (SY) × year master in
`R/Maternity analysis.R`. Apply five modifiers, then allocate:

1. Split ONS ≤15 across ages 13–15 and ONS 44+ across ages 44–54 using resident-LA
   delivery counts pooled across 2023/24–2025/26. Ages 16–43 retain their own ONS count.
   Retain zero counts for ages with no recorded activity; do not extend beyond 13–54.
2. Apply 1 + S/L from national Table 5 counts pooled across 2023–2025 at each
   published age.
3. Apply M/(L+S) from those same national counts.
4. Apply the Private–NHS modifier: 1 minus pooled 2023–2025 England Table 4
   non-NHS maternities / all maternities, within each available age band.
5. Apply the maternity pathway adjustment: all cohort pregnancies / recorded cohort deliveries
   for each cluster LA/age. Only No recorded delivery drives the uplift;
   home births and Delivery location unknown remain recorded births.

Use supplied cohort rows aged 13–54, without another date filter. BBCS resident
delivery counts and age weights come from `final_cohort_2326`, using
`age_at_birth_mother`. Only contributing outside LAs use `total_births_by_LA_2326`
for their full resident delivery counts and age distributions. Outside LAs use the
same-age ratio pooled among residents of the configured cluster LAs. The same
proxy applies where a cluster LA lacks local recorded-birth evidence. Do not
borrow a rate from another age. A positive source count without same-age
uplift evidence remains unestimated, including an unsplit boundary with no
exact age. Zero-weight source contributions remain zero.

## Shared denominator and residual groups

At each LA/age, estimated resident maternities = resident deliveries × maternity pathway adjustment. Both delivery and booking shares divide by this same denominator.
Both allocation bases equal the completed adjusted maternity projection.

Retain observed trust, home and unknown-location counts. Resident deliveries
missing from the cohort remain Not represented in linked cohort. The estimated
No recorded delivery count is the shared denominator minus resident deliveries.

Every cohort row has a booking category. No recorded booking means both raw
provider and date are absent. A supplied date without a usable provider is
Booking provider unknown; a supplied but invalid date is still recorded
booking evidence. A valid provider can be allocated without a booking date.

**Booking Denominator Factor = estimated resident maternities − cohort
pregnancy count.** This is a separate residual group, not observed bookings
or known no-booking records. The cluster proxy is an explicit assumption for
unobserved flows, not a national MSDS ratio.

Example: an outside LA with 100 resident deliveries, 60 cohort deliveries and 15
no-delivery records, using a same-age cluster uplift of 1.25, has denominator 125,
delivery gap 40 and booking residual 50. Within BBCS, the cohort supplies both
local counts, so the residual is zero where the local ratio can be calculated.
Outside-LA proxy rates can create differences from their
observed cohort mix; these stay visible in the review.

## Review and reporting

The saved `booking_modifications` table compares resident and cohort deliveries
by LA and age, shows the uplift source, and calculates the booking residual.
Resident deliveries outside the linked cohort remain in the residual category.
Negative gaps or a proxy no-delivery estimate below the observed cohort count
leave provider allocations Unallocated; negative residuals are never silently
clamped. Missing uplift evidence is shown explicitly. QA checks coverage as
well as totals. The booking section shows projected residual counts and shares
by LA and age band, separately from provider activity.
Its projected residual and share remain unavailable where a positive source
contribution cannot be estimated or allocated. Zero source contributions stay zero.

The LA source check covers configured cluster LAs and other LAs with recorded
bookings or deliveries at cluster trusts, at ages 13–54. Areas appearing only in
the wider totals file do not enter the model. The console summarises issues once;
`la_source_matching` and `qa_details` retain the individual rows. Missing uplift
evidence is counted by LA/age group, without repeating each projection year.

Provider coverage uses supplied rows aged 13–54, including those without a usable LA
or ONS match, and retains unknown-age records for checks. Known ages outside 13–54
are out of scope, not missing coverage. Incomplete allocations leave the affected provider totals and
changes unavailable. The summaries retain `Available_projected` for reconciliation
and `Projection_complete` for display. Age-specific gaps affect their band;
unknown-age gaps can affect any band. The flow diagrams show available allocations
explicitly and suppress changes for incomplete flows. Missing coverage leaves
the affected estimate unavailable.

The report presents the current method and results. QA details and code inspection
remain in the saved analysis objects and console.

Inspect `projection_master`, `booking_modifications`, `ons_modifications`,
`delivery_proportions`, `booking_proportions` and the column dictionary.
`Exact_age_live_births` is additive; source `ONS_live_births` repeats across
split ages. `Delivery_projection_base` and `Booking_projection_base` are two
references to the same `Adjusted_maternities` value. Do not add their totals.
`National_conversion_factor` contains the two national factors only;
the master's `Maternity_modifier` also includes the Private–NHS modifier and maternity pathway adjustment and is
applied after the age split.
Calculation version: `ons-shared-age-calculations-v20`.

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
modifiers, and England Table 4 supplies the Private–NHS modifier. BBCS cohort
deliveries and outside-LA resident totals supply age weights, and the cohort supplies the
maternity pathway adjustment.

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
Run `Rscript R/test_projection_scale.R` for the provider totals and comparison lines.
Further arithmetic and chart checks can run with the optional local
`R/test_report_consistency.R` and `R/check_projection_cohort.R` scripts, which are
excluded from Git. They use small counted examples, not an alternative MSDS
project dataset.
Full secure-data execution is still required to verify the actual cohort results.

Provider coverage: available allocations remain visible when other records cannot
be matched. Counts are labelled complete, partial or no usable estimate, with a
cohort coverage table. Delivery changes and retrospective differences use matched
recorded LA/age cells; full recorded activity remains visible as context. Booking
and age-band growth compare modelled counts on the same fixed support. No missing
age redistribution is applied. MSDS residence codes are harmonised to the ONS
2023 codes using the versioned crosswalk in `data/lookups/la_to_2023.csv` before
aggregation in both inputs. Whole-district mergers combine their constituent
counts; the 2025 Barnsley/Sheffield codes use their 2023 predecessors as a documented
boundary approximation, without attempting to redistribute the transferred area.
The report lists applied mappings and cites the ONS change list. Original source
objects and ONS values are unchanged. Structural absent count
categories within supported inputs are zero; missing source areas, explicit NAs
and undefined zero-denominator rates remain unavailable. The HH scratch file is
a collection of optional snippets to select and run against the parquet-backed
objects; it is not a pipeline step and does not create a prescribed output.

The report calls the fifth modifier **Maternity pathway adjustment**. The existing
`Bookings_uplift_modifier` column name is retained for compatibility; it still means
all cohort pregnancies divided by recorded cohort deliveries, not bookings divided
by deliveries. Within BBCS, allocating the shared total by B/T and D/T gives
P × B/D bookings and P deliveries, where P is the ONS-derived delivery-equivalent
projection after the Private–NHS modifier and before pathway adjustment. Apply
each share once; do not apply the factor again.


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
