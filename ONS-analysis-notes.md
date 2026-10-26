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
5. Apply the maternity pathway adjustment from the corrected LA total file:
   (`antenatal+delivery` + `birth only` + `antenatal only`) /
   (`antenatal+delivery` + `birth only`), using sums of `count` at each LA/age.

Every LA uses its own `total_births_by_LA_2326` delivery counts, age distribution
and pathway ratio. There is no borrowed cluster uplift. The `group` column
defines pathway membership independently of dates. Both components use the
assigned pregnancy FY: `birth_yr` first, otherwise `edd_yr`, pooled over
2023/24-2025/26. Age uses birth age first, with booking age only for source NA.
Missing counts are zero in the model but remain NA in raw diagnostics; an
unknown group is visible in QA and prevents allocation at that LA/age.
Zero delivery denominators give zero uplift. Positive ONS contributions without
usable same-age evidence remain unavailable; zero source contributions stay zero.

Provider shares use supplied cohort rows aged 13-54 without another date filter.
Each pregnancy contributes once, including separate pregnancies for one mother.
Home births and unknown delivery locations remain recorded deliveries.

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
or known no-booking records. For example, 80 antenatal+delivery, 20 birth only
and 25 antenatal only give 100 deliveries, 105 observed antenatal pregnancies
and 125 total pregnancies. The pathway factor is 125/100 = 1.25.
With 60 cohort deliveries and 15 cohort no-delivery records, the delivery gap
is 40 and the booking residual is 50. The shared denominator is 125; it is not
a count of observed bookings. These rules apply to cluster and outside LAs.

## Review and reporting

The saved `booking_modifications` table compares resident and cohort deliveries
by LA and age, shows the uplift source, and calculates the booking residual.
Resident deliveries outside the linked cohort remain in the residual category.
Negative gaps or an LA antenatal-only count below the observed cohort count
leave provider allocations Unallocated; negative residuals are never silently
clamped. Missing uplift evidence is shown explicitly. QA checks coverage as
well as totals. The booking section shows projected residual counts and shares
by LA and age band, separately from provider activity.
Its projected residual and share remain unavailable where a positive source
contribution cannot be estimated or allocated. Zero source contributions stay zero.

The LA source check covers configured cluster LAs and other LAs with recorded
bookings or deliveries at cluster trusts, at ages 13–54. Areas appearing only in
the wider totals file do not enter the model. The console summarises issues once;
`la_source_matching` retains the individual rows. Missing uplift
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
Calculation version: `ons-shared-age-calculations-v24`.

Resident charts sum the master before allocation; trust charts sum completed
provider contributions. The historical comparison is in-sample, not independent
validation. All final age bands are applied after the calculations. Descriptive
annual delivery/booking-share charts use one assigned pregnancy FY: birth FY
first, otherwise EDD FY in both cohort and LA counts;
they do not rerun the full-cohort model on mismatched annual populations.
The detailed ONS growth tables compare source birth counts directly, taking the
repeated source count once per LA/source age/year. An unavailable modifier at
one split age does not remove the known source growth for another age.

## Source editions and checks

The three registration workbooks (2023, 2024 and 2025) matched the ONS downloads
checked on 6 October 2026. Table 5 supplies the stillbirth and multiplicity
modifiers, and England Table 4 supplies the Private–NHS modifier. Each LA's own
expanded resident totals supply its age weights and maternity pathway adjustment.
The pregnancy cohort supplies observed booking and delivery provider counts for allocation.

The report reads the existing supplied `data/2022 SNPP Births persons.csv` directly.
That file is unchanged and matches the persons entry byte-for-byte in the official
2023-geography birth archive:
[ONS 2023-geography birth projections ZIP](https://www.ons.gov.uk/file?uri=%2Fpeoplepopulationandcommunity%2Fpopulationandmigration%2Fpopulationprojections%2Fdatasets%2Fbirthsbyageofmotherz3%2F2022basedmigrationcategoryvarianton2023localauthoritygeographies%2F2022snppbirthsmigcat23.zip).

The female-population snapshot uses `2022 SNPP Population females.csv` from the matching
[ONS 2023-geography population projections ZIP](https://www.ons.gov.uk/file?uri=%2Fpeoplepopulationandcommunity%2Fpopulationandmigration%2Fpopulationprojections%2Fdatasets%2Flocalauthoritiesinenglandz1%2F2022basedmigrationcategoryvarianton2023localauthoritygeographies%2F2022snpppopulationsyoamigcat23.zip).
Archive SHA-256:
`c452e4e8d04747d4f05484efcdefe100e7d6117147c3d2b4051007a71e14ee82`.

Snapshot QA found 13,500 complete, unique, positive cells: six configured
authorities x 90 ages (0-89) x 25 years (2023-2047). Values are copied directly
from ONS; metadata identifies female sex, the 2022 base year, April 2023 geography,
migration-category variant and source archive. The five columns required by
`read_female_population_single_age()` are retained, with direct ONS metadata instead
of Nomis-specific fields. Published ONS population values retain their source precision.

The historical birth snapshot is `data/ons_live_births_la_2013_2025.csv`, from
Nomis dataset `NM_205_1` (`LEBIRTHSLA`). It includes total births (L) and published
maternal age bands for the configured authorities. Adding an authority requires
refreshing this extract and the female-population extract with its GSS code.

## Run and review

Run `source("R/Run analysis.R")` from the project root in the secure
environment. The launcher reads Sarah's parquet inputs, prepares all outputs,
renders the report and checks disclosure controls in the finished HTML. See
`README.md` for files and settings.
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
and undefined zero-denominator rates remain unavailable.

The report calls the fifth modifier **Maternity pathway adjustment**. The existing
`Bookings_uplift_modifier` column name is retained for compatibility; it still means
all resident pregnancies divided by resident deliveries, not bookings divided
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
The total `Maternity_modifier` includes the Private–NHS reduction and pathway uplift.
