# Maternity modelling terminology key

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
