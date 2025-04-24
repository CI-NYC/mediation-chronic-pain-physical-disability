
# Mediation Unsafe Pain Management Repository

# Pain Management Mediators

This repository contains scripts and documentation for creating mediator
variables related to pain management practices. The NDC (National Drug
Code) codes used for the non-opioid and opioid mediators, as well as the
CPT (Current Procedural Terminology), HCPCS (Healthcare Common Procedure
Coding System), and ICD-10 (International Classification of Diseases,
10th Revision) codes for the remaining mediators, have been sourced from
the codes listed in
[`mediator_codes.yml`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/mediator_codes.yml)

[`codebook.yml`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/codebook.yml)
contains definitions for each of the mediator variables outlined below

## Opioid-Related Pain Management

### Opioid Pain Prescriptions

#### Opioid Pain Medication

[`0x_mediator_opioid_pain_rx_bin.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_opioid_pain_rx_bin.R)
creates `mediator_opioid_pain_rx`, a binary indicator variable for
whether or not an observation in the analysis cohort filled an opioid
prescription for pain management during the mediator period. It also
creates indicator variables for each of the following opioid categories:

-   Hydrocodone: `mediator_opioid_hydrocodone_rx`
-   Oxycodone: `mediator_opioid_oxycodone_rx`
-   Fentanyl: `mediator_opioid_fentanyl_rx`
-   Morphine: `mediator_opioid_morphine_rx`
-   Methadone: `mediator_opioid_methadone_rx`
-   Hydromorphone: `mediator_opioid_hydromorphone_rx`
-   Codeine: `mediator_opioid_codeine_rx`
-   Buprenorphine:`mediator_opioid_buprenorphine_rx`
-   Other opioids (tramadol, oxymorphone, butorphanol, meperidine,
    nalbuphine, pentazocine, and tapentadol): `mediator_opioid_other_rx`

The resulting dataset is saved as `mediator_opioid_pain_rx_bin.rds`

[`0x_mediator_opioid_pain_rx.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_opioid_pain_rx.R)
processes pharmacy line (RXL) and other services line (OTL) data by
filtering NDC codes listed in
[`mediation_unsafe_pain_mgmt_opioid_pain_ndc.rds`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/input/mediation_unsafe_pain_mgmt_opioid_pain_ndc.rds),
the opioid pain NDC list. The script generates two R datasets,
`mediator_rxl_opioid_pain_rx.rds` and `mediator_otl_opioid_pain_rx.rds`,
containing observations from the analysis cohort with claims for
non-opioid pain prescriptions during the mediator period.

#### Dose, Duration

**Maximum daily dose in MME (considering all opioid prescriptions)**

[`0x_mediator_max_daily_dose_mme.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_max_daily_dose_mme.R)
creates `mediator_max_daily_dose_mme`, the maximum daily dose of opioids
prescribed to a beneficiary within the mediator period, accounting for
multiple prescriptions on the same day. It is calculated by summing the
total dose in MME of all opioid prescriptions for each day within the
mediator period, considering multiple prescriptions on the same day, and
then identifying the highest cumulative dose across all days. The
resulting dataset is saved as `mediator_max_daily_dose_mme.rds`, where
there is a single row/value for each beneficiary. Beneficiaries who did
not have an opioid prescription during the mediator period have a 0.

**Duration**

[`0x_mediator_opioid_months.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_opioid_months.R)
creates `mediator_months_opioid_rx`, the cumulative count of months in
the mediator window during which a beneficiary filled at least one
opioid prescription. The resulting dataset is saved as
`mediator_months_opioid_prescription.rds`.

#### High-Risk Opioid Prescribing Practices

**Prescriber count**

[`0x_mediator_prescribers_per_month.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_prescribers_per_month.R)
creates `mediator_prescribers_6mo_sum`, the total number of unique
prescribers each beneficiary received opioid prescriptions from during
the entire 6-month mediator period. It also creates the following counts
of unique prescribers each beneficiary received opioid prescriptions
from during each month of the mediator period:

-   First month: `mediator_prescribers_month1`
-   Second month: `mediator_prescribers_month2`
-   Third month: `mediator_prescribers_month3`
-   Fourth month: `mediator_prescribers_month4`
-   Fifth month: `mediator_prescribers_month5`
-   Sixth month: `mediator_prescribers_month6`

The output dataset is saved as `mediator_prescribers_per_month.rds`,
where there is a single row/value for each beneficiary. Beneficiaries
who did not have an opioid prescription during the mediator period have
a 0.

Note: Currently, only opioid prescription claims in the pharmacy line
(RXL) are considered in determining the prescriber count per month
mediator values.

**Average daily dose in MME (over 30 days)**

[`0x_mediator_dose_per_month_mediator_period_only.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_dose_per_month_mediator_period_only.R)
creates:

-   `mediator_avg_daily_dose_mme_month[1-6]` (6 variables), the average
    daily dose of opioids in MME units prescribed to a beneficiary over
    a 30-day interval during the 6-month mediator period. The wide
    format output dataset is saved as
    `mediator_average_daily_dose_mme_per_month.rds`, where there is a
    row for each beneficiary in the analysis cohort. Beneficiaries who
    did not have an opioid prescription during any of the 30-day
    intervals of the mediator period receives a 0 for that interval.

-   `mediator_avg_daily_dose_mme_overall`, the overall average daily
    dose of opioids in MME units prescribed over the mediator period,
    considering all months that a beneficiary received an opioid
    prescription. For beneficiaries with more than one month of data,
    the total opioid dose is divided by the total number of days (#
    months \* 30 days) in the mediator period that a beneficiary was
    prescribed opioids to obtain the overall average daily dose. If a
    beneficiary has only one month of data,
    `mediator_avg_daily_dose_mme_overall` is simply
    `avg_30d_opioid_dose` taken from the corresponding monthly summary.
    The output dataset is saved as
    `mediator_average_daily_dose_mme_overall.rds`, where there is a
    single row/value for each beneficiary. Beneficiaries who did not
    have an opioid prescription during the mediator period have a 0.

#### Co-Prescriptions

[`0x_mediator_coprescription_bin.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_coprescription_bin.R)
creates `mediator_opioid_copresc_any`, a binary indicator variable for
whether or not an observation in the analysis cohort filled ≥ 1
stimulant and/or benzodiazepine and/or gabapentinoid prescription(s) and
≥ 1 opioid prescription with the first prescription \>5 days supply and
\>25% overlap of the days prescribed during the mediator period. It also
creates indicator variables for each of the following co-prescription
categories:

-   Stimulant co-prescription: `mediator_opioid_stimulant_copresc`
-   Benzodiazepine co-prescription: `mediator_opioid_benzo_copresc`
-   Gabapentinoid co-prescription:
    `mediator_opioid_gabapentinoid_copresc`
-   Muscle relaxant co-prescription:
    `mediator_opioid_muscle_relaxant_copresc`

The resulting dataset is saved as
`mediator_opioid_coprescriptions_bin.rds`

Note: Only stimulant/benzodiazepine/gabapentinoid/muscle relaxant
prescription claims in the pharmacy line (RXL) are considered in
determining the co-prescription mediator variable values. The other
services line (OTL) is generally reserved for healthcare services and
encounters provided on an outpatient basis and prescription claims in
the OTL dataset are generally assumed to be for single-use/1-day supply.
Less than 1% of the stimulant/benzodiazepine/gabapentinoid prescription
claims in the OTL were for \>5 days supply, therefore, OTL prescription
claims were not considered in the creation of the coprescription
mediator variables.

***Inputs***

-   Stimulant:
    [`0x_mediator_stimulant_rx.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_stimulant_rx.R)
    processes pharmacy line (RXL) data by filtering NDC codes listed in
    [`mediation_unsafe_pain_mgmt_stimulant_ndc.rds`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/input/mediation_unsafe_pain_mgmt_stimulant_ndc.rds),
    the stimulant NDC list. The script generates
    `mediator_rxl_stimulant_rx.rds`, containing observations from the
    analysis cohort with claims for stimulant prescriptions during the
    mediator period.

-   Benzodiazepine:
    [`0x_mediator_benzo_rx`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_benzo_rx.R)
    processes pharmacy line (RXL) data by filtering NDC codes listed in
    [`mediation_unsafe_pain_mgmt_benzo_ndc.rds`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/input/mediation_unsafe_pain_mgmt_benzo_ndc.rds),
    the benzodiazepine NDC list. The script generates
    `mediator_rxl_benzo_rx.rds`, containing observations from the
    analysis cohort with claims for benzodiazepine prescriptions during
    the mediator period.

-   Gabapentinoid:
    [`0x_mediator_gabapentinoid_rx.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_gabapentinoid_rx.R)
    processes pharmacy line (RXL) data by filtering NDC codes listed in
    [`mediation_unsafe_pain_mgmt_gabapentinoid_ndc.rds`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/input/mediation_unsafe_pain_mgmt_gabapentinoid_ndc.rds),
    the gabapentinoid NDC list. The script generates
    `mediator_rxl_gabapentinoid_rx.rds`, containing observations from
    the analysis cohort with claims for gabapentinoid prescriptions
    during the mediator period.

-   Muscle relaxant:
    [`0x_mediator_coprescription_bin.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_coprescription_bin.R)
    filters the pharmacy line (RXL) nonopiod data for NDC codes listed
    in
    [`mediation_unsafe_pain_mgmt_nonopioid_pain_ndc.rds`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/input/mediation_unsafe_pain_mgmt_nonopioid_pain_ndc.rds)
    and that begin with M03 for muscle relaxants.

#### Tapering

[`0x_mediator_tapering.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_tapering.R)
creates `mediator_has_tapering`, an indicator variable for whether or
not an observation in the analysis cohort experienced tapering during
the mediator period. It is calculated by comparing opioid prescription
averages during baseline months and consecutive two-month periods for
three separate sets of baseline and two-month segments. Tapering is
identified if the baseline average is at least 50 MME and there is a
minimum 15% reduction in the subsequent two-month periods as compared to
baseline. The script also considers censoring (beneficiaries who aged
out based on enrollment date) by assigning NA values to censored
observations.

The resulting dataset is saved as `mediator_has_tapering.rds`

## Non-Opioid-Related Pain Management

#### Non-Opioid Pain Medication

[`0x_mediator_nonopioid_pain_rx_bin.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_nonopioid_pain_rx_bin.R)
creates `mediator_nonopioid_pain_rx`, a binary indicator variable for
whether or not an observation in the analysis cohort filled a non-opioid
prescription for pain management during the mediator period, without an
overlapping opioid prescription. It also creates indicator variables for
each of the following non-opioid categories:

-   Anti-depressants: `mediator_nonopioid_antidepressant_rx`
-   Anti-inflammatory and antirheumatic products:
    `mediator_nonopioid_antiinflammatory_rx`
-   Muscle relaxants: `mediator_nonopioid_muscle_relaxant_rx`
-   Topical products for joint and muscular pain:
    `mediator_nonopioid_topical_rx`
-   Gabapentinoids: `mediator_nonopioid_gabapentin_rx`
-   Other analgesics and antipyretics (excluding gabapentinoids):
    `mediator_nonopioid_other_analgesic_rx`

The resulting dataset is saved as `mediator_nonopioid_pain_rx_bin.rds`

[`0x_mediator_nonopioid_pain_rx.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_nonopioid_pain_rx.R)
processes pharmacy line (RXL) and other services line (OTL) data by
filtering NDC codes listed in
[`mediation_unsafe_pain_mgmt_nonopioid_pain_ndc.rds`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/input/mediation_unsafe_pain_mgmt_nonopioid_pain_ndc.rds),
the non-opioid pain NDC list. The script generates two R datasets,
`mediator_rxl_nonopioid_pain_rx.rds` and
`mediator_otl_nonopioid_pain_rx.rds`, containing observations from the
analysis cohort with claims for non-opioid pain prescriptions during the
mediator period.

[`0x_mediator_nonopioid_nonoverlap_pain_rx.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_nonopioid_nonoverlap_pain_rx.R)
filters the two R datasets created above,
`mediator_rxl_nonopioid_pain_rx.rds` and
`mediator_otl_nonopioid_pain_rx.rds`, to include only non-opioid pain
prescriptions without an overlapping opioid prescription. The script
generates 2 out datasets,
`mediator_rxl_nonoverlap_nonopioid_pain_rx.rds` and
`mediator_otl_nonoverlap_nonopioid_pain_rx.rds`, containing observations
from the analysis cohort with claims for non-opioid pain prescriptions
during the mediator period *without an overlapping opioid prescription*.
Note that if a nonopioid prescription ever overlapped with an opioid
prescription during the mediator period for a given beneficiary, that
nonopioid was not included in the output dataset for that beneficiary
(even if the nonopioid was prescribed without an overlapping opioid
prescription at another point during the mediator period).

Note: code has been commented out in
[`0x_mediator_nonopioid_pain_rx_bin.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_nonopioid_pain_rx_bin.R)
for the following non-opioid category indicator variables because there
are no NDC codes in `NDC_to_ATC_crosswalk.rds` that track to ATC codes
A03D or A03DEA:

-   Antispasmodics in combination with analgesics:
    `mediator_nonopioid_analgesic_antispasmodic_rx` (ATC code A03D)
-   Antispasmodics, psycholeptics and analgesics in combination:
    `mediator_nonopioid_analgesic_antispasmodic_psycholeptic_rx` (ATC
    code A03EA)

#### Physical Therapy

[`merge_mediators.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/merge_mediators.R)
reads in the R datasets detailed below and creates
`mediator_has_physical_therapy_any`, an overall binary indicator
variable for whether or not an observation in the analysis cohort had a
claim for any type of physical or restorative therapy during the
mediator period. It is created using the following as input:

-   Acupuncture: `mediator_has_acupuncture` created in
    [`0x_mediator_acupuncture.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_acupuncture.R)
-   Chiropractic: `mediator_has_chiropractic` created in
    [`0x_mediator_chiropractic.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_chiropractic.R)
-   Electrical nerve stimulation:
    `mediator_has_electrical_nerve_stimulation` created in
    [`0x_mediator_electrical_nerve_stimulation.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_electrical_nerve_stimulation.R)
-   Massage therapy: `mediator_has_massage_therapy` created in
    [`0x_mediator_massage_therapy.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_massage_therapy.R)
-   Physical therapy (other): `mediator_has_physical_therapy` created in
    [`0x_mediator_physical_therapy.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_physical_therapy.R)

#### Counseling

[`0x_mediator_counseling.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_counseling.R)
creates `mediator_has_counseling`, a binary indicator variable for
whether or not an observation in the analysis cohort had a claim for
mental health counseling during the mediator period.

#### Multimodal/Multidisciplinary Pain Treatment

1.  `mediator_has_multimodal_pain_treatment`:
    [`merge_mediators.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/merge_mediators.R)
    reads in the R datasets detailed below and creates
    `mediator_has_multimodal_pain_treatment`, an overall binary
    indicator variable for whether or not an observation in the analysis
    cohort had ≥ 2 claims for any of the pain management approaches
    below. It is created using the following as input:

    1.  Ablative techniques: `mediator_has_ablative_techniques` created
        in
        [`0x_mediator_ablative_techniques.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_ablative_techniques.R)
    2.  Acupuncture: `mediator_has_acupuncture` created in
        [`0x_mediator_acupuncture.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_acupuncture.R)
    3.  Blocks (i.e., joint and nerve or nerve root):
        `mediator_has_blocks` created in
        [`0x_mediator_blocks.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_blocks.R)
    4.  Botulinum toxin injections: `mediator_has_botulinum_toxin`
        created in
        [`0x_mediator_botulinum_toxin.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_botulinum_toxin.R)
    5.  Electrical nerve stimulation:
        `mediator_has_electrical_nerve_stimulation` created in
        [`0x_mediator_electrical_nerve_stimulation.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_electrical_nerve_stimulation.R)
    6.  Epidural steroids: `mediator_has_epidural_steroid` created in
        [`0x_mediator_epidural_steroid.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_epidural_steroid.R)
    7.  Intrathecal drug therapies:
        `mediator_has_intrathecal_drug_therapy` created in
        [`0x_mediator_intrathecal_drug_therapy.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_intrathecal_drug_therapy.R)
    8.  Minimally invasive spinal procedures:
        `mediator_has_minimally_invasive_spinal_procedure` created in
        [`0x_mediator_minimally_invasive_spinal_procedure.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_minimally_invasive_spinal_procedure.R)
    9.  Pharmacologic management: `mediator_has_nonopioid_pain_rx` ≥ 1
        non-opioid prescriptions for pain)
    10. Physical/restorative therapy ≥ 1 of the following):
        -   Massage therapy: `mediator_has_massage_therapy` created in
            [`0x_mediator_massage_therapy.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_massage_therapy.R)
        -   Chiropractic: `mediator_has_chiropractic` created in
            [`0x_mediator_chiropractic.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_chiropractic.R)
        -   Physical therapy (other): `mediator_has_physical_therapy`
            created in
            [`0x_mediator_physical_therapy.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_physical_therapy.R)
    11. Psychological treatment: `mediator_has_counseling` created in
        [`0x_mediator_counseling.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_counseling.R)
    12. Trigger point injection: `mediator_has_trigger_point_injection`
        created in
        [`0x_mediator_trigger_point_injection.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators/0x_mediator_trigger_point_injection.R)

2.  `mediator_has_multimodal_pain_treatment_restrict`:
    [`merge_mediators.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/merge_mediators.R)
    also creates `mediator_has_multimodal_pain_treatment_restrict`, an
    overall binary indicator variable for whether or not an observation
    in the analysis cohort had any (one or more) of the following
    nonopioid treatments:

    -   Ablative techniques
    -   Acupuncture
    -   Blocks
    -   Botulinum toxin injections
    -   Electrical nerve stimulation
    -   Epidural steroids
    -   Intrathecal drug therapies
    -   Trigger point injection
    -   Massage therapy
    -   Chiropractic

# Final Analysis Cohort Considerations

This repository also contains the following scripts to create the final
cohort for analysis:

-   [`merge_mediators.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/merge_mediators.R)
    merges all of the mediator datasets in the
    [`0x_create_mediators`](https://github.com/CI-NYC/disability/tree/main/projects/mediation_unsafe_pain_mgmt/0x_create_mediators)
    folder and the analysis cohort dataset to create the joined mediator
    output dataset, `joined_mediator_df.rds`. The output dataset
    contains 1 row for every beneficiary and all of the baseline
    confounder, exposure, outcome, and mediator variables for analysis.

-   [`0x_censoring_18mo_24mo.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_censoring_18mo_24mo.R)
    creates 2 censoring variables, `uncens_18mo` and `uncens_24mo`, for
    every beneficiary in the analysis cohort dataset. They are
    calculated based on whether the `censoring_ever_dt` is missing (NA)
    or greater than or equal to 18/24 months after `washout_start_dt`.
    If true, the variable is assigned a value of 1; otherwise, it is
    assigned a value of 0. The output dataset is saved as
    `censoring_18mo_24mo.rds`

-   [`0x_oud_12mo_18mo_24mo.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/04_create_outcomes/02_oud_12mo_18mo_24mo.R)
    creates outcome variables, `oud_12mo` (OUD by 12 months), `oud_18mo`
    (OUD by 18 months), and `oud_24mo` (OUD by 24 months), for every
    beneficiary in the analysis cohort dataset. They are calculated
    based on whether the `moud_start_date`, `oud_hillary_dt`, or
    `oud_poison_dt` falls within 12/18/24 months after
    `washout_start_dt`. If true, the variable is assigned a value of 1;
    otherwise, it is assigned a value of 0. The output dataset is saved
    as `oud_12mo_18mo_24mo.rds`.

    -   MOUD start dates are extracted in
        [`0x_extract_moud_start_dates.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/04_create_outcomes/01_extract_moud_start_dates.R)

-   The
    [`0x_create_post_exposure_confounders`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_post_exposure_confounders)
    folder contains 2 scripts for creating 2 post-exposure confounder
    variables:

    -   [`0x_create_post_exposure_confounder_anxiety.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_post_exposure_confounders/0x_create_post_exposure_confounder_anxiety.R)
        creates `anxiety_post_exposure_cal`, an indicator variable for
        whether or not an observation in the analysis cohort had an
        anxiety disorder diagnosis during months 7-12 post-Medicaid
        enrollment. The output dataset is saved as
        `post_exposure_anxiety.rds`
    -   [`0x_create_post_exposure_confounder_depression.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_create_post_exposure_confounders/0x_create_post_exposure_confounder_depression.R)
        creates`depression_post_exposure_cal`, an indicator variable for
        whether or not an observation in the analysis cohort had a
        depression disorder diagnosis during months 7-12 post-Medicaid
        enrollment. The output dataset is saved as
        `post_exposure_depression.rds`

-   [`create_final_analysis_cohort.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/create_final_analysis_cohort.R)
    merges the censoring variables in `censoring_18mo_24mo.rds`, and
    post-exposure confounding variables `post_exposure_anxiety.rds`, and
    `post_exposure_depression.rds` to `joined_mediator_df.rds`. It also
    applies the inclusion/exclusion logic detailed below to create the
    final merged analysis dataset, `mediation_analysis_df.rds`. The
    final output dataset contains 1 row for every beneficiary and all of
    the variables for necessary for analysis using the [HDmediation
    package](https://github.com/nt-williams/HDmediation/blob/mlr3superlearner/HDmediation/man/mediation.Rd)
    and filtered based on the following:

    -   Beneficiaries with a minimum study duration of 12 months (see
        [Filtering by Study
        Duration](##%20Dealing%20with%20Missing%20Mediators%20-%20Filtering%20by%20Study%20Duration))
    -   Beneficiaries with a minimum age at enrollment of 35 years (see
        [Filtering by
        Age](##%20Dealing%20with%20Extreme%20Issues%20in%20Positivity%20Between%20Exposure%20Levels%20at%20the%20Younger%20Ages%20-%20Filtering%20by%20Age))

## Dealing with Missing Mediators - Filtering by Study Duration

**Problem**: Some beneficiaries in the analysis cohort may not have had
the full 12 months required to observe the mediators of interest. For
instance, a “0” value for a mediator doesn’t necessarily mean they
didn’t experience the mediator, but rather they may not have had the
relevant diagnosis code due to leaving Medicaid coverage.

**Solution**: To ensure a least biased analysis, only beneficiaries with
a minimum study duration of 12 months are included in the final analysis
cohort. This ensures that beneficiaries have enough time in the study
for both baseline and mediator variables to be observed.

**Operationalization**: Beneficiaries with a `censoring_ever_dt` \< 1
year from their `washout_start_dt` are filtered out of the final
analysis cohort

Note: this is a cohort we analyzed in a secondary/sensitivity analysis
in the descriptive paper. Note: this restriction reduces the N from
5,043,232 to 3,785,577 beneficiaries.

## Dealing with Extreme Issues in Positivity Between Exposure Levels at the Younger Ages - Filtering by Age

**Problem**: Many younger beneficiaries in the analysis cohort have “no
disability, no chronic pain” as their exposure and barely any
comorbidities, making it difficult to find equivalent comparison units
at the other exposure levels for the younger age group.

**Solution**: Only beneficiaries with a minimum age at enrollment of 35
are included in the final analysis cohort.

**Operationalization**: Beneficiaries with `dem_age` \<35 are filtered
out of the final analysis cohort.

Note: this restriction further reduces the N from 3,785,577 to 1,816,185
beneficiaries.

## Dealing with Outcomes During the Mediator Period

**Problem**: Some beneficiaries in the analysis cohort may get the
outcome during the mediator period, making it unfeasible to know whether
the outcome or the mediator came first.

**Solution**: Need to discuss

# Analysis

### Example Dataframe

### Cleaning the Data for Analysis

[`clean_analysis_data.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_analysis/00_process_data/clean_analysis_data.R)
performs data preprocessing and transformation tasks on the
`joined_mediator_df` dataframe. It selects specific variables, imputes
missing values, converts non-numeric variables into numeric using dummy
variables, creates data subsets and dummy exposure variables for
analysis. Each step is detailed below:

1.  `joined_mediator_df.rds` is restricted to only the exposure (A),
    outcome (Y), baseline confounder (W), intermediate confounder (Z),
    mediator (M), and censoring (cens) variables of interest.

2.  The mode is imputed for missing/NA values for the following baseline
    confounders:

    -   `dem_race` = “White, non-Hispanic”
    -   `dem_primary_language_english` = 1 (primary language of English)
    -   `dem_married_or_partnered` = 0 (not married or partnered)
    -   `dem_household_size` = “1” (household size of 1)
    -   `dem_veteran` = 0 (not a veteran)
    -   `dem_tanf_benefits` = 0 (not receiving Temporary Assistance for
        Needy Families (TANF) benefits)
    -   `dem_ssi_benefits` = “Not Applicable”

Additionally, the following binary indicator variables for missing/NA
values were created: - `missing_dem_race` -
`missing_dem_primary_language_english` -
`missing_dem_married_or_partnered` - `missing_dem_household_size` -
`missing_dem_veteran` - `missing_dem_tanf_benefits` -
`missing_dem_ssi_benefits`

3.  Non-numeric baseline confounder variables `dem_sex`, `dem_race`,
    `dem_household_size`, and `dem_ssi_benefits` are converted to binary
    numeric variables using dummy variable coding. N-1 dummy variables
    were created for each the variables as follows:
    -   Sex (reference category set to female):
        -   `dem_sex_m`
    -   Race (reference category set to white, non-Hispanic)
        -   `dem_race_aian`
        -   `dem_race_asian`
        -   `dem_race_black`
        -   `dem_race_hawaiian`
        -   `dem_race_hispanic`
        -   `dem_race_multiracial`
    -   Household size (reference category set to 1)
        -   `dem_household_size_2`
        -   `dem_household_size_2plus`
    -   SSI benefits (reference category set to not applicable)
        -   `dem_ssi_benefits_mandatory_optional`

Note: After all of the above steps are complete, the numeric,
dummy-variable-coded dataset is saved as
`mediation_analysis_df_clean.rds`

4.  `mediation_analysis_df_clean.rds` is subset into 3 analysis datasets
    to compare the “disability and chronic pain”, “disability only”, and
    “chronic pain only” exposure groups against the “neither”
    (reference) group based on the following criteria. Additionally, 3
    exposure dummy variables `exposure_disability_pain`,
    `exposure_disability_only`, and `exposure_pain_only` are created:
    -   `subset_disability_pain_ref` contains all beneficiaries in the
        “disability and chronic pain” and “neither” exposure groups, as
        well as the `exposure_disability_pain` dummy exposure variable
        (1 if exposure = “disability and chronic pain”, 0 otherwise).
        The subset dataframe contains N = 1,710,587 rows/beneficiaries
        and is saved as `subset_disability_pain_ref_df.rds`.

    -   `subset_disability_only_ref` contains all beneficiaries in the
        “disability only” and “neither” exposure groups, as well as the
        `exposure_disability_only` dummy exposure variable (1 if
        exposure = “disability only”, 0 otherwise). The subset dataframe
        contains N = 1,742,614 rows/beneficiaries and is saved as
        `subset_disability_only_ref_df.rds`.

    -   `subset_pain_only_ref` contains all beneficiaries in the
        “chronic pain only” and “neither” exposure groups, as well as
        the `exposure_pain_only` dummy exposure variable (1 if exposure
        = “chronic pain only”, 0 otherwise). The subset dataframe
        contains N = 1,771,892 rows/beneficiaries and is saved as
        `subset_pain_only_ref_df.rds`.

### HDMedidation

[`HDmediation`](https://github.com/nt-williams/HDmediation/blob/mlr3superlearner/HDmediation/man/mediation.Rd)
is used to calculate the indirect and direct effects, confidence
intervals, and other statistics for all mediators (M), or all mediators
excluding mediator j (M_j).

To calculate variable importance, each of the M_j (j excluded) indirect
effects can be subtracted the M (all mediators) indirect effect and then
ranked based on the difference \|M - M_j\| to decide which mediators are
more important. A high value for \|M - M_j\| is a bigger indication of
variable importance. Mediators identified as consistently unimportant
across all 3 exposures do not need further exploration.

In the scripts linked below, the 3 subset dataframes created in
[`clean_analysis_data.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_calculate_variable_importance/clean_analysis_data.R)
are read in and a 50% random sample of each is generated using
`set.seed(1)`. The scripts define a list of machine learning algorithms
(glm, mean (intercept-only), gbm, glmnet, and earth (MARS)) and lists of
variables for baseline confounders (W), post-exposure confounders (Z),
mediators (M), outcome (Y), censoring (cens), and specifies the number
of folds for CV (num_folds). To calculate \[\], all summary
mediators/mediator components are analyzed together as a mediator list
using the `mediation()` function. To calculate \[\]j, a dataframe is
initialized to store output results for each of the exposure subset
dataframes. Within each loop, it iterates over the list of mediators
(M), pulls a single mediator/component out at a time as intermediate
confounder (Z), and performs analysis on the set of remaining mediator
variables using the `mediation()` function.

**01. Indirect effects through all mediators (M)**

[`01_all_mediators.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_analysis/01_all_mediators.R)
calculates the estimated direct and indirect effect for each exposure
dummy on 50% of the dataset by analyzing all mediators together as the
mediator list (M) and the intermediate confounder list (Z) set to NULL.
There is 1 direct and 1 indirect effect (with confidence intervals) for
each exposure group vs. neither (reference).

-   **Full dataset**

|                             | Estimated Direct Effect | \(CI\)               | Estimated Indirect Effect | \(CI\)               | Estimated Total Effect |
|-----------------------------|-------------------------|----------------------|---------------------------|----------------------|------------------------|
| Disability and Chronic Pain | 0.009311                | (0.000417, 0.018205) | 0.052509                  | (0.042392, 0.062626) | 0.061820               |
| Disability Only             | 0.010833                | (0.008007, 0.013659) | 0.005170                  | (0.003904, 0.006437) | 0.016003               |
| Chronic Pain Only           | 0.001694                | (0.000372, 0.003016) | 0.01956                   | (0.018188, 0.020932) | 0.021254               |

-   **50% of the dataset**

|                             | Estimated Direct Effect | \(CI\)                | Estimated Indirect Effect | \(CI\)               | Estimated Total Effect |
|-----------------------------|-------------------------|-----------------------|---------------------------|----------------------|------------------------|
| Disability and Chronic Pain | 0.010488                | (-0.001851, 0.022827) | 0.045622                  | (0.031434, 0.059810) | 0.056110               |
| Disability Only             | 0.012310                | (0.008278, 0.016341)  | 0.006340                  | (0.004331, 0.008349) | 0.018650               |
| Chronic Pain Only           | 0.001644                | (-0.000237, 0.003526) | 0.019546                  | (0.017624, 0.021468) | 0.021190               |

**02. Variable importance of each mediator**

[`02_single_mediator_excluded.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_analysis/02_single_mediator_excluded.R)
calculates the estimated direct and indirect effect for each exposure
dummy on 50% of the dataset by analyzing all mediators (M) but excluding
one at a time as an intermediate confounder (Z). Each of the 11
estimated indirect effects were then subtracted from the indirect
effects of all mediators (M).

-   ***Disability and chronic pain (vs. neither)***

| Mediator Variable                                 | M        | (CI_M)               | M_j      | (CI_M\_j)            | M - M_j   | CI_M\_j includes M |
|---------------------------------------------------|----------|----------------------|----------|----------------------|-----------|--------------------|
| `mediator_opioid_stimulant_copresc`               | 0.045622 | (0.031434, 0.059810) | 0.046126 | (0.031592, 0.060660) | -0.000504 | Yes                |
| `mediator_has_counseling`                         | 0.045622 | (0.031434, 0.059810) | 0.046929 | (0.032014, 0.061844) | -0.001307 | Yes                |
| `mediator_nonopioid_pain_rx`                      | 0.045622 | (0.031434, 0.059810) | 0.053124 | (0.036309, 0.069938) | -0.007502 | Yes                |
| `mediator_has_physical_therapy`                   | 0.045622 | (0.031434, 0.059810) | 0.060319 | (0.040246, 0.080392) | -0.014697 | Yes                |
| `mediator_opioid_benzo_copresc`                   | 0.045622 | (0.031434, 0.059810) | 0.064429 | (0.044183, 0.084675) | -0.018807 | Yes                |
| `mediator_has_multimodal_pain_treatment_restrict` | 0.045622 | (0.031434, 0.059810) | 0.064876 | (0.045534, 0.084218) | -0.019254 | Yes                |
| `mediator_has_tapering`                           | 0.045622 | (0.031434, 0.059810) | 0.106053 | (0.080495, 0.131611) | -0.060431 | No                 |
| `mediator_opioid_gabapentinoid_copresc`           | 0.045622 | (0.031434, 0.059810) | 0.117876 | (0.092121, 0.143631) | -0.072254 | No                 |
| `mediator_months_opioid_rx`                       | 0.045622 | (0.031434, 0.059810) | 0.273920 | (0.222098, 0.325742) | -0.228298 | No                 |
| `mediator_prescribers_6mo_sum`                    | 0.045622 | (0.031434, 0.059810) | 0.288520 | (0.235567, 0.341473) | -0.242898 | No                 |
| `mediator_max_daily_dose_mme`                     | 0.045622 | (0.031434, 0.059810) | 0.318332 | (0.244087, 0.392579) | -0.272710 | No                 |

-   ***Disability only (vs. neither)***

| Mediator Variable                                 | M        | (CI_M)               | M_j      | (CI_M\_j)            | M - M_j   | CI_M\_j includes M |
|---------------------------------------------------|----------|----------------------|----------|----------------------|-----------|--------------------|
| `mediator_has_tapering`                           | 0.006340 | (0.004331, 0.008349) | 0.007346 | (0.004838, 0.009853) | -0.000006 | Yes                |
| `mediator_opioid_stimulant_copresc`               | 0.006340 | (0.004331, 0.008349) | 0.006324 | (0.004297, 0.008351) | 0.000016  | Yes                |
| `mediator_nonopioid_pain_rx`                      | 0.006340 | (0.004331, 0.008349) | 0.005976 | (0.003974, 0.007979) | 0.000364  | Yes                |
| `mediator_has_multimodal_pain_treatment_restrict` | 0.006340 | (0.004331, 0.008349) | 0.006151 | (0.003973, 0.008328) | 0.000189  | Yes                |
| `mediator_has_physical_therapy`                   | 0.006340 | (0.004331, 0.008349) | 0.007022 | (0.004930, 0.009115) | -0.000682 | Yes                |
| `mediator_opioid_gabapentinoid_copresc`           | 0.006340 | (0.004331, 0.008349) | 0.007239 | (0.004956, 0.009522) | -0.000899 | Yes                |
| `mediator_has_counseling`                         | 0.006340 | (0.004331, 0.008349) | 0.005415 | (0.003587, 0.007242) | 0.000925  | Yes                |
| `mediator_opioid_benzo_copresc`                   | 0.006340 | (0.004331, 0.008349) | 0.007948 | (0.005360, 0.010536) | -0.001608 | Yes                |
| `mediator_months_opioid_rx`                       | 0.006340 | (0.004331, 0.008349) | 0.008435 | (0.005860, 0.011011) | -0.002095 | Yes                |
| `mediator_max_daily_dose_mme`                     | 0.006340 | (0.004331, 0.008349) | 0.054769 | (0.037706, 0.071833) | -0.048429 | No                 |
| `mediator_prescribers_6mo_sum`                    | 0.006340 | (0.004331, 0.008349) | 0.066937 | (0.046119, 0.087756) | -0.060597 | No                 |

-   ***Chronic pain only (vs. neither)***

| Mediator Variable                                 | M        | (CI_M)               | M_j       | (CI_M\_j)            | M - M_j   | CI_M\_j includes M |
|---------------------------------------------------|----------|----------------------|-----------|----------------------|-----------|--------------------|
| `mediator_has_counseling`                         | 0.019546 | (0.017624, 0.021468) | 0.019086  | (0.017166, 0.021005) | 0.000460  | Yes                |
| `mediator_opioid_stimulant_copresc`               | 0.019546 | (0.017624, 0.021468) | 0.020213  | (0.018230, 0.022196) | -0.000667 | Yes                |
| `mediator_nonopioid_pain_rx`                      | 0.019546 | (0.017624, 0.021468) | 0.020856  | (0.018754, 0.022958) | -0.001310 | Yes                |
| `mediator_opioid_benzo_copresc`                   | 0.019546 | (0.017624, 0.021468) | 0.024250  | (0.021841, 0.026659) | -0.004704 | No                 |
| `mediator_opioid_gabapentinoid_copresc`           | 0.019546 | (0.017624, 0.021468) | 0.026704  | (0.024231, 0.029177) | -0.007158 | No                 |
| `mediator_has_tapering`                           | 0.019546 | (0.017624, 0.021468) | 0.026838  | (0.024186, 0.029490) | -0.007292 | No                 |
| `mediator_has_physical_therapy`                   | 0.019546 | (0.017624, 0.021468) | 0.027508  | (0.025062, 0.029953) | -0.007962 | No                 |
| `mediator_has_multimodal_pain_treatment_restrict` | 0.019546 | (0.017624, 0.021468) | 0.0284557 | (0.026028, 0.030884) | -0.008909 | No                 |
| `mediator_months_opioid_rx`                       | 0.019546 | (0.017624, 0.021468) | 0.076668  | (0.072988, 0.080348) | -0.057122 | No                 |
| `mediator_prescribers_6mo_sum`                    | 0.019546 | (0.017624, 0.021468) | 0.137100  | (0.127821, 0.146379) | -0.117554 | No                 |
| `mediator_max_daily_dose_mme`                     | 0.019546 | (0.017624, 0.021468) | 0.182537  | (0.167913, 0.197161) | -0.162991 | No                 |

**03. Indirect effects through all opioid related (harmful) mediators**

[`03_all_opioid_related_mediators.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_analysis/03_all_opioid_related_mediators.R)
calculates the estimated direct and indirect effect for each exposure
dummy on 50% of the dataset by analyzing all opioid-related, harmful
mediators together as the mediator list (M) and all beneficial,
non-opioid-related mediators together as intermediate confounders (Z).
There is 1 direct and 1 indirect effect (with confidence intervals) for
each exposure group vs. neither (reference).

-   Mediator list (M)
    -   `mediator_max_daily_dose_mme`
    -   `mediator_has_tapering`
    -   `mediator_months_opioid_rx`
    -   `mediator_opioid_benzo_copresc`
    -   `mediator_opioid_stimulant_copresc`
    -   `mediator_opioid_gabapentinoid_copresc`
    -   `mediator_prescribers_6mo_sum`
-   Intermediate confounder list (Z)
    -   `mediator_has_physical_therapy`
    -   `mediator_has_multimodal_pain_treatment_restrict`
    -   `mediator_has_counseling`
    -   `mediator_nonopioid_pain_rx`

|                             | Estimated Direct Effect | \(CI\)               | Estimated Indirect Effect | \(CI\)               | Estimated Total Effect |
|-----------------------------|-------------------------|----------------------|---------------------------|----------------------|------------------------|
| Disability and Chronic Pain | 0.106756                | (0.059574, 0.153938) | 0.282665                  | (0.236446, 0.328884) | 0.389421               |
| Disability Only             | 0.025040                | (0.014563, 0.035517) | 0.026496                  | (0.020898, 0.032094) | 0.051536               |
| Chronic Pain Only           | 0.002168                | (0.000430, 0.003906) | 0.025434                  | (0.023181, 0.027686) | 0.027602               |

**04. Indirect effects through all non-opioid-related (beneficial)
mediators**

[`04_all_nonopioid_related_mediators.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_analysis/04_all_nonopioid_related_mediators.R)
calculates the estimated direct and indirect effect for each exposure
dummy on 50% of the dataset by analyzing all
non-opioid-related-beneficial mediators together as the mediator list
(M) and all opioid-related harmful mediators together as intermediate
confounders (Z). There is 1 direct and 1 indirect effect (with
confidence intervals) for each exposure group vs. neither (reference).

-   Mediator list (M)
    -   `mediator_has_physical_therapy`
    -   `mediator_has_multimodal_pain_treatment_restrict`
    -   `mediator_has_counseling`
    -   `mediator_nonopioid_pain_rx`
-   Intermediate confounder list (Z)
    -   `mediator_max_daily_dose_mme`
    -   `mediator_has_tapering`
    -   `mediator_months_opioid_rx`
    -   `mediator_opioid_benzo_copresc`
    -   `mediator_opioid_stimulant_copresc`
    -   `mediator_opioid_gabapentinoid_copresc`
    -   `mediator_prescribers_6mo_sum`

|                             | Estimated Direct Effect | \(CI\)               | Estimated Indirect Effect | \(CI\)                | Estimated Total Effect |
|-----------------------------|-------------------------|----------------------|---------------------------|-----------------------|------------------------|
| Disability and Chronic Pain | 0.039451                | (0.026042, 0.052861) | -0.000460                 | (-0.006766, 0.005847) | 0.038991               |
| Disability Only             | 0.015306                | (0.011064, 0.019548) | 0.001244                  | (0.000228, 0.002259)  | 0.016550               |
| Chronic Pain Only           | 0.013467                | (0.010976, 0.015958) | 0.001602                  | (0.000437, 0.002767)  | 0.015069               |

**05. Indirect effect of a single mediator at a time**

[`05_single_mediator.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_analysis/05_single_mediator.R)
calculates the estimated direct and indirect effect for each exposure
dummy on 50% of the dataset by analyzing one mediator at a time as M and
the rest as intermediate confounders (Z). There are 11 direct and 11
indirect effect (with confidence intervals) for each exposure group
vs. neither (reference).

-   ***Disability and chronic pain (vs. neither)***

| Mediator Variable                                 | Estimated Direct Effect | \(CI\)                | Estimated Indirect Effect | \(CI\)                | Estimated Total Effect |
|---------------------------------------------------|-------------------------|-----------------------|---------------------------|-----------------------|------------------------|
| `mediator_has_counseling`                         | 0.048019                | (0.031483, 0.064554)  | -0.000622                 | (-0.002950, 0.001706) | 0.047397               |
| `mediator_opioid_stimulant_copresc`               | 0.047107                | (0.030615, 0.063600)  | -0.000149                 | (-0.000902, 0.000605) | 0.046958               |
| `mediator_has_multimodal_pain_treatment_restrict` | 0.036020                | (0.019378, 0.052663)  | 0.000157                  | (-0.005221, 0.005535) | 0.036177               |
| `mediator_max_daily_dose_mme`                     | 0.307452                | (0.287060, 0.327844)  | 0.001254                  | (-0.021302, 0.023810) | 0.308706               |
| `mediator_prescribers_6mo_sum`                    | 0.003086                | (-0.008813, 0.014985) | 0.002714                  | (-0.000890, 0.006317) | 0.005800               |
| `mediator_nonopioid_pain_rx`                      | 0.028735                | (0.014805, 0.042664)  | 0.002982                  | (0.000196, 0.005768)  | 0.031717               |
| `mediator_has_physical_therapy`                   | 0.034468                | (0.017763, 0.051172)  | 0.003027                  | (-0.003667, 0.009722) | 0.037495               |
| `mediator_opioid_benzo_copresc`                   | 0.036450                | (0.020796, 0.052104)  | 0.005753                  | (-0.000267, 0.011773) | 0.042203               |
| `mediator_months_opioid_rx`                       | 0.003210                | (-0.007961, 0.014381) | 0.006794                  | (0.002190, 0.011398)  | 0.009004               |
| `mediator_has_tapering`                           | 0.034628                | (0.019016, 0.050240)  | 0.007807                  | (0.000507, 0.015107)  | 0.042435               |
| `mediator_opioid_gabapentinoid_copresc`           | 0.028969                | (0.014267, 0.043670)  | 0.017156                  | (0.006650, 0.027662)  | 0.046125               |

-   ***Disability only (vs. neither)***

| Mediator Variable                                 | Estimated Direct Effect | \(CI\)               | Estimated Indirect Effect | \(CI\)                  | Estimated Total Effect |
|---------------------------------------------------|-------------------------|----------------------|---------------------------|-------------------------|------------------------|
| `mediator_has_physical_therapy`                   | 0.016392                | (0.011679, 0.021104) | -0.000312                 | (-0.000617, -0.0000073) | 0.016080               |
| `mediator_has_multimodal_pain_treatment_restrict` | 0.016302                | (0.011601, 0.021003) | -0.000047                 | (-0.000094, -0.000000)  | 0.016255               |
| `mediator_opioid_stimulant_copresc`               | 0.015942                | (0.011324, 0.020559) | 0.000020                  | (-0.000121, 0.000160)   | 0.015962               |
| `mediator_prescribers_6mo_sum`                    | 0.011568                | (0.007090, 0.016047) | 0.000224                  | (-0.000131, 0.000578)   | 0.011792               |
| `mediator_has_tapering`                           | 0.015110                | (0.010529, 0.019692) | 0.000279                  | (-0.000463, 0.001022)   | 0.015389               |
| `mediator_opioid_benzo_copresc`                   | 0.014473                | (0.009892, 0.019054) | 0.000356                  | (-0.000346, 0.001059)   | 0.014829               |
| `mediator_nonopioid_pain_rx`                      | 0.014212                | (0.009795, 0.018629) | 0.000412                  | (0.000040, 0.000784)    | 0.014624               |
| `mediator_months_opioid_rx`                       | 0.011795                | (0.007124, 0.016465) | 0.000518                  | (-0.000442, 0.001477)   | 0.012313               |
| `mediator_has_counseling`                         | 0.014828                | (0.010378, 0.019276) | 0.001251                  | (0.000320, 0.002182)    | 0.016079               |
| `mediator_max_daily_dose_mme`                     | 0.024001                | (0.019258, 0.028743) | 0.001430                  | (-0.000345, 0.003205)   | 0.025431               |
| `mediator_opioid_gabapentinoid_copresc`           | 0.013547                | (0.009048, 0.018047) | 0.001648                  | (0.000604, 0.002692)    | 0.015195               |

-   ***Chronic pain only (vs. neither)***

| Mediator Variable                                 | Estimated Direct Effect | \(CI\)               | Estimated Indirect Effect | \(CI\)                 | Estimated Total Effect |
|---------------------------------------------------|-------------------------|----------------------|---------------------------|------------------------|------------------------|
| `mediator_prescribers_6mo_sum`                    | 0.027890                | (0.025851, 0.029928) | -0.004123                 | (-0.005369, -0.002877) | 0.023767               |
| `mediator_has_multimodal_pain_treatment_restrict` | 0.015797                | (0.013176, 0.018419) | -0.000355                 | (-0.001259, 0.000550)  | 0.015442               |
| `mediator_has_physical_therapy`                   | 0.014391                | (0.011772, 0.017010) | -0.000137                 | (-0.001095, 0.000821)  | 0.014254               |
| `mediator_opioid_stimulant_copresc`               | 0.020308                | (0.017904, 0.022711) | 0.000201                  | (-0.000011, 0.000412)  | 0.020509               |
| `mediator_has_counseling`                         | 0.020424                | (0.018038, 0.022809) | 0.000248                  | (0.000030, 0.000466)   | 0.020672               |
| `mediator_nonopioid_pain_rx`                      | 0.012132                | (0.010032, 0.014233) | 0.001506                  | (0.001011, 0.002002)   | 0.013638               |
| `mediator_opioid_benzo_copresc`                   | 0.017865                | (0.015540, 0.020190) | 0.002226                  | (0.001551, 0.002902)   | 0.020091               |
| `mediator_has_tapering`                           | 0.015698                | (0.013444, 0.017951) | 0.004595                  | (0.003587, 0.005603)   | 0.020293               |
| `mediator_months_opioid_rx`                       | 0.028011                | (0.026073, 0.029949) | 0.004788                  | (0.003659, 0.005918)   | 0.032799               |
| `mediator_opioid_gabapentinoid_copresc`           | 0.012253                | (0.010126, 0.014380) | 0.007841                  | (0.006561, 0.009120)   | 0.020094               |
| `mediator_max_daily_dose_mme`                     | 0.048585                | (0.046368, 0.050803) | 0.009744                  | (0.005800, 0.013688)   | 0.058329               |

### Tables

[`0x_tables`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_tables)
contains
[`table1_components.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_tables/table1_components.R),
and R script used to generate the Table 1 counts and Latex code for the
for the mediation manuscript. The output saved in
[`table1_latex_code.txt`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_tables/table1_latex_code.txt).

### Figures

[`0x_figures`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_figures)
contains the following files used to create figures for the mediation
manuscript:

-   [`indirect_effect_forest_plot.Rmd`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_figures/indirect_effect_forest_plot.Rmd)
    creates 3 forest plots and of the indirect effect estimates (with
    CIs) and total effect estimates for each of the 3 exposure
    categories as well as a stacked plot with all 3 figures combined.
    Estimates for the following mediators/mediator groupings were
    included in the plot:
    -   All mediators
        -   From
            [`01_all_mediators.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_analysis/01_all_mediators.R)
    -   All opioid-related (harmful) mediators
        -   From
            [`03_all_opioid_related_mediators.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_analysis/04_all_opioid_related_mediators.R)
    -   All non-opioid-related (beneficial) mediators
        -   From
            [`04_all_nonopioid_related_mediators.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_analysis/04_all_nonopioid_related_mediators.R)
    -   Each of the following (single) mediators from
        [`05_single_mediator.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_analysis/05_single_mediator.R)
        -   Maximum daily opioid dose (in MME) considering doses of all
            fills for a particular day
        -   Number of months of opioid prescriptions
        -   Number of unique opioid prescribers
        -   Whether or not experienced opioid tapering
        -   Co-prescription of benzodiazapines
        -   Co-prescription of stimulants
        -   Co-prescription of gabapentinoids
        -   Presence of a non-opioid pain prescription
        -   Mental health counseling
        -   Physical therapy
        -   Multimodel pain treatment (blative techniques, acupuncture,
            joint and nerve blocks, botulinum toxin injections,
            chiropractic, electrical nerve stimulation, epidural
            steroids, intrathecal drug therapies, massage therapy, and
            trigger point injection)
-   [`variable_importance_heat_plot.Rmd`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_figures/variable_importance_heat_plot.Rmd)
    creates a heat plot of the of the \|M - M_j\| difference estimates
    for each mediator by exposure category to display the variable
    importance results and compare across mediators/exposures. The
    plotted estimates were calculated in
    [`02_single_mediator_excluded.R`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_analysis/02_single_mediator_excluded.R).
-   [`study_timeline.pptx`](https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/0x_figures/study_timeline.pptx)
    is an editable PowerPoint template to create a study timeline of
    measure assessement windows for the mediation manuscript.
