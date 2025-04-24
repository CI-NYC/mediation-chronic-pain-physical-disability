# READ ME -----------------------------------------------------------------
#
# Author: Sarah Forrest
# Created: 2023-09-18
# Last updated: 2023-10-31
#
# Creates table 1 components and Latex code for the mediation analysis cohort
#
# -------------------------------------------------------------------------

# Set up -----------------------------------------------------------------------

# Load libraries
library(tidylog)
library(tidyverse)
library(janitor)
library(arrow)
library(gtsummary)
library(gt)

drv_root <- "/home/data/disability/mediation_unsafe_pain_mgmt"

# Read in dataset
analysis_cohort <- read_rds(file.path(drv_root, "mediation_analysis_df.rds"))
# analysis_cohort <- mediation_analysis_df_clean


# Create data for table 1 pieces -----------------------------------------------

# define the desired order of the exposure levels
level_order <- c("disability and chronic pain", "disability only", "chronic pain only", "neither")

# convert the character variable to a factor with the desired level order
analysis_cohort$disability_pain_cal <- fct_relevel(analysis_cohort$disability_pain_cal, level_order)

analysis_cohort <- analysis_cohort |>
    mutate(disability_pain_cal = recode(disability_pain_cal,
                              "disability and chronic pain" = "Physical Disability and Chronic Pain",
                              "disability only" = "Physical Disability",
                              "chronic pain only" = "Chronic Pain",
                              "neither" = "Neither"))

dem_tbl_data <- analysis_cohort |>
    select(disability_pain_cal, 
           dem_age,
           dem_sex,
           dem_race,
           dem_primary_language_english,
           dem_married_or_partnered,
           dem_probable_high_income,
           dem_household_size,
           dem_veteran,
           dem_tanf_benefits,
           dem_ssi_benefits) |>
    labelled::set_variable_labels(
        dem_age = "Age",
        dem_sex = "Sex",
        dem_race = "Race/Ethnicity",
        dem_primary_language_english = "Primary Language English",
        dem_married_or_partnered = "Married/Partnered",
        dem_probable_high_income = "High Income",
        dem_household_size = "Household Size",
        dem_veteran = "Veteran",
        dem_tanf_benefits = "TANF Benefits",
        dem_ssi_benefits = "SSI Benefits"
    ) 

comorb_tbl_data <- analysis_cohort |>
    select(disability_pain_cal, 
           bipolar_washout_cal,
           anxiety_washout_cal,
           adhd_washout_cal,
           depression_washout_cal,
           mental_ill_washout_cal,
           anxiety_post_exposure_cal,
           depression_post_exposure_cal) |>
    labelled::set_variable_labels(
        bipolar_washout_cal = "Bipolar",
        anxiety_washout_cal = "Anxiety",
        adhd_washout_cal = "ADD/ADHD",
        depression_washout_cal = "Depression",
        mental_ill_washout_cal = "Other Mental Illness",
        anxiety_post_exposure_cal = "Post Exposure Anxiety",
        depression_post_exposure_cal = "Post Exposure Depression")

med_tbl_data <- analysis_cohort |>
    select(disability_pain_cal, 
    # Opioid pain management
        # Opioid pain prescriptions
           mediator_opioid_pain_rx,
           mediator_opioid_hydrocodone_rx,
           mediator_opioid_oxycodone_rx,
           mediator_opioid_fentanyl_rx,
           mediator_opioid_morphine_rx,
           mediator_opioid_methadone_rx,
           mediator_opioid_hydromorphone_rx,
           mediator_opioid_codeine_rx,
           mediator_opioid_buprenorphine_rx,
           mediator_opioid_other_rx,
           mediator_count_opioid_pain_rx,
        # Dose, duration
           mediator_max_daily_dose_mme,
           mediator_months_opioid_rx,
        # High-risk opioid prescribing practices
           mediator_prescribers_6mo_sum,
           mediator_avg_daily_dose_mme_overall,
        # Co-prescriptions
           mediator_opioid_copresc_any,
           mediator_opioid_benzo_copresc,
           mediator_opioid_stimulant_copresc,
           mediator_opioid_gabapentinoid_copresc,
           mediator_opioid_muscle_relaxant_copresc,
           mediator_count_opioid_copresc,
        # Tapering
           mediator_has_tapering,
    # Nonopioid pain management
        # Nonopioid pain prescriptions
           mediator_nonopioid_pain_rx,
           mediator_nonopioid_antidepressant_rx,
           mediator_nonopioid_muscle_relaxant_rx,
           mediator_nonopioid_antiinflammatory_rx,
           mediator_nonopioid_topical_rx,
           mediator_nonopioid_gabapentin_rx,
           mediator_nonopioid_other_analgesic_rx,
           # mediator_nonopioid_combo_antispasmodics_analgesic (A03D: Antispasmodics in combination with analgesics)
           # mediator_nonopioid_combo_psycholeptic_analgesic (A03EA: Antispasmodics, psycholeptics and analgesics in combination)
           mediator_count_nonopioid_pain_rx,
        # Physical therapy
           mediator_has_physical_therapy,
           mediator_has_massage_therapy,
           mediator_has_chiropractic,
        # Multimodal/multidisciplinary pain treatment
           mediator_has_ablative_techniques,
           mediator_has_acupuncture,
           mediator_has_blocks,
           mediator_has_botulinum_toxin,
           mediator_has_electrical_nerve_stimulation,
           mediator_has_epidural_steroid,
           mediator_has_intrathecal_drug_therapy,
           mediator_has_minimally_invasive_spinal_procedure,
           mediator_has_trigger_point_injection,
        # Counseling
           mediator_has_counseling,
        # Additional summary variables
           mediator_has_physical_therapy_any,
           mediator_has_multimodal_pain_treatment_restrict,
           mediator_count_multimodal_pain_treatment_restrict) 

med_tbl_data <- med_tbl_data |>
    labelled::set_variable_labels(
        mediator_opioid_pain_rx = "Any",
        mediator_opioid_hydrocodone_rx = "Hydrocodone",
        mediator_opioid_oxycodone_rx = "Oxycodone",
        mediator_opioid_fentanyl_rx = "Fentanyl",
        mediator_opioid_morphine_rx = "Morphine",
        mediator_opioid_methadone_rx = "Methadone",
        mediator_opioid_hydromorphone_rx = "Hydromorphone",
        mediator_opioid_codeine_rx = "Codeine",
        mediator_opioid_buprenorphine_rx = "Buprenorphine",
        mediator_opioid_other_rx = "Other",
        mediator_count_opioid_pain_rx = "Count",
        mediator_max_daily_dose_mme = "Max Daily Dose (MME)",
        mediator_months_opioid_rx = "Months Supply",
        mediator_prescribers_6mo_sum = "Distinct Prescribers",
        mediator_avg_daily_dose_mme_overall = "Avg. Daily Dose (MME)",
        mediator_opioid_copresc_any = "Any",
        mediator_opioid_benzo_copresc = "Benzodiazepine",
        mediator_opioid_stimulant_copresc = "Stimulant",
        mediator_opioid_gabapentinoid_copresc = "Gabapentinoid",
        mediator_opioid_muscle_relaxant_copresc = "Muscle Relaxant",
        mediator_count_opioid_copresc = "Count",
        mediator_has_tapering = "Tapering",
        mediator_nonopioid_pain_rx = "Any",
        mediator_nonopioid_antidepressant_rx = "Antidepressant",
        mediator_nonopioid_muscle_relaxant_rx = "Muscle Relaxant",
        mediator_nonopioid_antiinflammatory_rx = "Antiinflammatory & Antirheumatic",
        mediator_nonopioid_topical_rx = "Topical",
        mediator_nonopioid_gabapentin_rx = "Gabapentinoid",
        mediator_nonopioid_other_analgesic_rx = "Other Analgesic & Antipyretic",
        # mediator_nonopioid_combo_antispasmodics_analgesic = "Antispasmodics in combination with analgesics",
        # mediator_nonopioid_combo_psycholeptic_analgesic = "Antispasmodics, psycholeptics and analgesics in combination",
        mediator_count_opioid_pain_rx = "Count",
        mediator_has_massage_therapy = "Massage Therapy",
        mediator_has_chiropractic = "Chiropractic",
        mediator_has_physical_therapy = "Physical Therapy",
        mediator_has_ablative_techniques = "Ablative Techniques",
        mediator_has_acupuncture = "Acupuncture",
        mediator_has_blocks = "Blocks",
        mediator_has_botulinum_toxin = "Botulinum Toxin Injection",
        mediator_has_electrical_nerve_stimulation = "Electrical Nerve Stimulation",
        mediator_has_epidural_steroid = "Epidural Steroid",
        mediator_has_intrathecal_drug_therapy = "Intrathecal Drug Therapy",
        mediator_has_minimally_invasive_spinal_procedure = "Minimally Invasive Spinal Procedure",
        mediator_has_trigger_point_injection = "Trigger Point Injection",
        mediator_has_counseling = "Counseling",
        mediator_has_physical_therapy_any = "Any",
        mediator_has_multimodal_pain_treatment_restrict = "Multimodal/Multidisciplinary Pain Treatment",
        mediator_count_multimodal_pain_treatment_restrict = "Count"
    )

oud_tbl_data <- analysis_cohort  |>
    select(disability_pain_cal,
           oud_cal,
           oud_12mo,
           oud_18mo,
           oud_24mo) |>
    labelled::set_variable_labels(
        oud_cal = "OUD",
        oud_12mo = "OUD by 12 months",
        oud_18mo = "OUD by 18 months",
        oud_24mo = "OUD by 24 months"
        )


# Create tables for the full cohort --------------------------------------------

## Demographics
dem_tbl <- dem_tbl_data |>
    tbl_summary(by = disability_pain_cal) |>
    bold_labels()

latex_dem_tbl <- dem_tbl |>
    as_gt() |>
    as_latex()

dem_tbl_df <- dem_tbl |>
    as_gt() |>
    as.data.frame() |>
    select(label:stat_4)


## Comorbidities
comorb_tbl <- comorb_tbl_data |>
    tbl_summary(by = disability_pain_cal) 

latex_comorb_tbl <- comorb_tbl |>
    as_gt() |>
    as_latex()

comorb_tbl_df <- comorb_tbl |>
    as_gt() |>
    as.data.frame() |>
    select(label:stat_4)


## Mediators
med_tbl <- med_tbl_data |>
    tbl_summary(by = disability_pain_cal) 

latex_med_tbl <- med_tbl |>
    as_gt() |>
    as_latex()

med_tbl_df <- med_tbl |>
    as_gt() |>
    as.data.frame() |>
    select(label:stat_4)


## OUD

oud_tbl <- oud_tbl_data |>
    tbl_summary(by = disability_pain_cal)

latex_oud_tbl <- oud_tbl |>
    as_gt() |>
    as_latex()

oud_tbl_df <- oud_tbl |>
    as_gt() |>
    as.data.frame() |>
    select(label:stat_4)


# Create table for the opioid subset cohort ------------------------------------

med_tbl_data_opioid_subset <- med_tbl_data |>
    filter(mediator_opioid_pain_rx == 1) |>
    select(disability_pain_cal,
           mediator_count_opioid_pain_rx,
           mediator_max_daily_dose_mme,
           mediator_months_opioid_rx,
           mediator_prescribers_6mo_sum,
           mediator_avg_daily_dose_mme_overall,
           mediator_opioid_copresc_any,
           mediator_opioid_benzo_copresc,
           mediator_opioid_stimulant_copresc,
           mediator_opioid_gabapentinoid_copresc,
           mediator_opioid_muscle_relaxant_copresc,
           mediator_count_opioid_copresc,
           mediator_has_tapering) |>
    tbl_summary(by = disability_pain_cal) 

latex_med_tbl_opioid_subset <- med_tbl_data_opioid_subset |>
    as_gt() |>
    as_latex()


# Print out Latex code ---------------------------------------------------------

# Full cohort
cat(latex_dem_tbl)
cat(latex_comorb_tbl)
cat(latex_med_tbl)
cat(latex_oud_tbl)

# Opioid subset cohort
cat(latex_med_tbl_opioid_subset)


# Calculate summary median and IQR for mediator_months_opioid_rx ---------------

# Full dataset
    # mediator_months_opioid_rx
med_iqr_med_months_opioid_rx <- med_tbl_data |>
    select(disability_pain_cal,
           mediator_months_opioid_rx) |>
    group_by(disability_pain_cal) |>
    summarize(
        median_months_supply = round(median(mediator_months_opioid_rx, na.rm = TRUE), 2),
        lower_iqr_bound = round(quantile(mediator_months_opioid_rx, 0.25, na.rm = TRUE), 2),
        upper_iqr_bound = round(quantile(mediator_months_opioid_rx, 0.75, na.rm = TRUE), 2)
    )
med_iqr_med_months_opioid_rx

    # mediator_count_opioid_pain_rx
med_iqr_med_count_opioid_pain_rx <- med_tbl_data |>
    select(disability_pain_cal,
           mediator_count_opioid_pain_rx) |>
    group_by(disability_pain_cal) |>
    summarize(
        median_months_supply = round(median(mediator_count_opioid_pain_rx, na.rm = TRUE), 2),
        lower_iqr_bound = round(quantile(mediator_count_opioid_pain_rx, 0.25, na.rm = TRUE), 2),
        upper_iqr_bound = round(quantile(mediator_count_opioid_pain_rx, 0.75, na.rm = TRUE), 2)
    )
med_iqr_med_count_opioid_pain_rx

    # mediator_count_nonopioid_pain_rx
med_iqr_med_count_nonopioid_pain_rx <- med_tbl_data |>
    select(disability_pain_cal,
           mediator_count_nonopioid_pain_rx) |>
    group_by(disability_pain_cal) |>
    summarize(
        median_months_supply = round(median(mediator_count_nonopioid_pain_rx, na.rm = TRUE), 2),
        lower_iqr_bound = round(quantile(mediator_count_nonopioid_pain_rx, 0.25, na.rm = TRUE), 2),
        upper_iqr_bound = round(quantile(mediator_count_nonopioid_pain_rx, 0.75, na.rm = TRUE), 2)
    )
med_iqr_med_count_nonopioid_pain_rx

    # mediator_count_opioid_copresc
med_iqr_med_count_opioid_copresc <- med_tbl_data |>
    select(disability_pain_cal,
           mediator_count_opioid_copresc) |>
    group_by(disability_pain_cal) |>
    summarize(
        median_months_supply = round(median(mediator_count_opioid_copresc, na.rm = TRUE), 2),
        lower_iqr_bound = round(quantile(mediator_count_opioid_copresc, 0.25, na.rm = TRUE), 2),
        upper_iqr_bound = round(quantile(mediator_count_opioid_copresc, 0.75, na.rm = TRUE), 2)
    )
med_iqr_med_count_opioid_copresc

    # mediator_count_multimodal_pain_treatment_restrict
med_iqr_med_mediator_count_multimodal_pain_treatment_restrict <- med_tbl_data |>
    select(disability_pain_cal,
           mediator_count_multimodal_pain_treatment_restrict) |>
    group_by(disability_pain_cal) |>
    summarize(
        median_months_supply = round(median(mediator_count_multimodal_pain_treatment_restrict, na.rm = TRUE), 2),
        lower_iqr_bound = round(quantile(mediator_count_multimodal_pain_treatment_restrict, 0.25, na.rm = TRUE), 2),
        upper_iqr_bound = round(quantile(mediator_count_multimodal_pain_treatment_restrict, 0.75, na.rm = TRUE), 2)
    )
med_iqr_med_mediator_count_multimodal_pain_treatment_restrict

# Opioid subset dataset
    # mediator_months_opioid_rx
med_iqr_med_months_opioid_rx_opioid_subset <- med_tbl_data |>
    filter(mediator_opioid_pain_rx == 1) |>
    select(disability_pain_cal,
           mediator_months_opioid_rx) |>
    group_by(disability_pain_cal) |>
    summarize(
        median_months_supply = round(median(mediator_months_opioid_rx, na.rm = TRUE), 2),
        lower_iqr_bound = round(quantile(mediator_months_opioid_rx, 0.25, na.rm = TRUE), 2),
        upper_iqr_bound = round(quantile(mediator_months_opioid_rx, 0.75, na.rm = TRUE), 2)
    )
med_iqr_med_months_opioid_rx_opioid_subset

    # mediator_count_opioid_copresc
med_iqr_med_count_opioid_copresc_opioid_subset <- med_tbl_data |>
    filter(mediator_opioid_pain_rx == 1) |>
    select(disability_pain_cal,
           mediator_count_opioid_copresc) |>
    group_by(disability_pain_cal) |>
    summarize(
        median_months_supply = round(median(mediator_count_opioid_copresc, na.rm = TRUE), 2),
        lower_iqr_bound = round(quantile(mediator_count_opioid_copresc, 0.25, na.rm = TRUE), 2),
        upper_iqr_bound = round(quantile(mediator_count_opioid_copresc, 0.75, na.rm = TRUE), 2)
    )
med_iqr_med_count_opioid_copresc_opioid_subset

