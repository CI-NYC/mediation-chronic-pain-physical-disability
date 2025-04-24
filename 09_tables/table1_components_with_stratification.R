# READ ME -----------------------------------------------------------------
#
# Author: Shodai Inose
# Created: 2024-02-22
# Last updated: 2024-04-29
#
# Updated code from SF's table1_components.R file 
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

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

disability_subset <- readRDS(paste0(drv_root, "/subset_disability_only_ref_df.rds"))|>
    filter(exposure_disability_only_subset == 1)

disability_subset_id <- disability_subset$BENE_ID

# Read in dataset
analysis_cohort <- read_rds(file.path(drv_root, "mediation_analysis_df.rds")) |>
    filter(disability_pain_cal != "disability and chronic pain") |>
    mutate(keep = case_when(disability_pain_cal == "chronic pain only" ~ 1,
                            disability_pain_cal == "neither" ~ 1,
                            disability_pain_cal == "disability only" & BENE_ID %in% disability_subset_id~ 1,
                            TRUE ~ 0)) |>
    filter(keep == 1) |>
    mutate(mediator_opioid_copresc_benzo_gaba = 
               ifelse(mediator_opioid_benzo_copresc == 1 |
                          mediator_opioid_gaba_copresc == 1, 1, 0),
           mediator_nonopioid_nonoverlap_mrelax_gaba_benzo = 
               ifelse(mediator_nonopioid_nonoverlap_mrelax == 1 |
                          mediator_nonopioid_nonoverlap_gaba == 1 |
                          mediator_nonopioid_nonoverlap_benzo == 1, 1, 0),
           mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory = 
               ifelse(mediator_nonopioid_nonoverlap_antidepressant == 1 |
                          mediator_nonopioid_nonoverlap_antiinflamatory == 1, 1, 0)) |>
    mutate(disability_pain_cal = factor(disability_pain_cal))

rm(disability_subset)
rm(disability_subset_id)

# Create data for table 1 pieces -----------------------------------------------

# define the desired order of the exposure levels
level_order <- c("disability only", "chronic pain only", "neither")

# convert the character variable to a factor with the desired level order
analysis_cohort$disability_pain_cal <- fct_relevel(analysis_cohort$disability_pain_cal, level_order)

analysis_cohort <- analysis_cohort |>
    mutate(disability_pain_cal = recode(disability_pain_cal,
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
           dem_ssi_benefits,
           has_2plus_ED_visit_washout,
           has_2plus_ED_visit_post_exposure) |>
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
        dem_ssi_benefits = "SSI Benefits",
        has_2plus_ED_visit_washout = "2+ ED Visits (Washout)",
        has_2plus_ED_visit_post_exposure = "2+ ED Visits (Post-Exposure)"
    ) 


comorb_tbl_data <- analysis_cohort |>
    select(disability_pain_cal, 
           bipolar_washout_cal,
           anxiety_washout_cal,
           adhd_washout_cal,
           depression_washout_cal,
           mental_ill_washout_cal,
           anxiety_post_exposure_cal,
           depression_post_exposure_cal,
           bipolar_post_exposure_cal,
           mediator_has_counseling
           ) |>
    labelled::set_variable_labels(
        bipolar_washout_cal = "Bipolar",
        anxiety_washout_cal = "Anxiety",
        adhd_washout_cal = "ADD/ADHD",
        depression_washout_cal = "Depression",
        mental_ill_washout_cal = "Other Mental Illness",
        anxiety_post_exposure_cal = "Anxiety (Post-Exposure)",
        depression_post_exposure_cal = "Depression (Post-Exposure)",
        bipolar_post_exposure_cal = "Bipolar (Post-Exposure)",
        mediator_has_counseling = "Mental Health Counseling")

med_tbl_data <- analysis_cohort |>
    select(disability_pain_cal, 
    # Opioid pain management
        # Dose, duration
           mediator_max_daily_dose_mme,
           mediator_opioid_days_covered,
    mediator_high_dose_longer_duration_mme,
        # High-risk opioid prescribing practices
           mediator_prescribers_6mo_sum,
        # Co-prescriptions
           mediator_opioid_benzo_copresc,
           mediator_opioid_gaba_copresc,
    mediator_opioid_copresc_benzo_gaba,
           mediator_opioid_mrelax_copresc,
    # Nonopioid pain management
        # Nonopioid pain prescriptions
    mediator_nonopioid_nonoverlap_benzo,
    mediator_nonopioid_nonoverlap_gaba,
    mediator_nonopioid_nonoverlap_mrelax,
    mediator_nonopioid_nonoverlap_mrelax_gaba_benzo,
    mediator_nonopioid_nonoverlap_antidepressant,
    mediator_nonopioid_nonoverlap_antiinflamatory,
    mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory,
    # Physical therapy
           mediator_has_physical_therapy) 

med_tbl_data <- med_tbl_data |>
    labelled::set_variable_labels(
        mediator_max_daily_dose_mme = "Max Daily Dose (MME)",
        mediator_opioid_days_covered = "Proportion of Days Supply",
        mediator_high_dose_longer_duration_mme = "High Dose, Long Duration MME",
        mediator_opioid_benzo_copresc = "Benzodiazepine Co-prescription",
        mediator_opioid_gaba_copresc = "Gabapentinoid Co-prescription",
        mediator_opioid_copresc_benzo_gaba = "Benzodiazepine or Gabapentinoid Co-prescription",
        mediator_opioid_mrelax_copresc = "Muscle Relaxant Co-prescription",
        mediator_nonopioid_nonoverlap_benzo = "Benzodiazepine Nonopioid, Nonoverlap Prescription",
        mediator_nonopioid_nonoverlap_gaba = "Gabapentinoid Nonopioid, Nonoverlap Prescription",
        mediator_nonopioid_nonoverlap_mrelax = "Muscle Relaxant Nonopioid, Nonoverlap Prescription",
        mediator_nonopioid_nonoverlap_mrelax_gaba_benzo = "Benzo/Gaba/Muscle,  Nonopioid, Nonoverlap Prescription",
        mediator_nonopioid_nonoverlap_antidepressant = "Antidepressant Nonopioid",
        mediator_nonopioid_nonoverlap_antiinflamatory = "Antiinflamatory Nonopioid",
        mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory = "Antidepressant/Antiinflamatory Nonopioid",
        mediator_has_physical_therapy = "Physical Therapy"
    )

oud_tbl_data <- analysis_cohort  |>
    select(disability_pain_cal,
           oud_18mo,
           oud_24mo,
           oud_18mo_icd,
           oud_24mo_icd) |>
    labelled::set_variable_labels(
        oud_18mo = "OUD by 18 months",
        oud_24mo = "OUD by 24 months",
        oud_18mo_icd = "OUD (ICD) by 18 months",
        oud_24mo_icd = "OUD (ICD) by 24 months"
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
    select(label:stat_3)


## Comorbidities
comorb_tbl <- comorb_tbl_data |>
    tbl_summary(by = disability_pain_cal) 

latex_comorb_tbl <- comorb_tbl |>
    as_gt() |>
    as_latex()

comorb_tbl_df <- comorb_tbl |>
    as_gt() |>
    as.data.frame() |>
    select(label:stat_3)


## Mediators
med_tbl <- med_tbl_data |>
    tbl_summary(by = disability_pain_cal) 

med_tbl_data |>
    filter(mediator_opioid_days_covered > 0) |>
    select(disability_pain_cal, mediator_max_daily_dose_mme, 
           mediator_opioid_days_covered) |>
    tbl_summary(by = disability_pain_cal) 

latex_med_tbl <- med_tbl |>
    as_gt() |>
    as_latex()

med_tbl_df <- med_tbl |>
    as_gt() |>
    as.data.frame() |>
    select(label:stat_3)


## OUD

oud_tbl <- oud_tbl_data |>
    tbl_summary(by = disability_pain_cal)

latex_oud_tbl <- oud_tbl |>
    as_gt() |>
    as_latex()

oud_tbl_df <- oud_tbl |>
    as_gt() |>
    as.data.frame() |>
    select(label:stat_3)


# Print out Latex code ---------------------------------------------------------

# Full cohort
cat(latex_dem_tbl)
cat(latex_comorb_tbl)
cat(latex_med_tbl)
cat(latex_oud_tbl)
 
# Stratified analysis ----------------------------------------------------------

# Read in dataset
analysis_cohort_depanx <- analysis_cohort |>
    filter(anxiety_washout_cal == 1 | depression_washout_cal == 1 |
               bipolar_washout_cal == 1) |>
    filter(disability_pain_cal != "disability and chronic pain") |>
    mutate(disability_pain_cal = factor(disability_pain_cal))
# analysis_cohort_depanx <- mediation_analysis_df_clean


# Create data for table 1 pieces -----------------------------------------------

# define the desired order of the exposure levels
level_order <- c("disability only", "chronic pain only", "neither")

# convert the character variable to a factor with the desired level order
analysis_cohort_depanx$disability_pain_cal <- fct_relevel(analysis_cohort_depanx$disability_pain_cal, level_order)

analysis_cohort_depanx <- analysis_cohort_depanx |>
    mutate(disability_pain_cal = recode(disability_pain_cal,
                                        "disability only" = "Physical Disability",
                                        "chronic pain only" = "Chronic Pain",
                                        "neither" = "Neither"))

dem_tbl_data <- analysis_cohort_depanx |>
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
           dem_ssi_benefits,
           has_2plus_ED_visit_washout,
           has_2plus_ED_visit_post_exposure) |>
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
        dem_ssi_benefits = "SSI Benefits",
        has_2plus_ED_visit_washout = "2+ ED Visits (Washout)",
        has_2plus_ED_visit_post_exposure = "2+ ED Visits (Post-Exposure)"
    ) 

comorb_tbl_data <- analysis_cohort_depanx |>
    select(disability_pain_cal, 
           bipolar_washout_cal,
           anxiety_washout_cal,
           adhd_washout_cal,
           depression_washout_cal,
           mental_ill_washout_cal,
           anxiety_post_exposure_cal,
           depression_post_exposure_cal,
           bipolar_post_exposure_cal,
           mediator_has_counseling
    ) |>
    labelled::set_variable_labels(
        bipolar_washout_cal = "Bipolar",
        anxiety_washout_cal = "Anxiety",
        adhd_washout_cal = "ADD/ADHD",
        depression_washout_cal = "Depression",
        mental_ill_washout_cal = "Other Mental Illness",
        anxiety_post_exposure_cal = "Anxiety (Post-Exposure)",
        depression_post_exposure_cal = "Depression (Post-Exposure)",
        bipolar_post_exposure_cal = "Bipolar (Post-Exposure)",
        mediator_has_counseling = "Mental Health Counseling")

med_tbl_data <- analysis_cohort_depanx |>
    select(disability_pain_cal, 
           # Opioid pain management
           # Dose, duration
           mediator_max_daily_dose_mme,
           mediator_opioid_days_covered,
           mediator_high_dose_longer_duration_mme,
           # High-risk opioid prescribing practices
           mediator_prescribers_6mo_sum,
           # Co-prescriptions
           mediator_opioid_benzo_copresc,
           mediator_opioid_gaba_copresc,
           mediator_opioid_copresc_benzo_gaba,
           mediator_opioid_mrelax_copresc,
           # Nonopioid pain management
           # Nonopioid pain prescriptions
           mediator_nonopioid_nonoverlap_benzo,
           mediator_nonopioid_nonoverlap_gaba,
           mediator_nonopioid_nonoverlap_mrelax,
           mediator_nonopioid_nonoverlap_mrelax_gaba_benzo,
           mediator_nonopioid_nonoverlap_antidepressant,
           mediator_nonopioid_nonoverlap_antiinflamatory,
           mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory,
           # Physical therapy
           mediator_has_physical_therapy) 

med_tbl_data <- med_tbl_data |>
    labelled::set_variable_labels(
        mediator_max_daily_dose_mme = "Max Daily Dose (MME)",
        mediator_opioid_days_covered = "Proportion of Days Supply",
        mediator_high_dose_longer_duration_mme = "High Dose, Long Duration MME",
        mediator_opioid_benzo_copresc = "Benzodiazepine Co-prescription",
        mediator_opioid_gaba_copresc = "Gabapentinoid Co-prescription",
        mediator_opioid_copresc_benzo_gaba = "Benzodiazepine or Gabapentinoid Co-prescription",
        mediator_opioid_mrelax_copresc = "Muscle Relaxant Co-prescription",
        mediator_nonopioid_nonoverlap_benzo = "Benzodiazepine Nonopioid, Nonoverlap Prescription",
        mediator_nonopioid_nonoverlap_gaba = "Gabapentinoid Nonopioid, Nonoverlap Prescription",
        mediator_nonopioid_nonoverlap_mrelax = "Muscle Relaxant Nonopioid, Nonoverlap Prescription",
        mediator_nonopioid_nonoverlap_mrelax_gaba_benzo = "Benzo/Gaba/Muscle,  Nonopioid, Nonoverlap Prescription",
        mediator_nonopioid_nonoverlap_antidepressant = "Antidepressant Nonopioid",
        mediator_nonopioid_nonoverlap_antiinflamatory = "Antiinflamatory Nonopioid",
        mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory = "Antidepressant/Antiinflamatory Nonopioid",
        mediator_has_physical_therapy = "Physical Therapy"
    )

oud_tbl_data <- analysis_cohort_depanx  |>
    select(disability_pain_cal,
           oud_18mo,
           oud_24mo,
           oud_18mo_icd,
           oud_24mo_icd) |>
    labelled::set_variable_labels(
        oud_18mo = "OUD by 18 months",
        oud_24mo = "OUD by 24 months",
        oud_18mo_icd = "OUD (ICD) by 18 months",
        oud_24mo_icd = "OUD (ICD) by 24 months"
    )

# Read in dataset
analysis_cohort_nodepanx <- analysis_cohort |>
    filter(anxiety_washout_cal == 0 & depression_washout_cal == 0 &
               bipolar_washout_cal == 0) |>
    filter(disability_pain_cal != "disability and chronic pain") |>
    mutate(disability_pain_cal = factor(disability_pain_cal))
# analysis_cohort_nodepanx <- mediation_analysis_df_clean


# Create data for table 1 pieces -----------------------------------------------

# define the desired order of the exposure levels
level_order <- c("disability only", "chronic pain only", "neither")

# convert the character variable to a factor with the desired level order
analysis_cohort_nodepanx$disability_pain_cal <- fct_relevel(analysis_cohort_nodepanx$disability_pain_cal, level_order)

analysis_cohort <- analysis_cohort |>
    mutate(disability_pain_cal = recode(disability_pain_cal,
                                        "disability only" = "Physical Disability",
                                        "chronic pain only" = "Chronic Pain",
                                        "neither" = "Neither"))

dem_tbl_data_nodepanx <- analysis_cohort_nodepanx |>
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
           dem_ssi_benefits,
           has_2plus_ED_visit_washout,
           has_2plus_ED_visit_post_exposure) |>
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
        dem_ssi_benefits = "SSI Benefits",
        has_2plus_ED_visit_washout = "2+ ED Visits (Washout)",
        has_2plus_ED_visit_post_exposure = "2+ ED Visits (Post-Exposure)"
    ) 

comorb_tbl_data_nodepanx <- analysis_cohort_nodepanx |>
    select(disability_pain_cal, 
           bipolar_washout_cal,
           anxiety_washout_cal,
           adhd_washout_cal,
           depression_washout_cal,
           mental_ill_washout_cal,
           anxiety_post_exposure_cal,
           depression_post_exposure_cal,
           bipolar_post_exposure_cal,
           mediator_has_counseling
    ) |>
    labelled::set_variable_labels(
        bipolar_washout_cal = "Bipolar",
        anxiety_washout_cal = "Anxiety",
        adhd_washout_cal = "ADD/ADHD",
        depression_washout_cal = "Depression",
        mental_ill_washout_cal = "Other Mental Illness",
        anxiety_post_exposure_cal = "Anxiety (Post-Exposure)",
        depression_post_exposure_cal = "Depression (Post-Exposure)",
        bipolar_post_exposure_cal = "Bipolar (Post-Exposure)",
        mediator_has_counseling = "Mental Health Counseling")

med_tbl_data_nodepanx <- analysis_cohort_nodepanx |>
    select(disability_pain_cal, 
           # Opioid pain management
           # Dose, duration
           mediator_max_daily_dose_mme,
           mediator_opioid_days_covered,
           mediator_high_dose_longer_duration_mme,
           # High-risk opioid prescribing practices
           mediator_prescribers_6mo_sum,
           # Co-prescriptions
           mediator_opioid_benzo_copresc,
           mediator_opioid_gaba_copresc,
           mediator_opioid_copresc_benzo_gaba,
           mediator_opioid_mrelax_copresc,
           # Nonopioid pain management
           # Nonopioid pain prescriptions
           mediator_nonopioid_nonoverlap_benzo,
           mediator_nonopioid_nonoverlap_gaba,
           mediator_nonopioid_nonoverlap_mrelax,
           mediator_nonopioid_nonoverlap_mrelax_gaba_benzo,
           mediator_nonopioid_nonoverlap_antidepressant,
           mediator_nonopioid_nonoverlap_antiinflamatory,
           mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory,
           # Physical therapy
           mediator_has_physical_therapy) 

med_tbl_data_nodepanx <- med_tbl_data_nodepanx |>
    labelled::set_variable_labels(
        mediator_max_daily_dose_mme = "Max Daily Dose (MME)",
        mediator_opioid_days_covered = "Proportion of Days Supply",
        mediator_high_dose_longer_duration_mme = "High Dose, Long Duration MME",
        mediator_opioid_benzo_copresc = "Benzodiazepine Co-prescription",
        mediator_opioid_gaba_copresc = "Gabapentinoid Co-prescription",
        mediator_opioid_copresc_benzo_gaba = "Benzodiazepine or Gabapentinoid Co-prescription",
        mediator_opioid_mrelax_copresc = "Muscle Relaxant Co-prescription",
        mediator_nonopioid_nonoverlap_benzo = "Benzodiazepine Nonopioid, Nonoverlap Prescription",
        mediator_nonopioid_nonoverlap_gaba = "Gabapentinoid Nonopioid, Nonoverlap Prescription",
        mediator_nonopioid_nonoverlap_mrelax = "Muscle Relaxant Nonopioid, Nonoverlap Prescription",
        mediator_nonopioid_nonoverlap_mrelax_gaba_benzo = "Benzo/Gaba/Muscle,  Nonopioid, Nonoverlap Prescription",
        mediator_nonopioid_nonoverlap_antidepressant = "Antidepressant Nonopioid",
        mediator_nonopioid_nonoverlap_antiinflamatory = "Antiinflamatory Nonopioid",
        mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory = "Antidepressant/Antiinflamatory Nonopioid",
        mediator_has_physical_therapy = "Physical Therapy"
    )

oud_tbl_data_nodepanx <- analysis_cohort_nodepanx  |>
    select(disability_pain_cal,
           oud_18mo,
           oud_24mo,
           oud_18mo_icd,
           oud_24mo_icd) |>
    labelled::set_variable_labels(
        oud_18mo = "OUD by 18 months",
        oud_24mo = "OUD by 24 months",
        oud_18mo_icd = "OUD (ICD) by 18 months",
        oud_24mo_icd = "OUD (ICD) by 24 months"
    )


# Create tables for the full cohort --------------------------------------------

## Demographics
dem_tbl <- dem_tbl_data |>
    tbl_summary(by = disability_pain_cal) |>
    bold_labels()

dem_tbl_nodepanx <- dem_tbl_data_nodepanx |>
    tbl_summary(by = disability_pain_cal) |>
    bold_labels()

tbls_dem <- list(dem_tbl, dem_tbl_nodepanx)

latex_dem_tbl <- tbl_merge(tbls_dem) |>
    as_gt() |>
    as_latex()


## Comorbidities
comorb_tbl <- comorb_tbl_data |>
    tbl_summary(by = disability_pain_cal) 

comorb_tbl_nodepanx <- comorb_tbl_data_nodepanx |>
    tbl_summary(by = disability_pain_cal) 

tbls_comorb <- list(comorb_tbl, comorb_tbl_nodepanx)

latex_comorb_tbl <- tbl_merge(tbls_comorb) |>
    as_gt() |>
    as_latex()


## Mediators
med_tbl <- med_tbl_data |>
    tbl_summary(by = disability_pain_cal) 

med_tbl_data |>
    filter(mediator_opioid_days_covered > 0) |>
    select(disability_pain_cal, mediator_max_daily_dose_mme, mediator_opioid_days_covered) |>
    tbl_summary(by = disability_pain_cal) 
    

med_tbl_nodepanx <- med_tbl_data_nodepanx |>
    tbl_summary(by = disability_pain_cal) 

med_tbl_data_nodepanx |>
    filter(mediator_opioid_days_covered > 0) |>
    select(disability_pain_cal, mediator_max_daily_dose_mme, mediator_opioid_days_covered) |>
    tbl_summary(by = disability_pain_cal) 

med_comorb <- list(med_tbl, med_tbl_nodepanx)

latex_med_tbl <- tbl_merge(med_comorb) |>
    as_gt() |>
    as_latex()


## OUD

oud_tbl <- oud_tbl_data |>
    tbl_summary(by = disability_pain_cal)

oud_tbl_nodepanx <- oud_tbl_data_nodepanx |>
    tbl_summary(by = disability_pain_cal)

oud_comorb <- list(oud_tbl, oud_tbl_nodepanx)

latex_oud_tbl <- tbl_merge(oud_comorb) |>
    as_gt() |>
    as_latex()

# Print out Latex code ---------------------------------------------------------

# Full cohort
cat(latex_dem_tbl)
cat(latex_comorb_tbl)
cat(latex_med_tbl)
cat(latex_oud_tbl)

