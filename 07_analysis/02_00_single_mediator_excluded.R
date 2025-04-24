# -------------------------------------
# Script: 02_00_single_mediator_excluded.R
# Author: Nick Williams
# Purpose: Estimate indirect effects for each exposure, 
#   analyzing all mediators (M) but excluding one at a
#   time as an intermediate confounder (Z)
# Notes: Run using `callr` package with 02_01_all_mediators.R
# -------------------------------------

library(HDmediation)
library(mlr3superlearner)
library(mlr3extralearners)
library(glue)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

ca <- commandArgs(TRUE)
a <- ca[[1]]
z <- ca[[2]]

if (a == "exposure_disability_pain") {
    dat <- readRDS(file.path(drv_root, "subset_disability_pain_ref_df.rds"))
}

if (a == "exposure_disability_only") {
    dat <- readRDS(file.path(drv_root, "subset_disability_only_ref_df.rds"))
}

if (a == "exposure_pain_only") {
    dat <- readRDS(file.path(drv_root, "subset_pain_only_ref_df.rds"))
}

dat <- as.data.frame(dat)

# Baseline confounders
W <- c("dem_age",
       "dem_sex_m",
       "dem_race_aian",
       "dem_race_asian",
       "dem_race_black",
       "dem_race_hawaiian",
       "dem_race_hispanic",
       "dem_race_multiracial",
       "dem_primary_language_english",
       "dem_married_or_partnered", 
       "dem_household_size_2",
       "dem_household_size_2plus",
       "dem_veteran", 
       "dem_probable_high_income",
       "dem_tanf_benefits", 
       "dem_ssi_benefits_mandatory_optional",
       "bipolar_washout_cal",
       "anxiety_washout_cal",
       "adhd_washout_cal",
       "depression_washout_cal",
       "mental_ill_washout_cal",
       # NA/missing indicators
       "missing_dem_race",
       "missing_dem_primary_language_english",
       "missing_dem_married_or_partnered",
       "missing_dem_household_size",
       "missing_dem_veteran",
       "missing_dem_tanf_benefits",
       "missing_dem_ssi_benefits") 

# Mediator list
M <- c("mediator_max_daily_dose_mme",
       "mediator_has_tapering",
       "mediator_months_opioid_rx", 
       "mediator_opioid_benzo_copresc", 
       "mediator_opioid_stimulant_copresc", 
       "mediator_opioid_gabapentinoid_copresc", 
       "mediator_has_physical_therapy", 
       "mediator_has_multimodal_pain_treatment_restrict", 
       "mediator_has_counseling", 
       "mediator_prescribers_6mo_sum", 
       "mediator_nonopioid_pain_rx" 
) 

M <- setdiff(M, z)

# Post-exposure confounders
Z <- c("anxiety_post_exposure_cal",
       "depression_post_exposure_cal", 
       z)

# Outcome
Y <- "oud_cal"

# Censoring variable
# ["uncens_18mo", "uncens_24mo"]
cens <- "uncens_18mo"

# Number of folds for cross-validation
num_folds <- 1

# Run HDmediation
result <- mediation(data = dat,
                    A = a,
                    W = W,
                    Z = Z,
                    M = M,
                    Y = Y,
                    cens = cens,
                    S = NULL,
                    family = "binomial",
                    folds = num_folds,
                    partial_tmle = TRUE,
                    learners_g = learners,
                    learners_e = learners,
                    learners_c = learners,
                    learners_b = learners,
                    learners_hz = learners,
                    learners_u = learners,
                    learners_ubar = learners,
                    learners_v = learners,
                    learners_vbar = learners,
                    learners_cens = learners)

saveRDS(result, file.path(drv_root, glue("result_{a}_exclude_{z}.rds")))
