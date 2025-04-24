# READ ME -----------------------------------------------------------------
#
# Author: Sarah Forrest
# Created: 2023-10-04
#
# # Output: An output containing the estimated indirect effect for each 
#           exposure dummy, singling out one mediator at a time as M and
#           analyzing the remaining mediators as intermediate confounders (Z) 
#           using the HDmediation package, MLR3 Superlearner
#
# -------------------------------------------------------------------------

# Set up -----------------------------------------------------------------------

# Install packages
# devtools::install_github("nt-williams/mlr3superlearner")
# devtools::install_github("nt-williams/mlr3extralearners")
# devtools::install_github("nt-williams/HDmediation/HDmediation", ref = "mlr3superlearner")

# Load libraries
library(dplyr)
library(HDmediation)
library(mlr3superlearner)
library(mlr3extralearners)
library(glmnet)
library(earth)
library(lightgbm) # library(xgboost)
library(ranger)
library(nnet)
library(nnls) # Fixed "unused argument (metalearner = "glm")" error

drv_root <- "/home/data/disability/mediation_unsafe_pain_mgmt"

# Read in data subsets created in clean_analysis_data.R
subset_disability_pain_ref <- readRDS(file.path(drv_root, "subset_disability_pain_ref_df.rds"))
subset_disability_only_ref <- readRDS(file.path(drv_root, "subset_disability_only_ref_df.rds"))
subset_pain_only_ref <- readRDS(file.path(drv_root, "subset_pain_only_ref_df.rds"))

# Create samples of the the cohort subset dataframes
set.seed(1)
subset_disability_pain_ref <- subset_disability_pain_ref |>
    sample_frac(0.5, replace = FALSE)

set.seed(1)
subset_disability_only_ref <- subset_disability_only_ref |>
    sample_frac(0.5, replace = FALSE)

set.seed(1)
subset_pain_only_ref <- subset_pain_only_ref |>
    sample_frac(0.5, replace = FALSE)

# Algorithm libraries
learners <- c(
    "glm",
    "mean",
    "gbm",
    "glmnet", # Remove if running slow
    "earth"
)

# Baseline confounders
W <- c(
    "dem_age",
    # "dem_sex",
    "dem_sex_m",
    # "dem_race",
    "dem_race_aian",
    "dem_race_asian",
    "dem_race_black",
    "dem_race_hawaiian",
    "dem_race_hispanic",
    "dem_race_multiracial",
    "dem_primary_language_english", # NAs
    "dem_married_or_partnered", # NAs
    # "dem_household_size",
    "dem_household_size_2",
    "dem_household_size_2plus",
    "dem_veteran", # NAs
    "dem_probable_high_income",
    "dem_tanf_benefits", # NAs
    # "dem_ssi_benefits", # character
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
    "missing_dem_ssi_benefits"
) 

# Exposure dummies
A <- c("exposure_disability_pain", "exposure_disability_only", "exposure_pain_only")

# Outcome
Y <- "oud_cal"

# Censoring variable
cens <- "uncens_18mo" # c("uncens_18mo", "uncens_24mo")

# Number of folds for cross-validation
num_folds <- 1 # c(1, 3)


# mediator_max_daily_dose_mme --------------------------------------------------

mediator_df <- data.frame(
    exposure = character(0),
    mediator_exluded = character(0),
    indirect = numeric(0),
    direct = numeric(0),
    gcomp_indirect = numeric(0),
    gcomp_direct = numeric(0),
    var_indirect = numeric(0),
    var_direct = numeric(0),
    ci_indirect_low = numeric(0),
    ci_indirect_high = numeric(0),
    ci_direct_low = numeric(0),
    ci_direct_high = numeric(0))

mediator <- "mediator_max_daily_dose_mme"

M <- c("mediator_max_daily_dose_mme"
       # "mediator_has_tapering"
       # "mediator_months_opioid_rx"
       # "mediator_opioid_benzo_copresc"
       # "mediator_opioid_stimulant_copresc"
       # "mediator_opioid_gabapentinoid_copresc"
       # "mediator_has_physical_therapy"
       # "mediator_has_multimodal_pain_treatment_restrict"
       # "mediator_has_counseling"
       # "mediator_prescribers_6mo_sum"
       # "mediator_nonopioid_pain_rx"
)

Z <- c("anxiety_post_exposure_cal", # Post exposure confounder
       "depression_post_exposure_cal", # Post exposure confounder
       # "mediator_max_daily_dose_mme",
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

options(future.globals.maxSize = 1152194191)

# Iterate over the exposure groups
for (exposure in A) {
    
    # Conditionally select the appropriate data based on 'A'
    if (exposure == "exposure_disability_pain") {
        data_subset <- subset_disability_pain_ref
    } else if (exposure == "exposure_disability_only") {
        data_subset <- subset_disability_only_ref
    } else if (exposure == "exposure_pain_only") {
        data_subset <- subset_pain_only_ref
    }
    
    # Run the mediation function with the current exposure
    result <- mediation(
        data = data_subset,
        A = exposure,
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
        learners_cens = learners
    )
    
    # Store the results from the current exposure
    mediator_df <- rbind(mediator_df, data.frame(
        exposure = exposure,
        mediator = mediator,
        indirect = result$indirect,
        direct = result$direct,
        gcomp_indirect = result$gcomp_indirect,
        gcomp_direct = result$gcomp_direct,
        var_indirect = result$var_indirect,
        var_direct = result$var_direct,
        ci_indirect_low = result$ci_indirect_low,
        ci_indirect_high = result$ci_indirect_high,
        ci_direct_low = result$ci_direct_low,
        ci_direct_high = result$ci_direct_high
    ))
    
    mediator_df
}

mediator_df


# mediator_has_tapering --------------------------------------------------------

mediator_df <- data.frame(
    exposure = character(0),
    mediator_exluded = character(0),
    indirect = numeric(0),
    direct = numeric(0),
    gcomp_indirect = numeric(0),
    gcomp_direct = numeric(0),
    var_indirect = numeric(0),
    var_direct = numeric(0),
    ci_indirect_low = numeric(0),
    ci_indirect_high = numeric(0),
    ci_direct_low = numeric(0),
    ci_direct_high = numeric(0))

mediator <- "mediator_has_tapering"

M <- c(# "mediator_max_daily_dose_mme"
    "mediator_has_tapering"
    # "mediator_months_opioid_rx"
    # "mediator_opioid_benzo_copresc"
    # "mediator_opioid_stimulant_copresc"
    # "mediator_opioid_gabapentinoid_copresc"
    # "mediator_has_physical_therapy"
    # "mediator_has_multimodal_pain_treatment_restrict"
    # "mediator_has_counseling"
    # "mediator_prescribers_6mo_sum"
    # "mediator_nonopioid_pain_rx"
)

Z <- c("anxiety_post_exposure_cal", # Post exposure confounder
       "depression_post_exposure_cal", # Post exposure confounder
       "mediator_max_daily_dose_mme",
       # "mediator_has_tapering",
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

options(future.globals.maxSize = 1152194191)

# Iterate over the exposure groups
for (exposure in A) {
    
    # Conditionally select the appropriate data based on 'A'
    if (exposure == "exposure_disability_pain") {
        data_subset <- subset_disability_pain_ref
    } else if (exposure == "exposure_disability_only") {
        data_subset <- subset_disability_only_ref
    } else if (exposure == "exposure_pain_only") {
        data_subset <- subset_pain_only_ref
    }
    
    # Run the mediation function with the current exposure
    result <- mediation(
        data = data_subset,
        A = exposure,
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
        learners_cens = learners
    )
    
    # Store the results from the current exposure
    mediator_df <- rbind(mediator_df, data.frame(
        exposure = exposure,
        mediator = mediator,
        indirect = result$indirect,
        direct = result$direct,
        gcomp_indirect = result$gcomp_indirect,
        gcomp_direct = result$gcomp_direct,
        var_indirect = result$var_indirect,
        var_direct = result$var_direct,
        ci_indirect_low = result$ci_indirect_low,
        ci_indirect_high = result$ci_indirect_high,
        ci_direct_low = result$ci_direct_low,
        ci_direct_high = result$ci_direct_high
    ))
    
    mediator_df
}

mediator_df


# mediator_months_opioid_rx ----------------------------------------------------

mediator_df <- data.frame(
    exposure = character(0),
    mediator_exluded = character(0),
    indirect = numeric(0),
    direct = numeric(0),
    gcomp_indirect = numeric(0),
    gcomp_direct = numeric(0),
    var_indirect = numeric(0),
    var_direct = numeric(0),
    ci_indirect_low = numeric(0),
    ci_indirect_high = numeric(0),
    ci_direct_low = numeric(0),
    ci_direct_high = numeric(0))

mediator <- "mediator_months_opioid_rx"

M <- c(# "mediator_max_daily_dose_mme"
    # "mediator_has_tapering"
    "mediator_months_opioid_rx"
    # "mediator_opioid_benzo_copresc"
    # "mediator_opioid_stimulant_copresc"
    # "mediator_opioid_gabapentinoid_copresc"
    # "mediator_has_physical_therapy"
    # "mediator_has_multimodal_pain_treatment_restrict"
    # "mediator_has_counseling"
    # "mediator_prescribers_6mo_sum"
    # "mediator_nonopioid_pain_rx"
)
  
Z <- c("anxiety_post_exposure_cal", # Post exposure confounder
       "depression_post_exposure_cal", # Post exposure confounder
       "mediator_max_daily_dose_mme",
       "mediator_has_tapering",
       # "mediator_months_opioid_rx",
       "mediator_opioid_benzo_copresc",
       "mediator_opioid_stimulant_copresc",
       "mediator_opioid_gabapentinoid_copresc",
       "mediator_has_physical_therapy",
       "mediator_has_multimodal_pain_treatment_restrict",
       "mediator_has_counseling",
       "mediator_prescribers_6mo_sum",
       "mediator_nonopioid_pain_rx"
)

options(future.globals.maxSize = 1152194191)

# Iterate over the exposure groups
for (exposure in A) {
    
    # Conditionally select the appropriate data based on 'A'
    if (exposure == "exposure_disability_pain") {
        data_subset <- subset_disability_pain_ref
    } else if (exposure == "exposure_disability_only") {
        data_subset <- subset_disability_only_ref
    } else if (exposure == "exposure_pain_only") {
        data_subset <- subset_pain_only_ref
    }
    
    # Run the mediation function with the current exposure
    result <- mediation(
        data = data_subset,
        A = exposure,
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
        learners_cens = learners
    )
    
    # Store the results from the current exposure
    mediator_df <- rbind(mediator_df, data.frame(
        exposure = exposure,
        mediator = mediator,
        indirect = result$indirect,
        direct = result$direct,
        gcomp_indirect = result$gcomp_indirect,
        gcomp_direct = result$gcomp_direct,
        var_indirect = result$var_indirect,
        var_direct = result$var_direct,
        ci_indirect_low = result$ci_indirect_low,
        ci_indirect_high = result$ci_indirect_high,
        ci_direct_low = result$ci_direct_low,
        ci_direct_high = result$ci_direct_high
    ))
    
    mediator_df
}

mediator_df


# mediator_opioid_benzo_copresc ------------------------------------------------

mediator_df <- data.frame(
    exposure = character(0),
    mediator_exluded = character(0),
    indirect = numeric(0),
    direct = numeric(0),
    gcomp_indirect = numeric(0),
    gcomp_direct = numeric(0),
    var_indirect = numeric(0),
    var_direct = numeric(0),
    ci_indirect_low = numeric(0),
    ci_indirect_high = numeric(0),
    ci_direct_low = numeric(0),
    ci_direct_high = numeric(0))

mediator <- "mediator_opioid_benzo_copresc"

M <- c(# "mediator_max_daily_dose_mme"
    # "mediator_has_tapering"
    # "mediator_months_opioid_rx"
    "mediator_opioid_benzo_copresc"
    # "mediator_opioid_stimulant_copresc"
    # "mediator_opioid_gabapentinoid_copresc"
    # "mediator_has_physical_therapy"
    # "mediator_has_multimodal_pain_treatment_restrict"
    # "mediator_has_counseling"
    # "mediator_prescribers_6mo_sum"
    # "mediator_nonopioid_pain_rx"
)

Z <- c("anxiety_post_exposure_cal", # Post exposure confounder
       "depression_post_exposure_cal", # Post exposure confounder
       "mediator_max_daily_dose_mme",
       "mediator_has_tapering",
       "mediator_months_opioid_rx",
       # "mediator_opioid_benzo_copresc",
       "mediator_opioid_stimulant_copresc",
       "mediator_opioid_gabapentinoid_copresc",
       "mediator_has_physical_therapy",
       "mediator_has_multimodal_pain_treatment_restrict",
       "mediator_has_counseling",
       "mediator_prescribers_6mo_sum",
       "mediator_nonopioid_pain_rx"
)

options(future.globals.maxSize = 1152194191)

# Iterate over the exposure groups
for (exposure in A) {
    
    # Conditionally select the appropriate data based on 'A'
    if (exposure == "exposure_disability_pain") {
        data_subset <- subset_disability_pain_ref
    } else if (exposure == "exposure_disability_only") {
        data_subset <- subset_disability_only_ref
    } else if (exposure == "exposure_pain_only") {
        data_subset <- subset_pain_only_ref
    }
    
    # Run the mediation function with the current exposure
    result <- mediation(
        data = data_subset,
        A = exposure,
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
        learners_cens = learners
    )
    
    # Store the results from the current exposure
    mediator_df <- rbind(mediator_df, data.frame(
        exposure = exposure,
        mediator = mediator,
        indirect = result$indirect,
        direct = result$direct,
        gcomp_indirect = result$gcomp_indirect,
        gcomp_direct = result$gcomp_direct,
        var_indirect = result$var_indirect,
        var_direct = result$var_direct,
        ci_indirect_low = result$ci_indirect_low,
        ci_indirect_high = result$ci_indirect_high,
        ci_direct_low = result$ci_direct_low,
        ci_direct_high = result$ci_direct_high
    ))
    
    mediator_df
}

mediator_df


# mediator_opioid_stimulant_copresc --------------------------------------------

mediator_df <- data.frame(
    exposure = character(0),
    mediator_exluded = character(0),
    indirect = numeric(0),
    direct = numeric(0),
    gcomp_indirect = numeric(0),
    gcomp_direct = numeric(0),
    var_indirect = numeric(0),
    var_direct = numeric(0),
    ci_indirect_low = numeric(0),
    ci_indirect_high = numeric(0),
    ci_direct_low = numeric(0),
    ci_direct_high = numeric(0))

mediator <- "mediator_opioid_stimulant_copresc"

M <- c(# "mediator_max_daily_dose_mme"
    # "mediator_has_tapering"
    # "mediator_months_opioid_rx"
    # "mediator_opioid_benzo_copresc"
    "mediator_opioid_stimulant_copresc"
    # "mediator_opioid_gabapentinoid_copresc"
    # "mediator_has_physical_therapy"
    # "mediator_has_multimodal_pain_treatment_restrict"
    # "mediator_has_counseling"
    # "mediator_prescribers_6mo_sum"
    # "mediator_nonopioid_pain_rx"
)

Z <- c("anxiety_post_exposure_cal", # Post exposure confounder
       "depression_post_exposure_cal", # Post exposure confounder
       "mediator_max_daily_dose_mme",
       "mediator_has_tapering",
       "mediator_months_opioid_rx",
       "mediator_opioid_benzo_copresc",
       # "mediator_opioid_stimulant_copresc",
       "mediator_opioid_gabapentinoid_copresc",
       "mediator_has_physical_therapy",
       "mediator_has_multimodal_pain_treatment_restrict",
       "mediator_has_counseling",
       "mediator_prescribers_6mo_sum",
       "mediator_nonopioid_pain_rx"
)

options(future.globals.maxSize = 1152194191)

# Iterate over the exposure groups
for (exposure in A) {
    
    # Conditionally select the appropriate data based on 'A'
    if (exposure == "exposure_disability_pain") {
        data_subset <- subset_disability_pain_ref
    } else if (exposure == "exposure_disability_only") {
        data_subset <- subset_disability_only_ref
    } else if (exposure == "exposure_pain_only") {
        data_subset <- subset_pain_only_ref
    }
    
    # Run the mediation function with the current exposure
    result <- mediation(
        data = data_subset,
        A = exposure,
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
        learners_cens = learners
    )
    
    # Store the results from the current exposure
    mediator_df <- rbind(mediator_df, data.frame(
        exposure = exposure,
        mediator = mediator,
        indirect = result$indirect,
        direct = result$direct,
        gcomp_indirect = result$gcomp_indirect,
        gcomp_direct = result$gcomp_direct,
        var_indirect = result$var_indirect,
        var_direct = result$var_direct,
        ci_indirect_low = result$ci_indirect_low,
        ci_indirect_high = result$ci_indirect_high,
        ci_direct_low = result$ci_direct_low,
        ci_direct_high = result$ci_direct_high
    ))
    
    mediator_df
}

mediator_df


# mediator_opioid_gabapentinoid_copresc ----------------------------------------

mediator_df <- data.frame(
    exposure = character(0),
    mediator_exluded = character(0),
    indirect = numeric(0),
    direct = numeric(0),
    gcomp_indirect = numeric(0),
    gcomp_direct = numeric(0),
    var_indirect = numeric(0),
    var_direct = numeric(0),
    ci_indirect_low = numeric(0),
    ci_indirect_high = numeric(0),
    ci_direct_low = numeric(0),
    ci_direct_high = numeric(0))

mediator <- "mediator_opioid_gabapentinoid_copresc"

M <- c(# "mediator_max_daily_dose_mme"
    # "mediator_has_tapering"
    # "mediator_months_opioid_rx"
    # "mediator_opioid_benzo_copresc"
    # "mediator_opioid_stimulant_copresc"
    "mediator_opioid_gabapentinoid_copresc"
    # "mediator_has_physical_therapy"
    # "mediator_has_multimodal_pain_treatment_restrict"
    # "mediator_has_counseling"
    # "mediator_prescribers_6mo_sum"
    # "mediator_nonopioid_pain_rx"
)

Z <- c("anxiety_post_exposure_cal", # Post exposure confounder
       "depression_post_exposure_cal", # Post exposure confounder
       "mediator_max_daily_dose_mme",
       "mediator_has_tapering",
       "mediator_months_opioid_rx",
       "mediator_opioid_benzo_copresc",
       "mediator_opioid_stimulant_copresc",
       # "mediator_opioid_gabapentinoid_copresc",
       "mediator_has_physical_therapy",
       "mediator_has_multimodal_pain_treatment_restrict",
       "mediator_has_counseling",
       "mediator_prescribers_6mo_sum",
       "mediator_nonopioid_pain_rx"
)

options(future.globals.maxSize = 1152194191)

# Iterate over the exposure groups
for (exposure in A) {
    
    # Conditionally select the appropriate data based on 'A'
    if (exposure == "exposure_disability_pain") {
        data_subset <- subset_disability_pain_ref
    } else if (exposure == "exposure_disability_only") {
        data_subset <- subset_disability_only_ref
    } else if (exposure == "exposure_pain_only") {
        data_subset <- subset_pain_only_ref
    }
    
    # Run the mediation function with the current exposure
    result <- mediation(
        data = data_subset,
        A = exposure,
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
        learners_cens = learners
    )
    
    # Store the results from the current exposure
    mediator_df <- rbind(mediator_df, data.frame(
        exposure = exposure,
        mediator = mediator,
        indirect = result$indirect,
        direct = result$direct,
        gcomp_indirect = result$gcomp_indirect,
        gcomp_direct = result$gcomp_direct,
        var_indirect = result$var_indirect,
        var_direct = result$var_direct,
        ci_indirect_low = result$ci_indirect_low,
        ci_indirect_high = result$ci_indirect_high,
        ci_direct_low = result$ci_direct_low,
        ci_direct_high = result$ci_direct_high
    ))
    
    mediator_df
}

mediator_df


# mediator_has_physical_therapy ----------------------------------------

mediator_df <- data.frame(
    exposure = character(0),
    mediator_exluded = character(0),
    indirect = numeric(0),
    direct = numeric(0),
    gcomp_indirect = numeric(0),
    gcomp_direct = numeric(0),
    var_indirect = numeric(0),
    var_direct = numeric(0),
    ci_indirect_low = numeric(0),
    ci_indirect_high = numeric(0),
    ci_direct_low = numeric(0),
    ci_direct_high = numeric(0))

mediator <- "mediator_has_physical_therapy"

M <- c(# "mediator_max_daily_dose_mme"
    # "mediator_has_tapering"
    # "mediator_months_opioid_rx"
    # "mediator_opioid_benzo_copresc"
    # "mediator_opioid_stimulant_copresc"
    # "mediator_opioid_gabapentinoid_copresc"
    "mediator_has_physical_therapy"
    # "mediator_has_multimodal_pain_treatment_restrict"
    # "mediator_has_counseling"
    # "mediator_prescribers_6mo_sum"
    # "mediator_nonopioid_pain_rx"
)

Z <- c("anxiety_post_exposure_cal", # Post exposure confounder
       "depression_post_exposure_cal", # Post exposure confounder
       "mediator_max_daily_dose_mme",
       "mediator_has_tapering",
       "mediator_months_opioid_rx",
       "mediator_opioid_benzo_copresc",
       "mediator_opioid_stimulant_copresc",
       "mediator_opioid_gabapentinoid_copresc",
       # "mediator_has_physical_therapy",
       "mediator_has_multimodal_pain_treatment_restrict",
       "mediator_has_counseling",
       "mediator_prescribers_6mo_sum",
       "mediator_nonopioid_pain_rx"
)

options(future.globals.maxSize = 1152194191)

# Iterate over the exposure groups
for (exposure in A) {
    
    # Conditionally select the appropriate data based on 'A'
    if (exposure == "exposure_disability_pain") {
        data_subset <- subset_disability_pain_ref
    } else if (exposure == "exposure_disability_only") {
        data_subset <- subset_disability_only_ref
    } else if (exposure == "exposure_pain_only") {
        data_subset <- subset_pain_only_ref
    }
    
    # Run the mediation function with the current exposure
    result <- mediation(
        data = data_subset,
        A = exposure,
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
        learners_cens = learners
    )
    
    # Store the results from the current exposure
    mediator_df <- rbind(mediator_df, data.frame(
        exposure = exposure,
        mediator = mediator,
        indirect = result$indirect,
        direct = result$direct,
        gcomp_indirect = result$gcomp_indirect,
        gcomp_direct = result$gcomp_direct,
        var_indirect = result$var_indirect,
        var_direct = result$var_direct,
        ci_indirect_low = result$ci_indirect_low,
        ci_indirect_high = result$ci_indirect_high,
        ci_direct_low = result$ci_direct_low,
        ci_direct_high = result$ci_direct_high
    ))
    
    mediator_df
}

mediator_df


# mediator_has_multimodal_pain_treatment_restrict ------------------------------

mediator_df <- data.frame(
    exposure = character(0),
    mediator_exluded = character(0),
    indirect = numeric(0),
    direct = numeric(0),
    gcomp_indirect = numeric(0),
    gcomp_direct = numeric(0),
    var_indirect = numeric(0),
    var_direct = numeric(0),
    ci_indirect_low = numeric(0),
    ci_indirect_high = numeric(0),
    ci_direct_low = numeric(0),
    ci_direct_high = numeric(0))

mediator <- "mediator_has_multimodal_pain_treatment_restrict"

M <- c(# "mediator_max_daily_dose_mme"
    # "mediator_has_tapering"
    # "mediator_months_opioid_rx"
    # "mediator_opioid_benzo_copresc"
    # "mediator_opioid_stimulant_copresc"
    # "mediator_opioid_gabapentinoid_copresc"
    # "mediator_has_physical_therapy"
    "mediator_has_multimodal_pain_treatment_restrict"
    # "mediator_has_counseling"
    # "mediator_prescribers_6mo_sum"
    # "mediator_nonopioid_pain_rx"
)

Z <- c("anxiety_post_exposure_cal", # Post exposure confounder
       "depression_post_exposure_cal", # Post exposure confounder
       "mediator_max_daily_dose_mme",
       "mediator_has_tapering",
       "mediator_months_opioid_rx",
       "mediator_opioid_benzo_copresc",
       "mediator_opioid_stimulant_copresc",
       "mediator_opioid_gabapentinoid_copresc",
       "mediator_has_physical_therapy",
       # "mediator_has_multimodal_pain_treatment_restrict",
       "mediator_has_counseling",
       "mediator_prescribers_6mo_sum",
       "mediator_nonopioid_pain_rx"
)

options(future.globals.maxSize = 1152194191)

# Iterate over the exposure groups
for (exposure in A) {
    
    # Conditionally select the appropriate data based on 'A'
    if (exposure == "exposure_disability_pain") {
        data_subset <- subset_disability_pain_ref
    } else if (exposure == "exposure_disability_only") {
        data_subset <- subset_disability_only_ref
    } else if (exposure == "exposure_pain_only") {
        data_subset <- subset_pain_only_ref
    }
    
    # Run the mediation function with the current exposure
    result <- mediation(
        data = data_subset,
        A = exposure,
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
        learners_cens = learners
    )
    
    # Store the results from the current exposure
    mediator_df <- rbind(mediator_df, data.frame(
        exposure = exposure,
        mediator = mediator,
        indirect = result$indirect,
        direct = result$direct,
        gcomp_indirect = result$gcomp_indirect,
        gcomp_direct = result$gcomp_direct,
        var_indirect = result$var_indirect,
        var_direct = result$var_direct,
        ci_indirect_low = result$ci_indirect_low,
        ci_indirect_high = result$ci_indirect_high,
        ci_direct_low = result$ci_direct_low,
        ci_direct_high = result$ci_direct_high
    ))
    
    mediator_df
}

mediator_df


# mediator_has_counseling ------------------------------------------------------

mediator_df <- data.frame(
    exposure = character(0),
    mediator_exluded = character(0),
    indirect = numeric(0),
    direct = numeric(0),
    gcomp_indirect = numeric(0),
    gcomp_direct = numeric(0),
    var_indirect = numeric(0),
    var_direct = numeric(0),
    ci_indirect_low = numeric(0),
    ci_indirect_high = numeric(0),
    ci_direct_low = numeric(0),
    ci_direct_high = numeric(0))

mediator <- "mediator_has_counseling"

M <- c(# "mediator_max_daily_dose_mme"
    # "mediator_has_tapering"
    # "mediator_months_opioid_rx"
    # "mediator_opioid_benzo_copresc"
    # "mediator_opioid_stimulant_copresc"
    # "mediator_opioid_gabapentinoid_copresc"
    # "mediator_has_physical_therapy"
    # "mediator_has_multimodal_pain_treatment_restrict"
    "mediator_has_counseling"
    # "mediator_prescribers_6mo_sum"
    # "mediator_nonopioid_pain_rx"
)

Z <- c("anxiety_post_exposure_cal", # Post exposure confounder
       "depression_post_exposure_cal", # Post exposure confounder
       "mediator_max_daily_dose_mme",
       "mediator_has_tapering",
       "mediator_months_opioid_rx",
       "mediator_opioid_benzo_copresc",
       "mediator_opioid_stimulant_copresc",
       "mediator_opioid_gabapentinoid_copresc",
       "mediator_has_physical_therapy",
       "mediator_has_multimodal_pain_treatment_restrict",
       # "mediator_has_counseling",
       "mediator_prescribers_6mo_sum",
       "mediator_nonopioid_pain_rx"
)

options(future.globals.maxSize = 1152194191)

# Iterate over the exposure groups
for (exposure in A) {
    
    # Conditionally select the appropriate data based on 'A'
    if (exposure == "exposure_disability_pain") {
        data_subset <- subset_disability_pain_ref
    } else if (exposure == "exposure_disability_only") {
        data_subset <- subset_disability_only_ref
    } else if (exposure == "exposure_pain_only") {
        data_subset <- subset_pain_only_ref
    }
    
    # Run the mediation function with the current exposure
    result <- mediation(
        data = data_subset,
        A = exposure,
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
        learners_cens = learners
    )
    
    # Store the results from the current exposure
    mediator_df <- rbind(mediator_df, data.frame(
        exposure = exposure,
        mediator = mediator,
        indirect = result$indirect,
        direct = result$direct,
        gcomp_indirect = result$gcomp_indirect,
        gcomp_direct = result$gcomp_direct,
        var_indirect = result$var_indirect,
        var_direct = result$var_direct,
        ci_indirect_low = result$ci_indirect_low,
        ci_indirect_high = result$ci_indirect_high,
        ci_direct_low = result$ci_direct_low,
        ci_direct_high = result$ci_direct_high
    ))
    
    mediator_df
}

mediator_df


# mediator_prescribers_6mo_sum -------------------------------------------------

mediator_df <- data.frame(
    exposure = character(0),
    mediator_exluded = character(0),
    indirect = numeric(0),
    direct = numeric(0),
    gcomp_indirect = numeric(0),
    gcomp_direct = numeric(0),
    var_indirect = numeric(0),
    var_direct = numeric(0),
    ci_indirect_low = numeric(0),
    ci_indirect_high = numeric(0),
    ci_direct_low = numeric(0),
    ci_direct_high = numeric(0))

mediator <- "mediator_prescribers_6mo_sum"

M <- c(# "mediator_max_daily_dose_mme"
    # "mediator_has_tapering"
    # "mediator_months_opioid_rx"
    # "mediator_opioid_benzo_copresc"
    # "mediator_opioid_stimulant_copresc"
    # "mediator_opioid_gabapentinoid_copresc"
    # "mediator_has_physical_therapy"
    # "mediator_has_multimodal_pain_treatment_restrict"
    # "mediator_has_counseling"
    "mediator_prescribers_6mo_sum"
    # "mediator_nonopioid_pain_rx"
)

Z <- c("anxiety_post_exposure_cal", # Post exposure confounder
       "depression_post_exposure_cal", # Post exposure confounder
       "mediator_max_daily_dose_mme",
       "mediator_has_tapering",
       "mediator_months_opioid_rx",
       "mediator_opioid_benzo_copresc",
       "mediator_opioid_stimulant_copresc",
       "mediator_opioid_gabapentinoid_copresc",
       "mediator_has_physical_therapy",
       "mediator_has_multimodal_pain_treatment_restrict",
       "mediator_has_counseling",
       # "mediator_prescribers_6mo_sum",
       "mediator_nonopioid_pain_rx"
)

options(future.globals.maxSize = 1152194191)

# Iterate over the exposure groups
for (exposure in A) {
    
    # Conditionally select the appropriate data based on 'A'
    if (exposure == "exposure_disability_pain") {
        data_subset <- subset_disability_pain_ref
    } else if (exposure == "exposure_disability_only") {
        data_subset <- subset_disability_only_ref
    } else if (exposure == "exposure_pain_only") {
        data_subset <- subset_pain_only_ref
    }
    
    # Run the mediation function with the current exposure
    result <- mediation(
        data = data_subset,
        A = exposure,
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
        learners_cens = learners
    )
    
    # Store the results from the current exposure
    mediator_df <- rbind(mediator_df, data.frame(
        exposure = exposure,
        mediator = mediator,
        indirect = result$indirect,
        direct = result$direct,
        gcomp_indirect = result$gcomp_indirect,
        gcomp_direct = result$gcomp_direct,
        var_indirect = result$var_indirect,
        var_direct = result$var_direct,
        ci_indirect_low = result$ci_indirect_low,
        ci_indirect_high = result$ci_indirect_high,
        ci_direct_low = result$ci_direct_low,
        ci_direct_high = result$ci_direct_high
    ))
    
    mediator_df
}

mediator_df


# mediator_nonopioid_pain_rx -------------------------------------------------

mediator_df <- data.frame(
    exposure = character(0),
    mediator_exluded = character(0),
    indirect = numeric(0),
    direct = numeric(0),
    gcomp_indirect = numeric(0),
    gcomp_direct = numeric(0),
    var_indirect = numeric(0),
    var_direct = numeric(0),
    ci_indirect_low = numeric(0),
    ci_indirect_high = numeric(0),
    ci_direct_low = numeric(0),
    ci_direct_high = numeric(0))

mediator <- "mediator_nonopioid_pain_rx"

M <- c(# "mediator_max_daily_dose_mme"
    # "mediator_has_tapering"
    # "mediator_months_opioid_rx"
    # "mediator_opioid_benzo_copresc"
    # "mediator_opioid_stimulant_copresc"
    # "mediator_opioid_gabapentinoid_copresc"
    # "mediator_has_physical_therapy"
    # "mediator_has_multimodal_pain_treatment_restrict"
    # "mediator_has_counseling"
    # "mediator_prescribers_6mo_sum"
    "mediator_nonopioid_pain_rx"
)

Z <- c("anxiety_post_exposure_cal", # Post exposure confounder
       "depression_post_exposure_cal", # Post exposure confounder
       "mediator_max_daily_dose_mme",
       "mediator_has_tapering",
       "mediator_months_opioid_rx",
       "mediator_opioid_benzo_copresc",
       "mediator_opioid_stimulant_copresc",
       "mediator_opioid_gabapentinoid_copresc",
       "mediator_has_physical_therapy",
       "mediator_has_multimodal_pain_treatment_restrict",
       "mediator_has_counseling",
       "mediator_prescribers_6mo_sum"
       # "mediator_nonopioid_pain_rx"
)

options(future.globals.maxSize = 1152194191)

# Iterate over the exposure groups
for (exposure in A) {
    
    # Conditionally select the appropriate data based on 'A'
    if (exposure == "exposure_disability_pain") {
        data_subset <- subset_disability_pain_ref
    } else if (exposure == "exposure_disability_only") {
        data_subset <- subset_disability_only_ref
    } else if (exposure == "exposure_pain_only") {
        data_subset <- subset_pain_only_ref
    }
    
    # Run the mediation function with the current exposure
    result <- mediation(
        data = data_subset,
        A = exposure,
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
        learners_cens = learners
    )
    
    # Store the results from the current exposure
    mediator_df <- rbind(mediator_df, data.frame(
        exposure = exposure,
        mediator = mediator,
        indirect = result$indirect,
        direct = result$direct,
        gcomp_indirect = result$gcomp_indirect,
        gcomp_direct = result$gcomp_direct,
        var_indirect = result$var_indirect,
        var_direct = result$var_direct,
        ci_indirect_low = result$ci_indirect_low,
        ci_indirect_high = result$ci_indirect_high,
        ci_direct_low = result$ci_direct_low,
        ci_direct_high = result$ci_direct_high
    ))
    
    mediator_df
}

mediator_df
