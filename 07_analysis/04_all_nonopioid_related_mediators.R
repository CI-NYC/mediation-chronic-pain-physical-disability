# READ ME -----------------------------------------------------------------
#
# Author: Sarah Forrest
# Created: 2023-10-04
#
# Output: An output containing the estimated indirect effect for each 
#         exposure dummy, analyzing all beneficial, non-opioid-related mediators 
#         together using the HDmediation package, MLR3 Superlearner
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
    "glmnet",
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
    "missing_dem_ssi_benefits") 

# Beneficial mediators
M <- c("mediator_has_physical_therapy",
       "mediator_has_multimodal_pain_treatment_restrict",
       "mediator_has_counseling",
       "mediator_nonopioid_pain_rx")

# Harmful mediators as intermediate confounders
Z <- c("mediator_max_daily_dose_mme", 
       "mediator_has_tapering",
       "mediator_months_opioid_rx",
       "mediator_opioid_benzo_copresc",
       "mediator_opioid_stimulant_copresc",
       "mediator_opioid_gabapentinoid_copresc",
       "mediator_prescribers_6mo_sum",
       "anxiety_post_exposure_cal", # Post exposure confounder
       "depression_post_exposure_cal" # Post exposure confounder
) 

# Outcome
Y <- "oud_cal"

# Censoring variable
cens <- "uncens_18mo" # c("uncens_18mo", "uncens_24mo")

# Number of folds for cross-validation
num_folds <- 1 # c(1, 3)


# Multimodal pain treatment summary --------------------------------------------

### Disability and chronic pain ------------------------------------------------

options(future.globals.maxSize = 1152194191)

# Run the mediation function for exposure_disability_pain
result_disability_pain <- mediation(
    data = subset_disability_pain_ref,
    A = "exposure_disability_pain",
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

result_disability_pain


### Disability only ------------------------------------------------------------

options(future.globals.maxSize = 1152194191)

# Run the mediation function for exposure_disability_only
result_disability_only <- mediation(
    data = subset_disability_only_ref,
    A = "exposure_disability_only",
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

result_disability_only


### Chronic pain only ----------------------------------------------------------

options(future.globals.maxSize = 1152194191)

# Run the mediation function for exposure_pain_only
result_pain_only <- mediation(
    data = subset_pain_only_ref,
    A = "exposure_pain_only",
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

result_pain_only
