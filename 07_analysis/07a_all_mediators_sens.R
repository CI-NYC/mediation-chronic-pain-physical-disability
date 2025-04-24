# -------------------------------------
# Script: Run HDmediation with ALL mediators
# Author:
# Purpose:
# Notes: devtools::install_github("shodaiinose/HDmediation2/HDmediation", ref = "zimple2") #version of package that returns EIF
# -------------------------------------

library(tidyverse)
library(HDmediation4)
library(mlr3superlearner)
library(mlr3extralearners)
library(tictoc)
library(ranger)
library(earth)
library(lightgbm) # library(xgboost)
library(nnet)
library(doFuture)
library(nnls) # Fixed "unused argument (metalearner = "glm")" error

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Algorithm libraries
learners <- list("glm", 
                 "earth",
                 # "ranger",
                 list("lightgbm", 
                      max_depth = 10, 
                      num_leaves = 20, 
                      min_gain_to_split = 0.1, 
                      min_data_in_leaf = 100, 
                      learning_rate = 0.25, 
                      id = "lightgbm"))

learners_propensity <- list("glm", 
                            "earth",
                            # "ranger",
                            list("lightgbm", 
                                 max_depth = 10, 
                                 num_leaves = 20, 
                                 min_gain_to_split = 0.1, 
                                 is_unbalance = TRUE,
                                 objective = "binary",
                                 min_data_in_leaf = 100, 
                                 learning_rate = 0.25, 
                                 id = "lightgbm"))

set.seed(1)
disability <- readRDS(paste0(drv_root, "/subset_12mo_disability_only_ref_df_updated.rds")) 


disability_subset <- disability |> # filtering to those without dep/anx/bipolar
    filter(is.na(exposure_disability_only_subset) == FALSE)

disability_only_no_depanx_subset <- disability_subset |> # filtering to those without dep/anx/bipolar
    filter(depression_washout_cal == 0 &
               anxiety_washout_cal == 0 &
               bipolar_washout_cal == 0)

disability_depanx_subset <- disability_subset |> # filtering to those with dep/anx/bipolar
    filter(depression_washout_cal == 1 |
               anxiety_washout_cal == 1 |
               bipolar_washout_cal == 1)

pain <- readRDS(paste0(drv_root, "/subset_12mo_pain_only_ref_df_updated.rds"))

pain_only_no_depanx <- pain |> # filtering to those without dep/anx/bipolar
    filter(depression_washout_cal == 0 &
               anxiety_washout_cal == 0 &
               bipolar_washout_cal == 0)

pain_depanx <- pain |> # filtering to those with dep/anx/bipolar
    filter(depression_washout_cal == 1 |
               anxiety_washout_cal == 1 |
               bipolar_washout_cal == 1)

rm(disability)

# Exposure dummies
A <- c("exposure_disability_only_subset", 
       "exposure_pain_only"
)


# Censoring variable
cens <- "uncens_24mo" # c("uncens_18mo", "uncens_24mo")

# Number of folds for cross-validation
num_folds <- 2

## OUD ICD definition

Y <- "oud_24mo_icd"

# Has at least one of depression, anxiety, bipolar --------------------------------------------------
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
    #"bipolar_washout_cal",
    #"anxiety_washout_cal",
    "adhd_washout_cal",
    #"depression_washout_cal",
    #"mental_ill_washout_cal",
    # NA/missing indicators
    "missing_dem_race",
    "missing_dem_primary_language_english",
    "missing_dem_married_or_partnered",
    "missing_dem_household_size",
    "missing_dem_veteran",
    "missing_dem_tanf_benefits",
    "missing_dem_ssi_benefits",
    # pain severity
    "has_2plus_ED_visit_washout"
)

mediator_include_df <- data.frame(
    exposure = character(0),
    mediator_included = character(0),
    total = numeric(0),
    indirect = numeric(0),
    direct = numeric(0),
    gcomp_total = numeric(0),
    gcomp_indirect = numeric(0),
    gcomp_direct = numeric(0),
    var_total = numeric(0),
    var_indirect = numeric(0),
    var_direct = numeric(0),
    ci_total_low = numeric(0),
    ci_total_high = numeric(0),
    ci_indirect_low = numeric(0),
    ci_indirect_high = numeric(0),
    ci_direct_low = numeric(0),
    ci_direct_high = numeric(0))

mediator_included <- "all_mediators_depanx_icd_12mo"

# Intermediate confounder list
Z <- NULL

# Mediator list
M <- c("mediator_max_daily_dose_mme", #MME
       "mediator_opioid_days_covered", #proportion of days
       "mediator_opioid_benzo_copresc", #opioid copresc
       "mediator_opioid_gaba_copresc", #opioid copresc
       "mediator_opioid_mrelax_copresc", #opioid copresc
       "mediator_12mo_nonopioid_nonoverlap_mrelax", #nonopioid
       "mediator_12mo_nonopioid_nonoverlap_gaba", #nonopioid
       "mediator_12mo_nonopioid_nonoverlap_benzo", #nonopioid
       "mediator_12mo_nonopioid_nonoverlap_antidepressant", #nonopioid
       "mediator_12mo_nonopioid_nonoverlap_antiinflamatory", #nonopioid
       "mediator_has_physical_therapy" #PT
)

options(future.globals.maxSize = 1152194191)
tic()
set.seed(9)
# Iterate over the exposure groups
for (exposure in A) {

    # Conditionally select the appropriate data based on 'A'
    if (exposure == "exposure_disability_only") {
        data_subset <- disability_depanx
    } else if (exposure == "exposure_disability_only_subset") {
        data_subset <- disability_depanx_subset
    } else if (exposure == "exposure_pain_only") {
        data_subset <- pain_depanx
    }

    data_subset <- data_subset |>
        as.data.frame()

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
        learners_g = learners_propensity,
        learners_e = learners_propensity,
        learners_c = learners,
        learners_b = learners,
        learners_hz = learners,
        learners_u = learners,
        learners_ubar = learners,
        learners_v = learners,
        learners_vbar = learners,
        learners_cens = learners
    )
    saveRDS(result, paste0("/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_results_final_r1/washout_12mo/all_mediators_12mo/", mediator_included, "_results_", exposure, ".rds"))
}

mediator_include_df <- data.frame(
    exposure = character(0),
    mediator_included = character(0),
    total = numeric(0),
    indirect = numeric(0),
    direct = numeric(0),
    gcomp_total = numeric(0),
    gcomp_indirect = numeric(0),
    gcomp_direct = numeric(0),
    var_total = numeric(0),
    var_indirect = numeric(0),
    var_direct = numeric(0),
    ci_total_low = numeric(0),
    ci_total_high = numeric(0),
    ci_indirect_low = numeric(0),
    ci_indirect_high = numeric(0),
    ci_direct_low = numeric(0),
    ci_direct_high = numeric(0))

mediator_included <- "all_mediators_nodepanx_icd_12mo"

# Intermediate confounder list
Z <- NULL

# Mediator list
M <- c("mediator_max_daily_dose_mme", #MME
       "mediator_opioid_days_covered", #proportion of days
       "mediator_opioid_benzo_copresc", #opioid copresc
       "mediator_opioid_gaba_copresc", #opioid copresc
       "mediator_opioid_mrelax_copresc", #opioid copresc
       "mediator_12mo_nonopioid_nonoverlap_mrelax", #nonopioid 
       "mediator_12mo_nonopioid_nonoverlap_gaba", #nonopioid
       "mediator_12mo_nonopioid_nonoverlap_benzo", #nonopioid 
       "mediator_12mo_nonopioid_nonoverlap_antidepressant", #nonopioid 
       "mediator_12mo_nonopioid_nonoverlap_antiinflamatory", #nonopioid
       "mediator_has_physical_therapy" #PT
)

options(future.globals.maxSize = 1152194191)
tic()
set.seed(9)
# Iterate over the exposure groups
for (exposure in A) {
    
    # Conditionally select the appropriate data based on 'A'
    if (exposure == "exposure_disability_only") {
        data_subset <- disability_only_no_depanx
    } else if (exposure == "exposure_disability_only_subset") {
        data_subset <- disability_only_no_depanx_subset
    } else if (exposure == "exposure_pain_only") {
        data_subset <- pain_only_no_depanx
    }
    
    data_subset <- data_subset |>
        as.data.frame()
    
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
        learners_g = learners_propensity,
        learners_e = learners_propensity,
        learners_c = learners,
        learners_b = learners,
        learners_hz = learners,
        learners_u = learners,
        learners_ubar = learners,
        learners_v = learners,
        learners_vbar = learners,
        learners_cens = learners
    )
    saveRDS(result, paste0("/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_results_final_r1/washout_12mo/all_mediators_12mo/", mediator_included, "_results_", exposure, ".rds"))
}






