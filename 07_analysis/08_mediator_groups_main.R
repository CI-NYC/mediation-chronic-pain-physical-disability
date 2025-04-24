# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes: devtools::install_github("nt-williams/HDmediation/HDmediation2@patch-1")
# -------------------------------------

library(tidyverse)
library(HDmediationmlr3)
library(mlr3superlearner)
library(mlr3extralearners)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

args <- commandArgs(TRUE)

if (is_empty(args)) {
    exposure <- "exposure_disability_only_subset"
    mediator <- "mediator_has_physical_therapy"
    Y <- "oud_24mo_icd"
    mh <- "FALSE"
    seed <- as.numeric("9")
} else {
    exposure <- args[[1]]
    mediator <- args[[2]]
    Y <- args[[3]]
    mh <- as.logical(args[[4]])
    seed <- as.numeric(args[[5]])
}



if(exposure == "exposure_disability_only_subset")
{
disability <- readRDS(paste0(drv_root, "/subset_disability_only_ref_df.rds"))|>
    mutate(mediator_opioid_copresc_benzo_gaba = 
               ifelse(mediator_opioid_benzo_copresc == 1 |
                          mediator_opioid_gaba_copresc == 1, 1, 0),
           mediator_nonopioid_nonoverlap_mrelax_gaba_benzo = 
               ifelse(mediator_nonopioid_nonoverlap_mrelax == 1 |
                          mediator_nonopioid_nonoverlap_gaba == 1 |
                          mediator_nonopioid_nonoverlap_benzo == 1, 1, 0),
           mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory = 
               ifelse(mediator_nonopioid_nonoverlap_antidepressant == 1 |
                          mediator_nonopioid_nonoverlap_antiinflamatory == 1, 1, 0))


disability_subset <- disability |> # filtering to those without dep/anx/bipolar
    filter(is.na(exposure_disability_only_subset) == FALSE)

rm(disability)

if(mh == FALSE)
{
    disability_only_no_depanx_subset <- disability_subset |> # filtering to those without dep/anx/bipolar
        filter(depression_washout_cal == 0 &
                   anxiety_washout_cal == 0 &
                   bipolar_washout_cal == 0)
} else
{
    disability_depanx_subset <- disability_subset |> # filtering to those with dep/anx/bipolar
        filter(depression_washout_cal == 1 |
                   anxiety_washout_cal == 1 |
                   bipolar_washout_cal == 1)
}
} else
{
    pain <- readRDS(paste0(drv_root, "/subset_pain_only_ref_df.rds")) |>
        mutate(mediator_opioid_copresc_benzo_gaba = 
                   ifelse(mediator_opioid_benzo_copresc == 1 |
                              mediator_opioid_gaba_copresc == 1, 1, 0),
               mediator_nonopioid_nonoverlap_mrelax_gaba_benzo = 
                   ifelse(mediator_nonopioid_nonoverlap_mrelax == 1 |
                              mediator_nonopioid_nonoverlap_gaba == 1 |
                              mediator_nonopioid_nonoverlap_benzo == 1, 1, 0),
               mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory = 
                   ifelse(mediator_nonopioid_nonoverlap_antidepressant == 1 |
                              mediator_nonopioid_nonoverlap_antiinflamatory == 1, 1, 0))
    
    if (mh == FALSE) {
        pain_only_no_depanx <-
            pain |> # filtering to those without dep/anx/bipolar
            filter(depression_washout_cal == 0 &
                       anxiety_washout_cal == 0 &
                       bipolar_washout_cal == 0)
    } else {
        pain_depanx <- pain |> # filtering to those with dep/anx/bipolar
            filter(depression_washout_cal == 1 |
                       anxiety_washout_cal == 1 |
                       bipolar_washout_cal == 1)
    }
}

# Algorithm libraries
learners <- list(
    "glm",
    "earth",
    list(
        "lightgbm",
        max_depth = 10,
        num_leaves = 20,
        min_gain_to_split = 0.1,
        min_data_in_leaf = 100,
        learning_rate = 0.25,
        id = "lightgbm"
    )
)

learners_propensity <- list(
    "glm",
    "earth",
    list(
        "lightgbm",
        max_depth = 10,
        num_leaves = 20,
        min_gain_to_split = 0.1,
        is_unbalance = TRUE,
        objective = "binary",
        min_data_in_leaf = 100,
        learning_rate = 0.25,
        id = "lightgbm"
    )
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

M <- c(mediator)

Z <- c("mediator_high_dose_longer_duration_mme", #MME
       "mediator_opioid_benzo_copresc",
       "mediator_opioid_gaba_copresc",
       "mediator_opioid_mrelax_copresc", #opioid copresc
       "mediator_nonopioid_nonoverlap_mrelax_gaba_benzo",
       "mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory",
       "mediator_has_physical_therapy", #PT
       "has_2plus_ED_visit_post_exposure" # post-exposure
)

Z <- Z[Z != M]

num_folds <- 2

mo <- as.numeric(unlist(stringr::str_extract_all(Y, "\\d+")))
cens <- glue::glue("uncens_{mo}mo")

options(future.globals.maxSize = 11521941910)
set.seed(seed)

if (mh == TRUE) {
    # Conditionally select the appropriate data based on 'A'
    if (exposure == "exposure_disability_only") {
        data_subset <- disability_depanx
    } else if (exposure == "exposure_disability_only_subset") {
        data_subset <- disability_depanx_subset
    } else if (exposure == "exposure_pain_only") {
        data_subset <- pain_depanx
    }
} else {
    # Conditionally select the appropriate data based on 'A'
    if (exposure == "exposure_disability_only") {
        data_subset <- disability_only_no_depanx
    } else if (exposure == "exposure_disability_only_subset") {
        data_subset <- disability_only_no_depanx_subset
    } else if (exposure == "exposure_pain_only") {
        data_subset <- pain_only_no_depanx
    }
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
    
if (Y == "oud_24mo_icd") {
    if (mh == TRUE) {
        saveRDS(result, paste0("/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_results_final_r1/depanx_icd/", mediator, "_icd_results_", exposure, "_depanx.rds"))
    } else {
        saveRDS(result, paste0("/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_results_final_r1/nodepanx_icd/", mediator, "_icd_results_", exposure, "_nodepanx.rds"))
    }
} else {
    if (mh == TRUE) {
        saveRDS(result, paste0("/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_results_final_r1/depanx/", mediator, "_results_", exposure, "_depanx.rds"))
    } else {
        saveRDS(result, paste0("/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_results_final_r1/nodepanx/", mediator, "_results_", exposure, "_nodepanx.rds"))
    }
}

