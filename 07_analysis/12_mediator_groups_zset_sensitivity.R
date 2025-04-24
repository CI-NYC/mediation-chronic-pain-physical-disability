# -------------------------------------
# Script: 15_mediator_groups_zset_sensitivity.R
# Author: Nick Williams
# Updated:
# Purpose: Sensitivity analysis for the by-group analysis.
# Notes: devtools::install_github("nt-williams/HDmediation/HDmediation2@patch-1")
# -------------------------------------

library(tidyverse)
library(HDmediationmlr3)
library(mlr3superlearner)
library(mlr3extralearners)
library(fs)

options(future.globals.maxSize = 11521941910)

root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

args <- commandArgs(TRUE)

if (is_empty(args)) {
    exposure <- "exposure_disability_only_subset"
    mediator <- "mediator_has_physical_therapy"
    Y <- "oud_24mo_icd"
    mh <- FALSE
    seed <- 9
} else {
    exposure <- args[[1]]
    mediator <- args[[2]]
    Y <- args[[3]]
    mh <- as.logical(args[[4]])
    seed <- as.numeric(args[[5]])
}

if (exposure == "exposure_disability_only_subset") {
    data_subset <- readRDS(path(root, "subset_disability_only_ref_df.rds")) |> 
        filter(is.na(exposure_disability_only_subset) == FALSE) |> 
        as.data.frame()
} else {
    data_subset <- readRDS(path(root, "subset_pain_only_ref_df.rds")) |> 
        as.data.frame()
}

if (mh) {
    # filter to those with dep/anx/bipolar
    data_subset <- filter(data_subset, depression_washout_cal == 1 | anxiety_washout_cal == 1 | bipolar_washout_cal == 1)
} else {
    data_subset <- filter(data_subset, depression_washout_cal == 0 & anxiety_washout_cal == 0 & bipolar_washout_cal == 0)
}

data_subset <- mutate(
    data_subset, 
    mediator_opioid_copresc_benzo_gaba = 
        ifelse(mediator_opioid_benzo_copresc == 1 |
                   mediator_opioid_gaba_copresc == 1, 1, 0), 
    mediator_nonopioid_nonoverlap_mrelax_gaba_benzo = 
        ifelse(mediator_nonopioid_nonoverlap_mrelax == 1 |
                   mediator_nonopioid_nonoverlap_gaba == 1 |
                   mediator_nonopioid_nonoverlap_benzo == 1, 1, 0), 
    mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory = 
        ifelse(mediator_nonopioid_nonoverlap_antidepressant == 1 |
                   mediator_nonopioid_nonoverlap_antiinflamatory == 1, 1, 0)
)

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

no_folds <- 2

# Baseline confounders
W <- c(
    "dem_age",
    "dem_sex_m",
    "dem_race_aian",
    "dem_race_asian",
    "dem_race_black",
    "dem_race_hawaiian",
    "dem_race_hispanic",
    "dem_race_multiracial",
    "dem_primary_language_english", # NAs
    "dem_married_or_partnered", # NAs
    "dem_household_size_2",
    "dem_household_size_2plus",
    "dem_veteran", # NAs
    "dem_probable_high_income",
    "dem_tanf_benefits", # NAs
    "dem_ssi_benefits_mandatory_optional",
    "adhd_washout_cal",
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

Z <- list(
    "mediator_high_dose_longer_duration_mme" = 
        c("mediator_opioid_benzo_copresc", 
          "mediator_opioid_gaba_copresc", 
          "mediator_opioid_mrelax_copresc", 
          "mediator_nonopioid_nonoverlap_mrelax_gaba_benzo", 
          "mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory", 
          "mediator_has_physical_therapy",
          "has_2plus_ED_visit_post_exposure"),
    "mediator_opioid_benzo_copresc" = c(
        "mediator_nonopioid_nonoverlap_mrelax_gaba_benzo", 
        "mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory",
        "has_2plus_ED_visit_post_exposure"
    ),
    "mediator_opioid_gaba_copresc" = c(
        "mediator_nonopioid_nonoverlap_mrelax_gaba_benzo", 
        "mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory", 
        "mediator_has_physical_therapy",
        "has_2plus_ED_visit_post_exposure"
    ),
    "mediator_opioid_mrelax_copresc" = c(
        "mediator_nonopioid_nonoverlap_mrelax_gaba_benzo", 
        "mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory",
        "has_2plus_ED_visit_post_exposure"
    ),
    "mediator_nonopioid_nonoverlap_mrelax_gaba_benzo" = c("has_2plus_ED_visit_post_exposure"),
    "mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory" = c("has_2plus_ED_visit_post_exposure"),
    "mediator_has_physical_therapy" = c(
        "mediator_opioid_benzo_copresc", 
        "mediator_opioid_mrelax_copresc", 
        "mediator_nonopioid_nonoverlap_mrelax_gaba_benzo", 
        "mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory",
        "has_2plus_ED_visit_post_exposure"
    )
)

mo <- as.numeric(unlist(stringr::str_extract_all(Y, "\\d+")))
cens <- glue::glue("uncens_{mo}mo")

set.seed(seed)

result <- mediation(
    data = data_subset,
    A = exposure,
    W = W,
    Z = Z[[mediator]],
    M = mediator,
    Y = Y,
    cens = cens,
    S = NULL,
    family = "binomial",
    folds = no_folds,
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

saveRDS(
    result, 
    path(root, "mediation_results_final_r1", "conditional_Z_final", ifelse(mh, "depanx", "nodepanx"), 
         glue::glue(mediator, "_icd_results_", exposure, ".rds"))
)
