# -------------------------------------
# Script: 0x_ate.R
# Author: Nick Williams
# Purpose: Calculate ATE among all strata
# Notes:
# -------------------------------------

# library(lmtp)
library(mlr3extralearners)
# library(ranger)
# library(lightgbm)
library(earth)
library(data.table)
library(doFuture)
library(purrr)

devtools::source_gist("https://gist.github.com/nt-williams/702427d62cfe76f2efb69d240fdb3b2d") # adjust folds?

load_data <- function(file) {
    root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"
    as.data.table(readRDS(file.path(root, file)))
}

# Read in data subsets created in clean_analysis_data.R
do <- load_data("subset_disability_only_ref_df.rds")
cp <- load_data("subset_pain_only_ref_df.rds") 

do <- do[!is.na(exposure_disability_only_subset)]

for (df in c("do", "cp")) {
    dt <- get(df)
    set(dt,
        j = "mental_health",
        value = dt[["depression_washout_cal"]] == 1 |
            dt[["anxiety_washout_cal"]] == 1 |
            dt[["bipolar_washout_cal"]] == 1)
    assign(df, dt)
    rm(dt)
}

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

Y <- "oud_24mo_icd"
cens <- "uncens_24mo"

learners <- list("glm", 
                 "earth", 
                 "mean",
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

learners_Q <- list("glm", 
                 "earth", 
                 "mean",
                 # "ranger",
                 list("lightgbm", 
                      max_depth = 10, 
                      num_leaves = 20, 
                      min_gain_to_split = 0.1, 
                      objective = "binary",
                      min_data_in_leaf = 100, 
                      learning_rate = 0.25, 
                      id = "lightgbm"))

params <- tidyr::expand_grid(data = c("cp", "do"), mh = c(TRUE, FALSE))
set.seed(1)
options(future.globals.maxSize = 3 * 1024^3) 

plan(multisession, workers = 4)
res <- foreach(
    i = 1:nrow(params),
    .options.future = list(
        globals = structure(TRUE, add = c("cp", "do")),
        packages = "mlr3extralearners",
        seed = TRUE
    )
) %dofuture% {
    tmle_mlr3(
        data = as.data.frame(get(params$data[i])[mental_health == params$mh[i]]),
        trt = ifelse(
            i %in% c(1, 2),
            "exposure_pain_only",
            "exposure_disability_only_subset"
        ),
        covar_trt = W,
        covar_outcome = W,
        covar_cens = W,
        outcome = Y,
        g_learners = learners,
        Q_learners = learners_Q,
        c_learners = learners,
        .mlr3superlearner_folds = 2, 
        folds = 10,
        .discrete = TRUE
    )
}
plan(sequential)

params <- cbind(params, list_rbind(map(res, \(x) ife::tidy(x$psi[["1"]] - x$psi[["0"]]))))

saveRDS(params, "/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_results_final_r1/ATE/ate_icd.rds")

## Comprehensive definition

Y <- "oud_24mo"

params <- tidyr::expand_grid(data = c("cp", "do"), mh = c(TRUE, FALSE))
set.seed(1)
options(future.globals.maxSize = 3 * 1024^3) 

plan(multisession, workers = 4)
res <- foreach(
    i = 1:nrow(params),
    .options.future = list(
        globals = structure(TRUE, add = c("cp", "do")),
        packages = "mlr3extralearners",
        seed = TRUE
    )
) %dofuture% {
    tmle_mlr3(
        data = as.data.frame(get(params$data[i])[mental_health == params$mh[i]]),
        trt = ifelse(
            i %in% c(1, 2),
            "exposure_pain_only",
            "exposure_disability_only_subset"
        ),
        covar_trt = W,
        covar_outcome = W,
        covar_cens = W,
        outcome = Y,
        g_learners = learners,
        Q_learners = learners,
        c_learners = learners,
        .mlr3superlearner_folds = 2, 
        folds = 10,
        .discrete = TRUE
    )
}
plan(sequential)

params <- cbind(params, list_rbind(map(res, \(x) ife::tidy(x$psi[["1"]] - x$psi[["0"]]))))

saveRDS(params, "/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_results_final_r1/ATE/ate_comp.rds")

# 12 month washout

do <- load_data("subset_12mo_disability_only_ref_df_updated.rds")
cp <- load_data("subset_12mo_pain_only_ref_df_updated.rds")

do <- do[!is.na(exposure_disability_only_subset)]

for (df in c("do", "cp")) {
    dt <- get(df)
    set(dt,
        j = "mental_health",
        value = dt[["depression_washout_cal"]] == 1 |
            dt[["anxiety_washout_cal"]] == 1 |
            dt[["bipolar_washout_cal"]] == 1)
    assign(df, dt)
    rm(dt)
}


Y <- "oud_24mo_icd"

learners <- list("glm", "earth", "mean", #"glmnet", #"ranger",
                 list("lightgbm", 
                      max_depth = 10, 
                      num_leaves = 20, 
                      min_gain_to_split = 0.1, 
                      is_unbalance = TRUE,
                      objective = "binary",
                      min_data_in_leaf = 100, 
                      learning_rate = 0.25, 
                      id = "lightgbm"))

params <- tidyr::expand_grid(data = c("cp", "do"), mh = c(TRUE, FALSE))
set.seed(1)
options(future.globals.maxSize = 3 * 1024^3) 

plan(multisession, workers = 1)
res <- foreach(
    i = 1:nrow(params),
    .options.future = list(
        globals = structure(TRUE, add = c("cp", "do")),
        packages = "mlr3extralearners",
        seed = TRUE
    )
) %dofuture% {
    tmle_mlr3(
        data = as.data.frame(get(params$data[i])[mental_health == params$mh[i]]),
        trt = ifelse(
            i %in% c(1, 2),
            "exposure_pain_only",
            "exposure_disability_only_subset"
        ),
        covar_trt = W,
        covar_outcome = W,
        covar_cens = W,
        outcome = Y,
        g_learners = learners,
        Q_learners = learners,
        c_learners = learners,
        .mlr3superlearner_folds = 2, 
        folds = 10,
        .discrete = TRUE
    )
}
plan(sequential)

params <- cbind(params, list_rbind(map(res, \(x) ife::tidy(x$psi[["1"]] - x$psi[["0"]]))))

saveRDS(params, "/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_results_final_r1/ATE/ate_12mo.rds")
