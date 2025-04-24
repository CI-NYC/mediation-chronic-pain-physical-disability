# -------------------------------------
# Script: 15a_run.R
# Author: Nick Williams
# Updated:
# Purpose: Run 15_mediator_groups_zset_sensitivity.R
# Notes:
# -------------------------------------

library(callr)
library(tibble)
library(tidyr)

script <- "/home/ntw2117/disability/projects/mediation_unsafe_pain_mgmt/07_analysis/15_mediator_groups_zset_sensitivity.R"

param <- expand_grid(
    exposure = c("exposure_disability_only_subset", "exposure_pain_only"), 
    mediator = c("mediator_high_dose_longer_duration_mme",
                 "mediator_opioid_benzo_copresc",
                 "mediator_opioid_gaba_copresc",
                 "mediator_opioid_mrelax_copresc",
                 "mediator_nonopioid_nonoverlap_mrelax_gaba_benzo",
                 "mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory",
                 "mediator_has_physical_therapy"), 
    y = "oud_24mo_icd", 
    mh = c("TRUE", "FALSE"), 
    seed = "9"
)

# hold processes
prx <- list()
for(i in 1:nrow(param)) {
    prx[[i]] <- rscript_process$new(
        rscript_process_options(
            script = script, 
            cmdargs = c(param$exposure[i], 
                        param$mediator[i], 
                        param$y[i], 
                        param$mh[i],
                        param$seed[i]))
    )
}
