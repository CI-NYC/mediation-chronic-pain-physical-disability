# run scripts ....

library(callr)
library(tibble)
library(tidyverse)

script <- "/home/ntw2117/disability/projects/mediation_unsafe_pain_mgmt/07_analysis/08_mediator_groups_main.R"

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

tribble_main <- tribble(~exposure, ~mediator, ~seed, ~y, ~mh,
                     "exposure_disability_only_subset", "mediator_high_dose_longer_duration_mme", 9, "oud_24mo_icd", TRUE,
                     "exposure_disability_only_subset", "mediator_high_dose_longer_duration_mme", 9, "oud_24mo_icd", FALSE,
                     "exposure_disability_only_subset", "mediator_opioid_benzo_copresc", 9,"oud_24mo_icd", TRUE,
                     "exposure_disability_only_subset", "mediator_opioid_benzo_copresc", 9,"oud_24mo_icd", FALSE,
                     "exposure_disability_only_subset", "mediator_opioid_gaba_copresc", 9,"oud_24mo_icd", TRUE,
                     "exposure_disability_only_subset", "mediator_opioid_gaba_copresc", 9,"oud_24mo_icd", FALSE,
                     "exposure_disability_only_subset", "mediator_opioid_mrelax_copresc", 9, "oud_24mo_icd", TRUE,
                     "exposure_disability_only_subset", "mediator_opioid_mrelax_copresc", 9, "oud_24mo_icd", FALSE,
                     "exposure_disability_only_subset", "mediator_nonopioid_nonoverlap_mrelax_gaba_benzo", 9, "oud_24mo_icd", TRUE,
                     "exposure_disability_only_subset", "mediator_nonopioid_nonoverlap_mrelax_gaba_benzo", 9, "oud_24mo_icd", FALSE,
                     "exposure_disability_only_subset", "mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory", 9, "oud_24mo_icd", TRUE,
                     "exposure_disability_only_subset", "mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory", 9, "oud_24mo_icd", FALSE,
                     "exposure_disability_only_subset", "mediator_has_physical_therapy", 9, "oud_24mo_icd", TRUE,
                     "exposure_disability_only_subset", "mediator_has_physical_therapy", 9, "oud_24mo_icd", FALSE,
                     "exposure_pain_only", "mediator_high_dose_longer_duration_mme", 9,"oud_24mo_icd", TRUE,
                     "exposure_pain_only", "mediator_high_dose_longer_duration_mme", 9,"oud_24mo_icd", FALSE,
                     "exposure_pain_only", "mediator_opioid_benzo_copresc", 9, "oud_24mo_icd", TRUE,
                     "exposure_pain_only", "mediator_opioid_benzo_copresc", 9, "oud_24mo_icd", FALSE,
                     "exposure_pain_only", "mediator_opioid_gaba_copresc", 9, "oud_24mo_icd", TRUE,
                     "exposure_pain_only", "mediator_opioid_gaba_copresc", 9, "oud_24mo_icd", FALSE,
                     "exposure_pain_only", "mediator_opioid_mrelax_copresc", 9, "oud_24mo_icd", TRUE,
                     "exposure_pain_only", "mediator_opioid_mrelax_copresc", 9, "oud_24mo_icd", FALSE,
                     "exposure_pain_only", "mediator_nonopioid_nonoverlap_mrelax_gaba_benzo", 9, "oud_24mo_icd", TRUE,
                     "exposure_pain_only", "mediator_nonopioid_nonoverlap_mrelax_gaba_benzo", 9, "oud_24mo_icd", FALSE,
                     "exposure_pain_only", "mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory", 9, "oud_24mo_icd", TRUE,
                     "exposure_pain_only", "mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory", 9, "oud_24mo_icd", FALSE,
                     "exposure_pain_only", "mediator_has_physical_therapy", 9, "oud_24mo_icd", TRUE,
                     "exposure_pain_only", "mediator_has_physical_therapy", 9, "oud_24mo_icd", FALSE
)

param <- tribble_main |>
    #select(exposure, mediator, y, mh) |>
    #distinct() |>
    #expand_grid(seed = 9:18) |>
    #left_join(tribble_main, by = c("exposure", "mediator", "seed", "y", "mh")) |>
    mutate(seed = as.character(seed),
           mh = as.character(mh))

y <- "oud_24mo_icd"

# Execute for disability only and chronic pain only -------------------------------------------
# hold a processes
prx <- list()
for(i in 1:nrow(param))
{
prx[[i]] <- rscript_process$new(
                rscript_process_options(
                    script = script, 
                    cmdargs = c(param$exposure[i], #exposure
                                param$mediator[i], #mediator of interest
                                param$y[i], #outcome
                                param$mh[i], #baseline MH disorder or not
                                param$seed[i])) #seed to use
            )
}

# if (prx[[1]]$get_exit_status() != 0) {
#     error_message <- Rprocess$read_error()
#     cat("Status", Rprocess$get_exit_status(), "\n")
#     cat("Error captured:\n", error_message, "\n")
#     cat("Output", Rprocess$read_output())
# }
