# READ ME -----------------------------------------------------------------
#
#       Author: Nick Williams
#       Last updated: 2024-01-26
# 
# -------------------------------------------------------------------------

library(data.table)
library(tidyverse)
library(yaml)
library(foreach)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"
input_root <- "~/disability/projects/mediation_unsafe_pain_mgmt"

ndc <- readRDS(file.path(input_root, "input/NDC_to_ATC_crosswalk.rds"))
codes <- read_yaml(paste0(input_root, "/01_create_mediators/mediator_codes.yml"))

# Non-opioid pain ---------------------------------------------------------

nop_codes <- c(names(codes[["Non-opioid pain"]]$ATC))

nop_codes <- c(nop_codes, "N03AE", "N05BA","N05CD") #adding benzodiazepines

nop_flag <- foreach(code = ndc[, atc], .combine = "c") %do% {
    any(sapply(nop_codes, \(x) str_detect(code, x)), na.rm = TRUE)
}

ndc[, .(NDC, atc, flag_nop = nop_flag)][flag_nop == TRUE] |> 
    saveRDS(file.path(drv_root, "mediation_unsafe_pain_mgmt_nonopioid_pain_ndc.rds"))

# Opioid pain -------------------------------------------------------------

opioids_pain <- ndc[flag_opioid_analgesic == T | flag_opioid_anesthetics == T, ]

opioids_pain[, .(NDC, atc, flag_opioid_analgesic, flag_opioid_anesthetics)] |> 
    saveRDS(file.path(drv_root, "mediation_unsafe_pain_mgmt_opioid_pain_ndc.rds"))

# Opioid dependence -------------------------------------------------------

opioids_moud <- ndc[flag_opioid_treat_dependence == T, ]

opioids_moud[, .(NDC, atc, flag_opioid_treat_dependence)] |> 
    saveRDS(file.path(drv_root, "mediation_unsafe_pain_mgmt_opioid_moud_ndc.rds"))

# Benzodiazepines ---------------------------------------------------------

benzo_codes <- c(names(codes[["Benzodiazepines"]]$ATC))

benzo_flag <- foreach(code = ndc[, atc], .combine = "c") %do% {
    any(sapply(benzo_codes, \(x) str_detect(code, x)), na.rm = TRUE)
}

ndc[, .(NDC, atc, flag_benzo = benzo_flag)][flag_benzo == TRUE, ] |> 
    saveRDS(file.path(drv_root, "mediation_unsafe_pain_mgmt_benzo_ndc.rds"))

# Stimulants --------------------------------------------------------------

stim_codes <- c(names(codes[["Stimulants"]]$ATC))

stim_flag <- foreach(code = ndc[, atc], .combine = "c") %do% {
    any(sapply(stim_codes, \(x) str_detect(code, x)), na.rm = TRUE)
}

ndc[, .(NDC, atc, flag_stim = stim_flag)][flag_stim == TRUE, ] |> 
    saveRDS(file.path(drv_root, "mediation_unsafe_pain_mgmt_stimulant_ndc.rds"))

# Gabapentin --------------------------------------------------------------

gab_code <- c(names(codes[["Gabapentin"]]$ATC))

gab_flag <- foreach(code = ndc[, atc], .combine = "c") %do% {
    any(sapply(gab_code, \(x) str_detect(code, x)), na.rm = TRUE)
}

ndc[, .(NDC, atc, flag_gab = gab_flag)][flag_gab == TRUE, ] |> 
    saveRDS(file.path(drv_root, "mediation_unsafe_pain_mgmt_gabapentinoid_ndc.rds"))
