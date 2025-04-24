# -------------------------------------
# Script: 03_oud_overdose.R
# Author: Nick Williams
# Purpose:
# Notes:
# -------------------------------------

library(data.table)
library(lubridate)
library(purrr)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds")

setDT(cohort)

overdose <- copy(cohort)
overdose <- overdose[, .(BENE_ID, washout_cal_end_dt, oud_poison_dt, oud_poison_washout_12mos_cal, oud_poison_study_cal)]

overdose[, oud_overdose_18mo := 
             fcase(oud_poison_washout_12mos_cal == 1, 1, 
                   oud_poison_dt %within% 
                       interval(washout_cal_end_dt + days(182), 
                                washout_cal_end_dt + days(365)), 1, 
                   default = 0)]

setnames(overdose, 
         c("oud_poison_washout_12mos_cal", "oud_poison_study_cal"), 
         c("oud_overdose_12mo", "oud_overdose_24mo"))

saveRDS(overdose, file.path(drv_root, "oud_overdose_12_18_24mo.rds"))
