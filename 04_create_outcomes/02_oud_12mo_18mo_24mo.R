# READ ME -----------------------------------------------------------------
#
# Author: Sarah Forrest
# Created: 2023-10-31
# Last updated: 2023-11-16 (Nick)
#
# Creates an indicator variable for whether or not a beneficiary in the 
#   analysis cohort had the outcome, OUD at 12 months, 18 months and 24 
#   months after their washout start date. 
# 
# -------------------------------------------------------------------------

library(lubridate)
library(data.table)
library(purrr)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Read in cohort and dates
cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds")

setDT(cohort)
setkey(cohort, BENE_ID)

cohort <- cohort[, .(BENE_ID, washout_cal_end_dt, oud_moud_met_washout_cal, 
                     cohort_exclusion_oud_12mos_cal, oud_cal, oud_hillary_dt, oud_poison_dt, 
                     oud_hillary_washout_12mos_cal, oud_hillary_study_cal)]

# Read in MOUD start date data sets
bup <- readRDS(file.path(drv_root, "bup_start_dt.rds"))
met <- readRDS(file.path(drv_root, "met_start_dt.rds"))
nal <- readRDS(file.path(drv_root, "nal_start_dt.rds"))

walk(list(bup, met, nal), function(data) {
    setDT(data)
    setkey(data, BENE_ID)
})

# Combine MOUD datasets & merge ------------------------------------------------

moud_data <- rbindlist(list(bup, met, nal))

# Group the data by BENE_ID and select the first row within each group
moud_data <- moud_data[order(moud_start_dt), .SD[1], by = BENE_ID
                       ][, .(BENE_ID, moud_start_dt)]

moud_data <- merge(cohort, moud_data, all.x = TRUE)

# Create variables -------------------------------------------------------------

moud_data[, oud_12mo := fifelse(cohort_exclusion_oud_12mos_cal > 0, 1, 0)
          ][, oud_18mo := fcase(
              oud_12mo == 1, 1, 
              moud_start_dt %within% interval(washout_cal_end_dt + days(182), 
                                              washout_cal_end_dt + days(365)), 1, 
              oud_hillary_dt %within% interval(washout_cal_end_dt + days(182), 
                                               washout_cal_end_dt + days(365)), 1, 
              oud_poison_dt %within% interval(washout_cal_end_dt + days(182), 
                                              washout_cal_end_dt + days(365)), 1, 
              default = 0
          )
          ][, oud_24mo := fifelse(oud_cal > 0, 1, 0)]

moud_data[, oud_12mo_icd := fifelse(oud_hillary_washout_12mos_cal > 0, 1, 0)
          ][, oud_18mo_icd := fcase(
              oud_12mo_icd == 1, 1, 
              oud_hillary_dt %within% interval(washout_cal_end_dt + days(182), 
                                               washout_cal_end_dt + days(365)), 1, 
              default = 0
          )
          ][, oud_24mo_icd := fifelse(oud_hillary_study_cal > 0, 1, 0)]

saveRDS(moud_data[, .(BENE_ID, oud_12mo, oud_18mo, oud_24mo, 
                      oud_12mo_icd, oud_18mo_icd, oud_24mo_icd)], 
        file.path(drv_root, "oud_12mo_18mo_24mo.rds"))
