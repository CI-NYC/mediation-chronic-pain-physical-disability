# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------

library(data.table)
library(dplyr)
library(lubridate)
library(arrow)

save_dir <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Load cohort
dts_cohorts <- as.data.table(readRDS("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds"))
dts_cohorts <- dts_cohorts[, .(BENE_ID, washout_start_dt, washout_cal_end_dt)]
setkey(dts_cohorts, BENE_ID)

claims <- readRDS("/mnt/general-data/disability/pain-severity/intermediate/visits_cleaned_with_procedures_and_inpatients_excluded.rds")

claims <- unique(merge(claims, dts_cohorts, by = "BENE_ID"))

# filter within the timeframe
claims_post_exposure <- claims[start_dt %within% interval(washout_cal_end_dt,
                                            washout_cal_end_dt + days(182)),
                 .(BENE_ID, ED_visit_dt = start_dt, washout_cal_end_dt, LINE_PRCDR_CD)]


######## 1 ED VISIT
# Create indicator variable for whether or not a patient had claim in mediator period
# Right join with cohort
claims_post_exposure <- claims_post_exposure[, .(has_1_ED_visit_post_exposure = as.numeric(.N > 0), 
                     has_2plus_ED_visit_post_exposure = as.numeric(.N > 1)), by = "BENE_ID"]

# filter within the timeframe
claims_washout <- claims[start_dt %within% interval(washout_start_dt,
                                                          washout_cal_end_dt),
                               .(BENE_ID, ED_visit_dt = start_dt, washout_cal_end_dt, LINE_PRCDR_CD)]


######## 1 ED VISIT
# Create indicator variable for whether or not a patient had claim in washout period
# Right join with cohort
claims_washout <- claims_washout[, .(has_1_ED_visit_washout = as.numeric(.N > 0), 
                                                 has_2plus_ED_visit_washout = as.numeric(.N > 1)), by = "BENE_ID"]

claims_all <- merge(claims_washout,
                    claims_post_exposure, 
                    all.x = TRUE,
                    all.y = TRUE, 
                    by = "BENE_ID")

claims_final <- merge(claims_all, 
                dts_cohorts[, .(BENE_ID)], 
                all.y = TRUE, by = "BENE_ID")

# Convert NAs to 0 for observations in the cohort that didn't have a claim
fix <- c("has_1_ED_visit_washout", "has_2plus_ED_visit_washout",
         "has_1_ED_visit_post_exposure", "has_2plus_ED_visit_post_exposure"
         )
claims_final[, (fix) := lapply(.SD, \(x) fifelse(is.na(x), 0, x)), .SDcols = fix]

saveRDS(claims_final, file.path(save_dir, "confounder_num_ED_visit.rds"))

## 12 month

# Load cohort
dts_cohorts <- as.data.table(readRDS("/mnt/general-data/disability/create_cohort/final/sens_12mos_analysis_cohort.rds"))
dts_cohorts <- dts_cohorts[, .(BENE_ID, washout_start_dt, washout_12mos_end_dt)]
setkey(dts_cohorts, BENE_ID)

claims <- readRDS("/mnt/general-data/disability/pain-severity/intermediate/visits_cleaned_with_procedures_and_inpatients_excluded.rds")

claims <- unique(merge(claims, dts_cohorts, by = "BENE_ID"))

# filter within the timeframe
claims_post_exposure <- claims[start_dt %within% interval(washout_12mos_end_dt,
                                                          washout_12mos_end_dt + days(182)),
                               .(BENE_ID, ED_visit_dt = start_dt, washout_12mos_end_dt, LINE_PRCDR_CD)]


######## 1 ED VISIT
# Create indicator variable for whether or not a patient had claim in mediator period
# Right join with cohort
claims_post_exposure <- claims_post_exposure[, .(has_1_ED_visit_post_exposure = as.numeric(.N > 0), 
                                                 has_2plus_ED_visit_post_exposure = as.numeric(.N > 1)), by = "BENE_ID"]

# filter within the timeframe
claims_washout <- claims[start_dt %within% interval(washout_start_dt,
                                                    washout_12mos_end_dt),
                         .(BENE_ID, ED_visit_dt = start_dt, washout_12mos_end_dt, LINE_PRCDR_CD)]


######## 1 ED VISIT
# Create indicator variable for whether or not a patient had claim in washout period
# Right join with cohort
claims_washout <- claims_washout[, .(has_1_ED_visit_washout = as.numeric(.N > 0), 
                                     has_2plus_ED_visit_washout = as.numeric(.N > 1)), by = "BENE_ID"]

claims_all <- merge(claims_washout,
                    claims_post_exposure, 
                    all.x = TRUE,
                    all.y = TRUE, 
                    by = "BENE_ID")

claims_final <- merge(claims_all, 
                      dts_cohorts[, .(BENE_ID)], 
                      all.y = TRUE, by = "BENE_ID")

# Convert NAs to 0 for observations in the cohort that didn't have a claim
fix <- c("has_1_ED_visit_washout", "has_2plus_ED_visit_washout",
         "has_1_ED_visit_post_exposure", "has_2plus_ED_visit_post_exposure"
)
claims_final[, (fix) := lapply(.SD, \(x) fifelse(is.na(x), 0, x)), .SDcols = fix]

saveRDS(claims_final, file.path(save_dir, "confounder_num_ED_visit_12mo.rds"))
