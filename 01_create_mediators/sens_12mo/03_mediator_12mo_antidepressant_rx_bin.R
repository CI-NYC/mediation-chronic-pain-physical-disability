# -------------------------------------
# Script: 03_mediator_12mo_antidepressant_rx_bin.R
# Author: Shodai Inose
# Purpose: Create binary indicator for antidepressant prescription during mediator period
# Notes:
# -------------------------------------

library(arrow)
library(dplyr)
library(lubridate)
library(data.table)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

dts_cohorts <- as.data.table(readRDS("/mnt/general-data/disability/create_cohort/final/sens_12mos_analysis_cohort.rds"))
dts_cohorts <- dts_cohorts[, .(BENE_ID, washout_12mos_end_dt)]
setkey(dts_cohorts, BENE_ID)

# Non-opioid pain rx data set
nop_rxl <- readRDS(file.path(drv_root, "mediator_12mo_rxl_nonopioid_pain_rx.rds"))
nop_otl <- readRDS(file.path(drv_root, "mediator_12mo_otl_nonopioid_pain_rx.rds"))

# Non-opioid NDC/ATC codes
nop_ndc <- readRDS(file.path(drv_root, "mediation_unsafe_pain_mgmt_nonopioid_pain_ndc.rds"))

N06A <- nop_ndc[grepl("^N06A", atc, ignore.case = TRUE)]

ad_rxl <- 
    nop_rxl[, mediator_12mo_nonopioid_antidepressant_rx := fifelse(NDC %in% N06A$NDC, 1, 0)
    ][mediator_12mo_nonopioid_antidepressant_rx == 1, ]

ad_otl <- 
    nop_otl[, mediator_12mo_nonopioid_antidepressant_rx := fifelse(NDC %in% N06A$NDC, 1, 0)
    ][mediator_12mo_nonopioid_antidepressant_rx == 1, ]

# Save
saveRDS(ad_rxl, file.path(drv_root, "mediator_12mo_rxl_antidepressant_rx.rds"))
saveRDS(ad_otl, file.path(drv_root, "mediator_12mo_otl_antidepressant_rx.rds"))

# Make binary -------------------------------------------------------------

# Combine both datasets and keep only unique rows
ad <- rbind(ad_otl[, .(BENE_ID, NDC, NDC_QTY)], 
            ad_rxl[, .(BENE_ID, NDC, NDC_QTY)])

ad <- unique(ad)

ad <- merge(dts_cohorts, ad, all.x = TRUE)

ad[, mediator_12mo_nonopioid_antidepressant_rx := as.numeric(any(!is.na(NDC))), by = BENE_ID]
ad <- unique(ad[, .(BENE_ID, mediator_12mo_nonopioid_antidepressant_rx)])

saveRDS(ad, file.path(drv_root, "mediator_12mo_antidepressant_rx_bin.rds"))
