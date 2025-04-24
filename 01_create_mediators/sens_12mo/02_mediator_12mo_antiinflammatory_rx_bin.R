# -------------------------------------
# Script: 02_mediator_12mo_antiinflammatory_rx_bin.R
# Author: Shodai Inose
# Purpose: Create binary indicator for antiinflammatory prescription during mediator period
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

M01 <- nop_ndc[grepl("^M01", atc, ignore.case = TRUE)]

ai_rxl <- 
    nop_rxl[, mediator_12mo_nonopioid_antiinflammatory_rx := fifelse(NDC %in% M01$NDC, 1, 0)
    ][mediator_12mo_nonopioid_antiinflammatory_rx == 1, ]

ai_otl <- 
    nop_otl[, mediator_12mo_nonopioid_antiinflammatory_rx := fifelse(NDC %in% M01$NDC, 1, 0)
    ][mediator_12mo_nonopioid_antiinflammatory_rx == 1, ]

# Save
saveRDS(ai_rxl, file.path(drv_root, "mediator_12mo_rxl_antiinflammatory_rx.rds"))
saveRDS(ai_otl, file.path(drv_root, "mediator_12mo_otl_antiinflammatory_rx.rds"))

# Make binary -------------------------------------------------------------

# Combine both datasets and keep only unique rows
ai <- rbind(ai_otl[, .(BENE_ID, NDC, NDC_QTY)], 
            ai_rxl[, .(BENE_ID, NDC, NDC_QTY)])

ai <- unique(ai)

ai <- merge(dts_cohorts, ai, all.x = TRUE)

ai[, mediator_12mo_nonopioid_antiinflammatory_rx := as.numeric(any(!is.na(NDC))), by = BENE_ID]
ai <- unique(ai[, .(BENE_ID, mediator_12mo_nonopioid_antiinflammatory_rx)])

saveRDS(ai, file.path(drv_root, "mediator_12mo_antiinflammatory_rx_bin.rds"))
