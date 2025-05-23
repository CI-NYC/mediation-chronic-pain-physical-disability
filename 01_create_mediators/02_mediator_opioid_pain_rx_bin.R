# READ ME -----------------------------------------------------------------
#
# Author: Sarah Forrest
# Created: 2023-08-01
# Last edited: 2024-01-26 (Nick)
#
# Creates an indicator variable for whether or not an observation in
#   the analysis cohort had a claim for an opioid pain medication
#   prescription overall
# 
# -------------------------------------------------------------------------

library(arrow)
library(dplyr)
library(tidyverse)
library(data.table)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

analysis_df <- readRDS("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds")

setDT(analysis_df)
analysis_df <- analysis_df[, .(BENE_ID)]

otl <- readRDS(file.path(drv_root, "mediator_otl_opioid_pain_rx.rds"))
rxl <- readRDS(file.path(drv_root, "mediator_rxl_opioid_pain_rx.rds"))

# Combine both datasets and keep only unique rows
opioid_pain <- rbind(otl[, .(BENE_ID, NDC, NDC_QTY, flag_opioid_analgesic, flag_opioid_anesthetics)], 
                     rxl[, .(BENE_ID, NDC, NDC_QTY, flag_opioid_analgesic, flag_opioid_anesthetics)])

opioid_pain <- unique(opioid_pain)

opioid_pain <- merge(analysis_df, opioid_pain, all.x = TRUE)

opioid_pain[, mediator_opioid_pain_rx := as.numeric(any(!is.na(NDC))), by = BENE_ID]
opioid_pain <- unique(opioid_pain[, .(BENE_ID, mediator_opioid_pain_rx)])

saveRDS(opioid_pain, file.path(drv_root, "mediator_opioid_pain_rx_bin.rds"))
