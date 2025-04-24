# READ ME -----------------------------------------------------------------
#
# Author: Shodai Inose
# Created: 2024-09-11
# Note: mediators not made in previous analysis are made in this folder
# See https://github.com/CI-NYC/medicaid-treatments-oud-risk/tree/main/scripts/01_create_treatments/02_12mo for other mediators
#
# -------------------------------------------------------------------------

# Load libraries
library(arrow)
library(dplyr)
library(tidyverse)
library(data.table)


# Define root directory for data
drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Read the opioid and nonopioid pain prescription datasets
# RXL datasets
opioid_pain_rx_rxl <- readRDS(file.path(drv_root, "mediator_12mo_rxl_opioid_pain_rx.rds"))
nonopioid_pain_rx_rxl <- readRDS(file.path(drv_root, "mediator_12mo_rxl_nonopioid_pain_rx.rds"))
# OTL datasets
opioid_pain_rx_otl <- readRDS(file.path(drv_root, "mediator_12mo_otl_opioid_pain_rx.rds"))
nonopioid_pain_rx_otl <- readRDS(file.path(drv_root, "mediator_12mo_otl_nonopioid_pain_rx.rds"))


# Check for overlapping opioid prescription ------------------------------------------------------------------------------

# Create RXL and OTL overlap check datasets
# RXL 
opioid_overlap_check_rxl <- opioid_pain_rx_rxl[, .(BENE_ID, 
                                                   opioids_fill_date = RX_FILL_DT, 
                                                   opioids_end_date = RX_FILL_DT + DAYS_SUPPLY,
                                                   opioids_ndc = NDC)]

nonopioid_overlap_check_rxl <- nonopioid_pain_rx_rxl[, .(BENE_ID, 
                                                         nonopioids_fill_date = RX_FILL_DT, 
                                                         nonopioids_end_date = RX_FILL_DT + DAYS_SUPPLY,
                                                         nonopioids_ndc = NDC)]
# OTL 
opioid_overlap_check_otl <- opioid_pain_rx_otl[, .(BENE_ID, 
                                                   opioids_fill_date = LINE_SRVC_BGN_DT, 
                                                   opioids_end_date = LINE_SRVC_BGN_DT + 1,
                                                   opioids_ndc = NDC)] # 1 day assumption

nonopioid_overlap_check_otl <- nonopioid_pain_rx_otl[, .(BENE_ID, 
                                                         nonopioids_fill_date = LINE_SRVC_BGN_DT, 
                                                         nonopioids_end_date = LINE_SRVC_BGN_DT + 1,
                                                         nonopioids_ndc = NDC)] # 1 day assumption

# Merge RXL and OTL overlap check datasets
opioid_nonopioid_rxl <- merge(opioid_overlap_check_rxl, nonopioid_overlap_check_rxl, by = "BENE_ID",
                              allow.cartesian = TRUE)
opioid_nonopioid_otl <- merge(opioid_overlap_check_otl, nonopioid_overlap_check_otl, by = "BENE_ID")

# Calculate opioid and nonopioid rx overlap in days
# RXL 
opioid_nonopioid_rxl[, overlap := pmax(pmin(opioids_end_date, nonopioids_end_date) - 
                                           pmax(opioids_fill_date, nonopioids_fill_date) + 1, 0)]
# OTL 
opioid_nonopioid_otl[, overlap := pmax(pmin(opioids_end_date, nonopioids_end_date) - 
                                           pmax(opioids_fill_date, nonopioids_fill_date) + 1, 0)]

# Identify nonopioid fill date values with overlap == 0 for each BENE_ID
# RXL 
nonopioids_fill_date_with_overlap_log_rxl <- opioid_nonopioid_rxl[
    , .(overlap = any(overlap > 0)), by = .(BENE_ID, nonopioids_fill_date, nonopioids_ndc)]
# OTL
nonopioids_fill_date_with_overlap_log_otl <- opioid_nonopioid_otl[
    , .(overlap = any(overlap > 0)), by = .(BENE_ID, nonopioids_fill_date, nonopioids_ndc)]

# Identify unique combinations of beneficiary ID and NDC that have a value of TRUE for the overlap variable and set all to TRUE
# RXL 
nonopioids_fill_date_with_overlap_log_rxl <- nonopioids_fill_date_with_overlap_log_rxl |>
    group_by(BENE_ID, nonopioids_ndc) |>
    mutate(overlap = any(overlap == TRUE)) |>
    ungroup()

# OTL
nonopioids_fill_date_with_overlap_log_otl <- nonopioids_fill_date_with_overlap_log_otl |>
    group_by(BENE_ID, nonopioids_ndc) |>
    mutate(overlap = any(overlap == TRUE)) |>
    ungroup()

# Filter to include only rows with 0 overlap
nonopioids_fill_date_with_zero_overlap_rxl <- nonopioids_fill_date_with_overlap_log_rxl[nonopioids_fill_date_with_overlap_log_rxl$overlap == FALSE, ]
nonopioids_fill_date_with_zero_overlap_otl <- nonopioids_fill_date_with_overlap_log_otl[nonopioids_fill_date_with_overlap_log_otl$overlap == FALSE, ]

# Create nonopioid pain rx datasets with non-overlapping prescriptions only
nonoverlap_nonopioid_pain_rx_rxl <- nonopioids_fill_date_with_zero_overlap_rxl |>
    select(BENE_ID, nonopioids_fill_date, nonopioids_ndc) |>
    rename(RX_FILL_DT = nonopioids_fill_date,
           NDC = nonopioids_ndc)

nonoverlap_nonopioid_pain_rx_otl <- nonopioids_fill_date_with_zero_overlap_otl |>
    select(BENE_ID, nonopioids_fill_date, nonopioids_ndc) |>
    rename(LINE_SRVC_BGN_DT = nonopioids_fill_date,
           NDC = nonopioids_ndc)

# Merge the data frames by BENE_ID and RX_FILL_DT, keeping only rows from nonoverlap_nonopioid_pain_rx_rxl
# For nonoverlap_nonopioid_pain_rx_rxl
nonoverlap_nonopioid_pain_rx_rxl <- inner_join(nonoverlap_nonopioid_pain_rx_rxl, nonopioid_pain_rx_rxl, by = c("BENE_ID", "RX_FILL_DT", "NDC")) %>%
    mutate(flag_nop = TRUE)

# For nonoverlap_nonopioid_pain_rx_otl
nonoverlap_nonopioid_pain_rx_otl <- inner_join(nonoverlap_nonopioid_pain_rx_otl, nonopioid_pain_rx_otl, by = c("BENE_ID", "LINE_SRVC_BGN_DT", "NDC")) %>%
    mutate(flag_nop = TRUE)

# Export ------------------------------------------------------------------

saveRDS(nonoverlap_nonopioid_pain_rx_rxl, file.path(drv_root, "mediator_12mo_rxl_nonoverlap_nonopioid_pain_rx.rds"))
saveRDS(nonoverlap_nonopioid_pain_rx_otl, file.path(drv_root, "mediator_12mo_otl_nonoverlap_nonopioid_pain_rx.rds"))
