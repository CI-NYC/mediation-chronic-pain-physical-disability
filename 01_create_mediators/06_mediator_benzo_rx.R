# READ ME -----------------------------------------------------------------
#
#       Author: Nick Williams
#       Created: 2023-07-27
#       Last edited: 2024-01-26
# 
# -------------------------------------------------------------------------

library(arrow)
library(dplyr)
library(lubridate)
library(data.table)

src_root <- "/mnt/processed-data/disability"
drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Read in RXL (pharmacy line)
files <- paste0(list.files(src_root, pattern = "TAFRXL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
rxl <- open_dataset(file.path(src_root, parquet_files))

# Read in OTL (Other services line) 
files <- paste0(list.files(src_root, pattern = "TAFOTL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
otl <- open_dataset(file.path(src_root, parquet_files))

# Read in cohort and dates
dts_cohorts <- as.data.table(readRDS("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds"))
dts_cohorts <- dts_cohorts[, .(BENE_ID, washout_cal_end_dt)]
setkey(dts_cohorts, BENE_ID)

# Read in benzo list
ben <- readRDS(file.path(drv_root, "mediation_unsafe_pain_mgmt_benzo_ndc.rds"))

# OTL ---------------------------------------------------------------------

# Filter OTL to benzo NDC
otl_vars <- c("BENE_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "NDC", "NDC_QTY")

otl <- select(otl, all_of(otl_vars)) |> 
    filter(NDC %in% ben$NDC) |>
    collect() |> 
    as.data.table()

otl[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
                                  LINE_SRVC_END_DT, 
                                  LINE_SRVC_BGN_DT)]

# Inner join with cohort 
otl <- unique(merge(otl, dts_cohorts, by = "BENE_ID"))
otl <- merge(otl, ben[, c(1, 3)], all.x = TRUE, by = "NDC")

# Filter to claims within mediator time-frame
otl <- otl[LINE_SRVC_BGN_DT %within% interval(washout_cal_end_dt, 
                                              washout_cal_end_dt + days(182)), 
           .(BENE_ID, LINE_SRVC_BGN_DT, washout_cal_end_dt, NDC, NDC_QTY, flag_benzo)]

# RXL ---------------------------------------------------------------------

rxl_vars <- c("BENE_ID", "RX_FILL_DT", "NDC", "NDC_QTY", "DAYS_SUPPLY")

rxl <- select(rxl, all_of(rxl_vars)) |> 
    filter(NDC %in% ben$NDC) |>
    collect() |> 
    as.data.table()

# Inner join with cohort 
rxl <- unique(merge(rxl, dts_cohorts, by = "BENE_ID"))
rxl <- merge(rxl, ben[, c(1, 3)], all.x = TRUE, by = "NDC")

# Filter to claims within mediator time-frame
rxl <- rxl[RX_FILL_DT %within% interval(washout_cal_end_dt, 
                                        washout_cal_end_dt + days(182)), 
           .(BENE_ID, RX_FILL_DT, washout_cal_end_dt, NDC, NDC_QTY, DAYS_SUPPLY, flag_benzo)]

# Export ------------------------------------------------------------------

saveRDS(otl, file.path(drv_root, "mediator_otl_benzo_rx.rds"))
saveRDS(rxl, file.path(drv_root, "mediator_rxl_benzo_rx.rds"))

# Make binary -------------------------------------------------------------

# Combine both datasets and keep only unique rows
benzo <- rbind(otl[, .(BENE_ID, NDC, NDC_QTY, flag_benzo)], 
               rxl[, .(BENE_ID, NDC, NDC_QTY, flag_benzo)])

benzo <- unique(benzo)

benzo <- merge(dts_cohorts, benzo, all.x = TRUE)

benzo[, mediator_benzo_rx := as.numeric(any(!is.na(NDC))), by = BENE_ID]
benzo <- unique(benzo[, .(BENE_ID, mediator_benzo_rx)])

saveRDS(benzo, file.path(drv_root, "mediator_benzo_rx_bin.rds"))
