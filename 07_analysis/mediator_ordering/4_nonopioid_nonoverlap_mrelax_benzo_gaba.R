# mediator_nonopioid_nonoverlap_mrelax_gaba_benzo

# READ ME -----------------------------------------------------------------
#
#       Author: Anton
# 
# -------------------------------------------------------------------------

library(arrow)
library(dplyr)
library(lubridate)
library(data.table)

src_root <- "/mnt/processed-data/disability"
drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

cohort <- readRDS(file.path(drv_root, "mediation_analysis_df_subsetted.rds"))

tmp <- readRDS(file.path(drv_root, "mediator_nonoverlap.rds")) |>
    filter(BENE_ID %in% cohort$BENE_ID)

# Benzo ------------------------------------------------------------------

benzo_eligible <- tmp |> filter(mediator_nonopioid_nonoverlap_benzo == 1) |> pull(BENE_ID)

benzo <- rbind(readRDS(file.path(drv_root, "mediator_otl_benzo_rx.rds"))[, .(BENE_ID, date = LINE_SRVC_BGN_DT)], 
               readRDS(file.path(drv_root, "mediator_rxl_benzo_rx.rds"))[, .(BENE_ID, date = RX_FILL_DT)]) |>
    filter(BENE_ID %in% benzo_eligible)

benzo <- benzo[, .(benzo_dt = min(date)), by = BENE_ID] 


# Gaba ------------------------------------------------------------------

gaba_eligible <- tmp |> filter(mediator_nonopioid_nonoverlap_gaba == 1) |> pull(BENE_ID)

gaba <- rbind(readRDS(file.path(drv_root, "mediator_otl_gabapentinoid_rx.rds"))[, .(BENE_ID, date = LINE_SRVC_BGN_DT)], 
              readRDS(file.path(drv_root, "mediator_rxl_gabapentinoid_rx.rds"))[, .(BENE_ID, date = RX_FILL_DT)]) |>
    filter(BENE_ID %in% gaba_eligible)

gaba <- gaba[, .(gaba_dt = min(date)), by = BENE_ID] 


# Mrelax ------------------------------------------------------------------

mrelax_eligible <- tmp |> filter(mediator_nonopioid_nonoverlap_mrelax == 1) |> pull(BENE_ID)

mrelax <- rbind(readRDS(file.path(drv_root, "mediator_rxl_muscle_relaxant_rx.rds"))[, .(BENE_ID, date = RX_FILL_DT)], 
                readRDS(file.path(drv_root, "mediator_otl_muscle_relaxant_rx.rds"))[, .(BENE_ID, date = LINE_SRVC_BGN_DT)]) |>
    filter(BENE_ID %in% mrelax_eligible)

mrelax <- mrelax[, .(mrelax_dt = min(date)), by = BENE_ID] 


# all
all <- full_join(benzo, gaba) |>
    full_join(mrelax) |>
    rowwise() |>
    mutate(nonoverlap_mrelax_gaba_benzo_first_dt = min(benzo_dt, gaba_dt, mrelax_dt, na.rm=T)) |>
    ungroup() |>
    select(BENE_ID, nonoverlap_mrelax_gaba_benzo_first_dt)

saveRDS(all, "/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_first_dates/nonoverlap_mrelax_gaba_benzo_first_dt.rds")
