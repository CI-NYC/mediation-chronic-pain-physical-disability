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
# antiinflammatory ------------------------------------------------------------------

ai_eligible <- tmp |> filter(mediator_nonopioid_nonoverlap_antiinflamatory == 1) |> pull(BENE_ID)

ai <- rbind(readRDS(file.path(drv_root, "mediator_otl_antiinflammatory_rx.rds"))[, .(BENE_ID, date = LINE_SRVC_BGN_DT)], 
            readRDS(file.path(drv_root, "mediator_rxl_antiinflammatory_rx.rds"))[, .(BENE_ID, date = RX_FILL_DT)]) |>
    filter(BENE_ID %in% ai_eligible)

ai <- ai[, .(ai_dt = min(date)), by = BENE_ID] 


# antidepressant ------------------------------------------------------------------

ad_eligible <- tmp |> filter(mediator_nonopioid_nonoverlap_antidepressant == 1) |> pull(BENE_ID)

ad <- rbind(readRDS(file.path(drv_root, "mediator_otl_antidepressant_rx.rds"))[, .(BENE_ID, date = LINE_SRVC_BGN_DT)], 
            readRDS(file.path(drv_root, "mediator_rxl_antidepressant_rx.rds"))[, .(BENE_ID, date = RX_FILL_DT)]) |>
    filter(BENE_ID %in% ad_eligible)

ad <- ad[, .(ad_dt = min(date)), by = BENE_ID] 


# all
all <- full_join(ai, ad) |>
    rowwise() |>
    mutate(nonoverlap_ai_ad_first_dt = min(ai_dt, ad_dt, na.rm=T)) |>
    ungroup() |>
    select(BENE_ID, nonoverlap_ai_ad_first_dt)

saveRDS(all, "/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_first_dates/nonoverlap_ai_ad_first_dt.rds")
