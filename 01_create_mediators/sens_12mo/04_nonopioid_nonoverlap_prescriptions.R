# -------------------------------------
# Script:
# Author: Shodai Inose, September 2024
# Purpose: Create binary indicators for benzodiazepine, gabapentinoid, antidepressant, or antiinflammatory 
# prescription during mediator period (without overlapping opioid prescription)
# Notes:
# -------------------------------------
library(tidyverse)
library(arrow)
library(dplyr)
library(lubridate)
library(data.table)

int_overlaps_numeric <- function (int1, int2) {
    stopifnot(c(is.interval(int1), is.interval(int2)))
    
    x <- intersect(int1, int2)@.Data
    x[is.na(x)] <- 0
    time_length(as.duration(x), "days")
}

which_min <- function(...) {
    l <- list(...)
    which.min(Reduce(cbind, l))
}

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Opioid pain rx data set
opioid <- readRDS(file.path(drv_root, "mediator_12mo_rxl_opioid_pain_rx.rds"))

# Non-opioid pain rx data set
nop_rxl <- readRDS(file.path(drv_root, "mediator_12mo_rxl_nonopioid_pain_rx.rds"))
nop_otl <- readRDS(file.path(drv_root, "mediator_12mo_otl_nonopioid_pain_rx.rds"))

ai <- readRDS(file.path(drv_root, "mediator_12mo_rxl_antiinflammatory_rx.rds"))

id <- "BENE_ID"
vars_opioids <- c("RX_FILL_DT", "DAYS_SUPPLY", "opioids_end_date")
keep <- c(id, vars_opioids)
throwaway <- setdiff(names(opioid), keep)

opioid[, opioids_end_date := RX_FILL_DT + DAYS_SUPPLY
][, (throwaway) := lapply(.SD, \(x) NULL), .SDcols = throwaway
][, opioids_end_date := 
      fifelse(is.na(opioids_end_date), RX_FILL_DT + days(1), opioids_end_date)]
setnames(opioid, vars_opioids, c("opioids_fill_date", "opioids_days_supply", "opioids_end_date"))

vars_ai_rxl <- c("RX_FILL_DT", "DAYS_SUPPLY", "ai_rxl_end_date")
keep <- c(id, vars_ai_rxl)
throwaway <- setdiff(names(ai), keep)

ai[, ai_rxl_end_date := RX_FILL_DT + DAYS_SUPPLY
][, (throwaway) := lapply(.SD, \(x) NULL), .SDcols = throwaway
][, ai_rxl_end_date := 
      fifelse(is.na(ai_rxl_end_date), RX_FILL_DT + days(1), ai_rxl_end_date)]
setnames(ai, vars_ai_rxl, c("ai_fill_date", "ai_days_supply", "ai_end_date"))

ad <- readRDS(file.path(drv_root, "mediator_12mo_rxl_antidepressant_rx.rds"))

vars_ad_rxl <- c("RX_FILL_DT", "DAYS_SUPPLY", "ad_rxl_end_date")
keep <- c(id, vars_ad_rxl)
throwaway <- setdiff(names(ad), keep)

ad[, ad_rxl_end_date := RX_FILL_DT + DAYS_SUPPLY
][, (throwaway) := lapply(.SD, \(x) NULL), .SDcols = throwaway
][, ad_rxl_end_date := 
      fifelse(is.na(ad_rxl_end_date), RX_FILL_DT + days(1), ad_rxl_end_date)]
setnames(ad, vars_ad_rxl, c("ad_fill_date", "ad_days_supply", "ad_end_date"))

# Create co-prescription variables ----------------------------------------

#plan(multisession, workers = 50)

## Opioids and ai
opioids_ai <- 
    merge(opioid, ai, by = "BENE_ID") |> 
    as_tibble() |> 
    mutate(ai_interval = interval(ai_fill_date, ai_end_date), 
           opioids_interval = interval(opioids_fill_date, opioids_end_date), 
           overlap = int_overlaps_numeric(ai_interval, opioids_interval)) |> 
    rowwise() |> 
    mutate(first_drug = c("opioids", "ai")[which_min(opioids_fill_date, ai_fill_date)]) |> 
    ungroup() |> 
    filter((first_drug == "ai" & 
                ai_days_supply > 5 & 
                overlap / ai_days_supply >= 0.25) |
               (first_drug == "opioids" & 
                    opioids_days_supply > 5 & 
                    overlap / opioids_days_supply >= 0.25)) |> 
    mutate(rx_int = intersect(ai_interval, opioids_interval)) |> 
    group_by(BENE_ID) |> 
    nest()

opioids_ai <- 
    ungroup(opioids_ai) |> 
    mutate(mediator_12mo_opioid_antiinflamatory_copresc = 1) |> 
    select(-data)

## Opioids and ad
opioids_ad <- 
    merge(opioid, ad, by = "BENE_ID") |> 
    as_tibble() |> 
    mutate(ad_interval = interval(ad_fill_date, ad_end_date), 
           opioids_interval = interval(opioids_fill_date, opioids_end_date), 
           overlap = int_overlaps_numeric(ad_interval, opioids_interval)) |> 
    rowwise() |> 
    mutate(first_drug = c("opioids", "ad")[which_min(opioids_fill_date, ad_fill_date)]) |> 
    ungroup() |> 
    filter((first_drug == "ad" & 
                ad_days_supply > 5 & 
                overlap / ad_days_supply >= 0.25) |
               (first_drug == "opioids" & 
                    opioids_days_supply > 5 & 
                    overlap / opioids_days_supply >= 0.25)) |> 
    mutate(rx_int = intersect(ad_interval, opioids_interval)) |> 
    group_by(BENE_ID) |> 
    nest()

opioids_ad <- 
    ungroup(opioids_ad) |> 
    mutate(mediator_12mo_opioid_antidepressant_copresc = 1) |> 
    select(-data)

## Benzo and gaba non-copresc

benzo <- readRDS(file.path(drv_root, "mediator_12mo_benzo_rx_bin.rds")) 
gaba <- readRDS(file.path(drv_root, "mediator_12mo_gabapentinoid_rx_bin.rds"))
mrelax <- readRDS(file.path(drv_root, "mediator_12mo_muscle_relaxant_rx_bin.rds"))

ai_mediator_12mo <- readRDS(file.path(drv_root, "mediator_12mo_antiinflammatory_rx_bin.rds"))
ad_mediator_12mo <- readRDS(file.path(drv_root, "mediator_12mo_antidepressant_rx_bin.rds"))

copresc <- readRDS(file.path(drv_root, "mediator_12mo_opioid_coprescriptions.rds")) |>
    left_join(benzo, by = c("BENE_ID" = "BENE_ID")) |>
    left_join(gaba, by = c("BENE_ID" = "BENE_ID")) |>
    left_join(mrelax, by = c("BENE_ID" = "BENE_ID")) |>
    left_join(ai_mediator_12mo, by = c("BENE_ID" = "BENE_ID")) |>
    left_join(ad_mediator_12mo, by = c("BENE_ID" = "BENE_ID")) |>
    left_join(opioids_ai, by = c("BENE_ID" = "BENE_ID")) |>
    left_join(opioids_ad, by = c("BENE_ID" = "BENE_ID")) 

nonoverlap <- copresc |>
    mutate(mediator_12mo_opioid_antiinflamatory_copresc = ifelse(is.na(mediator_12mo_opioid_antiinflamatory_copresc), 0, mediator_12mo_opioid_antiinflamatory_copresc),
           mediator_12mo_opioid_antidepressant_copresc = ifelse(is.na(mediator_12mo_opioid_antidepressant_copresc), 0, mediator_12mo_opioid_antidepressant_copresc)) |>
    mutate(mediator_12mo_nonopioid_nonoverlap_benzo = ifelse(mediator_opioid_benzo_copresc == 0 & mediator_benzo_rx == 1, 1, 0),
           mediator_12mo_nonopioid_nonoverlap_gaba = ifelse(mediator_opioid_gaba_copresc == 0 & mediator_gaba_rx == 1, 1, 0),
           mediator_12mo_nonopioid_nonoverlap_mrelax = ifelse(mediator_opioid_mrelax_copresc == 0 & mediator_nonopioid_muscle_relaxant_rx == 1, 1, 0),
           mediator_12mo_nonopioid_nonoverlap_antiinflamatory = ifelse(mediator_12mo_opioid_antiinflamatory_copresc == 0 & mediator_12mo_nonopioid_antiinflammatory_rx == 1, 1, 0),
           mediator_12mo_nonopioid_nonoverlap_antidepressant = ifelse(mediator_12mo_opioid_antidepressant_copresc == 0 & mediator_12mo_nonopioid_antidepressant_rx == 1, 1, 0)) |>
    select(BENE_ID, mediator_12mo_nonopioid_nonoverlap_benzo, mediator_12mo_nonopioid_nonoverlap_gaba, mediator_12mo_nonopioid_nonoverlap_mrelax,
           mediator_12mo_nonopioid_nonoverlap_antiinflamatory, mediator_12mo_nonopioid_nonoverlap_antidepressant)

saveRDS(nonoverlap, file.path(drv_root, "mediator_12mo_nonoverlap.rds"))
