# mediator_opioid_benzo_copresc

# READ ME -----------------------------------------------------------------
#
# Author: Anton
#
# Creates variables for whether or not an observation in
#   the analysis cohort had a claim for stimulant, benzodiazepine, 
#   gabapentinoid and/or muscle relaxant coprescription
# -------------------------------------------------------------------------

library(data.table)
library(purrr)
library(dplyr)
library(tidyr)
library(doFuture)
library(lubridate)

library(tidyverse)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

disability <- readRDS(paste0(drv_root, "/subset_disability_only_ref_df.rds")) |>
    select(BENE_ID, exposure_disability_only_subset)

# Analysis cohort

cohort <- readRDS(file.path(drv_root, "mediation_analysis_df.rds")) |>
    left_join(disability) |>
    mutate(keep = case_when(disability_pain_cal == "disability only" & is.na(exposure_disability_only_subset) ~ 0,
                            disability_pain_cal == "disability and chronic pain" ~ 0,
                            TRUE ~ 1)) |>
    filter(keep == 1) |> 
    select(-c(keep, exposure_disability_only_subset))

saveRDS(cohort, file.path(drv_root, "mediation_analysis_df_subsetted.rds"))

rm(disability)
# RXL datasets
opioid <- readRDS(file.path(drv_root, "mediator_rxl_opioid_pain_rx.rds"))
benzo <- readRDS(file.path(drv_root, "mediator_rxl_benzo_rx.rds"))
stimulant <- readRDS(file.path(drv_root, "mediator_rxl_stimulant_rx.rds"))
gaba <- readRDS(file.path(drv_root, "mediator_rxl_gabapentinoid_rx.rds"))
mrelax <- readRDS(file.path(drv_root, "mediator_rxl_muscle_relaxant_rx.rds"))

prop_days_covered <- function(data) {
    dur <- 0
    current_int <- data$rx_int[1]
    for (i in 1:nrow(data)) {
        check <- intersect(current_int, data$rx_int[i + 1])
        if (is.na(check)) {
            # if they don't intersect, add the duration of the first interval
            dur <- dur + as.duration(current_int)
            current_int <- data$rx_int[i + 1]
        } else {
            # if they do intersect, then update current interval as the union
            current_int <- union(current_int, data$rx_int[i + 1])
        }
    }
    
    min(max(time_length(dur, "days") / 182, 1 / 182), 1)
}

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

# Create datasets---------------------------------------------------------------

id <- "BENE_ID"
vars <- c("RX_FILL_DT", "DAYS_SUPPLY")
keep <- c(id, vars)
throwaway <- setdiff(names(opioid), keep)

opioid[, opioids_end_date := RX_FILL_DT + DAYS_SUPPLY
][, (throwaway) := lapply(.SD, \(x) NULL), .SDcols = throwaway
][, opioids_end_date := 
      fifelse(is.na(opioids_end_date), RX_FILL_DT + days(1), opioids_end_date)]
setnames(opioid, vars, c("opioids_fill_date", "opioids_days_supply"))

throwaway <- setdiff(names(benzo), keep)

benzo[, benzo_end_date := RX_FILL_DT + DAYS_SUPPLY
][, (throwaway) := lapply(.SD, \(x) NULL), .SDcols = throwaway
]
benzo[, benzo_end_date := 
          fifelse(is.na(benzo_end_date), RX_FILL_DT + days(1), benzo_end_date)]
setnames(benzo, vars, c("benzo_fill_date", "benzo_days_supply"))

throwaway <- setdiff(names(stimulant), keep)

stimulant[, stimulant_end_date := RX_FILL_DT + DAYS_SUPPLY
][, (throwaway) := lapply(.SD, \(x) NULL), .SDcols = throwaway
]
stimulant[, stimulant_end_date := 
              fifelse(is.na(stimulant_end_date), RX_FILL_DT + days(1), stimulant_end_date)]
setnames(stimulant, vars, c("stimulant_fill_date", "stimulant_days_supply"))

throwaway <- setdiff(names(gaba), keep)

gaba[, gaba_end_date := RX_FILL_DT + DAYS_SUPPLY
][, (throwaway) := lapply(.SD, \(x) NULL), .SDcols = throwaway
]
gaba[, gaba_end_date := 
         fifelse(is.na(gaba_end_date), RX_FILL_DT + days(1), gaba_end_date)]
setnames(gaba, vars, c("gaba_fill_date", "gaba_days_supply"))

throwaway <- setdiff(names(mrelax), keep)

mrelax[, mrelax_end_date := RX_FILL_DT + DAYS_SUPPLY
][, (throwaway) := lapply(.SD, \(x) NULL), .SDcols = throwaway
]
mrelax[, mrelax_end_date := 
           fifelse(is.na(mrelax_end_date), RX_FILL_DT + days(1), mrelax_end_date)]
setnames(mrelax, vars, c("mrelax_fill_date", "mrelax_days_supply"))

# Create co-prescription variables ----------------------------------------

# plan(multisession, workers = 50)

## Opioids and benzos
opioids_benzos <- 
    merge(opioid, benzo, by = "BENE_ID") |> 
    as_tibble() |> 
    mutate(benzos_interval = interval(benzo_fill_date, benzo_end_date), 
           opioids_interval = interval(opioids_fill_date, opioids_end_date), 
           overlap = int_overlaps_numeric(benzos_interval, opioids_interval)) |> 
    rowwise() |> 
    mutate(first_drug = c("opioids", "benzo")[which_min(opioids_fill_date, benzo_fill_date)]) |> 
    ungroup() |> 
    filter((first_drug == "benzo" & 
                benzo_days_supply > 5 & 
                overlap / benzo_days_supply >= 0.25) |
               (first_drug == "opioids" & 
                    opioids_days_supply > 5 & 
                    overlap / opioids_days_supply >= 0.25)) |> 
    mutate(rx_int = intersect(benzos_interval, opioids_interval)) #|> 
# group_by(BENE_ID) |> 
# nest()

opioids_benzos2 <- opioids_benzos |>
    group_by(BENE_ID) |>
    summarise(opioid_benzo_copresc_first_dt = min(int_start(rx_int)))


## Opioids and stimulants
opioids_stimulants <- 
    merge(opioid, stimulant, by = "BENE_ID") |> 
    as_tibble() |> 
    mutate(stim_interval = interval(stimulant_fill_date, stimulant_end_date), 
           opioids_interval = interval(opioids_fill_date, opioids_end_date), 
           overlap = int_overlaps_numeric(stim_interval, opioids_interval)) |> 
    rowwise() |> 
    mutate(first_drug = c("opioids", "stim")[which_min(opioids_fill_date, stimulant_fill_date)]) |> 
    ungroup() |> 
    filter((first_drug == "stim" & 
                stimulant_days_supply > 5 & 
                overlap / stimulant_days_supply >= 0.25) |
               (first_drug == "opioids" & 
                    opioids_days_supply > 5 & 
                    overlap / opioids_days_supply >= 0.25)) |> 
    mutate(rx_int = intersect(stim_interval, opioids_interval))
# group_by(BENE_ID) |> 
# nest()

opioids_stimulants2 <- opioids_stimulants |>
    group_by(BENE_ID) |>
    summarise(opioid_stimulants_copresc_first_dt = min(int_start(rx_int)))

## Opioids and gabapentinoids
opioids_gabas <- 
    merge(opioid, gaba, by = "BENE_ID") |> 
    as_tibble() |> 
    mutate(gaba_interval = interval(gaba_fill_date, gaba_end_date), 
           opioids_interval = interval(opioids_fill_date, opioids_end_date), 
           overlap = int_overlaps_numeric(gaba_interval, opioids_interval)) |> 
    rowwise() |> 
    mutate(first_drug = c("opioids", "gaba")[which_min(opioids_fill_date, gaba_fill_date)]) |> 
    ungroup() |> 
    filter((first_drug == "gaba" & 
                gaba_days_supply > 5 & 
                overlap / gaba_days_supply >= 0.25) |
               (first_drug == "opioids" & 
                    opioids_days_supply > 5 & 
                    overlap / opioids_days_supply >= 0.25)) |> 
    mutate(rx_int = intersect(gaba_interval, opioids_interval)) #|> 
# group_by(BENE_ID) |> 
# nest()

opioids_gabas2 <- opioids_gabas |>
    group_by(BENE_ID) |>
    summarise(opioid_gabas_copresc_first_dt = min(int_start(rx_int)))


##  Opioids and muscle relaxants
opioids_mrelax <- 
    merge(opioid, mrelax, by = "BENE_ID") |> 
    as_tibble() |> 
    mutate(mrelax_interval = interval(mrelax_fill_date, mrelax_end_date), 
           opioids_interval = interval(opioids_fill_date, opioids_end_date), 
           overlap = int_overlaps_numeric(mrelax_interval, opioids_interval)) |> 
    rowwise() |> 
    mutate(first_drug = c("opioids", "mrelax")[which_min(opioids_fill_date, mrelax_fill_date)]) |> 
    ungroup() |> 
    filter((first_drug == "mrelax" & 
                mrelax_days_supply > 5 & 
                overlap / mrelax_days_supply >= 0.25) |
               (first_drug == "opioids" & 
                    opioids_days_supply > 5 & 
                    overlap / opioids_days_supply >= 0.25)) |> 
    mutate(rx_int = intersect(mrelax_interval, opioids_interval)) #|> 

opioids_mrelax2 <- opioids_mrelax |>
    group_by(BENE_ID) |>
    summarise(opioids_mrelax_copresc_first_dt = min(int_start(rx_int)))

# plan(sequential)

# Merge datasets ---------------------------------------------------------------

# walk(list(opioids_benzos2, opioids_stimulants2, opioids_gabas2, opioids_mrelax2), 
#      setDT)
# 
# walk(list(opioids_benzos, opioids_stimulants2, opioids_gabas2, opioids_mrelax2), 
#      \(x) setkey(x, BENE_ID))

coprescriptions <- 
    reduce(list(opioids_benzos2, 
                # opioids_stimulants2, 
                opioids_gabas2, 
                opioids_mrelax2), merge, all = TRUE)

# coprescriptions[is.na(coprescriptions)] <- 0

saveRDS(coprescriptions, "/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_first_dates/copresc_first_dates.rds")
