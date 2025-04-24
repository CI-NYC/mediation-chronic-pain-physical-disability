# READ ME -----------------------------------------------------------------
#
#       Author: Shodai Inose
#      Created: 6 Jun 2024
#       Edited: 25 Feb 2025
#
# -------------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(readxl)
library(arrow)
library(lubridate)
library(data.table)
library(tictoc)
library(foreach)
library(doFuture)
library(furrr)
library(zoo)

data_dir <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"
proj_dir <- "projects/mediation_unsafe_pain_mgmt"

# Read in cohort and dates
dts_cohorts <- readRDS("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds")
setDT(dts_cohorts)
dts_cohorts <- dts_cohorts[, .(BENE_ID, washout_cal_end_dt)]
setkey(dts_cohorts, BENE_ID)

all_opioids_clean <- readRDS(file.path(data_dir, "all_pain_opioids.rds"))
setDT(all_opioids_clean)
setkey(all_opioids_clean, BENE_ID)
all_opioids_clean <- all_opioids_clean[, .(BENE_ID, NDC, opioid, mme_strength_per_day, rx_start_dt, rx_end_dt)]

# Merge to the analysis cohort
all_opioids_clean_merged <- merge(all_opioids_clean, dts_cohorts, by = "BENE_ID")

# Filter opioid prescriptions to only contain those within mediator period
all_opioids_clean_mediator_period <- all_opioids_clean_merged[
    all_opioids_clean_merged$rx_start_dt %within% interval(
        all_opioids_clean_merged$washout_cal_end_dt, all_opioids_clean_merged$washout_cal_end_dt + days(182)
    ), 
]

# Calculate cumulative daily dose -----------------------------------------------------

# Group by beneficiary and create a list column containing each beneficiary's data
opioids <- all_opioids_clean_mediator_period[, list(data = list(data.table(.SD))), by = BENE_ID]

calculate_total_daily_dose <- function(data) {
    to_modify <- copy(data)
    
    # to_modify[, c("rx_start_dt", "rx_end_dt") := lapply(.SD, as.Date), 
    #           .SDcols = c("rx_start_dt", "rx_end_dt")]
    
    # Calculate the date limit based on washout_cal_end_dt + 182 days
    washout_date_limit <- to_modify$washout_cal_end_dt + lubridate::days(182)
    
    long <- to_modify[, .(date = seq(rx_start_dt, rx_end_dt, by = "1 day"), 
                          NDC, opioid, mme_strength_per_day), by = .(seq_len(nrow(data)))
    ][date <= washout_date_limit, ]  # Filter rows based on date limit
    
    long[, .(total_mme_strength = sum(mme_strength_per_day, na.rm = TRUE)), by = .(date)
    ]#[, .(mediator_max_daily_dose_mme = max(total_mme_strength))]
}

plan(multisession, workers = 10)

# Apply function
out <- foreach(data = opioids$data, 
               id = opioids$BENE_ID, 
               .combine = "rbind",
               .options.future = list(chunk.size = 1e4)) %dofuture% {
                   out <- calculate_total_daily_dose(data)
                   out$BENE_ID <- id
                   setcolorder(out, "BENE_ID")
                   out
               }

plan(sequential)

ids <- out$BENE_ID

dates <- dts_cohorts |>
    filter(BENE_ID %in% ids) |>
    select(BENE_ID, washout_cal_end_dt)

rm(ids)

dates_complete <- map_dfr(split(dates, dates$BENE_ID),
                          function(x) {
                              tibble(BENE_ID = x$BENE_ID[1],
                                     when = seq(min(x$washout_cal_end_dt), min(x$washout_cal_end_dt) + days(182), by = "day"))
                          })

out_complete <- dates_complete |>
    rename("date" = "when") |>
    left_join(out) |> 
    mutate(total_mme_strength = ifelse(is.na(total_mme_strength), 0, total_mme_strength))

out_complete <- out_complete |>
    left_join((dts_cohorts |> select(BENE_ID, washout_cal_end_dt)), by = c('BENE_ID' = 'BENE_ID')) |>
    filter(date >= washout_cal_end_dt,
           date <= washout_cal_end_dt + lubridate::days(182)) # only dates in months 7-12
    

# function to check for periods of > 50 mme total daily dose for > 7 consecutive days
consecutive_days <- function(data) {
    data |>
        group_by(BENE_ID) |>
        arrange(date) |>
        mutate(dose_check = ifelse(total_mme_strength > 50, 1, 0),
               consecutive_days = rollapply(dose_check, width = 8, by = 1, align = 'right', FUN = sum, fill = NA)) |> # checks for 8 consecutive days
        filter(consecutive_days == 8) |>
        distinct(BENE_ID) |>
        mutate(mediator_high_dose_longer_duration_mme = 1)
}

cohort_longer_duration_COMPLETE <- consecutive_days(out_complete)

final <- merge(cohort_longer_duration_COMPLETE, dts_cohorts[, .(BENE_ID)], all.y = TRUE, by = "BENE_ID") |>
    as.data.table()

# Convert NAs to 0 for observations in the cohort that didn't have consecutive high MME
final[, mediator_high_dose_longer_duration_mme := fifelse(is.na(mediator_high_dose_longer_duration_mme), 0, mediator_high_dose_longer_duration_mme)]

# Save final dataset -----------------------------------------------------------

saveRDS(final, file.path(data_dir, "mediator_high_dose_longer_duration_mme.rds"))



