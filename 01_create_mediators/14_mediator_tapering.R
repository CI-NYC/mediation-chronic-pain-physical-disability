# READ ME -----------------------------------------------------------------
#
#       Author: Kat Hoffman
# Last updated: 30 January 2024 (Nick)
# 
# -------------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(arrow)
library(lubridate)
library(data.table)
library(tictoc)
library(doFuture)
library(foreach)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

mme_months <- readRDS(file.path(drv_root, "mediator_average_daily_dose_mme_per_month.rds"))
# mme_months <- readRDS(file.path(drv_root, "mme_month_summary.rds"))

cens_dts <- readRDS("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds")
setDT(cens_dts)
cens_dts <- cens_dts[, .(BENE_ID, washout_start_dt, censoring_dts_cal_dt)]

tapering <- copy(mme_months)

# Tapering logic using only MME from the mediator window
# comparing first 2 mediator months to the consecutive 4.
tapering$period1_dose <- rowMeans(tapering[, .(mediator_avg_daily_dose_mme_month1, mediator_avg_daily_dose_mme_month2)])
tapering$period2_dose <- rowMeans(tapering[, .(mediator_avg_daily_dose_mme_month3, mediator_avg_daily_dose_mme_month4)])
tapering$period3_dose <- rowMeans(tapering[, .(mediator_avg_daily_dose_mme_month5, mediator_avg_daily_dose_mme_month6)])

# # Tapering logic using only MME from the mediator window
# # comparing first 2 mediator months to the consecutive 4.
# mme_windows <- reduce(
#     list(mme_months[months_from_washout >= 6 & months_from_washout <= 7, 
#                     .(period1_dose = mean(avg_30d_opioid_dose)), BENE_ID],
#          mme_months[months_from_washout >= 8 & months_from_washout <= 9, 
#                     .(period2_dose = mean(avg_30d_opioid_dose)), BENE_ID],
#          mme_months[months_from_washout >= 10 & months_from_washout <= 11, 
#                     .(period3_dose = mean(avg_30d_opioid_dose)), BENE_ID]), 
#     merge, all.x = TRUE, all.y = TRUE
# )

# mme_windows[is.na(mme_windows)] <- 0

tapering[, tapering := fcase(period1_dose < 50, 0, 
                             period1_dose >= 50 & 
                                 period2_dose <= period1_dose*0.85 & 
                                 period3_dose <= period1_dose*0.85, 1, 
                             default = 0)]

#  figure out which beneficiares can't have tapering because not in study long enough
cens_dts[, days_from_washout := difftime( censoring_dts_cal_dt, washout_start_dt, units = "day")
         ][, months_to_cens := as.numeric(ceiling(days_from_washout / 30))
           ][, cens_tapering := fifelse(months_to_cens <= 10, 1, 0)]

tapering <- merge(cens_dts, tapering, all.x = TRUE)

tapering <- tapering[, .(BENE_ID, 
                         mediator_has_tapering = fcase(tapering == 1, 1, 
                                                       cens_tapering == 1, NA_real_, 
                                                       default = 0))]

saveRDS(tapering, file.path(drv_root, "mediator_has_tapering.rds"))

# # Tapering logic
# # 1. Take average of months 1-6 (baseline), and months 7-8 (months 1-2 of mediator window) then look to see if:
# # a. baseline >= 50 mme AND 
# # b. >= 15% reduction for months 7-8 as compared to months 1-6 for two consecutive periods (so for 7-8, 9-10)
# # 2. Repeat (1) for:
# # a. baseline: 2-7, consecutive months 8-9 and 10-11
# # b. baseline 3-8, consecutive months 9-10 and 11-12
# # 3. if any tapering in the 3 variables in the mediator window, then tapering =1, or if not tapering =0
# compute_taper <- function(df, month_start) {
#     
#     # get opioid scripts in baseline months
#     baseline_df <-
#         df |>
#         filter(months_from_washout >= month_start,
#                months_from_washout <= month_start + 6) |>
#         group_by(BENE_ID) |>
#         summarize(baseline = mean(avg_30d_opioid_dose)) # take the average over all baseline months
#     
#     # get opioid scripts in next two 2-month periods
#     next_two_months1_df <- df |>
#         filter(months_from_washout >= month_start + 7,
#                months_from_washout <= month_start + 8) |>
#         group_by(BENE_ID) |>
#         summarize(next_two_months1 = mean(avg_30d_opioid_dose)) # take the average (over 2 months)
#     
#     next_two_months2_df <- df |>
#         filter(months_from_washout >= month_start + 9,
#                months_from_washout <= month_start + 10) |>
#         group_by(BENE_ID) |>
#         summarize(next_two_months2 = mean(avg_30d_opioid_dose)) # take the average (over 2 months)
#     
#     # combine the datasets
#     check <- full_join(baseline_df, next_two_months1_df) |>
#         full_join(next_two_months2_df) |>
#         mutate(next_two_months1 = ifelse(is.na(next_two_months1), 0, 1), # if not in the next two months data sets, their 2 month average dose is 0
#               next_two_months2 = ifelse(is.na(next_two_months2), 0, 1),
#               taper = case_when(baseline < 50 ~ 0, # if avg baseline MME < 50, no tapering
#                                  is.na(baseline) ~ 0, # if missing baseline opioids, no tapering
#                                  # tapering if both the consecutive two month periods are less than 85% (a 15% reduction) from baseline
#                                 (next_two_months1 <= baseline * .85) & 
#                                     (next_two_months2 <= baseline * .85) ~ 1,
#                                  TRUE ~ 0) # 
#                )
#     
#     return(check)
# }
# 
# # run tapering calculation for all three time periods
# tic()
# taper1 <- compute_taper(all_months_summary, 1) |> rename(taper1 = taper) # months 1-6 as baseline
# taper2 <- compute_taper(all_months_summary, 2) |> rename(taper2 = taper) # months 2-7 as baseline
# taper3 <- compute_taper(all_months_summary, 3) |> rename(taper3 = taper) # months 3-8 as baseline
# toc()
# 
# # combine intermediate tapering data sets
# taper1 <- taper1 |> select(BENE_ID, taper1) # months 1-6 as baseline
# taper2 <- taper2 |> select(BENE_ID, taper2) # months 2-7 as baseline
# taper3 <- taper3 |> select(BENE_ID, taper3) # months 3-8 as baseline
# 
# # figure out which beneficiares can't have tapering because not in study long enough
# cens_df <- cens_dts |> mutate(
#     days_from_washout = difftime( censoring_dts_cal_dt, washout_start_dt, units = "day"),
#     months_to_cens = as.numeric(ceiling(days_from_washout / 30))) |>
#     mutate(cens_tapering = ifelse(months_to_cens <= 10, 1, 0))
# 
# # if any tapering variable is 1, then tapering for entire mediator period is 1
# tapering <- cens_df |>
#     left_join(taper1) |>
#     left_join(taper2) |>
#     left_join(taper3) |>
#     # Not considering tapering from baseline to first period
#     mutate(mediator_has_tapering = case_when(# taper1 == 1 ~ 1,
#                                              taper2 == 1 ~ 1,
#                                              taper3 == 1 ~ 1,
#                                              cens_tapering == 1 ~ NA_real_,
#                                              TRUE ~ 0
#                                              )) |>
#     select(BENE_ID, mediator_has_tapering)
# 
# saveRDS(tapering, file.path(drv_root, "mediator_has_tapering.rds"))
