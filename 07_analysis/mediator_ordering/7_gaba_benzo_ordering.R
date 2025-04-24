# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(tidyverse)
library(knitr)

copresc <- readRDS("/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_first_dates/copresc_first_dates.rds") |>
    select(opioid_gabas_copresc_first_dt, opioid_benzo_copresc_first_dt)

# gaba only
copresc |>
    filter(is.na(opioid_gabas_copresc_first_dt) == FALSE,
           is.na(opioid_benzo_copresc_first_dt)) |>
    nrow() # 22,035

# benzo only
copresc |>
    filter(is.na(opioid_gabas_copresc_first_dt),
           is.na(opioid_benzo_copresc_first_dt) == FALSE) |>
    nrow() #14,818

# both
both <- copresc |>
    filter(is.na(opioid_gabas_copresc_first_dt) == FALSE,
           is.na(opioid_benzo_copresc_first_dt) == FALSE) 

both |>
    nrow()

both <- both |>
    mutate(ordering = case_when(opioid_gabas_copresc_first_dt > opioid_benzo_copresc_first_dt ~ "benzo first",
                                opioid_gabas_copresc_first_dt < opioid_benzo_copresc_first_dt ~ "gaba first",
                                opioid_benzo_copresc_first_dt == opioid_benzo_copresc_first_dt ~ "same time"))

both |>
    group_by(ordering) |>
    summarize(count = n(),
              proportion = n()/nrow(both))




