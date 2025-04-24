# mediator_high_dose_longer_duration_mme
# mediator_opioid_benzo_copresc
# mediator_opioid_gaba_copresc
# mediator_opioid_mrelax_copresc
# mediator_nonopioid_nonoverlap_mrelax_gaba_benzo
# mediator_nonopioid_nonoverlap_antidepressant_antiinflamatory
# mediator_has_physical_therapy

# READ ME -----------------------------------------------------------------
#
#       Author: Anton
#
# Creates an indicator variable for whether or not an observation in
#   the analysis cohort had a claim for physical therapy
#   during the mediator period.
# 
# -------------------------------------------------------------------------

library(arrow)
library(dplyr)
library(lubridate)
library(data.table)
library(yaml)

# setwd("projects/mediation_unsafe_pain_mgmt")

src_root <- "/mnt/processed-data/disability"
drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

mediator <- "Physical therapy"

# Read in OTL (Other services line) 
files <- paste0(list.files(src_root, pattern = "TAFOTL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
otl <- open_dataset(file.path(src_root, parquet_files))

# Read in cohort and dates
cohort <- readRDS(file.path(drv_root, "mediation_analysis_df_subsetted.rds"))
setDT(cohort)
setkey(cohort, BENE_ID)
cohort <- cohort[, .(BENE_ID, washout_cal_end_dt)]

# Read in CPT, HCPC, and Modifier codes for mediator claims
codes <- read_yaml("~/disability_NEW/projects/mediation_unsafe_pain_mgmt/01_create_mediators/mediator_codes.yml")
codes <- c(names(codes[[mediator]]$CPT), 
           names(codes[[mediator]]$HCPC), 
           names(codes[[mediator]]$Modifiers))

# Filter OTL to claims codes
claims_vars <- c("BENE_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "LINE_PRCDR_CD_SYS", "LINE_PRCDR_CD")
claims <- select(otl, all_of(claims_vars)) |> 
    filter(LINE_PRCDR_CD %in% codes) |>
    collect()

setDT(claims)
setkey(claims, BENE_ID)

claims[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
                                     LINE_SRVC_END_DT, 
                                     LINE_SRVC_BGN_DT)]

# Inner join with cohort 
claims <- unique(merge(claims, cohort, by = "BENE_ID"))

# Filter to claims within mediator time-frame
claims <- claims[LINE_SRVC_BGN_DT %within% interval(washout_cal_end_dt, 
                                                    washout_cal_end_dt + days(182)), 
                 .(BENE_ID, LINE_SRVC_BGN_DT, washout_cal_end_dt, LINE_PRCDR_CD)]

# # Create indicator variable for whether or not a patient had claim in mediator period
# # Right join with cohort
# claims <- claims[, .(mediator_has_physical_therapy = as.numeric(.N > 0), 
#                      mediator_count_physical_therapy_claims = .N), by = "BENE_ID"]
# claims <- merge(claims, cohort[, .(BENE_ID)], all.y = TRUE, by = "BENE_ID")
# 
# # Convert NAs to 0 for observations in the cohort that didn't have a PT claim
# fix <- c("mediator_has_physical_therapy", "mediator_count_physical_therapy_claims")
# claims[, (fix) := lapply(.SD, \(x) fifelse(is.na(x), 0, x)), .SDcols = fix]

claims2 <- claims[, .(physical_therapy_dt = min(LINE_SRVC_BGN_DT)), by = BENE_ID] 

saveRDS(claims2, "/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_first_dates/physical_therapy_first_dates.rds")
