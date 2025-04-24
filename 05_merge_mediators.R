# READ ME -----------------------------------------------------------------
#
# Author: Sarah Forrest
# Created: 2023-07-28
# Last updated: 2024-06-20 (Shodai)
#
# Output: Data set with all mediation variables 
# 
# -------------------------------------------------------------------------

library(data.table)
library(purrr)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Read in mediator datasets ----------------------------------------------------

# Create a list of mediator datasets
mediator_files <- c(
    # Opioid pain management
    # Opioid pain medication
    "mediator_opioid_pain_rx_bin.rds",
    # Dose, Duration
    "mediator_max_daily_dose_mme.rds",
    "mediator_high_dose_longer_duration_mme.rds",
    # High-risk opioid prescribing practices
    "mediator_prescribers_per_month.rds",             # 1 variable for each month (1-6) and 1 overall variable for throughout the mediator period
    "mediator_average_daily_dose_mme_per_month.rds",  # 1 variable for avg daily dose for each month (1-6)
    "mediator_average_daily_dose_mme_overall.rds",    # 1 overall variable for avg daily dose for months throughout the mediator period with an opioid prescription
    # Co-prescription
    "mediator_opioid_coprescriptions.rds",
    # Tapering
    "mediator_has_tapering.rds",
    # Non-opioid pain medication
    "mediator_nonopioid_pain_rx_bin.rds",
    "mediator_antiinflammatory_rx_bin.rds",
    "mediator_benzo_rx_bin.rds",
    "mediator_stimulant_rx_bin.rds",
    "mediator_gabapentinoid_rx_bin.rds",
    "mediator_muscle_relaxant_rx_bin.rds",
    "mediator_antidepressant_rx_bin.rds",
    "mediator_nonoverlap.rds",
    # Physical therapy
    "mediator_has_physical_therapy.rds",
    # Other categories for multimodal/multidisciplinary pain treatment
    "mediator_has_massage_therapy.rds",
    "mediator_has_chiropractic.rds",
    "mediator_has_ablative_techniques.rds",
    "mediator_has_acupuncture.rds",
    "mediator_has_blocks.rds",
    "mediator_has_botulinum_toxin.rds",
    "mediator_has_electrical_nerve_stimulation.rds",
    "mediator_has_epidural_steroid.rds",
    "mediator_has_intrathecal_drug_therapy.rds",
    "mediator_has_minimally_invasive_spinal_procedure.rds",
    "mediator_has_trigger_point_injection.rds",
    # Counseling
    "mediator_has_counseling.rds",
    # Proportion of days during mediator period with opioid prescription
    "mediator_proportion_days_opioid.rds"
)

# Read in all mediator datasets from list above
mediator_list <- lapply(mediator_files, function(file) {
    data <- readRDS(file.path(drv_root, file))
    setDT(data)
    setkey(data, BENE_ID)
    data[]
})

# Read in opioid pain duration dataset and select only necessary variables
mediator_months_opioid_rx <- readRDS(file.path(drv_root, "mediator_months_opioid_prescription.rds"))

setDT(mediator_months_opioid_rx)
setkey(mediator_months_opioid_rx, BENE_ID)

mediator_months_opioid_rx <- mediator_months_opioid_rx[, .(BENE_ID, mediator_months_opioid_rx)]

# Merge mediator datasets ------------------------------------------------------

mediator_df <- reduce(mediator_list, merge, all.x = TRUE, all.y = TRUE)
mediator_df <- merge(mediator_df, mediator_months_opioid_rx, all.x = TRUE)

# replace NA with 0 (tapering and dose variables)
mediator_df[is.na(mediator_df)] <- 0

rm(mediator_list, mediator_months_opioid_rx)

# Create additional mediator variables -----------------------------------------

# Create an overall indicator variable for all physical therapy types
mediator_df[, mediator_has_physical_therapy_any := fcase(
    mediator_has_physical_therapy == 1, 1,
    mediator_has_massage_therapy == 1, 1,
    mediator_has_electrical_nerve_stimulation == 1, 1,
    mediator_has_chiropractic == 1, 1,
    mediator_has_acupuncture == 1, 1,
    default = 0
)]

# Create a count and indicator variable for multimodal/multidisciplinary pain treatment, 2 or more of: 
#   1) ablative techniques
#   2) acupuncture
#   3) blocks  
#   4) botulinum toxin injections
#   5) electrical nerve stimulation
#   6) epidural steroids
#   7) intrathecal drug therapies
#   8) minimally invasive spinal procedures
#   9) pharmacologic management 
#   10) physical/restorative therapy 
#   11) psychological treatment
#   12) trigger point injection
mediator_df[, mediator_has_pt_mt_chiro := 
                fifelse(mediator_has_physical_therapy + 
                            mediator_has_massage_therapy + 
                            mediator_has_chiropractic >= 1, 
                        1, 0)]

mediator_df[, mediator_count_multimodal_pain_treatment := rowSums(.SD), 
            .SDcols = c("mediator_has_ablative_techniques",
                        "mediator_has_blocks",
                        "mediator_has_botulinum_toxin",
                        "mediator_has_epidural_steroid",
                        "mediator_has_intrathecal_drug_therapy",
                        "mediator_has_minimally_invasive_spinal_procedure",
                        "mediator_nonopioid_pain_rx",
                        "mediator_has_counseling",
                        "mediator_has_trigger_point_injection", 
                        "mediator_has_pt_mt_chiro")
            ][, mediator_has_multimodal_pain_treatment := 
                  fifelse(mediator_count_multimodal_pain_treatment >= 2, 1, 0)]

# Create an indicator variable for multimodal/multidisciplinary pain treatment 
# (restricted -- counseling, physical therapy, and nonopioid pain rx removed) 1 or more of: 
#   1) ablative techniques
#   2) acupuncture
#   3) blocks  
#   4) botulinum toxin injections
#   5) electrical nerve stimulation
#   6) epidural steroids
#   7) intrathecal drug therapies
#   8) trigger point injection
#   9) massage therapy 
#   10) chiropractic
mediator_df[, mediator_has_multimodal_pain_treatment_restrict := fcase(
    mediator_has_ablative_techniques == 1, 1,
    mediator_has_acupuncture == 1, 1,
    mediator_has_blocks == 1, 1,
    mediator_has_botulinum_toxin == 1, 1,
    mediator_has_electrical_nerve_stimulation == 1, 1,
    mediator_has_epidural_steroid == 1, 1,
    mediator_has_intrathecal_drug_therapy == 1, 1,
    mediator_has_trigger_point_injection == 1, 1,
    mediator_has_massage_therapy == 1, 1,
    mediator_has_chiropractic == 1, 1, 
    default = 0
)]

mediator_df[, mediator_count_multimodal_pain_treatment_restrict := rowSums(.SD), 
            .SDcols = c("mediator_has_ablative_techniques",
                        "mediator_has_acupuncture",
                        "mediator_has_blocks",
                        "mediator_has_botulinum_toxin",
                        "mediator_has_electrical_nerve_stimulation",
                        "mediator_has_epidural_steroid",
                        "mediator_has_intrathecal_drug_therapy",
                        "mediator_has_trigger_point_injection",
                        "mediator_has_massage_therapy",
                        "mediator_has_chiropractic")]

# Save dataset
saveRDS(mediator_df, file.path(drv_root, "mediator_df.rds"))
