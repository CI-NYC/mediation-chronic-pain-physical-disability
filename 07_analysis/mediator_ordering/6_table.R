# READ ME -----------------------------------------------------------------
#
#       Author: Anton
# 
# -------------------------------------------------------------------------

library(tidyverse)
library(knitr)

high_dose <- readRDS("/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_first_dates/high_dose_first_dates.rds")
copresc <- readRDS("/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_first_dates/copresc_first_dates.rds")
mrelax_gaba_benzo <- readRDS("/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_first_dates/nonoverlap_mrelax_gaba_benzo_first_dt.rds")
antiinflammatory_antidepressant <- readRDS("/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_first_dates/nonoverlap_ai_ad_first_dt.rds")
physical_therapy <- readRDS("/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_first_dates/physical_therapy_first_dates.rds")

joined_dates <- high_dose |>
    full_join(copresc) |>
    full_join(mrelax_gaba_benzo) |>
    full_join(antiinflammatory_antidepressant) |>
    full_join(physical_therapy)


# Create matrices for "before" and "after"
before_matrix <- matrix(0, nrow = 7, ncol = 7)  # Initialize 7x7 matrix
after_matrix <- matrix(0, nrow = 7, ncol = 7)   # Initialize 7x7 matrix

# Populate "before_matrix"
for (i in 2:8) {
    for (j in 2:8) {
        if (i == j) {
            next
        }
        before_matrix[i-1, j-1] <- round(mean(joined_dates[[i]] <= joined_dates[[j]], na.rm=T),2)
        # break
    }
    #break
}

# # Populate "after_matrix"
# for (j in 2:8) {
#   for (i in 2:8) {
#     if (i == j) {
#       next
#     }
#     subset <- joined_dates[which(!is.na(joined_dates[[i]]) & 
#                                    !is.na(joined_dates[[j]]))]
#     x <- round(sum(subset[[j]] > subset[[i]]) / nrow(subset), 2)
#     after_matrix[i-1, j-1] <- x
#     # break
#   }
#   # break
# }

# what happens to those where the date is equal?

# result: 6 proportions * 7 mediators (before) and 6 proportions * 7 mediators (after)
# two 7x7 matrices


# high dose = first date that they have an MMe higher than x (maybe 50)
# 
# copresc = first day that they have an overlapping benzo and opioid prescription
# 
# w/o opioid = first day they have any of these non-opioids not overlapping with an opioid
# 
# Compare dates only AMONG THOSE PEOPLE WHO HAVE A DATE in both i and j.
# 
# don't add to either category if they land on the same day


# colnames(before_matrix) <- c("Occurs before high dose",
#                              "Occurs before benzodiazepine coprescription",
#                              "Occurs before gabapentin coprescription",
#                              "Occurs before muscle relaxant coprescription",
#                              "Occurs before mrelax, gaba, benzo w/o copresc",
#                              "Occurs before antiinflammatory, antidepressant w/o copresc",
#                              "Occurs before physical therapy")

rownames(before_matrix) <- c("High dose",
                             "Benzodiazepine copresc",
                             "Gabapentin copresc",
                             "Muscle relaxant copresc",
                             "Mrelax, gaba, benzo w/o copresc",
                             "Anti-inflam, Antidep w/o copresc",
                             "Physical therapy")

colnames(before_matrix) <- c("High dose",
                             "Benzodiazepine copresc",
                             "Gabapentin copresc",
                             "Muscle relaxant copresc",
                             "Mrelax, gaba, benzo w/o copresc",
                             "Anti-inflam, Antidep w/o copresc",
                             "Physical therapy")

write.csv(before_matrix, "~/mediator_dates.csv")

tbl <- kable(before_matrix, format = "latex")

print(tbl)

