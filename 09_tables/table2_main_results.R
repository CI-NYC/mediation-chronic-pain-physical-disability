library(tidyverse)
library(xtable)

df <- readRDS("~/disability_NEW/projects/mediation_unsafe_pain_mgmt/08_results/group_mediators_icd.rds") |>
    filter(Mediator != "ATE") |>
    mutate(exposure = factor(exposure, levels = c("Physical Disability Only vs. Neither", "Chronic Pain Only vs. Neither")),
           type = factor(type, levels = c("No Baseline Mood or Anxiety Disorder", "Baseline Mood or Anxiety Disorder")),
           Mediator = factor(Mediator, levels = c("High Dose, Long Duration MME",
                                                  "Benzodiazepine Co-presc w/ Opioid",
                                                  "Gabapentinoid Co-presc w/ Opioid",
                                                  "Muscle Relaxant Co-presc w/ Opioid",
                                                  "Benzo/Gaba/Muscle Relaxant Presc w/o Opioid",
                                                  "Antidepressant/Anti-inflamatory Presc w/o Opioid",
                                                  "Physical Therapy"))) |>
    arrange(exposure, type, Mediator)

# Function to create concatenated column
concat_cols <- function(df, rows, col_main, col_main_low, col_main_high) {
  sapply(rows, function(row) {
    paste0(round(df[row, col_main], 4), " (", round(df[row, col_main_low], 4), ", ", round(df[row, col_main_high], 4), ")")
  })
}

# Row mappings for columns 1-4
rows_1_7 <- 8:14
rows_8_14 <- 1:7

# Row mappings for columns 5-8
rows_15_21 <- 22:28
rows_22_28 <- 15:21

table_df <- data.frame(
  mediator = df$Mediator[1:14],
  ate = c(concat_cols(df, rows_1_7, "ate", "ci_ate_low", "ci_ate_high"), 
            concat_cols(df, rows_8_14, "ate", "ci_ate_low", "ci_ate_high")),
  total = c(concat_cols(df, rows_1_7, "ate", "ci_total_low", "ci_total_high"), 
            concat_cols(df, rows_8_14, "total", "ci_total_low", "ci_total_high")),
  indirect = c(concat_cols(df, rows_1_7, "indirect", "ci_indirect_low", "ci_indirect_high"), 
               concat_cols(df, rows_8_14, "indirect", "ci_indirect_low", "ci_indirect_high")),
  share = round(unlist(c(df[rows_1_7, "indirect"] / df[rows_1_7, "total"], 
                         df[rows_8_14, "indirect"] / df[rows_8_14, "total"])), 4),
  ate_pain = c(concat_cols(df, rows_15_21, "ate", "ci_ate_low", "ci_ate_high"), 
                 concat_cols(df, rows_22_28, "ate", "ci_ate_low", "ci_ate_high")),
  total_pain = c(concat_cols(df, rows_15_21, "total", "ci_total_low", "ci_total_high"), 
                 concat_cols(df, rows_22_28, "total", "ci_total_low", "ci_total_high")),
  indirect_pain = c(concat_cols(df, rows_15_21, "indirect", "ci_indirect_low", "ci_indirect_high"), 
                    concat_cols(df, rows_22_28, "indirect", "ci_indirect_low", "ci_indirect_high")),
  share_pain = round(unlist(c(df[rows_15_21, "indirect"] / df[rows_15_21, "total"], 
                              df[rows_22_28, "indirect"] / df[rows_22_28, "total"])), 4)
)

# Generate the LaTeX table
latex_table <- xtable(table_df, digits = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4))


df2 <- readRDS("~/disability_NEW/projects/mediation_unsafe_pain_mgmt/08_results/all_mediators_results_icd.rds") |>
    mutate(exposure = factor(exposure, levels = c("Physical Disability Only vs. Neither", "Chronic Pain Only vs. Neither")),
           type = factor(type, levels = c("No Baseline Mood or Anxiety Disorder", "Baseline Mood or Anxiety Disorder"))) |>
    arrange(type, exposure)

all_mediators <- data.frame(
  type = c("mood", "no mood"),
  ate = c(concat_cols(df2, 3, "ate", "ci_ate_low", "ci_ate_high"), 
          concat_cols(df2, 1, "ate", "ci_ate_low", "ci_ate_high")),
   total = c(concat_cols(df2, 3, "total", "ci_total_low", "ci_total_high"), 
              concat_cols(df2, 1, "total", "ci_total_low", "ci_total_high")),
  indirect = c(concat_cols(df2, 3, "indirect", "ci_indirect_low", "ci_indirect_high"),
               concat_cols(df2, 1, "indirect", "ci_indirect_low", "ci_indirect_high")),
  share = round(unlist(c(df2[3, "indirect"] / df2[3, "total"],
                         df2[1, "indirect"] / df2[1, "total"])), 4),
  ate_pain = c(concat_cols(df2, 4, "ate", "ci_ate_low", "ci_ate_high"),
               concat_cols(df2, 2, "ate", "ci_ate_low", "ci_ate_high")),
  total_pain = c(concat_cols(df2, 4, "total", "ci_total_low", "ci_total_high"),
                  concat_cols(df2, 2, "total", "ci_total_low", "ci_total_high")),
  indirect_pain = c(concat_cols(df2, 4, "indirect", "ci_indirect_low", "ci_indirect_high"),
                    concat_cols(df2, 2, "indirect", "ci_indirect_low", "ci_indirect_high")),
  share_pain = round(unlist(c(df2[4, "indirect"] / df2[4, "total"],
                              df2[2, "indirect"] / df2[2, "total"])), 4)
)

# Generate the LaTeX table
latex_table_all_mediators <- xtable(all_mediators, digits = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4))
