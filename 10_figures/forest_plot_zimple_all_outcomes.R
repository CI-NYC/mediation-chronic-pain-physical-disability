## ----setup, include=FALSE-----------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = F, warning = F, message = F)


## -----------------------------------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(forestplot)
library(gridExtra)
library(ggplot2)
library(patchwork)
library(stringr)


## -----------------------------------------------------------------------------------------------------------
directory <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_results_final_r1/all_mediators/expansive"

res <- list.files(directory, full.names = TRUE)

results <- 
    map(res, \(x) readRDS(x)[[1]]) |> 
    list_rbind() |> 
    round(4)

results$exposure <- str_extract(res, "(?<=results_exposure_).*?(?=.rds)")
results$type <- str_extract(res, "(?<=all_mediators_).*?(?=_results)")


## -----------------------------------------------------------------------------------------------------------
results_zimple_df <- select(results, 
       exposure, type,
       indirect, ci_indirect_low, ci_indirect_high, 
       direct, ci_direct_low, ci_direct_high, 
       ate, ci_ate_low, ci_ate_high) |> 
    mutate(
        exposure = case_when(
            exposure == "disability_only_subset" ~ "Physical Disability Only vs. Neither",
            TRUE ~ "Chronic Pain Only vs. Neither"
        ), 
        type = case_when(
            type == "depanx" ~ "Baseline Mood or Anxiety Disorder",
            TRUE ~ "No Baseline Mood or Anxiety Disorder"
        ), 
        type = factor(
            type,
            levels = c(
                "Unstratified",
                "Baseline Mood or Anxiety Disorder",
                "No Baseline Mood or Anxiety Disorder"
            )
        ), 
        exposure = factor(
            exposure,
            levels = c(
                "Physical Disability Only vs. Neither",
                "Chronic Pain Only vs. Neither"
            )
        )
    )

## -----------------------------------------------------------------------------------------------------------
zimple_plot <-
    ggplot(results_zimple_df) +
    geom_point(aes(
        x = exposure,
        y = indirect,
        shape = factor("NIE", levels = c("ATE", "NIE", "NDE"))
    ), size = 2) +
    geom_errorbar(aes(x = exposure,
                      ymin = ci_indirect_low, ymax = ci_indirect_high),
                  width = 0) +
    geom_point(aes(
        x = as.numeric(exposure) - 0.25,
        y = ate,
        shape = factor("ATE", levels = c("ATE", "NIE", "NDE"))
    ), size = 2) +
    geom_errorbar(aes(
        x = as.numeric(exposure) - 0.25,
        ymin = ci_ate_low,
        ymax = ci_ate_high
    ),
    width = 0) +
    geom_point(
        aes(
            x = as.numeric(exposure) + 0.25,
            y = direct,
            shape = factor("NDE", levels = c("ATE", "NIE", "NDE"))
        ),
        size = 2,
        position = position_dodge(width = 0.5)
    ) +
    geom_errorbar(
        aes(
            x = as.numeric(exposure) + 0.25,
            ymin = ci_direct_low,
            ymax = ci_direct_high
        ),
        width = 0,
        position = position_dodge(width = 0.5)
    ) +
  labs(y = "Effect Estimate", x = "") +
  theme_bw() + 
  theme(
  text = element_text(family = "Times", size = 10),
  panel.grid.major.x = element_blank(),  
  panel.grid.minor.x = element_blank(), 
  panel.border = element_rect(color = "black", fill = NA),  
  panel.spacing = unit(0.5, "lines"),  
  panel.background = element_blank(),
  legend.key.size = unit(0.00005, "lines"),
  legend.position = c(0.9, 0.875),
  axis.title = element_text(size = 10),
  axis.text = element_text(size = 10),
  legend.text = element_text(size = 10),
  plot.title = element_text(size = 10),
  plot.subtitle = element_text(size = 10),
  strip.text = element_text(size = 10)) +
    scale_x_discrete(labels = str_wrap(levels(results_zimple_df$exposure), width = 10)) + 
    scale_shape_manual(values = c("ATE" = 1, "NIE" = 8, "NDE" = 18),
                       breaks = c("ATE", "NIE", "NDE"),
                       labels = c("ATE", "NIE", "NDE")) +
    facet_wrap(~type, nrow = 1) +
    labs(shape = "Effect Type") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black")

zimple_plot
ggsave("~/mediation-chronic-pain-physical-disability/10_figures/final_figs/all_mediators.pdf", zimple_plot, dpi = 300, width = 7, height = 9)


## -----------------------------------------------------------------------------------------------------------
directory <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_results_final_r1/all_mediators/icd"

res <- list.files(directory, full.names = TRUE)

results_icd <- 
    map(res, \(x) readRDS(x)[[1]]) |> 
    list_rbind() |> 
    round(4)

results_icd$exposure <- str_extract(res, "(?<=results_exposure_).*?(?=.rds)")
results_icd$type <- str_extract(res, "(?<=all_mediators_).*?(?=_icd_results)")


## -----------------------------------------------------------------------------------------------------------
results_icd_zimple_df_icd <- select(results_icd, 
       exposure, type, total, ci_total_low, ci_total_high,
       indirect, ci_indirect_low, ci_indirect_high, 
       direct, ci_direct_low, ci_direct_high, 
       ate, ci_ate_low, ci_ate_high) |> 
    mutate(
        exposure = case_when(
            exposure == "disability_only_subset" ~ "Physical Disability Only vs. Neither",
            TRUE ~ "Chronic Pain Only vs. Neither"
        ), 
        type = case_when(
            type == "depanx" ~ "Baseline Mood or Anxiety Disorder",
            TRUE ~ "No Baseline Mood or Anxiety Disorder"
        ), 
        type = factor(
            type,
            levels = c(
                "Unstratified",
                "Baseline Mood or Anxiety Disorder",
                "No Baseline Mood or Anxiety Disorder"
            )
        ), 
        exposure = factor(
            exposure,
            levels = c(
                "Physical Disability Only vs. Neither",
                "Chronic Pain Only vs. Neither"
            )
        )
    )

saveRDS(results_icd_zimple_df_icd, "~/mediation-chronic-pain-physical-disability/08_results/all_mediators_results_icd.rds")

results_icd_zimple_df_icd <- results_icd_zimple_df_icd |>
    select(-c(total, ci_total_low, ci_total_high))

## -----------------------------------------------------------------------------------------------------------
#results_icd_zimple_df_icd$exposure <- factor(results_icd_zimple_df_icd$exposure, levels = results_icd_zimple_df_icd$exposure)

zimple_plot_icd <-
    ggplot(results_icd_zimple_df_icd) +
    geom_point(aes(
        x = exposure,
        y = indirect,
        shape = factor("NIE", levels = c("ATE", "NIE", "NDE"))
    ), size = 2) +
    geom_errorbar(aes(x = exposure,
                      ymin = ci_indirect_low, ymax = ci_indirect_high),
                  width = 0) +
    geom_point(aes(
        x = as.numeric(exposure) - 0.25,
        y = ate,
        shape = factor("ATE", levels = c("ATE", "NIE", "NDE"))
    ), size = 2) +
    geom_errorbar(aes(
        x = as.numeric(exposure) - 0.25,
        ymin = ci_ate_low,
        ymax = ci_ate_high
    ),
    width = 0) +
    geom_point(
        aes(
            x = as.numeric(exposure) + 0.25,
            y = direct,
            shape = factor("NDE", levels = c("ATE", "NIE", "NDE"))
        ),
        size = 2,
        position = position_dodge(width = 0.5)
    ) +
    geom_errorbar(
        aes(
            x = as.numeric(exposure) + 0.25,
            ymin = ci_direct_low,
            ymax = ci_direct_high
        ),
        width = 0,
        position = position_dodge(width = 0.5)
    ) +
  labs(y = "Effect Estimate", x = "") +
  theme_bw() + 
  theme(
  text = element_text(family = "Times", size = 10),
  panel.grid.major.x = element_blank(),  
  panel.grid.minor.x = element_blank(), 
  panel.border = element_rect(color = "black", fill = NA),  
  panel.spacing = unit(0.5, "lines"),  
  panel.background = element_blank(),
  legend.key.size = unit(0.00005, "lines"),
  legend.position = c(0.9, 0.875),
  axis.title = element_text(size = 10),
  axis.text = element_text(size = 10),
  legend.text = element_text(size = 10),
  plot.title = element_text(size = 10),
  plot.subtitle = element_text(size = 10),
  strip.text = element_text(size = 10)) +
    scale_x_discrete(labels = str_wrap(levels(results_icd_zimple_df_icd$exposure), width = 10)) + 
    scale_shape_manual(values = c("ATE" = 1, "NIE" = 8, "NDE" = 18),
                       breaks = c("ATE", "NIE", "NDE"),
                       labels = c("ATE", "NIE", "NDE")) +
    facet_wrap(~type, nrow = 1) +
    labs(shape = "Effect Type") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black")

zimple_plot_icd
ggsave("~/mediation-chronic-pain-physical-disability/10_figures/final_figs/all_mediators_icd.pdf", zimple_plot_icd, dpi = 300, width = 7, height = 9)


## -----------------------------------------------------------------------------------------------------------
directory <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_results_final_r1/washout_12mo/all_mediators_12mo"

res <- list.files(directory, full.names = TRUE)

results_icd_12mo <- 
    map(res, \(x) readRDS(x)[[1]]) |> 
    list_rbind() |> 
    round(4)

results_icd_12mo$exposure <- str_extract(res, "(?<=results_exposure_).*?(?=.rds)")
results_icd_12mo$type <- str_extract(fs::path_file(res), "(?<=all_mediators_).*?(?=_icd_12mo_results)")


## -----------------------------------------------------------------------------------------------------------
results_icd_zimple_df_icd_12mo <- select(
    results_icd_12mo,
    exposure,
    type,
    indirect,
    ci_indirect_low,
    ci_indirect_high,
    direct,
    ci_direct_low,
    ci_direct_high,
    ate,
    ci_ate_low,
    ci_ate_high
) |> 
    mutate(
        exposure = case_when(
            exposure == "disability_only_subset" ~ "Physical Disability Only vs. Neither",
            TRUE ~ "Chronic Pain Only vs. Neither"
        ), 
        type = case_when(
            type == "depanx" ~ "Baseline Mood or Anxiety Disorder",
            TRUE ~ "No Baseline Mood or Anxiety Disorder"
        ), 
        type = factor(
            type,
            levels = c(
                "Unstratified",
                "Baseline Mood or Anxiety Disorder",
                "No Baseline Mood or Anxiety Disorder"
            )
        ), 
        exposure = factor(
            exposure,
            levels = c(
                "Physical Disability Only vs. Neither",
                "Chronic Pain Only vs. Neither"
            )
        )
    )

## -----------------------------------------------------------------------------------------------------------
#results_icd_zimple_df_icd$exposure <- factor(results_icd_zimple_df_icd$exposure, levels = results_icd_zimple_df_icd$exposure)

zimple_plot_icd <-
    ggplot(results_icd_zimple_df_icd_12mo) +
    geom_point(aes(
        x = exposure,
        y = indirect,
        shape = factor("NIE", levels = c("ATE", "NIE", "NDE"))
    ), size = 2) +
    geom_errorbar(aes(x = exposure,
                      ymin = ci_indirect_low, ymax = ci_indirect_high),
                  width = 0) +
    geom_point(aes(
        x = as.numeric(exposure) - 0.25,
        y = ate,
        shape = factor("ATE", levels = c("ATE", "NIE", "NDE"))
    ), size = 2) +
    geom_errorbar(aes(
        x = as.numeric(exposure) - 0.25,
        ymin = ci_ate_low,
        ymax = ci_ate_high
    ),
    width = 0) +
    geom_point(
        aes(
            x = as.numeric(exposure) + 0.25,
            y = direct,
            shape = factor("NDE", levels = c("ATE", "NIE", "NDE"))
        ),
        size = 2,
        position = position_dodge(width = 0.5)
    ) +
    geom_errorbar(
        aes(
            x = as.numeric(exposure) + 0.25,
            ymin = ci_direct_low,
            ymax = ci_direct_high
        ),
        width = 0,
        position = position_dodge(width = 0.5)
    ) +
  labs(y = "Effect Estimate", x = "") +
  theme_bw() + 
  theme(
  text = element_text(family = "Times", size = 10),
  panel.grid.major.x = element_blank(),  
  panel.grid.minor.x = element_blank(), 
  panel.border = element_rect(color = "black", fill = NA),  
  panel.spacing = unit(0.5, "lines"),  
  panel.background = element_blank(),
  legend.key.size = unit(0.00005, "lines"),
  legend.position = c(0.9, 0.85),
  axis.title = element_text(size = 10),
  axis.text = element_text(size = 10),
  legend.text = element_text(size = 10),
  plot.title = element_text(size = 10),
  plot.subtitle = element_text(size = 10),
  strip.text = element_text(size = 10)) +
    scale_x_discrete(labels = str_wrap(levels(results_icd_zimple_df_icd$exposure), width = 10)) + 
    scale_shape_manual(values = c("ATE" = 1, "NIE" = 8, "NDE" = 18),
                       breaks = c("ATE", "NIE", "NDE"),
                       labels = c("ATE", "NIE", "NDE")) +
    facet_wrap(~type, nrow = 1) +
    labs(shape = "Effect Type") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black")

zimple_plot_icd
ggsave("~/mediation-chronic-pain-physical-disability/10_figures/final_figs/all_mediators_icd_12mo.pdf", zimple_plot_icd, dpi = 300)


## -----------------------------------------------------------------------------------------------------------
sens_total_effects_df <- select(results, 
       mediator = exposure, type, 
       total, ci_total_low, ci_total_high, 
       ate, ci_ate_low, ci_ate_high) |> 
    mutate(
        mediator = case_when(
            mediator == "disability_only_subset" ~ "Physical Disability Only vs. Neither", 
            TRUE ~ "Chronic Pain Only vs. Neither"
        ), 
        type = case_when(
            type == "depanx" ~ "Stratified, Baseline Mood or Anxiety Disorder", 
            TRUE ~ "Stratified, No Baseline Mood or Anxiety Disorder"
        ),
        type = factor(
            type,
            levels = c(
                "Unstratified",
                "Stratified, Baseline Mood or Anxiety Disorder",
                "Stratified, No Baseline Mood or Anxiety Disorder"
            )
        ),
        mediator = factor(
            mediator,
            levels = c(
                "Physical Disability Only vs. Neither",
                "Chronic Pain Only vs. Neither"
            )
        ), 
        oud = "OUD defined by a composite definition"
    )

sens_total_effects_df_icd <- select(results_icd, 
       mediator = exposure, type, 
       total, ci_total_low, ci_total_high, 
       ate, ci_ate_low, ci_ate_high) |> 
    mutate(
        mediator = case_when(
            mediator == "disability_only_subset" ~ "Physical Disability Only vs. Neither", 
            TRUE ~ "Chronic Pain Only vs. Neither"
        ), 
        type = case_when(
            type == "depanx" ~ "Stratified, Baseline Mood or Anxiety Disorder", 
            TRUE ~ "Stratified, No Baseline Mood or Anxiety Disorder"
        ),
        type = factor(
            type,
            levels = c(
                "Unstratified",
                "Stratified, Baseline Mood or Anxiety Disorder",
                "Stratified, No Baseline Mood or Anxiety Disorder"
            )
        ),
        mediator = factor(
            mediator,
            levels = c(
                "Physical Disability Only vs. Neither",
                "Chronic Pain Only vs. Neither"
            )
        ), 
        oud = "OUD defined by ICD diagnosis codes"
    )

sens_total_effects_df_icd_12mo <- select(results_icd_12mo, 
       mediator = exposure, type, 
       total, ci_total_low, ci_total_high, 
       ate, ci_ate_low, ci_ate_high) |> 
    mutate(
        mediator = case_when(
            mediator == "disability_only_subset" ~ "Physical Disability Only vs. Neither", 
            TRUE ~ "Chronic Pain Only vs. Neither"
        ), 
        type = case_when(
            type == "depanx" ~ "Stratified, Baseline Mood or Anxiety Disorder", 
            TRUE ~ "Stratified, No Baseline Mood or Anxiety Disorder"
        ),
        type = factor(
            type,
            levels = c(
                "Unstratified",
                "Stratified, Baseline Mood or Anxiety Disorder",
                "Stratified, No Baseline Mood or Anxiety Disorder"
            )
        ),
        mediator = factor(
            mediator,
            levels = c(
                "Physical Disability Only vs. Neither",
                "Chronic Pain Only vs. Neither"
            )
        ), 
        oud = "OUD, ICD diagnosis codes (12 month washout)"
    )

sens_total_effects_df <- sens_total_effects_df |>
    merge(sens_total_effects_df_icd, all = TRUE) |>
    merge(sens_total_effects_df_icd_12mo, all = TRUE) |>
    mutate(oud = factor(oud, levels = c("OUD defined by ICD diagnosis codes", "OUD defined by a composite definition", "OUD, ICD diagnosis codes (12 month washout)")))


## -----------------------------------------------------------------------------------------------------------
sens_total_effects_plot <-
  ggplot(sens_total_effects_df) +
    geom_point(aes(x = as.factor(mediator), y = total, shape = factor("ITE", levels = c("ATE", "ITE"))), size = 2) +
    geom_errorbar(aes(x = as.factor(mediator), 
                      ymin = ci_total_low, ymax = ci_total_high), width = 0) +
    geom_point(aes(x = as.numeric(as.factor(mediator)) - 0.25, y = ate, shape = factor("ATE", levels = c("ATE", "ITE"))), size = 2) +
    geom_errorbar(aes(x = as.numeric(as.factor(mediator)) - 0.25, 
                      ymin = ci_ate_low, ymax = ci_ate_high), width = 0) + 
  labs(y = "Effect Estimate", x = "") +
  theme_bw() +
  theme(
  text = element_text(family = "Times", size = 10),
  panel.grid.major.x = element_blank(),  
  panel.grid.minor.x = element_blank(), 
  panel.border = element_rect(color = "black", fill = NA),  
  panel.spacing = unit(0.5, "lines"),  
  panel.background = element_blank(),
  legend.key.size = unit(0.00005, "lines"),
  legend.position = c(0.9, 0.91),
  axis.title = element_text(size = 10),
  axis.text = element_text(size = 10),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10),
  plot.title = element_text(size = 10),
  plot.subtitle = element_text(size = 10),
  strip.text = element_text(size = 10)) +
    scale_x_discrete(labels = str_wrap(levels(results_zimple_df$exposure), width = 10)) + 
    scale_shape_manual(values = c("ATE" = 1, "ITE" = 8),
                       breaks = c("ATE", "ITE"),
                       labels = c("ATE", "ITE")) +
    facet_grid(oud~type) +
    labs(shape = "Effect Type") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black")

sens_total_effects_plot
ggsave("~/mediation-chronic-pain-physical-disability/10_figures/final_figs/all_mediators_ATE_ITE.pdf", sens_total_effects_plot, dpi = 300, width = 7, height = 10)

