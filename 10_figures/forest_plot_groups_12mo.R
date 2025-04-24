library(tidyverse)
library(dplyr)
library(forestplot)
library(gridExtra)
library(ggplot2)
library(patchwork)

directory <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_results_final_r1/washout_12mo/depanx_icd"

res <- list.files(directory, full.names = TRUE)

results_depanx <- 
    map(res, \(x) readRDS(x)[[1]]) |> 
    list_rbind()

results_depanx$mediator_included <- str_extract(res, "(?<=mediator_).*?(?=_icd)")
results_depanx$exposure <- str_extract(res, "(?<=results_exposure_).*?(?=_depanx)")
results_depanx$type <- "depanx"

directory <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_results_final_r1/washout_12mo/nodepanx_icd"

res <- list.files(directory, full.names = TRUE)

results_nodepanx <- 
    map(res, \(x) readRDS(x)[[1]]) |> 
    list_rbind()

results_nodepanx$mediator_included <- str_extract(res, "(?<=mediator_).*?(?=_icd)")
results_nodepanx$exposure <- str_extract(res, "(?<=results_exposure_).*?(?=_nodepanx)")
results_nodepanx$type <- "nodepanx"

results <- rbind(results_depanx, results_nodepanx) |>
    mutate(
        exposure = case_when(
            exposure == "disability_only_subset" ~ "Physical Disability Only vs. Neither",
            exposure == "pain_only" ~ "Chronic Pain Only vs. Neither"
        ),
        exposure = factor(
            exposure,
            levels = c(
                "Physical Disability Only vs. Neither",
                "Chronic Pain Only vs. Neither"
            )
        ), 
        type = case_when(
            type == "depanx" ~ "Baseline Mood or Anxiety Disorder",
            type == "nodepanx" ~ "No Baseline Mood or Anxiety Disorder"
        ),
        type = factor(
            type,
            levels = c(
                "Unstratified",
                "Baseline Mood or Anxiety Disorder",
                "No Baseline Mood or Anxiety Disorder"
            )
        ),
        mediator_included = case_when(
            mediator_included == "12mo_high_dose_longer_duration_mme" ~ "High Dose, Long Duration MME",
            mediator_included == "opioid_benzo_copresc" ~ "Benzodiazepine Co-presc w/ Opioid",
            mediator_included == "opioid_gaba_copresc" ~ "Gabapentinoid Co-presc w/ Opioid",
            mediator_included == "opioid_mrelax_copresc" ~ "Muscle Relaxant Co-presc w/ Opioid",
            mediator_included == "12mo_nonopioid_nonoverlap_mrelax_gaba_benzo" ~ 
                "Benzo/Gaba/Muscle Relaxant Presc w/o Opioid",
            mediator_included == "12mo_nonopioid_nonoverlap_antidepressant_antiinflamatory" ~
                "Antidepressant/Anti-inflamatory Presc w/o Opioid",
            mediator_included == "has_physical_therapy" ~ "Physical Therapy",
            TRUE ~ mediator_included
        )
    ) |> 
    rename("Mediator" = "mediator_included")

results <- 
    add_row(results,
            Mediator = "ATE",
            type = "Baseline Mood or Anxiety Disorder",
            exposure = "Physical Disability Only vs. Neither",
            ate = filter(
                results,
                type == "Baseline Mood or Anxiety Disorder",
                exposure == "Physical Disability Only vs. Neither"
            )$ate |> unique(),
            ci_ate_low = filter(
                results,
                type == "Baseline Mood or Anxiety Disorder",
                exposure == "Physical Disability Only vs. Neither"
            )$ci_ate_low |> unique(),
            ci_ate_high = filter(
                results,
                type == "Baseline Mood or Anxiety Disorder",
                exposure == "Physical Disability Only vs. Neither"
            )$ci_ate_high |> unique()
    ) |> 
    add_row(
        Mediator = "ATE",
        type = "Baseline Mood or Anxiety Disorder",
        exposure = "Chronic Pain Only vs. Neither",
        ate = filter(
            results,
            type == "Baseline Mood or Anxiety Disorder",
            exposure == "Chronic Pain Only vs. Neither"
        )$ate |> unique(),
        ci_ate_low = filter(
            results,
            type == "Baseline Mood or Anxiety Disorder",
            exposure == "Chronic Pain Only vs. Neither"
        )$ci_ate_low |> unique(),
        ci_ate_high = filter(
            results,
            type == "Baseline Mood or Anxiety Disorder",
            exposure == "Chronic Pain Only vs. Neither"
        )$ci_ate_high |> unique()
    ) |> 
    add_row(
        Mediator = "ATE",
        type = "No Baseline Mood or Anxiety Disorder",
        exposure = "Physical Disability Only vs. Neither",
        ate = filter(
            results,
            type == "No Baseline Mood or Anxiety Disorder",
            exposure == "Physical Disability Only vs. Neither"
        )$ate |> unique(),
        ci_ate_low = filter(
            results,
            type == "No Baseline Mood or Anxiety Disorder",
            exposure == "Physical Disability Only vs. Neither"
        )$ci_ate_low |> unique(),
        ci_ate_high = filter(
            results,
            type == "No Baseline Mood or Anxiety Disorder",
            exposure == "Physical Disability Only vs. Neither"
        )$ci_ate_high |> unique()
    ) |> 
    add_row(
        Mediator = "ATE",
        type = "No Baseline Mood or Anxiety Disorder",
        exposure = "Chronic Pain Only vs. Neither",
        ate = filter(
            results,
            type == "No Baseline Mood or Anxiety Disorder",
            exposure == "Chronic Pain Only vs. Neither"
        )$ate |> unique(),
        ci_ate_low = filter(
            results,
            type == "No Baseline Mood or Anxiety Disorder",
            exposure == "Chronic Pain Only vs. Neither"
        )$ci_ate_low |> unique(),
        ci_ate_high = filter(
            results,
            type == "No Baseline Mood or Anxiety Disorder",
            exposure == "Chronic Pain Only vs. Neither"
        )$ci_ate_high |> unique()
    ) |> 
    filter(exposure == "Chronic Pain Only vs. Neither")

custom_colors <- c("#bdbdbd", "#e41a1c","#000000", "#D2B48C", "#7570b3", "#66a61e", "#377eb8", "#ff7f00")

saveRDS(results, "~/disability_NEW/projects/mediation_unsafe_pain_mgmt/08_results/group_mediators_12mo.rds")


results <- results |>
    mutate(across(c(ate, ci_ate_low, ci_ate_high), \(x) ifelse(Mediator != "ATE", NA_real_, x))) |>
    mutate(Mediator = case_when(Mediator == "ATE" ~ "ATE (all mediators)",
                                TRUE ~ Mediator)) |> 
    mutate(
        Mediator = factor(
            Mediator,
            levels = c(
                "ATE (all mediators)",
                "High Dose, Long Duration MME",
                "Benzodiazepine Co-presc w/ Opioid",
                "Gabapentinoid Co-presc w/ Opioid",
                "Muscle Relaxant Co-presc w/ Opioid",
                "Benzo/Gaba/Muscle Relaxant Presc w/o Opioid",
                "Antidepressant/Anti-inflamatory Presc w/o Opioid",
                "Physical Therapy"
            )
        )
    )

group_plot <-
    ggplot(results) +
    geom_point(
        aes(
            x = Mediator,
            y = total,
            shape = factor("ITE", levels = c("ATE", "ITE", "IIE", "IDE")),
            color = Mediator
        ),
        size = 1.5,
        position = position_nudge(x = -0.25)
    ) +
    geom_errorbar(
        aes(
            x = Mediator,
            ymin = ci_total_low,
            ymax = ci_total_high,
            color = Mediator
        ),
        position = position_nudge(x = -0.25), 
        width = 0
    ) +
    geom_point(aes(
        x = Mediator,
        y = ate,
        shape = factor("ATE", levels = c("ATE", "ITE", "IIE", "IDE")),
        color = Mediator
    ),
    size = 1.5) +
    geom_errorbar(aes(
        x = Mediator,
        ymin = ci_ate_low,
        ymax = ci_ate_high,
        color = Mediator
    ),
    width = 0) +
    geom_point(
        aes(
            x = Mediator,
            y = indirect,
            shape = factor("IIE", levels = c("ATE", "ITE", "IIE", "IDE")),
            color = Mediator
        ),
        size = 1.5
    ) +
    geom_errorbar(
        aes(
            x = Mediator,
            ymin = ci_indirect_low,
            ymax = ci_indirect_high,
            color = Mediator
        ),
        width = 0
    ) +
    geom_point(
        aes(
            x = Mediator,
            y = direct,
            shape = factor("IDE", levels = c("ATE", "ITE", "IIE", "IDE")),
            color = Mediator
        ),
        size = 1.5,
        position = position_nudge(x = 0.25)
    ) +
    geom_errorbar(
        aes(
            x = Mediator,
            ymin = ci_direct_low,
            ymax = ci_direct_high,
            color = Mediator
        ),
        width = 0,
        position = position_nudge(x = 0.25)
    ) +
    facet_grid(type ~ exposure) + 
    scale_color_manual(values = custom_colors) +
    labs(y = "Effect Estimate", x = "") +
    theme_bw() +
    theme(
        text = element_text(family = "Times", size = 10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0.5, "lines"),
        panel.background = element_blank(),
        legend.key.size = unit(0.4, "lines"),
        #axis.text.x = element_blank(),
        legend.margin = margin(0, 0, 0,-5),
        legend.spacing = unit(0.1, "cm"),
        legend.box = "vertical",
        legend.box.spacing = unit(0.25, "cm"),
        legend.spacing.y = unit(0.1, "cm"),
        legend.key.width = unit(0.75, "cm"),
        legend.key.height = unit(0.75, "cm"),
        strip.text = element_text(size = 10),
        legend.title = element_text(size = 10, margin = margin(b = 3)),
        legend.text = element_text(size = 10, margin = margin(t = 0)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
    ) +
    scale_shape_manual(
        values = c(
            "ATE" = 1,
            "ITE" = 7,
            "IIE" = 8,
            "IDE" = 18
        ),
        breaks = c("ATE", "ITE", "IIE", "IDE"),
        labels = c("ATE", "ITE", "IIE", "IDE"),
        guide = guide_legend(keyheight = unit(0.5, "cm"))
    ) +
    scale_color_manual(
        values = custom_colors,
        labels = str_wrap(levels(results$Mediator), width = 28),
        breaks = levels(results$Mediator)
    ) + 
    labs(shape = "Effect Type",
         color = "Mediator") +
    geom_hline(yintercept = 0,
               linetype = "solid",
               color = "black") +
    scale_x_discrete(labels = c("ATE (all mediators)" = "ATE", 
                                "Physical Therapy" = "PT", 
                                "High Dose, Long Duration MME" = "MME",
                                "Antidepressant/Anti-inflamatory Presc w/o Opioid" = "AD/AI",
                                "Benzo/Gaba/Muscle Relaxant Presc w/o Opioid" = "BZ/GB/MR",
                                "Benzodiazepine Co-presc w/ Opioid" = "BZ",
                                "Gabapentinoid Co-presc w/ Opioid" = "GB",
                                "Muscle Relaxant Co-presc w/ Opioid" = "MR"
    )) +
    guides(color = guide_legend(nrow = 3, ncol = 3))

ggsave("~/mediation-chronic-pain-physical-disability/10_figures/final_figs/group_mediators_icd_12mo.pdf", 
       group_plot, dpi = 300, height = 8, width = 9)
