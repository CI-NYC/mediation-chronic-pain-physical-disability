## -----------------------------------------------------------------------------------------------------------
# Load packages
library(tidyverse)
library(dplyr)
library(forestplot)
library(gridExtra)
library(ggplot2)
library(patchwork)


## -----------------------------------------------------------------------------------------------------------
directory <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_results_final_r1/disability_icd"

results <- readRDS("~/disability/projects/mediation_unsafe_pain_mgmt/08_results/group_mediators_icd.rds")

results <- results |>
  mutate(across(c(ate, ci_ate_low, ci_ate_high), \(x) ifelse(Mediator != "ATE", NA_real_, x)))

## -----------------------------------------------------------------------------------------------------------
#results_zimple_df$exposure <- factor(results_zimple_df$exposure, levels = results_zimple_df$exposure)

custom_colors <- c("#e41a1c","#000000", "#D2B48C", "#7570b3", "#66a61e", "#377eb8", "#ff7f00", "#F4D03F")

group_plot_chronic_pain <-
  ggplot(results |> filter(exposure == "Chronic Pain Only vs. Neither")) +
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
    facet_grid(. ~ type) + 
  scale_color_manual(values = custom_colors) +
  labs(y = "Effect Estimate", x = "", title = "Chronic Pain Only v. Neither") +
  theme_bw() +
  theme(
    text = element_text(family = "Times", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    panel.spacing = unit(0.5, "lines"),
    panel.background = element_blank(),
    legend.key.size = unit(0.4, "lines"),
    axis.text.x = element_blank(),
    legend.margin = margin(0, 0, 0,-5),
    legend.spacing = unit(0.1, "cm"),
    legend.box = "vertical",
    legend.box.spacing = unit(0.25, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    legend.key.width = unit(0.75, "cm"),
    legend.key.height = unit(0.75, "cm"),
    strip.text = element_text(size = 10),
    legend.title = element_text(size = 10, margin = margin(b = 3)),
    legend.text = element_text(size = 10, margin = margin(t = 0))
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
    labels = str_wrap(levels(results$Mediator)[-1], width = 28),
    breaks = levels(results$Mediator)[-1]
  ) +
  labs(shape = "Effect Type",
       color = "Mediator") +
  geom_hline(yintercept = 0,
             linetype = "solid",
             color = "black")

suppressWarnings(group_plot_chronic_pain)
ggsave("~/disability_NEW/projects/mediation_unsafe_pain_mgmt/10_figures/final_figs/group_mediators_icd_chronic_pain.pdf", group_plot_chronic_pain, dpi = 300, height = 7, width = 7.5)


group_plot_disability <-
    ggplot(results |> filter(exposure == "Physical Disability Only vs. Neither")) +
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
  facet_grid(. ~ type) + 
  scale_color_manual(values = custom_colors) +
    labs(y = "Effect Estimate", x = "", title = "Physical Disability Only v. Neither") +
  theme_bw() +
  theme(
    text = element_text(family = "Times", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    panel.spacing = unit(0.5, "lines"),
    panel.background = element_blank(),
    legend.key.size = unit(0.4, "lines"),
    axis.text.x = element_blank(),
    legend.margin = margin(0, 0, 0,-5),
    legend.spacing = unit(0.1, "cm"),
    legend.box = "vertical",
    legend.box.spacing = unit(0.25, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    legend.key.width = unit(0.75, "cm"),
    legend.key.height = unit(0.75, "cm"),
    strip.text = element_text(size = 10),
    legend.title = element_text(size = 10, margin = margin(b = 3)),
    legend.text = element_text(size = 10, margin = margin(t = 0))
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
    labels = str_wrap(levels(results$Mediator)[-1], width = 28),
    breaks = levels(results$Mediator)[-1]
  ) + 
  labs(shape = "Effect Type",
       color = "Mediator") +
  geom_hline(yintercept = 0,
             linetype = "solid",
             color = "black")

suppressWarnings(group_plot_disability)
ggsave("~/disability_NEW/projects/mediation_unsafe_pain_mgmt/10_figures/final_figs/group_mediators_icd_disability.pdf", group_plot_disability, dpi = 300, height = 7, width = 7.5)




