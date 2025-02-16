# test whether larger discrepencies between the percieved intensity (based
# on the stimulus response task and a scaling factor from the perception
# model) and the cue mean weaken the behavioral effect, demonstrated asquadric effects


## Use checkpoint for reproducibility
library(checkpoint)
checkpoint("2024-04-01")

## Required R package
library(lme4)
library(lmerTest)
library(ggplot2)
library(tidyverse)
#library(gridExtra)
library(ggpubr)
#library(corrplot)
#library(rstatix)
#library(emmeans)
#library(gt)
#library(gtsummary)
#library(broom.mixed)

## Clear workspace
rm(list=ls())


# read data from the cued perception task
data_path = "data_for_analysis/processed_data"
data_filename = file.path(data_path, "task-expectpercept_all_subjs_with_pred_after_exclusions.csv")
data = read.csv(data_filename, header = T)

data$pe = data$stim_level_scale_calibration - data$cue_mean*100
data$pe_sub = data$rating - data$stim_level_scale_calibration
data$pe2 = data$pe^2
data$pe3 = data$pe^3
data$pe4 = data$pe^4
data$trial_num_per_modality = data$trial_num
data$trial_num_per_modality[data$trial_num_per_modality > 12] = data$trial_num_per_modality[data$trial_num_per_modality > 12] - 12


data_pain = data[data$modality == "pain",]
data_vision = data[data$modality == "vision",]
data_pain$trial_num_overall_modality = data_pain$trial_num_per_modality + 12 * (data_pain$block - 1)
data_vision$trial_num_overall_modality = data_vision$trial_num_per_modality + 12 * (data_vision$block - 1)


# run the "simple model" from Hird et al., Scientific Reports, 2019
contrast_effects_model_pain = lmer(pe_sub ~ 1 + pe + pe2 + pe3 + (1 | participant), data=data_pain, na.action=na.omit)
summary(contrast_effects_model_pain)

contrast_effects_model_vision = lmer(pe_sub ~ 1 + pe + pe2 + pe3 + (1 | participant), data=data_vision, na.action=na.omit)
summary(contrast_effects_model_vision)

# run the "complex model" from Hird et al., Scientific Reports, 2019
contrast_effects_model_pain_complex = lmer(pe_sub ~ 1 + pe + pe2 + pe3 + pe4 + trial_num_overall_modality + pe*trial_num_overall_modality + pe2*trial_num_overall_modality + pe3*trial_num_overall_modality + pe4*trial_num_overall_modality + (1 | participant), data=data_pain, na.action=na.omit)
summary(contrast_effects_model_pain_complex)

contrast_effects_model_vision_complex = lmer(pe_sub ~ 1 + pe + pe2 + pe3 + pe4 + trial_num_overall_modality + pe*trial_num_overall_modality + pe2*trial_num_overall_modality + pe3*trial_num_overall_modality + pe4*trial_num_overall_modality + (1 | participant), data=data_vision, na.action=na.omit)
summary(contrast_effects_model_vision_complex)

# plot the relationship between PE and PEsub
y_min = floor(min(data$pe_sub, na.rm = TRUE) / 10) * 10  # Round down to nearest multiple of 10
y_max = ceiling(max(data$pe_sub, na.rm = TRUE) / 10) * 10  # Round up to nearest multiple of 10
x_min = floor(min(data$pe, na.rm = TRUE) / 10) * 10  # Round down to nearest multiple of 10
x_max = ceiling(max(data$pe, na.rm = TRUE) / 10) * 10  # Round up to nearest multiple of 10

ggplot(data, aes(x = pe, y = pe_sub)) +
  geom_point(aes(color = participant, alpha = 0.1)) +  # Add points
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = TRUE, color = "blue") +  # Add polynomial trendline
  facet_wrap(
    ~ modality,
    labeller = as_labeller(c("pain" = "Pain", "vision" = "Visual perception"))
  ) +  # Custom facet titles
  labs(
    x = "Cue PE\n(subjective stimulus value - cue mean)",
    y = "Experienced PE\n(reported rating - subjective stimulus value)"
  ) +
  scale_y_continuous(
    breaks = seq(y_min, y_max, by = 10),  # Define breaks in gaps of 10
    limits = c(y_min, y_max)  # Set limits to the adjusted range
  ) +
  scale_x_continuous(
    breaks = seq(x_min, x_max, by = 10),  # Define breaks in gaps of 10
    limits = c(x_min, x_max)  # Set limits to the adjusted range
  ) +
  theme(
    panel.background = element_rect(fill = "white"),  # White plot background
    strip.background = element_rect(fill = "white"),  # White facet title background
    strip.text = element_text(size = 10),  # Customize facet title text
    panel.grid = element_line(color = "gray90")  # Optional: light gridlines
  ) +
  theme(legend.position = "none")  # Remove the legend
