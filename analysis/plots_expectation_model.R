## Use checkpoint for reproducibility
library(checkpoint)
checkpoint("2024-04-01")

library(ggplot2)
library(tidyverse)
library(gridExtra)

## Clear workspace
rm(list=ls())

data_filename = file.path("data_for_analysis", "processed_data", "task-expect_all_subjs_with_pred.csv")
data = read.csv(data_filename, header = T)
data$modality[data$modality == "vision"] = "visual perception"
fig_output_path = "plots"

# plot expected vs observed expectations - color by participant, column by modality
expectation_predicted_vs_observed = ggplot(data, aes(x = predicted_expect, y = rating, fill = participant, color= participant)) +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_line(stat="smooth",method = "lm", alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Predicted (weighted) expectation", y = "Expectation rating", color = "Participant") +
  scale_x_continuous(breaks=seq(0, 100, 10), limits = c(20,80)) +
  scale_y_continuous(breaks=seq(0, 100, 10), limits = c(20,80)) +                    
  facet_grid(. ~ modality, scales = "free") +
  theme_bw() +
  theme(strip.background = element_rect(color="white", fill="white"), text = element_text(size = 10), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none")

ggsave('expectation_predicted_vs_observed.pdf',
       plot = expectation_predicted_vs_observed,
       path = fig_output_path,
       width = 200,
       height = 120,
       units = "mm",
       dpi = 300)

# test predicted-observed expectation correlation and RMSE
correlation_per_participant_modality <- data %>%
     group_by(participant, modality) %>%
     summarize(correlation = cor(rating, predicted_expect, use = "complete.obs"), .groups = "drop")

corr_mean_sd <- correlation_per_participant_modality %>%
  group_by(modality) %>%
  summarize(
    mean_correlation = mean(correlation),
    sd_correlation = sd(correlation),
    n = n(),  # Number of participants per modality
    .groups = "drop"
  )

rmse_per_participant_modality <- data %>%
  group_by(participant, modality) %>%
  summarize(rmse = sqrt(mean((rating - predicted_expect)^2, na.rm = TRUE)), .groups = "drop")

rmse_mean_sd <- rmse_per_participant_modality %>%
  group_by(modality) %>%
  summarize(
    mean_cor = mean(rmse),
    sd_cor = sd(rmse),
    .groups = "drop"
  )

# test the correlation within each cue mean
correlation_per_participant_modality_cuemean <- data %>%
  group_by(participant, modality, cue_mean) %>%
  summarize(correlation = cor(rating, predicted_expect, use = "complete.obs"), .groups = "drop")

corr_mean_sd_bycuemean <- correlation_per_participant_modality_cuemean %>%
  group_by(modality, cue_mean) %>%
  summarize(mean_cor = mean(correlation), sd_cor = sd(correlation), .groups = "drop")

# Perform a one-sample t-test to check if the mean correlation is significantly different from zero
correlation_significance <- correlation_per_participant_modality %>%
  group_by(modality) %>%
  summarize(
    t_test_result = list(t.test(correlation, mu = 0)),  # Test if the mean correlation is different from 0
    .groups = "drop"
  )

correlation_significance %>%
  mutate(
    p_value = sapply(t_test_result, function(x) x$p.value),  # Extract p-value from t-test result
    mean_correlation = sapply(t_test_result, function(x) x$estimate),  # Extract mean correlation
    t_statistic = sapply(t_test_result, function(x) x$statistic)  # Extract t-statistic
  ) %>%
  select(modality, mean_correlation, t_statistic, p_value)  # Keep relevant columns

# by cue mean level
correlation_significance_bycuemean <- correlation_per_participant_modality_cuemean %>%
  group_by(modality, cue_mean) %>%
  summarize(
    t_test_result = list(t.test(correlation, mu = 0)),  # Test if the mean correlation is different from 0
    .groups = "drop"
  )

correlation_significance_bycuemean %>%
  mutate(
    p_value = sapply(t_test_result, function(x) x$p.value),  # Extract p-value from t-test result
    mean_correlation = sapply(t_test_result, function(x) x$estimate),  # Extract mean correlation
    t_statistic = sapply(t_test_result, function(x) x$statistic)  # Extract t-statistic
  ) %>%
  select(modality, mean_correlation, t_statistic, p_value)  # Keep relevant columns
