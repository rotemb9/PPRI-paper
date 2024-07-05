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
