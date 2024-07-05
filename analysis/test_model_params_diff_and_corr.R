# this script tests whether optimized subject-level parameters from the expectation model are correlated with questionnaires results (e.g., fear of pain), and whether optimized parameters differ within participant across modalities

## Use checkpoint for reproducibility
library(checkpoint)
checkpoint("2024-04-01")
## Required R package
library(readxl)

rm(list=ls())

## read data
data_path = "data_for_analysis"
exp_params = read.csv(paste(data_path, "processed_data", "expectation_model_params_measures.csv", sep = "/"), header = T)
percept_params = read.csv(paste(data_path, "processed_data", "perception_models_params_measures.csv", sep = "/"), header = T)
fop = read.csv(paste(data_path, "questionnaires", "fear_of_pain_processed.csv", sep = "/"))
pain_catastrophyzing = read.csv(paste(data_path, "questionnaires", "pain_catastrophyzing_processed.csv", sep = "/"))
stai_s = read.csv(paste(data_path, "questionnaires", "STAI-S_processed.csv", sep = "/"))
## test correlations between questionnaires scores and expectations params
# fear of pain
corr_k_pain_fop = cor.test(exp_params$k[exp_params$modalities_all == "pain"], fop$score_total)
corr_k_vision_fop = cor.test(exp_params$k[exp_params$modalities_all == "vision"], fop$score_total)
corr_b_pain_fop = cor.test(exp_params$b[exp_params$modalities_all == "pain"], fop$score_total)
corr_b_vision_fop = cor.test(exp_params$b[exp_params$modalities_all == "vision"], fop$score_total)

# pain catastrophizing - total
corr_k_pain_pc = cor.test(exp_params$k[exp_params$modalities_all == "pain"], pain_catastrophyzing$score_total)
corr_k_vision_pc = cor.test(exp_params$k[exp_params$modalities_all == "vision"], pain_catastrophyzing$score_total)
corr_b_pain_pc = cor.test(exp_params$b[exp_params$modalities_all == "pain"], pain_catastrophyzing$score_total)
corr_b_vision_pc = cor.test(exp_params$b[exp_params$modalities_all == "vision"], pain_catastrophyzing$score_total)

# pain catastrophyzing - rumination
corr_k_pain_pc_rum = cor.test(exp_params$k[exp_params$modalities_all == "pain"], pain_catastrophyzing$rumination_score)
corr_k_vision_pc_rum = cor.test(exp_params$k[exp_params$modalities_all == "vision"], pain_catastrophyzing$rumination_score)
corr_b_pain_pc_rum = cor.test(exp_params$b[exp_params$modalities_all == "pain"], pain_catastrophyzing$rumination_score)
corr_b_vision_pc_rum = cor.test(exp_params$b[exp_params$modalities_all == "vision"], pain_catastrophyzing$rumination_score)

# pain catastrophyzing - magnification
corr_k_pain_pc_mag = cor.test(exp_params$k[exp_params$modalities_all == "pain"], pain_catastrophyzing$magnification_score)
corr_k_vision_pc_mag = cor.test(exp_params$k[exp_params$modalities_all == "vision"], pain_catastrophyzing$magnification_score)
corr_b_pain_pc_mag = cor.test(exp_params$b[exp_params$modalities_all == "pain"], pain_catastrophyzing$magnification_score)
corr_b_vision_pc_mag = cor.test(exp_params$b[exp_params$modalities_all == "vision"], pain_catastrophyzing$magnification_score)

# pain catastrophyzing - helplessness
corr_k_pain_pc_help = cor.test(exp_params$k[exp_params$modalities_all == "pain"], pain_catastrophyzing$helplessness_score)
corr_k_vision_pc_help = cor.test(exp_params$k[exp_params$modalities_all == "vision"], pain_catastrophyzing$helplessness_score)
corr_b_pain_pc_help = cor.test(exp_params$b[exp_params$modalities_all == "pain"], pain_catastrophyzing$helplessness_score)
corr_b_vision_pc_help = cor.test(exp_params$b[exp_params$modalities_all == "vision"], pain_catastrophyzing$helplessness_score)

# anxiety
corr_k_pain_stais = cor.test(exp_params$k[exp_params$modalities_all == "pain"], stai_s$STAI_score)
corr_k_vision_stais = cor.test(exp_params$k[exp_params$modalities_all == "vision"], stai_s$STAI_score)
corr_b_pain_stais = cor.test(exp_params$b[exp_params$modalities_all == "pain"], stai_s$STAI_score)
corr_b_vision_stais = cor.test(exp_params$b[exp_params$modalities_all == "vision"], stai_s$STAI_score)

## compare w between pain and vision (do participants weight the cues more in pain, because it's more ambiguous?)
t.test(percept_params$model2mod_w_pain, percept_params$model2mod_w_vision, paired = TRUE)
# get effect size (Cohen's d)
mean_diff_w_modality = mean(percept_params$model2mod_w_pain - percept_params$model2mod_w_vision)
sd_diff_w_modality = sd(percept_params$model2mod_w_pain - percept_params$model2mod_w_vision)
cohens_d_w_by_modality = mean_diff_w_modality/sd_diff_w_modality

# compare w0 and alpha (learning rate) between modalities
t.test(percept_params$model3mod_w0_p, percept_params$model3mod_w0_v , paired = TRUE)
t.test(percept_params$model3mod_a_p, percept_params$model3mod_a_v, paired = TRUE)
