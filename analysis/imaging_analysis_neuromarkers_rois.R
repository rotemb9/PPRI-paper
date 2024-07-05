## Use checkpoint for reproducibility
library(checkpoint)
checkpoint("2024-04-01")

## Required R package
library(lme4)
library(lmerTest)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(corrplot)
library(rstatix)
#library(emmeans)
library(gt)
library(gtsummary)
library(broom.mixed)
library("scales")
library(png, include.only = 'readPNG')
library(Rmisc)
library(Hmisc)
library(gridExtra)

## Clear workspace
rm(list=ls())

## read data
results_path = "data_for_analysis/processed_data"
fig_output_path = "plots"
data_filename = paste(results_path, "/results_nps_siips.csv", sep = "")
data = read.csv(data_filename, header = T)
data = select(data, -c(exclude_tech_issues, exclude_issues_reason)) # the roi file is more updated with regard to exclusions
rois_filename = paste(results_path, "/results_rois_all.csv", sep ="")
data_rois = read.csv(rois_filename, header = T)
data = merge(data, data_rois)

# remove trials with technical issues or invalid RTs
warning(paste('removing', sum(data$exclude_tech_issues), 'trials with technical issues', sep = " "))
data = data[!data$exclude_tech_issues,]
warning(paste('removing', sum(data$exclude_RT), 'invalid RTs', sep = " "))
data = data[!data$exclude_RT,]

data$stim_level_cat[data$stim_level==1] = "low"
data$stim_level_cat[data$stim_level==2] = "high"
data$cue_mean_cat[data$cue_mean<0.5] = "low"
data$cue_mean_cat[data$cue_mean>0.5] = "high"

data$cue_std_cat[data$cue_std<0.1] = "low"
data$cue_std_cat[data$cue_std>0.1] = "high"
data$cue_sk_cat[data$cue_sk<0] = "negative"
data$cue_sk_cat[data$cue_sk==0] = "none"
data$cue_sk_cat[data$cue_sk>0] = "positive"
data$modality = factor(data$modality, levels = c("pain","vision"))
data$stim_level_cat = factor(data$stim_level_cat, levels = c("low","high"))
data$cue_mean_cat = factor(data$cue_mean_cat, levels = c("low","high"))
data$cue_std_cat = factor(data$cue_std_cat, levels = c("low","high"))
data$cue_sk_cat = factor(data$cue_sk_cat, levels = c("none","negative","positive"))
contrasts(data$modality) = c(1,-1)
contrasts(data$stim_level_cat) = c(-1,1)
contrasts(data$cue_mean_cat) = c(-1,1)
contrasts(data$cue_std_cat) = c(-1,1)

# remove trials with large VIFs
vif_thresh = 5

num_rows_data = nrow(data)
warning(paste("removing trials with VIF larger than", vif_thresh, sep = " "))
data = data[!data$vif_cue > vif_thresh & !data$vif_stim > vif_thresh,]

# count how many trials removed
removed = num_rows_data - nrow(data)
paste('removed', removed, 'trials', 'out of', num_rows_data, '(%', round(removed / num_rows_data * 100,2), ')', sep=" ")

# get all the neuromarkers and rois
var_names_all = variable.names(data)
stim_vars = var_names_all[which(grepl("_stim", names(data)) & !grepl("cue_stim_delay", names(data)) & !grepl("vif_stim", names(data)))]
anticip_vars = var_names_all[which(grepl("_anticip", names(data)))]

## convert from wide to long format
data_long = gather(data, brain_measure, brain_measure_score, c(all_of(stim_vars), all_of(anticip_vars)),factor_key = TRUE)
# organize information about period (stim/cue) and type of measure (neuromarker/pain/vision/higher-level)
data_long$period[grepl("_stim", data_long$brain_measure)] = "stim"
data_long$period[grepl("_anticip", data_long$brain_measure)] = "cue"
data_long$type[grepl("nps", data_long$brain_measure) | grepl("siips", data_long$brain_measure)] = "neuromarker"
data_long$type[grepl("pain", data_long$brain_measure)] = "pain"
data_long$type[grepl("visual", data_long$brain_measure)] = "visual"
data_long$type[grepl("higher", data_long$brain_measure)] = "higher_level"
# clean the ROIs names
data_long$brain_measure = gsub("anticip_", "", data_long$brain_measure)
data_long$brain_measure = gsub("_anticip", "", data_long$brain_measure)
data_long$brain_measure = gsub("stim_", "", data_long$brain_measure)
data_long$brain_measure = gsub("_stim", "", data_long$brain_measure)
data_long$brain_measure = gsub("pain_", "", data_long$brain_measure)
data_long$brain_measure = gsub("visual_", "", data_long$brain_measure)
data_long$brain_measure = gsub("higher_", "", data_long$brain_measure)

# remove trials with unreasonably extreme brain scores
num_sds_for_thresh = 3.5
brain_measures = unique(data_long$brain_measure)
periods = unique(data_long$period)
modalities = unique(data_long$modality)
data_long$outlier = FALSE
for (period in periods) {
  for (modality in modalities) {
    for (measure in brain_measures) {
      mean_score = mean(data_long$brain_measure_score[data_long$brain_measure == measure & data_long$period == period & data_long$modality == modality], na.rm = TRUE)
      sd_score = sd(data_long$brain_measure_score[data_long$brain_measure == measure & data_long$period == period & data_long$modality == modality], na.rm = TRUE)
      data_long$outlier[data_long$brain_measure == measure & data_long$period == period & data_long$modality == modality & data_long$brain_measure_score < mean_score - num_sds_for_thresh * sd_score] = TRUE
      data_long$outlier[data_long$brain_measure == measure & data_long$period == period & data_long$modality == modality & data_long$brain_measure_score > mean_score + num_sds_for_thresh * sd_score] = TRUE
      num_outliers = sum(data_long$outlier[data_long$brain_measure == measure & data_long$period == period & data_long$modality == modality])
      print(paste(measure, period, ":", num_outliers, "outliers", sep = " "))
    }
  }
}
num_rows_data = nrow(data)
warning(paste("removing outliers, defined as values smaller/larger than the mean by over", num_sds_for_thresh, "SDs, for each combination of brain measure and period (stim/cue)", sep = " "))
print(paste("removing", sum(data_long$outlier), "values overall across all measures, out of", nrow(data_long), "(", round(sum(data_long$outlier) / nrow(data_long) * 100, 3) ,"%)", sep=" "))
print(paste(sum(data_long$outlier & data_long$period=="cue"), "values from the cue period, and", sum(data_long$outlier & data_long$period=="stim"), "values from the stim period", sep=" "))
data_long_all = data_long
data_long = data_long[!data_long$outlier,]

## aggregate
data_summary_bySub = aggregate(x=data_long$brain_measure_score,by=list(participant = data_long$participant,modality = data_long$modality, stim_level_cat = data_long$stim_level_cat, cue_mean_cat = data_long$cue_mean_cat, cue_std_cat = data_long$cue_std_cat, cue_sk_cat = data_long$cue_sk_cat, type = data_long$type, period = data_long$period, brain_measure = data_long$brain_measure), function(x) mean = mean(x, na.rm = TRUE))
data_summary = aggregate(data_summary_bySub$x,list(modality=data_summary_bySub$modality, stim_level_cat = data_summary_bySub$stim_level_cat, cue_mean_cat=data_summary_bySub$cue_mean_cat, cue_std_cat=data_summary_bySub$cue_std_cat, cue_sk_cat=data_summary_bySub$cue_sk_cat, type = data_summary_bySub$type, period = data_summary_bySub$period, brain_measure = data_summary_bySub$brain_measure),function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), n = length(x)))
data_summary = cbind(modality=data_summary$modality, cue_mean_cat=data_summary$cue_mean_cat,cue_std_cat=data_summary$cue_std_cat,cue_sk_cat=data_summary$cue_sk_cat, type = data_summary$type, period = data_summary$period, brain_measure = data_summary$brain_measure, as.data.frame(data_summary$x))
data_summary$SEM = data_summary$sd/sqrt(data_summary$n)
data_summary$cue_sk_cat = factor(data_summary$cue_sk_cat, levels= c("negative","none","positive"))
data$cue_sk_cat = factor(data$cue_sk_cat, levels= c("negative","none","positive"))
data_summary_bySub$cue_sk_cat = factor(data_summary_bySub$cue_sk_cat, levels= c("negative","none","positive"))
data_summary_bySub = dplyr::rename(data_summary_bySub, score = x)

## linear models
data_long$cue_sk_cat = relevel(data_long$cue_sk_cat, ref = "none")
stats = unique.data.frame(data_long[,c("modality", "type", "period", "brain_measure")])
modalities = unique(stats$modality)
types = unique(stats$type)
periods = unique(stats$period)
measures = unique(stats$brain_measure)

# add a function to test if a model did not converge
hasConverged <- function (mm) {
  
  if ( !inherits(mm, "merMod")) stop("Error: must pass a lmerMod object")
  
  retval <- NULL
  
  if(is.null(unlist(mm@optinfo$conv$lme4))) {
    retval = 1
  }
  else {
    if (isSingular(mm)) {
      retval = 0
    } else {
      retval = -1
    }
  }
  return(retval)
}

for (modality in modalities){
  for (period in periods) {
    for (measure in measures) {
      print(paste(modality, period, measure))
      # get the row number in stats df
      cur_row = which(stats$modality == modality & stats$period==period & stats$brain_measure == measure)
      # get the measure's data
      cur_measure_data = data_long[data_long$modality == modality & data_long$period==period & data_long$brain_measure == measure,] 
      # z score the measure
      cur_measure_data$brain_measure_score_z = scale(cur_measure_data$brain_measure_score, center = TRUE, scale=TRUE)
      # suppress warnings before fitting the models- there will be many warnings because of singular fit, and they are handeled below)
      suppressWarnings({
        suppressMessages({
          # fit the full model (with all random effects)
          model_cur_measure <- lmer(brain_measure_score_z ~ 1 + stim_level_cat * cue_mean_cat * cue_std_cat * cue_sk_cat + (1 + stim_level_cat + cue_mean_cat + cue_std_cat + cue_sk_cat|participant), data=cur_measure_data, na.action=na.omit)
          # it the model is singular, start removing random effects until it is not singular
          if (hasConverged(model_cur_measure) != 1) {
            model_cur_measure <- lmer(brain_measure_score_z ~ 1 + stim_level_cat * cue_mean_cat * cue_std_cat * cue_sk_cat + (1 + stim_level_cat + cue_mean_cat + cue_std_cat|participant), data=cur_measure_data, na.action=na.omit)
            if (hasConverged(model_cur_measure) != 1) {
              model_cur_measure <- lmer(brain_measure_score_z ~ 1 + stim_level_cat * cue_mean_cat * cue_std_cat * cue_sk_cat + (1 + stim_level_cat + cue_mean_cat | participant), data=cur_measure_data, na.action=na.omit)
              if (hasConverged(model_cur_measure) != 1) {
                model_cur_measure <- lmer(brain_measure_score_z ~ 1 + stim_level_cat * cue_mean_cat * cue_std_cat * cue_sk_cat + (1 + cue_mean_cat | participant), data=cur_measure_data, na.action=na.omit)
                if (hasConverged(model_cur_measure) != 1) {
                  model_cur_measure <- lmer(brain_measure_score_z ~ 1 + stim_level_cat * cue_mean_cat * cue_std_cat * cue_sk_cat + (1 | participant), data=cur_measure_data, na.action=na.omit)
                }
              }
            }
          }
        })
      })
      model_cur_measure = summary(model_cur_measure)
      # stimulus intensity
      stats$stim_int_estimate[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "stim_level_cat1", variable.names(model_cur_measure$coefficients) == "Estimate"], 3)
      stats$stim_int_SE[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "stim_level_cat1", variable.names(model_cur_measure$coefficients) == "Std. Error"], 3)
      stats$stim_int_DF[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "stim_level_cat1", variable.names(model_cur_measure$coefficients) == "df"], 2)
      stats$stim_int_t[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "stim_level_cat1", variable.names(model_cur_measure$coefficients) == "t value"], 2)
      stats$stim_int_p[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "stim_level_cat1", variable.names(model_cur_measure$coefficients) == "Pr(>|t|)"], 3)
      # cue mean
      stats$cue_mean_estimate[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_mean_cat1", variable.names(model_cur_measure$coefficients) == "Estimate"], 3)
      stats$cue_mean_SE[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_mean_cat1", variable.names(model_cur_measure$coefficients) == "Std. Error"], 3)
      stats$cue_mean_DF[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_mean_cat1", variable.names(model_cur_measure$coefficients) == "df"], 2)
      stats$cue_mean_t[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_mean_cat1", variable.names(model_cur_measure$coefficients) == "t value"], 2)
      stats$cue_mean_p[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_mean_cat1", variable.names(model_cur_measure$coefficients) == "Pr(>|t|)"], 3)
      # cue var
      stats$cue_var_estimate[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_std_cat1", variable.names(model_cur_measure$coefficients) == "Estimate"], 3)
      stats$cue_var_SE[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_std_cat1", variable.names(model_cur_measure$coefficients) == "Std. Error"], 3)
      stats$cue_var_DF[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_std_cat1", variable.names(model_cur_measure$coefficients) == "df"], 2)
      stats$cue_var_t[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_std_cat1", variable.names(model_cur_measure$coefficients) == "t value"], 2)
      stats$cue_var_p[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_std_cat1", variable.names(model_cur_measure$coefficients) == "Pr(>|t|)"], 3)
      # cue sk neg-sym
      stats$cue_sk_neg_sym_estimate[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_sk_catnegative", variable.names(model_cur_measure$coefficients) == "Estimate"], 3)
      stats$cue_sk_neg_sym_SE[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_sk_catnegative", variable.names(model_cur_measure$coefficients) == "Std. Error"], 3)
      stats$cue_sk_neg_sym_DF[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_sk_catnegative", variable.names(model_cur_measure$coefficients) == "df"], 2)
      stats$cue_sk_neg_sym_t[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_sk_catnegative", variable.names(model_cur_measure$coefficients) == "t value"], 2)
      stats$cue_sk_neg_sym_p[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_sk_catnegative", variable.names(model_cur_measure$coefficients) == "Pr(>|t|)"], 3)
      # cue sk pos-sym
      stats$cue_sk_pos_sym_estimate[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_sk_catpositive", variable.names(model_cur_measure$coefficients) == "Estimate"], 3)
      stats$cue_sk_pos_sym_SE[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_sk_catpositive", variable.names(model_cur_measure$coefficients) == "Std. Error"], 3)
      stats$cue_sk_pos_sym_DF[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_sk_catpositive", variable.names(model_cur_measure$coefficients) == "df"], 2)
      stats$cue_sk_pos_sym_t[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_sk_catpositive", variable.names(model_cur_measure$coefficients) == "t value"], 2)
      stats$cue_sk_pos_sym_p[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_sk_catpositive", variable.names(model_cur_measure$coefficients) == "Pr(>|t|)"], 3)
      # cue mean X var
      stats$cue_mean_X_var_estimate[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_mean_cat1:cue_std_cat1", variable.names(model_cur_measure$coefficients) == "Estimate"], 3)
      stats$cue_mean_X_var_SE[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_mean_cat1:cue_std_cat1", variable.names(model_cur_measure$coefficients) == "Std. Error"], 3)
      stats$cue_mean_X_var_DF[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_mean_cat1:cue_std_cat1", variable.names(model_cur_measure$coefficients) == "df"], 2)
      stats$cue_mean_X_var_t[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_mean_cat1:cue_std_cat1", variable.names(model_cur_measure$coefficients) == "t value"], 2)
      stats$cue_mean_X_var_p[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "cue_mean_cat1:cue_std_cat1", variable.names(model_cur_measure$coefficients) == "Pr(>|t|)"], 3)
      # stim int X cue mean
      stats$stim_int_X_cue_mean_estimate[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "stim_level_cat1:cue_mean_cat1", variable.names(model_cur_measure$coefficients) == "Estimate"], 3)
      stats$stim_int_X_cue_mean_SE[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "stim_level_cat1:cue_mean_cat1", variable.names(model_cur_measure$coefficients) == "Std. Error"], 3)
      stats$stim_int_X_cue_mean_DF[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "stim_level_cat1:cue_mean_cat1", variable.names(model_cur_measure$coefficients) == "df"], 2)
      stats$stim_int_X_cue_mean_t[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "stim_level_cat1:cue_mean_cat1", variable.names(model_cur_measure$coefficients) == "t value"], 2)
      stats$stim_int_X_cue_mean_p[cur_row] = round(model_cur_measure$coefficients[row.names(model_cur_measure$coefficients) == "stim_level_cat1:cue_mean_cat1", variable.names(model_cur_measure$coefficients) == "Pr(>|t|)"], 3)
      
    }
  }
}

## create a long version for plots
stats_stim_int = select(stats, c(modality:brain_measure, contains("stim_int") & !contains("_cue")))
stats_stim_int$effect = "stim_int"
names(stats_stim_int) = gsub("stim_int_", "", names(stats_stim_int))

stats_cue_mean = select(stats, c(modality:brain_measure, contains("cue_mean") & !contains("_X_")))
stats_cue_mean$effect = "cue_mean"
names(stats_cue_mean) = gsub("cue_mean_", "", names(stats_cue_mean))

stats_cue_var = select(stats, c(modality:brain_measure, contains("cue_var")))
stats_cue_var$effect = "cue_var"
names(stats_cue_var) = gsub("cue_var_", "", names(stats_cue_var))

stats_cue_sk_neg = select(stats, c(modality:brain_measure, contains("cue_sk_neg")))
stats_cue_sk_neg$effect = "cue_sk_neg"
names(stats_cue_sk_neg) = gsub("cue_sk_neg_sym_", "", names(stats_cue_sk_neg))

stats_cue_sk_pos = select(stats, c(modality:brain_measure, contains("cue_sk_pos")))
stats_cue_sk_pos$effect = "cue_sk_pos"
names(stats_cue_sk_pos) = gsub("cue_sk_pos_sym_", "", names(stats_cue_sk_pos))

stats_cue_mean_X_var = select(stats, c(modality:brain_measure, contains("cue_mean_X_var")))
stats_cue_mean_X_var$effect = "cue_mean_X_var"
names(stats_cue_mean_X_var) = gsub("cue_mean_X_var_", "", names(stats_cue_mean_X_var))

stats_stim_int_X_cue_mean = select(stats, c(modality:brain_measure, contains("stim_int_X_cue_mean")))
stats_stim_int_X_cue_mean$effect = "stim_int_X_cue_mean"
#stats_cue_mean_X_var_long = gather(stats_stim_int_X_cue_mean, stat_measure, value, contains("stim_int_X_cue_mean"), factor_key = TRUE)
#stats_cue_mean_X_var_long$stat_measure = gsub("stim_int_X_cue_mean", "", stats_stim_int_X_cue_mean_long$stat_measure)
names(stats_stim_int_X_cue_mean) = gsub("stim_int_X_cue_mean_", "", names(stats_stim_int_X_cue_mean))

# combine to one df
#stats_long = bind_rows(stats_stim_int_long, stats_cue_mean_long, stats_cue_var_long, stats_cue_sk_neg_long, stats_cue_sk_pos_long, stats_cue_mean_X_var_long)
stats_long = bind_rows(stats_stim_int, stats_cue_mean, stats_cue_var, stats_cue_sk_neg, stats_cue_sk_pos, stats_cue_mean_X_var, stats_stim_int_X_cue_mean)

# add significance levels
stats_long$sig = stats_long$p < 0.05
stats_long$sig_level[stats_long$p >= 0.05] = ""
stats_long$sig_level[stats_long$p < 0.05] = "*"
stats_long$sig_level[stats_long$p < 0.01] = "**"
stats_long$sig_level[stats_long$p < 0.001] = "***"

# add more interpretable names (currently based on atlas labels)
# for effects
effects.labs = c("Stimulus\nIntensity", "Cue\nMean", "Cue\nVariance", "Cue Skewness\nNeg vs. Sym", "Cue Skewness\nPos vs. Sym", "Cue Mean x\nCue Variance", "Stimulus Intensity x\nCue Mean")
names(effects.labs) = c("stim_int", "cue_mean", "cue_var", "cue_sk_neg", "cue_sk_pos", "cue_mean_X_var", "stim_int_X_cue_mean")
stats_long$effect = factor(stats_long$effect, levels=c("stim_int", "cue_mean", "cue_var", "cue_sk_neg", "cue_sk_pos", "cue_mean_X_var", "stim_int_X_cue_mean"))
# for modality
modality.labs = c("Pain", "Visual Contrast")
names(modality.labs) = c("pain", "vision")
# for brain measures (neuromarkers and ROIs)
map_names = c("nps"="NPS",
              "siips"="SIIPS",
              "Thal_VPLM_R" = "Right VPL/VPM Thal",
              "Thal_VPLM_L" = "Left VPL/VPM Thal",
              "Thal_MD" = "Med Thal",
              "dpIns_R" = "Right dpIns",
              "dpIns_L" = "Left dpIns",
              "aIns_R" = "Right aIns",
              "aIns_L" = "Left aIns",
              "aMCC_MPFC" = "aMCC",
              "s1_handplus_R" = "Right S1 hand",
              "s1_handplus_L" = "Left S1 hand",
              "Bstem_PAG" = "PAG",
              "Ctx_IPS1_R" = "Right IPS",
              "Ctx_IPS1_L" = "Left IPS",
              "Ctx_11l_R" = "Right mid-lateral OFC",
              "Ctx_11l_L" = "Left mid-lateral OFC",
              "Ctx_p9_46v_R" = "Right dlPFC",
              "Ctx_8C_L_and_Ctx_46_L" = "Left dlPFC",
              "NAc_shell_like_R" = "Right NAc shell",
              "NAc_shell_like_L" = "Left NAc shell",
              "NAc_core_like_R" = "Right NAc core",
              "NAc_core_like_L" = "Left NAc core",
              "Thal_Lateral_Geniculate_Nucleus_R" = "Right LGN",
              "Thal_Lateral_Geniculate_Nucleus_L" = "Left LGN",
              "Ctx_V1_R" = "Right V1",
              "Ctx_V1_L" = "Left V1",
              "Ctx_V2_R" = "Right V2",
              "Ctx_V2_L" = "Left V2",
              "Ctx_V3_R" = "Right V3",
              "Ctx_V3_L" = "Left V3",
              "Ctx_V3A_R" = "Right V3A",
              "Ctx_V3A_L" = "Left V3A",
              "Ctx_V4_R" = "Right V4",
              "Ctx_V4_L" = "Left V4",
              "Ctx_VMV1_R" = "Right ventro-medial V1",
              "Ctx_VMV1_L" = "Left ventro-medial V1",
              "Ctx_VMV2_R" = "Right ventro-medial V2",
              "Ctx_VMV2_L" = "Left ventro-medial V2",
              "Ctx_VMV3_R" = "Right ventro-medial V3",
              "Ctx_VMV3_L" = "Left ventro-medial V3",
              "Ctx_V3B_R" = "Right V3B",
              "Ctx_V3B_L" = "Left V3B",
              "Ctx_V3CD_R" = "Right V3CD",
              "Ctx_V3CD_L" = "Left V3CD",
              "Ctx_V4t_R" = "Right V4t",
              "Ctx_V4t_L" = "Left V4t",
              "Ctx_MT_R" = "Right V5 (MT)",
              "Ctx_MT_L" = "Left V5 (MT)")
stats_long$brain_measure_name = map_names[stats_long$brain_measure]
data_long$brain_measure_name = map_names[data_long$brain_measure]
stats_long$brain_measure_name = factor(stats_long$brain_measure_name, levels=map_names)

# define subtypes of ROIs
pain_neuromarkers = c("NPS", "SIIPS")
pain_early_percept = c("Right VPL/VPM Thal", "Left VPL/VPM Thal", "Right dpIns", "Left dpIns")
visual_early_percept = c("Right LGN", "Left LGN", "Right V1", "Left V1", "Right V2", "Left V2")
pain_percept = c("PAG", "aMCC", "Med Thal")
visual_percept = c("Right V3", "Left V3", "Right V4", "Left V4", "Right V5 (MT)", "Left V5 (MT)")
pain_higher_level = c("Right NAc core", "Left NAc core", "Right NAc shell", "Left NAc shell", "Right mid-lateral OFC", "Left mid-lateral OFC","Right dlPFC", "Left dlPFC")
visual_higher_level = c("Right IPS", "Left IPS")
pain_other = c("Right aIns", "Left aIns", "Right S1 hand", "Left S1 hand")
visual_other = c("Right ventro-medial V1", "Left ventro-medial V1", "Right ventro-medial V2", "Left ventro-medial V2", "Right ventro-medial V3", "Left ventro-medial V3", "Right V3A", "Left V3A", "Right V3B", "Left V3B", "Right V3CD", "Left V3CD", "Right V4t", "Left V4t")
stats_long$subtype[stats_long$brain_measure_name %in% pain_neuromarkers] = "pain_neuromarker"
stats_long$subtype[stats_long$brain_measure_name %in% pain_early_percept] = "pain_early_percept"
stats_long$subtype[stats_long$brain_measure_name %in% visual_early_percept] = "visual_early_percept"
stats_long$subtype[stats_long$brain_measure_name %in% pain_percept] = "pain_percept"
stats_long$subtype[stats_long$brain_measure_name %in% visual_percept] = "pain_early_percept"
stats_long$subtype[stats_long$brain_measure_name %in% pain_higher_level] = "pain_higher_level"
stats_long$subtype[stats_long$brain_measure_name %in% visual_higher_level] = "visual_higher_level"
stats_long$subtype[stats_long$brain_measure_name %in% pain_other] = "pain_other"
stats_long$subtype[stats_long$brain_measure_name %in% visual_other] = "visual_other"

# save the table
output_filename = paste(results_path, "/stats_brain_measures_wide_format.csv", sep = "")
write.csv(stats, output_filename)
output_filename = paste(results_path, "/stats_brain_measures.csv", sep = "")
write.csv(stats_long, output_filename)

## bar plot with all regions and effects - estimates (standardized - so can be compared)
# stim period
map_types = c("neuromarker" = "Neuromarker",
              "pain" = "Pain",
              "visual" = "Visual",
              "higher_level" = "Higher Level")
stats_long$type_labels = map_types[stats_long$type]
  
min_estimate_stim = min(stats_long$estimate[stats_long$period == "stim"])
max_estimate_stim = max(stats_long$estimate[stats_long$period == "stim"])
effects_bar_plot_stim = ggplot(data = stats_long[stats_long$period == "stim",], aes(x = brain_measure_name, y = estimate, fill = type_labels)) +
  geom_bar(width=.8,stat="identity", position=position_dodge(0.8)) + # Bar plot
  geom_text(aes(label = sig_level, y = max_estimate_stim), position = position_dodge(0.8)) +
  labs(y = "Estimate", x="Brain measure", fill = "Measure type") +
  scale_y_continuous(limits = c(min_estimate_stim, max_estimate_stim), breaks = seq(-1,1, 0.05)) +
  ggtitle("Stimulus Period") +
  theme(text = element_text(size = 10),legend.position="bottom",panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white"), axis.text.x = element_text(angle = 90)) +
  facet_grid(effect ~ modality, labeller = labeller(effect = effects.labs, modality = modality.labs))

effects_bar_plot_stim_pain = ggplot(data = stats_long[stats_long$period == "stim" & stats_long$modality == "pain" & stats_long$type %in% c("neuromarker", "pain", "higher_level") & stats_long$effect != "stim_int_X_cue_mean",], aes(x = brain_measure_name, y = estimate, fill = type_labels)) +
  geom_bar(width=.8,stat="identity", position=position_dodge(0.8)) + # Bar plot
  geom_text(aes(label = sig_level, y = max_estimate_stim), position = position_dodge(0.8)) +
  labs(y = "Estimate", x="Brain measure", fill = "Measure type") +
  scale_y_continuous(limits = c(min_estimate_stim, max_estimate_stim), breaks = seq(-1,1, 0.05)) +
  theme(text = element_text(size = 9),legend.position="bottom",panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white"), axis.text.x = element_text(angle = 90)) +
  facet_grid(effect ~ ., labeller = labeller(effect = effects.labs))

effects_bar_plot_stim_vision = ggplot(data = stats_long[stats_long$period == "stim" & stats_long$modality == "vision" & stats_long$type %in% c("visual", "higher_level") & stats_long$effect != "stim_int_X_cue_mean",], aes(x = brain_measure_name, y = estimate, fill = type_labels)) +
  geom_bar(width=.8,stat="identity", position=position_dodge(0.8)) + # Bar plot
  geom_text(aes(label = sig_level, y = max_estimate_stim), position = position_dodge(0.8)) +
  labs(y = "Estimate", x="Brain measure", fill = "Measure type") +
  scale_y_continuous(limits = c(min_estimate_stim, max_estimate_stim), breaks = seq(-1,1, 0.05)) +
  theme(text = element_text(size = 9),legend.position="bottom",panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white"), axis.text.x = element_text(angle = 90)) +
  facet_grid(effect ~ ., labeller = labeller(effect = effects.labs))


# cue period
min_estimate_cue = min(stats_long$estimate[stats_long$period == "cue"])
max_estimate_cue = max(stats_long$estimate[stats_long$period == "cue"])
effects_bar_plot_cue = ggplot(data = stats_long[stats_long$period == "cue",], aes(x = brain_measure_name, y = estimate, fill = type)) +
  geom_bar(width=.8,stat="identity", position=position_dodge(0.8)) + 
  #geom_text(aes(label = sig_level, y = estimate + 0.02 * sign(estimate)), position = position_dodge(0.8)) +
  geom_text(aes(label = sig_level, y = max_estimate_cue), position = position_dodge(0.8)) +
  labs(y = "Estimate", x="Brain measure", fill = "Measure type") +
  scale_y_continuous(limits = c(min_estimate_cue, max_estimate_cue), breaks = seq(-1,1, 0.05)) +
  ggtitle("Cue Period") +
  theme(text = element_text(size = 10),legend.position="bottom",panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white"), axis.text.x = element_text(angle = 90)) +
  facet_grid(effect ~ modality, labeller = labeller(effect = effects.labs, modality = modality.labs))

effects_bar_plot_cue_pain = ggplot(data = stats_long[stats_long$period == "cue" & stats_long$modality == "pain" & stats_long$type %in% c("neuromarker", "pain", "higher_level") & !stats_long$effect %in% c("stim_int", "stim_int_X_cue_mean"),], aes(x = brain_measure_name, y = estimate, fill = type_labels)) +
  geom_bar(width=.8,stat="identity", position=position_dodge(0.8)) + 
  #geom_text(aes(label = sig_level, y = estimate + 0.02 * sign(estimate)), position = position_dodge(0.8)) +
  geom_text(aes(label = sig_level, y = max_estimate_cue), position = position_dodge(0.8)) +
  labs(y = "Estimate", x="Brain measure", fill = "Measure type") +
  scale_y_continuous(limits = c(min_estimate_cue, max_estimate_cue), breaks = seq(-1,1, 0.05)) +
  theme(text = element_text(size = 9),legend.position="bottom",panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white"), axis.text.x = element_text(angle = 90)) +
  facet_grid(effect ~ ., labeller = labeller(effect = effects.labs))

effects_bar_plot_cue_vision = ggplot(data = stats_long[stats_long$period == "cue" & stats_long$modality == "vision" & stats_long$type %in% c("visual", "higher_level") & !stats_long$effect %in% c("stim_int", "stim_int_X_cue_mean"),], aes(x = brain_measure_name, y = estimate, fill = type_labels)) +
  geom_bar(width=.8,stat="identity", position=position_dodge(0.8)) + 
  #geom_text(aes(label = sig_level, y = estimate + 0.02 * sign(estimate)), position = position_dodge(0.8)) +
  geom_text(aes(label = sig_level, y = max_estimate_cue), position = position_dodge(0.8)) +
  labs(y = "Estimate", x="Brain measure", fill = "Measure type") +
  scale_y_continuous(limits = c(min_estimate_cue, max_estimate_cue), breaks = seq(-1,1, 0.05)) +
  theme(text = element_text(size = 9),legend.position="bottom",panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white"), axis.text.x = element_text(angle = 90)) +
  facet_grid(effect ~ ., labeller = labeller(effect = effects.labs))


## plot main effects - detailed plots - ROIs with cue mean effect
regions_main_effects = c("PAG", "Right NAc core", "Left NAc core", "Right NAc shell", "Left NAc shell")
regions_main_effects_data = data_long[data_long$brain_measure_name %in% regions_main_effects,]
names(regions_main_effects_data)[names(regions_main_effects_data) == "brain_measure_score"] = "ROI_score"
names(regions_main_effects_data)[names(regions_main_effects_data) == "brain_measure"] = "ROI"
names(regions_main_effects_data)[names(regions_main_effects_data) == "brain_measure_name"] = "ROI_name"
regions_main_effects_data$period = factor(regions_main_effects_data$period)
regions_main_effects_data$ROI = factor(regions_main_effects_data$ROI, levels = regions_main_effects)

# mean
data_mean_for_plot_roi_bySub = aggregate(x=regions_main_effects_data$ROI_score,
                                     by=list(participant = regions_main_effects_data$participant, modality = regions_main_effects_data$modality, stim_level_cat = regions_main_effects_data$stim_level_cat, cue_mean_cat = regions_main_effects_data$cue_mean_cat, ROI = regions_main_effects_data$ROI_name, period = regions_main_effects_data$period),
                                     function(x) mean = mean(x, na.rm = TRUE))
data_mean_for_plot_roi = aggregate(data_mean_for_plot_roi_bySub$x,list(modality=data_mean_for_plot_roi_bySub$modality, stim_level_cat = data_mean_for_plot_roi_bySub$stim_level_cat, cue_mean_cat=data_mean_for_plot_roi_bySub$cue_mean_cat, ROI = data_mean_for_plot_roi_bySub$ROI, period = data_mean_for_plot_roi_bySub$period),function(x) c(ROI_score = mean(x), sd = sd(x), n = length(x)))
data_mean_for_plot_roi = cbind(modality=data_mean_for_plot_roi$modality, ROI = data_mean_for_plot_roi$ROI, period = data_mean_for_plot_roi$period, stim_level_cat = data_mean_for_plot_roi$stim_level_cat, cue_mean_cat=data_mean_for_plot_roi$cue_mean_cat, as.data.frame(data_mean_for_plot_roi$x))
data_mean_for_plot_roi$SEM = data_mean_for_plot_roi$sd/sqrt(data_mean_for_plot_roi$n)
data_mean_for_plot_roi_bySub = dplyr::rename(data_mean_for_plot_roi_bySub, ROI_score = x)
data_mean_for_plot_roi_bySub$ROI = factor(data_mean_for_plot_roi_bySub$ROI, levels = regions_main_effects)
data_mean_for_plot_roi$ROI = factor(data_mean_for_plot_roi$ROI, levels = regions_main_effects)

ROI_mean_effect_plot = ggplot(data = data_mean_for_plot_roi[data_mean_for_plot_roi$period == "stim",], aes(x = stim_level_cat, y = ROI_score, fill = cue_mean_cat, color = cue_mean_cat)) +
  geom_bar(width=.8,stat="identity", alpha = 0.3,position=position_dodge(0.8)) + # Bar plot
  geom_errorbar(position=position_dodge(0.8),width=1/8, aes(ymin=ROI_score-SEM, ymax=ROI_score+SEM), color ="black")  + # add error bar of SEM
  geom_point(data=data_mean_for_plot_roi_bySub, aes(y = ROI_score, x = stim_level_cat, color = cue_mean_cat), alpha = 0.2, size = 1, position=position_jitterdodge(jitter.width = .4, dodge.width = 0.8))+
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(y = "ROI score", x="Stimulus level", fill = "Cue mean") +
  guides(color = "none") +
  theme(text = element_text(size = 10),legend.position="bottom",panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white")) +
  facet_grid(modality ~ ROI, scales="free", labeller = labeller(modality = modality.labs))
  
ROI_mean_effect_plot_pain = ggplot(data = data_mean_for_plot_roi[data_mean_for_plot_roi$period == "stim" & data_mean_for_plot_roi$modality == "pain",], aes(x = cue_mean_cat, y = ROI_score, fill = stim_level_cat, color = stim_level_cat)) +
  geom_bar(width=.8,stat="identity", alpha = 0.3,position=position_dodge(0.8)) + # Bar plot
  geom_errorbar(position=position_dodge(0.8),width=1/8, aes(ymin=ROI_score-SEM, ymax=ROI_score+SEM), color ="black")  + # add error bar of SEM
  geom_point(data=data_mean_for_plot_roi_bySub[data_mean_for_plot_roi_bySub$period == "stim" & data_mean_for_plot_roi_bySub$modality == "pain",], aes(y = ROI_score, x = cue_mean_cat, color = stim_level_cat), alpha = 0.2, size = 1, position=position_jitterdodge(jitter.width = .4, dodge.width = 0.8))+
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(y = "ROI score", x="Cue mean", fill = "Stimulus level") +
  guides(color = "none") +
  theme(text = element_text(size = 10),legend.position="right",panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white")) +
  facet_grid(. ~ ROI, scales="free")


## plot main effects - detailed plots - neuromarkers
neuromarker_data = data_long[data_long$type == "neuromarker",]
names(neuromarker_data)[names(neuromarker_data) == "brain_measure_score"] = "neuromarker_score"
names(neuromarker_data)[names(neuromarker_data) == "brain_measure"] = "neuromarker"
names(neuromarker_data)[names(neuromarker_data) == "brain_measure_name"] = "neuromarker_name"

# mean
data_mean_for_plot_bySub = aggregate(x=neuromarker_data$neuromarker_score,
                                     by=list(participant = neuromarker_data$participant, modality = neuromarker_data$modality, stim_level_cat = neuromarker_data$stim_level_cat, cue_mean_cat = neuromarker_data$cue_mean_cat, neuromarker = neuromarker_data$neuromarker_name, period = neuromarker_data$period),
                                     function(x) mean = mean(x, na.rm = TRUE))
data_mean_for_plot = aggregate(data_mean_for_plot_bySub$x,list(modality=data_mean_for_plot_bySub$modality, stim_level_cat = data_mean_for_plot_bySub$stim_level_cat, cue_mean_cat=data_mean_for_plot_bySub$cue_mean_cat, neuromarker = data_mean_for_plot_bySub$neuromarker, period = data_mean_for_plot_bySub$period),function(x) c(neuromarker_score = mean(x), sd = sd(x), n = length(x)))
data_mean_for_plot = cbind(modality=data_mean_for_plot$modality, neuromarker = data_mean_for_plot$neuromarker, period = data_mean_for_plot$period, stim_level_cat = data_mean_for_plot$stim_level_cat, cue_mean_cat=data_mean_for_plot$cue_mean_cat, as.data.frame(data_mean_for_plot$x))
data_mean_for_plot$SEM = data_mean_for_plot$sd/sqrt(data_mean_for_plot$n)
data_mean_for_plot_bySub = dplyr::rename(data_mean_for_plot_bySub, neuromarker_score = x)

# plot
neuromarker_mean_effect_plot = ggplot(data = data_mean_for_plot[data_mean_for_plot$period == "stim",], aes(x = stim_level_cat, y = neuromarker_score, fill = cue_mean_cat, color = cue_mean_cat)) +
  geom_bar(width=.8,stat="identity", alpha = 0.3,position=position_dodge(0.8)) + # Bar plot
  geom_errorbar(position=position_dodge(0.8),width=1/8, aes(ymin=neuromarker_score-SEM, ymax=neuromarker_score+SEM), color ="black")  + # add error bar of SEM
  geom_point(data=data_mean_for_plot_bySub[data_mean_for_plot_bySub$period == "stim",], aes(y = neuromarker_score, x = stim_level_cat, color = cue_mean_cat), alpha = 0.2, size = 1, position=position_jitterdodge(jitter.width = .4, dodge.width = 0.8))+
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(y = "Neuromarker score", x="Stimulus level", fill = "Cue mean") +
  guides(color = "none") +
  theme(text = element_text(size = 10),legend.position="bottom",panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white")) +
  facet_grid(neuromarker ~ modality, scales="free", labeller = labeller(modality = modality.labs))


neuromarker_mean_effect_plot_paper_nps = ggplot(data = data_mean_for_plot[data_mean_for_plot$neuromarker == "NPS" & data_mean_for_plot$period == "stim",], aes(x = stim_level_cat, y = neuromarker_score, fill = cue_mean_cat, color = cue_mean_cat)) +
  geom_bar(width=.8,stat="identity", alpha = 0.3,position=position_dodge(0.8)) + # Bar plot
  geom_errorbar(position=position_dodge(0.8),width=1/8, aes(ymin=neuromarker_score-SEM, ymax=neuromarker_score+SEM), color ="black")  + # add error bar of SEM
  geom_point(data=data_mean_for_plot_bySub[data_mean_for_plot_bySub$neuromarker == "NPS" & data_mean_for_plot_bySub$period == "stim",], aes(y = neuromarker_score, x = stim_level_cat, color = cue_mean_cat), alpha = 0.2, size = 1, position=position_jitterdodge(jitter.width = .4, dodge.width = 0.8))+
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(y = "NPS score", x="Stimulus level", fill = "Cue mean") +
  guides(color = "none") +
  theme(text = element_text(size = 10),legend.position="bottom",panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white")) +
  facet_grid(. ~ modality, scales="free", labeller = labeller(modality = modality.labs))

neuromarker_mean_effect_plot_paper_siips = ggplot(data = data_mean_for_plot[data_mean_for_plot$neuromarker == "SIIPS" & data_mean_for_plot$period == "stim",], aes(x = stim_level_cat, y = neuromarker_score, fill = cue_mean_cat, color = cue_mean_cat)) +
  geom_bar(width=.8,stat="identity", alpha = 0.3,position=position_dodge(0.8)) + # Bar plot
  geom_errorbar(position=position_dodge(0.8),width=1/8, aes(ymin=neuromarker_score-SEM, ymax=neuromarker_score+SEM), color ="black")  + # add error bar of SEM
  geom_point(data=data_mean_for_plot_bySub[data_mean_for_plot_bySub$neuromarker == "SIIPS" & data_mean_for_plot_bySub$period == "stim",], aes(y = neuromarker_score, x = stim_level_cat, color = cue_mean_cat), alpha = 0.2, size = 1, position=position_jitterdodge(jitter.width = .4, dodge.width = 0.8))+
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(y = "SIIPS score", x="Stimulus level", fill = "Cue mean") +
  scale_y_continuous(labels = comma) +
  guides(color = "none") +
  theme(text = element_text(size = 10),legend.position="bottom",panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white")) +
  facet_grid(. ~ modality, scales="free", labeller = labeller(modality = modality.labs))

neuromarker_mean_effect_plot_paper_combined = ggarrange(neuromarker_mean_effect_plot_paper_nps, neuromarker_mean_effect_plot_paper_siips,
                                                        labels = c(""), nrow = 1, ncol = 2,
                                                        common.legend = TRUE, legend = "bottom")

# save plots
ggsave('plots_nps_siips.pdf',
       plot = neuromarker_mean_effect_plot_paper_combined,
       path = fig_output_path,
       width = 180,
       height = 100,
       units = "mm",
       dpi = 300)


ggsave('effects_bar_plot_stim_pain.pdf',
       plot = effects_bar_plot_stim_pain,
       path = fig_output_path,
       width = 200,
       height = 230,
       units = "mm",
       dpi = 300)

ggsave('effects_bar_plot_stim_visual.pdf',
       plot = effects_bar_plot_stim_vision,
       path = fig_output_path,
       width = 200,
       height = 230,
       units = "mm",
       dpi = 300)

ggsave('effects_bar_plot_cue_pain.pdf',
       plot = effects_bar_plot_cue_pain,
       path = fig_output_path,
       width = 200,
       height = 230,
       units = "mm",
       dpi = 300)

ggsave('effects_bar_plot_cue_visual.pdf',
       plot = effects_bar_plot_cue_vision,
       path = fig_output_path,
       width = 200,
       height = 230,
       units = "mm",
       dpi = 300)