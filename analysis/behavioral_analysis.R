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

fig_output_path = "plots"

## read data
data_path = "data_for_analysis/raw_data"
tasks = c("stimresp", "expect", "expectpercept")
for (t in tasks) {
  task_filename = file.path(data_path, paste("task-", t, "_all_subjs.csv", sep=""))
  task_data = read.csv(task_filename, header = T)
  assign(paste("data", t, sep="_"), task_data)
}

# for expectpercept task, need to exclude some runs/trials because of equipment issues (pain/response box)
# read the table that includes these exclusions
data_expectpercept_exclusions_filename = file.path(data_path, "task-expectpercept_all_subjs_with_exclusions.csv")
data_expectpercept_exclusions = read.csv(data_expectpercept_exclusions_filename, header = T)
data_expectpercept$exclude_tech_issues = data_expectpercept_exclusions$exclude_tech_issues
data_expectpercept$exclude_RT = data_expectpercept_exclusions$exclude_RT

# ---------------------------------
### analyze stimulus-response task
# ---------------------------------
print("analyzing stimulus-response task")
data_stimresp$modality = factor(data_stimresp$modality)
contrasts(data_stimresp$modality) = c(1, -1)
data_stimresp_pain = data_stimresp[data_stimresp$modality=="pain",]
data_stimresp_vision = data_stimresp[data_stimresp$modality=="vision",]

# z score values (for standerdized beta)
data_stimresp$stim_level_zscored = scale(data_stimresp$stim_level, center = TRUE, scale=TRUE)
data_stimresp$vas_initial_value_zscored = scale(data_stimresp$vas_initial_value, center = TRUE, scale=TRUE)
data_stimresp$rating_zscored = scale(data_stimresp$rating, center = TRUE, scale=TRUE)

data_stimresp_pain$stim_level_zscored = scale(data_stimresp_pain$stim_level, center = TRUE, scale=TRUE)
data_stimresp_pain$vas_initial_value_zscored = scale(data_stimresp_pain$vas_initial_value, center = TRUE, scale=TRUE)
data_stimresp_pain$rating_zscored = scale(data_stimresp_pain$rating, center = TRUE, scale=TRUE)

data_stimresp_vision$stim_level_zscored = scale(data_stimresp_vision$stim_level, center = TRUE, scale=TRUE)
data_stimresp_vision$vas_initial_value_zscored = scale(data_stimresp_vision$vas_initial_value, center = TRUE, scale=TRUE)
data_stimresp_vision$rating_zscored = scale(data_stimresp_vision$rating, center = TRUE, scale=TRUE)

## linear mixed effects model - ratings as a function of intensity and modality
stimresp_rating = lmer(rating_zscored ~ 1 + modality * stim_level_zscored  + (1 + modality + stim_level_zscored|participant), data=data_stimresp, na.action=na.omit)
summary(stimresp_rating)
stimresp_rating_pain = lmer(rating_zscored ~ 1 + stim_level_zscored + (1 + stim_level_zscored|participant), data=data_stimresp_pain, na.action=na.omit)
summary(stimresp_rating_pain)
stimresp_rating_vision = lmer(rating_zscored ~ 1 + stim_level_zscored + (1 + stim_level_zscored|participant), data=data_stimresp_vision, na.action=na.omit)
summary(stimresp_rating_vision)

# ------------------------------------
### analyze expectations (task-expect)
# ------------------------------------
print("analyzing expectation task")
data_expect$rating_zscored = scale(data_expect$rating, center = TRUE, scale=TRUE)
data_expect$cue_mean_zscored = scale(data_expect$cue_mean, center = TRUE, scale=TRUE)
data_expect$cue_std_zscored = scale(data_expect$cue_std, center = TRUE, scale=TRUE)
data_expect$cue_sk_zscored = scale(data_expect$cue_sk, center = TRUE, scale=TRUE)
data_expect$modality = factor(data_expect$modality)
contrasts(data_expect$modality) = c(1, -1)

# categorize cue_std and cue_sk
data_expect$cue_std_cat[data_expect$cue_std==0.050] = "low"
data_expect$cue_std_cat[data_expect$cue_std==0.125] = "high"
data_expect$cue_sk_cat[data_expect$cue_sk<0] = "negative"
data_expect$cue_sk_cat[data_expect$cue_sk==0] = "none"
data_expect$cue_sk_cat[data_expect$cue_sk>0] = "positive"
data_expect$cue_std_cat = factor(data_expect$cue_std_cat, levels = c("low","high"))
data_expect$cue_sk_cat = factor(data_expect$cue_sk_cat, levels = c("none","negative","positive"))
contrasts(data_expect$cue_std_cat) = c(-1, 1)

# prepare better labels for the modality
modality.labs = c("Pain", "Visual Contrast")
names(modality.labs) = c("pain", "vision")

## summarize data
expect_data_summary_bySub = aggregate(x=data_expect$rating,by=list(participant = data_expect$participant,modality = data_expect$modality, cue_mean = data_expect$cue_mean, cue_std_cat = data_expect$cue_std_cat, cue_sk_cat = data_expect$cue_sk_cat), FUN=mean)
expect_data_summary = aggregate(expect_data_summary_bySub$x,list(modality=expect_data_summary_bySub$modality, cue_mean=expect_data_summary_bySub$cue_mean, cue_std_cat=expect_data_summary_bySub$cue_std_cat, cue_sk_cat=expect_data_summary_bySub$cue_sk_cat),function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
expect_data_summary = cbind(modality=expect_data_summary$modality, cue_mean=expect_data_summary$cue_mean,cue_std_cat=expect_data_summary$cue_std_cat,cue_sk_cat=expect_data_summary$cue_sk_cat, as.data.frame(expect_data_summary$x))
expect_data_summary$SEM = expect_data_summary$sd/sqrt(expect_data_summary$n)
expect_data_summary$cue_sk_cat = factor(expect_data_summary$cue_sk_cat, levels= c("negative","none","positive"))
data_expect$cue_sk_cat = factor(data_expect$cue_sk_cat, levels= c("negative","none","positive"))
expect_data_summary_bySub =dplyr::rename(expect_data_summary_bySub, rating = x)

# mean and SE across participants
expect_data_summary_bySub_modality = aggregate(x=expect_data_summary_bySub$rating,by=list(participant = expect_data_summary_bySub$participant,modality = expect_data_summary_bySub$modality), FUN=mean)
# vision
expect_mean_vision = mean(expect_data_summary_bySub_modality$x[expect_data_summary_bySub_modality$modality == "vision"])
expect_sd_vision = sd(expect_data_summary_bySub_modality$x[expect_data_summary_bySub_modality$modality == "vision"])/sqrt(sum(expect_data_summary_bySub_modality$modality == "vision"))
# pain
expect_mean_pain = mean(expect_data_summary_bySub_modality$x[expect_data_summary_bySub_modality$modality == "pain"])
expect_sd_pain = sd(expect_data_summary_bySub_modality$x[expect_data_summary_bySub_modality$modality == "pain"])/sqrt(sum(expect_data_summary_bySub_modality$modality == "vision"))

## plot main effects
# Cue mean and variance
data_expect$cue_mean_cat[data_expect$cue_mean<0.5] = "low"
data_expect$cue_mean_cat[data_expect$cue_mean==0.5] = "medium"
data_expect$cue_mean_cat[data_expect$cue_mean>0.5] = "high"
data_expect$cue_mean_cat = factor(data_expect$cue_mean_cat, levels = c("low","medium","high"), order = TRUE)
expect_data_summary_bySub_mean_cat_no_sk = aggregate(x=data_expect$rating,by=list(participant = data_expect$participant,modality = data_expect$modality, cue_mean_cat = data_expect$cue_mean_cat, cue_std_cat = data_expect$cue_std_cat), FUN=mean)
expect_data_summary_mean_cat_no_sk = aggregate(expect_data_summary_bySub_mean_cat_no_sk$x,list(modality=expect_data_summary_bySub_mean_cat_no_sk$modality, cue_mean_cat=expect_data_summary_bySub_mean_cat_no_sk$cue_mean_cat, cue_std_cat=expect_data_summary_bySub_mean_cat_no_sk$cue_std_cat),function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
expect_data_summary_mean_cat_no_sk = cbind(modality=expect_data_summary_mean_cat_no_sk$modality, cue_mean_cat=expect_data_summary_mean_cat_no_sk$cue_mean_cat,cue_std_cat=expect_data_summary_mean_cat_no_sk$cue_std_cat, as.data.frame(expect_data_summary_mean_cat_no_sk$x))
expect_data_summary_mean_cat_no_sk$SEM = expect_data_summary_mean_cat_no_sk$sd/sqrt(expect_data_summary_mean_cat_no_sk$n)
expect_data_summary_bySub_mean_cat_no_sk = dplyr::rename(expect_data_summary_bySub_mean_cat_no_sk, rating = x)

expectations_by_cue_mean_var_int = ggplot(data=expect_data_summary_mean_cat_no_sk, aes(x=cue_mean_cat, y=mean, fill=cue_std_cat,color=cue_std_cat)) +
  geom_point(data=expect_data_summary_bySub_mean_cat_no_sk, aes(y = rating, x = cue_mean_cat, fill = cue_std_cat, color=cue_std_cat), alpha = 0.3, size = 1, position=position_jitterdodge(jitter.width = .4, dodge.width = 0.8))+
  geom_bar(width=.8,stat="identity", alpha = 0.2,position=position_dodge(0.8)) + # Bar plot
  geom_errorbar(position=position_dodge(0.8),width=1/8, aes(ymin=mean-SEM, ymax=mean+SEM), color ="black")  + # add error bar of SEM
  scale_x_discrete("Cue mean") +
  scale_y_continuous("Rating",breaks=seq(0, 100, 10)) +
  #scale_y_continuous("Rating", breaks=seq(10, 80, 10)) +
  coord_cartesian(ylim = c(20, 75)) +
  labs(fill = "Cue variance") +
  guides(color = "none") +
  theme(text = element_text(size = 10, color = "black"),legend.position="bottom",panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white")) +
  facet_grid(~modality, labeller = labeller(modality = modality.labs))


# SK
data_expect_sk_for_plot_bySub = aggregate(x=data_expect$rating,
                                            by=list(participant = data_expect$participant, modality = data_expect$modality, cue_sk_cat = data_expect$cue_sk_cat),
                                            FUN = mean)
data_expect_sk_for_plot_bySub$cue_sk_cat = factor(data_expect_sk_for_plot_bySub$cue_sk_cat, levels = c("negative","none","positive"))
data_expect_sk_for_plot = aggregate(data_expect_sk_for_plot_bySub$x,list(modality=data_expect_sk_for_plot_bySub$modality, cue_sk_cat=data_expect_sk_for_plot_bySub$cue_sk_cat),function(x) c(rating = mean(x), sd = sd(x), n = length(x)))
data_expect_sk_for_plot = cbind(modality=data_expect_sk_for_plot$modality, cue_sk_cat=data_expect_sk_for_plot$cue_sk_cat, as.data.frame(data_expect_sk_for_plot$x))
data_expect_sk_for_plot$SEM = data_expect_sk_for_plot$sd/sqrt(data_expect_sk_for_plot$n)
data_expect_sk_for_plot_bySub = dplyr::rename(data_expect_sk_for_plot_bySub, rating = x)

expect_sk_effect_plot = ggplot(data = data_expect_sk_for_plot, aes(x = cue_sk_cat, y = rating, fill = cue_sk_cat, color = cue_sk_cat)) +
  geom_point(data=data_expect_sk_for_plot_bySub, aes(y = rating, x = cue_sk_cat, color = cue_sk_cat), alpha = 0.3, size = 1, position=position_jitterdodge(jitter.width = .8, dodge.width = 0.8))+
  geom_bar(width=.8,stat="identity", alpha = 0.2,position=position_dodge(0.8)) + # Bar plot
  geom_errorbar(position=position_dodge(0.8),width=1/8, aes(ymin=rating-SEM, ymax=rating+SEM), color ="black")  + # add error bar of SEM
  #scale_y_continuous("Rating",breaks=seq(0, 100, 10), limits = c(0,100)) +
  scale_y_continuous("Rating",breaks=seq(0, 100, 5)) +
  coord_cartesian(ylim = c(33, 60)) +
  labs(x="Cue SK", fill = "Cue SK") +
  guides(color = "none") +
  theme(text = element_text(size = 10),legend.position="bottom",panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), strip.background =element_rect(fill="white"))+
  facet_grid(~modality, labeller = labeller(modality = modality.labs))

# Figure2: expect data results plot
expect_results = ggarrange(expectations_by_cue_mean_var_int, expect_sk_effect_plot,
                           labels = c("A", "B"), nrow = 1, ncol = 2,
                           common.legend = FALSE, widths = c(1.5,1))

ggsave('plots_expect_results.pdf',
       plot = expect_results,
       path = fig_output_path,
       width = 180,
       height = 100,
       units = "mm",
       dpi = 300)

## linear models - ratings as a function of modality and cue
data_expect$cue_sk_cat = relevel(data_expect$cue_sk_cat, ref = "none")

model_expect_rating = lmer(rating_zscored ~ 1 + modality * cue_mean_zscored * cue_std_cat * cue_sk_cat + (1 + modality + cue_mean_zscored + cue_std_cat|participant), data=data_expect, na.action=na.omit)
summary(model_expect_rating)

# separate for pain and vision
data_expect_pain = data_expect[data_expect$modality == "pain", ]
data_expect_pain$rating_zscored = scale(data_expect_pain$rating, center = TRUE, scale=TRUE)
data_expect_vision = data_expect[data_expect$modality == "vision", ]
data_expect_vision$rating_zscored = scale(data_expect_vision$rating, center = TRUE, scale=TRUE)

model_expect_rating_pain = lmer(rating_zscored ~ 1 + cue_mean_zscored * cue_std_cat * cue_sk_cat + (1 + cue_mean_zscored + cue_std_cat | participant), data=data_expect_pain, na.action=na.omit)
summary(model_expect_rating_pain)

model_expect_rating_vision = lmer(rating_zscored ~ 1 + cue_mean_zscored * cue_std_cat * cue_sk_cat + (1 + cue_mean_zscored + cue_std_cat | participant), data=data_expect_vision, na.action=na.omit)
summary(model_expect_rating_vision)

# -----------------------------------------
### analyze cued perception task (task-expectpercept)
# -----------------------------------------
print("analyzing cued perception task")

data_expectpercept$stim_level_cat[data_expectpercept$stim_level==1] = "low"
data_expectpercept$stim_level_cat[data_expectpercept$stim_level==2] = "high"
data_expectpercept$cue_mean_cat[data_expectpercept$cue_mean<0.5] = "low"
data_expectpercept$cue_mean_cat[data_expectpercept$cue_mean>0.5] = "high"

data_expectpercept$cue_std_cat[data_expectpercept$cue_std<0.1] = "low"
data_expectpercept$cue_std_cat[data_expectpercept$cue_std>0.1] = "high"
data_expectpercept$cue_sk_cat[data_expectpercept$cue_sk<0] = "negative"
data_expectpercept$cue_sk_cat[data_expectpercept$cue_sk==0] = "none"
data_expectpercept$cue_sk_cat[data_expectpercept$cue_sk>0] = "positive"
data_expectpercept$modality = factor(data_expectpercept$modality, levels = c("pain","vision"))
data_expectpercept$stim_level_cat = factor(data_expectpercept$stim_level_cat, levels = c("low","high"))
data_expectpercept$cue_mean_cat = factor(data_expectpercept$cue_mean_cat, levels = c("low","high"))
data_expectpercept$cue_std_cat = factor(data_expectpercept$cue_std_cat, levels = c("low","high"))
data_expectpercept$cue_sk_cat = factor(data_expectpercept$cue_sk_cat, levels = c("none","negative","positive"))
contrasts(data_expectpercept$modality) = c(1,-1)
contrasts(data_expectpercept$stim_level_cat) = c(-1,1)
contrasts(data_expectpercept$cue_mean_cat) = c(-1,1)
contrasts(data_expectpercept$cue_std_cat) = c(-1,1)

# before excluding trials, add information about trial order, and categorize manipulated values (cue mean, variance, skewness)
# (to later check whether participants learned the cues are not predictive and stopped being affected by them)
for (subj in unique(data_expectpercept$participant)) {
  data_expectpercept$order[data_expectpercept$participant==subj & data_expectpercept$modality=="pain" & data_expectpercept$cue_mean_cat=="low"] = 1:36
  data_expectpercept$order[data_expectpercept$participant==subj & data_expectpercept$modality=="pain" & data_expectpercept$cue_mean_cat=="high"] = 1:36
  data_expectpercept$order[data_expectpercept$participant==subj & data_expectpercept$modality=="vision" & data_expectpercept$cue_mean_cat=="low"] = 1:36
  data_expectpercept$order[data_expectpercept$participant==subj & data_expectpercept$modality=="vision" & data_expectpercept$cue_mean_cat=="high"] = 1:36  
}

# exclude trials
warning(paste(sum(data_expectpercept$exclude_tech_issues), "out of", nrow(data_expectpercept) , "trials excluded because of equipment issues", sep=" "))
data_expectpercept = data_expectpercept[!data_expectpercept$exclude_tech_issues,]
warning(paste(sum(data_expectpercept$exclude_RT), "out of", nrow(data_expectpercept) , "trials excluded because of RT < 0.2 or > 4.5", sep=" "))
data_expectpercept = data_expectpercept[!data_expectpercept$exclude_RT,]
warning(paste(nrow(data_expectpercept), "trials remaining", sep = " "))

# z score numerical measures
data_expectpercept$rating_zscored = scale(data_expectpercept$rating, center = TRUE, scale=TRUE)
data_expectpercept$cue_mean_zscored = scale(data_expectpercept$cue_mean, center = TRUE, scale=TRUE)
data_expectpercept$cue_std_zscored = scale(data_expectpercept$cue_std, center = TRUE, scale=TRUE)
data_expectpercept$cue_sk_zscored = scale(data_expectpercept$cue_sk, center = TRUE, scale=TRUE)
data_expectpercept$stim_level_zscored = scale(data_expectpercept$stim_level, center = TRUE, scale=TRUE)

# create DFs for pain and vision
data_expectpercept_pain = data_expectpercept[data_expectpercept$modality == "pain", ]
data_expectpercept_pain$rating_zscored = scale(data_expectpercept_pain$rating, center = TRUE, scale=TRUE)
data_expectpercept_vision = data_expectpercept[data_expectpercept$modality == "vision", ]
data_expectpercept_vision$rating_zscored = scale(data_expectpercept_vision$rating, center = TRUE, scale=TRUE)

## summarize data
expectpercept_data_summary_bySub = aggregate(x=data_expectpercept$rating,by=list(participant = data_expectpercept$participant,modality = data_expectpercept$modality, stim_level_cat = data_expectpercept$stim_level_cat, cue_mean_cat = data_expectpercept$cue_mean_cat, cue_std_cat = data_expectpercept$cue_std_cat, cue_sk_cat = data_expectpercept$cue_sk_cat), FUN=mean)
expectpercept_data_summary = aggregate(expectpercept_data_summary_bySub$x,list(modality=expectpercept_data_summary_bySub$modality, cue_mean_cat=expectpercept_data_summary_bySub$cue_mean_cat, cue_std_cat=expectpercept_data_summary_bySub$cue_std_cat, cue_sk_cat=expectpercept_data_summary_bySub$cue_sk_cat),function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
expectpercept_data_summary = cbind(modality=expectpercept_data_summary$modality, cue_mean_cat=expectpercept_data_summary$cue_mean_cat,cue_std_cat=expectpercept_data_summary$cue_std_cat,cue_sk_cat=expectpercept_data_summary$cue_sk_cat, as.data.frame(expectpercept_data_summary$x))
expectpercept_data_summary$SEM = expectpercept_data_summary$sd/sqrt(expectpercept_data_summary$n)
expectpercept_data_summary$cue_sk_cat = factor(expectpercept_data_summary$cue_sk_cat, levels= c("negative","none","positive"))
data_expectpercept$cue_sk_cat = factor(data_expectpercept$cue_sk_cat, levels= c("negative","none","positive"))
expectpercept_data_summary_bySub$cue_sk_cat = factor(expectpercept_data_summary_bySub$cue_sk_cat, levels= c("negative","none","positive"))
expectpercept_data_summary_bySub = dplyr::rename(expectpercept_data_summary_bySub, rating = x)

## linear models
data_expectpercept$cue_sk_cat = relevel(data_expectpercept$cue_sk_cat, ref = "none")

model_percept_rating = lmer(rating_zscored ~ 1 + modality * stim_level_cat * cue_mean_cat * cue_std_cat * cue_sk_cat + (1 + modality + cue_mean_cat|participant), data=data_expectpercept, na.action=na.omit)
summary(model_percept_rating)

## models for pain and vision separately
# pain
model_percept_rating_pain = lmer(rating_zscored ~ 1 + stim_level_cat * cue_mean_cat * cue_std_cat * cue_sk_cat + (1 + stim_level_cat + cue_mean_cat|participant), data=data_expectpercept_pain, na.action=na.omit)
summary(model_percept_rating_pain)

# vision
model_percept_rating_vision = lmer(rating_zscored ~ 1 + stim_level_cat * cue_mean_cat * cue_std_cat * cue_sk_cat + (1 + stim_level_cat+cue_mean_cat|participant), data=data_expectpercept_vision, na.action=na.omit)
summary(model_percept_rating_vision)

## other effects
# does the effect of cue mean on rating disappear with time? (interaction between cue_mean and trial_num)
data_expectpercept$trial_num_zscored = scale(data_expectpercept$trial_num, center = TRUE, scale = TRUE)
data_expectpercept_pain$trial_num_zscored = scale(data_expectpercept_pain$trial_num, center = TRUE, scale = TRUE)
data_expectpercept_vision$trial_num_zscored = scale(data_expectpercept_vision$trial_num, center = TRUE, scale = TRUE)
summary(lmer(rating_zscored ~ 1 + modality*cue_mean_cat*trial_num_zscored + (1 + cue_mean_cat + trial_num_zscored|participant), data=data_expectpercept, na.action=na.omit))
# only for pain
summary(lmer(rating_zscored ~ 1 + cue_mean_cat*trial_num_zscored + (1 + cue_mean_cat + trial_num_zscored|participant), data=data_expectpercept_pain, na.action=na.omit))
# only for vision
summary(lmer(rating_zscored ~ 1 + cue_mean_cat*trial_num_zscored + (1 + cue_mean_cat + trial_num_zscored|participant), data=data_expectpercept_vision, na.action=na.omit))

# y- rating; x - cue mean; color - cue SD; grid- modality
expectpercept_data_summary_bySub_mean_var_int = aggregate(x=data_expectpercept$rating,by=list(participant = data_expectpercept$participant,modality = data_expectpercept$modality, cue_mean_cat = data_expectpercept$cue_mean_cat, cue_std_cat = data_expectpercept$cue_std_cat), FUN=mean)
expectpercept_data_summary_mean_var_int = aggregate(expectpercept_data_summary_bySub_mean_var_int$x,list(modality=expectpercept_data_summary_bySub_mean_var_int$modality, cue_mean_cat=expectpercept_data_summary_bySub_mean_var_int$cue_mean_cat, cue_std_cat=expectpercept_data_summary_bySub_mean_var_int$cue_std_cat),function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
expectpercept_data_summary_mean_var_int = cbind(modality=expectpercept_data_summary_mean_var_int$modality, cue_mean_cat=expectpercept_data_summary_mean_var_int$cue_mean_cat,cue_std_cat=expectpercept_data_summary_mean_var_int$cue_std_cat, as.data.frame(expectpercept_data_summary_mean_var_int$x))
expectpercept_data_summary_mean_var_int$SEM = expectpercept_data_summary_mean_var_int$sd/sqrt(expectpercept_data_summary_mean_var_int$n)
expectpercept_data_summary_bySub_mean_var_int = dplyr::rename(expectpercept_data_summary_bySub_mean_var_int, rating = x)

perception_by_cue = ggplot(data=expectpercept_data_summary_mean_var_int, aes(x=cue_mean_cat, y=mean, fill=cue_std_cat,color=cue_std_cat)) +
  geom_point(data=expectpercept_data_summary_bySub_mean_var_int, aes(y = rating, x = cue_mean_cat, fill = cue_std_cat, color=cue_std_cat), alpha = 0.3, size = 1, position=position_jitterdodge(jitter.width = 0.4, dodge.width = 0.8))+
  geom_bar(width=.8,stat="identity", alpha = 0.2,position=position_dodge(0.8)) + # Bar plot
  geom_errorbar(position=position_dodge(0.8),width=1/8, aes(ymin=mean-SEM, ymax=mean+SEM), color ="black")  + # add error bar of SEM
  scale_x_discrete("Cue mean") +
  scale_y_continuous("Rating",breaks=seq(0, 100, 10)) +
  #coord_cartesian(ylim = c(10, 85)) +
  labs(fill = "Cue variance") +
  guides(color = "none") +
  theme(text = element_text(size = 10),legend.position="bottom",panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white")) +
  facet_grid(~ modality, labeller = labeller(modality = modality.labs))

# effect of skewness
data_expectpercept_sk_for_plot_bySub = aggregate(x=data_expectpercept$rating,
                                          by=list(participant = data_expectpercept$participant, modality = data_expectpercept$modality, cue_sk_cat = data_expectpercept$cue_sk_cat),
                                          FUN = mean)
data_expectpercept_sk_for_plot_bySub$cue_sk_cat = factor(data_expectpercept_sk_for_plot_bySub$cue_sk_cat, levels = c("negative","none","positive"))
data_expectpercept_sk_for_plot = aggregate(data_expectpercept_sk_for_plot_bySub$x,list(modality=data_expectpercept_sk_for_plot_bySub$modality, cue_sk_cat=data_expectpercept_sk_for_plot_bySub$cue_sk_cat),function(x) c(rating = mean(x), sd = sd(x), n = length(x)))
data_expectpercept_sk_for_plot = cbind(modality=data_expectpercept_sk_for_plot$modality, cue_sk_cat=data_expectpercept_sk_for_plot$cue_sk_cat, as.data.frame(data_expectpercept_sk_for_plot$x))
data_expectpercept_sk_for_plot$SEM = data_expectpercept_sk_for_plot$sd/sqrt(data_expectpercept_sk_for_plot$n)
data_expectpercept_sk_for_plot_bySub = dplyr::rename(data_expectpercept_sk_for_plot_bySub, rating = x)

expectpercept_sk_effect_plot = ggplot(data = data_expectpercept_sk_for_plot, aes(x = cue_sk_cat, y = rating, fill = cue_sk_cat, color = cue_sk_cat)) +
  geom_point(data=data_expectpercept_sk_for_plot_bySub, aes(y = rating, x = cue_sk_cat, color = cue_sk_cat), alpha = 0.3, size = 1, position=position_jitterdodge(jitter.width = .8, dodge.width = 0.8))+
  geom_bar(width=.8,stat="identity", alpha = 0.2,position=position_dodge(0.8)) + # Bar plot
  geom_errorbar(position=position_dodge(0.8),width=1/8, aes(ymin=rating-SEM, ymax=rating+SEM), color ="black")  + # add error bar of SEM
  #scale_y_continuous("Rating",breaks=seq(0, 100, 10), limits = c(0,100)) +
  scale_y_continuous("Rating",breaks=seq(0, 100, 10)) +
  coord_cartesian(ylim = c(15, 90)) +
  labs(x="Cue SK", fill = "Cue SK") +
  guides(color = "none") +
  theme(text = element_text(size = 10),legend.position="bottom",panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white"))+
  facet_grid(~modality, labeller = labeller(modality = modality.labs))


## effect across time
# summarize across participant for a grouped plot
rating_by_time = aggregate(x = data_expectpercept$rating, by = list(trial = data_expectpercept$order, modality = data_expectpercept$modality, cue_mean = data_expectpercept$cue_mean_cat), function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
rating_by_time = cbind(modality=rating_by_time$modality, trial = rating_by_time$trial, cue_mean = rating_by_time$cue_mean, as.data.frame(rating_by_time$x))
rating_by_time$SEM = rating_by_time$sd/sqrt(rating_by_time$n)


# plot
expectpercept_over_time_both = ggplot(data = rating_by_time,aes(y = mean, x = trial, color = cue_mean, group = cue_mean) ) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_errorbar(aes(ymin=mean-SEM, ymax=mean+SEM),width=.2, position=position_dodge(0.05)) +
  geom_vline(xintercept=seq(6.5,36.5,6), alpha = 0.5) +
  scale_y_continuous(breaks=seq(0, 80, 10), limits = c(30,80)) +
  #scale_y_continuous(breaks=seq(0, 80, 10)) +
  labs(x = "trial", y = "Rating", color = "Cue mean") +
  scale_color_manual(values = c("blue2","red2")) +
  theme(text = element_text(size = 10),legend.position="bottom",panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), strip.background=element_rect(fill="white")) +
  facet_grid(~ modality, labeller = labeller(modality = modality.labs))

# Figure3: expectpercept data - behavioral results plot
expectpercept_results = ggarrange(perception_by_cue, expectpercept_sk_effect_plot,
                                  labels = c("A", "B"), nrow = 1, ncol = 2,
                                  common.legend = FALSE)
expectpercept_results_figure = ggarrange(expectpercept_results, expectpercept_over_time_both,
                                         labels = c("", "C"), nrow = 2, ncol = 1)

ggsave('plots_expectpercept_results.pdf',
       plot = expectpercept_results_figure,
       path = fig_output_path,
       width = 180,
       height = 200,
       units = "mm",
       dpi = 300)

data_for_diff_by_time = select(data_expectpercept, participant, modality, cue_mean_cat, order, rating)
data_for_diff_by_time = spread(data_for_diff_by_time, cue_mean_cat, rating)
data_for_diff_by_time$cue_effect =  data_for_diff_by_time$high - data_for_diff_by_time$low
diff_by_time = aggregate(x = data_for_diff_by_time$cue_effect, by = list(trial = data_for_diff_by_time$order, modality = data_for_diff_by_time$modality), function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
diff_by_time = cbind(modality=diff_by_time$modality, trial = diff_by_time$trial, as.data.frame(diff_by_time$x))
diff_by_time$SEM = diff_by_time$sd/sqrt(diff_by_time$n)

# is there an effect of cue on the last trial? Across participants, t test
t.test(x=data_for_diff_by_time$low[data_for_diff_by_time$order==36],y=data_for_diff_by_time$high[data_for_diff_by_time$order==36], paired = TRUE)

# is there a correlation across participants with respect to the cue effect? E.g., cue effect for pain and cue effect for vision, across participants
# effect of mean per participant and modality
## rating diff high minus low cue, as a function of cue sd and sk
expectpercept_data_summary_bySub_mean_diff = pivot_wider(expectpercept_data_summary_bySub, names_from = cue_mean_cat, values_from = rating)
expectpercept_data_summary_bySub_mean_diff$rating_diff = expectpercept_data_summary_bySub_mean_diff$high - expectpercept_data_summary_bySub_mean_diff$low
expectpercept_data_summary_bySub_mean_diff = select(expectpercept_data_summary_bySub_mean_diff, -c(low, high))
expectpercept_data_summary_mean_diff = aggregate(expectpercept_data_summary_bySub_mean_diff$rating_diff,list(modality=expectpercept_data_summary_bySub_mean_diff$modality, cue_std_cat=expectpercept_data_summary_bySub_mean_diff$cue_std_cat, cue_sk_cat=expectpercept_data_summary_bySub_mean_diff$cue_sk_cat),function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
expectpercept_data_summary_mean_diff = cbind(modality=expectpercept_data_summary_mean_diff$modality, cue_std_cat=expectpercept_data_summary_mean_diff$cue_std_cat,cue_sk_cat=expectpercept_data_summary_mean_diff$cue_sk_cat, as.data.frame(expectpercept_data_summary_mean_diff$x))
expectpercept_data_summary_mean_diff$SEM = expectpercept_data_summary_mean_diff$sd/sqrt(expectpercept_data_summary_mean_diff$n)
expectpercept_data_summary_mean_diff$cue_sk_cat = factor(expectpercept_data_summary_mean_diff$cue_sk_cat, levels= c("negative","none","positive"))
expectpercept_data_summary_mean_diff$cue_sk_cat = factor(expectpercept_data_summary_mean_diff$cue_sk_cat, levels= c("negative","none","positive"))
expectpercept_data_summary_bySub_mean_diff$cue_sk_cat = factor(expectpercept_data_summary_bySub_mean_diff$cue_sk_cat, levels= c("negative","none","positive"))

data_for_corr_cue_mean_effect = aggregate(expectpercept_data_summary_bySub_mean_diff$rating_diff,list(participant = expectpercept_data_summary_bySub_mean_diff$participant, modality = expectpercept_data_summary_bySub_mean_diff$modality), FUN = mean)
data_for_corr_cue_mean_effect_wide = spread(data_for_corr_cue_mean_effect, modality, x)
cor.test(data_for_corr_cue_mean_effect_wide$pain, data_for_corr_cue_mean_effect_wide$vision)
