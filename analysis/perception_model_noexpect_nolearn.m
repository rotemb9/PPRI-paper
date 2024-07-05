function [predicted_perception,xdata] = perception_model_noexpect_nolearn(x, xdata)
% perception model with no expectation and no learning (baseline model)
% this function calculates the perception based on the modality and the
% intensity of the stimulus only
% on each trial, rating(t) = intensity * s, where s are two free
% parameters, s_p scales the intensity effect from the calibration task for pain and s_v for vision, for each participant
% input x is an array with the parameters s_p (first element) and s_v
% (second element)
% data is a table with the relevant data:
% .vas1:.vas10: values of the 10 cues
% .modality: the modality 'pain' / 'vision'
% .stim_level: the level of the stimulus intensity
% .stim_level_scale_calibration: the expected rating based on the
% calibration task only
% .weighted_expectation: the expectation based on the weighted cue mean

xdata.s(strcmp(xdata.modality, 'pain')) = repmat(x(1),height(xdata(strcmp(xdata.modality, 'pain'),:)),1);
xdata.s(strcmp(xdata.modality, 'vision')) = repmat(x(2),height(xdata(strcmp(xdata.modality, 'vision'),:)),1);
xdata.predicted_rating = xdata.stim_level_scale_calibration .*  xdata.s;
predicted_perception = xdata.predicted_rating;