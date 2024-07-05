function [predicted_perception,xdata] = perception_model_expect_nolearn_mod(x, xdata)
% perception model with expectation but no learning, but different
% weighting per modality
% this function calculates the perception based on the modality, the
% intensity of the stimulus, and the weighted mean of the cue
% (weights, represented by k and b values, are based on the optimization of the expectation task data per
% participant and modality) with a learning rate for expectation effects
% (based on prediction errors)
% on each trial, rating(t) = (1-w) * intensity * s + w * cue_signal(t), where w
% is a free parameter which is then fixed throughout the task for that
% participant
% input x is an array with the parameters: s_p, s_v, w for pain, and w for vision
% (in that order)
% data is a table with the relevant data:
% .vas1:.vas10: values of the 10 cues
% .modality: the modality 'pain' / 'vision'
% .stim_level: the level of the stimulus intensity
% .stim_level_scale_calibration: the expected rating based on the
% calibration task only
% .weighted_expectation: the expectation based on the weighted cue mean

xdata.s(strcmp(xdata.modality, 'pain')) = repmat(x(1),height(xdata(strcmp(xdata.modality, 'pain'),:)),1);
xdata.s(strcmp(xdata.modality, 'vision')) = repmat(x(2),height(xdata(strcmp(xdata.modality, 'vision'),:)),1);

xdata.w(strcmp(xdata.modality, 'pain')) = repmat(x(3),sum(strcmp(xdata.modality, 'pain')),1);
xdata.w(strcmp(xdata.modality, 'vision')) = repmat(x(4),sum(strcmp(xdata.modality, 'vision')),1);

xdata.predicted_rating = (1-xdata.w) .* xdata.stim_level_scale_calibration .* xdata.s + xdata.w .* xdata.weighted_expectation;
predicted_perception = xdata.predicted_rating;