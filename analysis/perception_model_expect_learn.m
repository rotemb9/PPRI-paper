function [predicted_perception,xdata] = perception_model_expect_learn(x, xdata)
% perception model - expectation and learning
% w(t+1) = w(t) - a * PE; rating(t) = (1-w) * intensity * s + w * cue_signal(t)
% 4 free parameters: a (learning rate), w0 (initial weighting of
% expectations), s_p (scaling of the rating data from the calibration task-
% pain), s_v (same as s_p but for vision)
% this function calculates the perception based on the modality, the
% intensity of the stimulus, and the weighted mean of the cue
% (weights, represented by k and b values, are based on the optimization of the expectation task data per
% participant and modality) with a learning rate for expectation effects
% (based on prediction errors)
% input x is an array with the parameters s_p, s_v, w0 and a (in that order)
% data is a table with the relevant data:
% .vas1:.vas10: values of the 10 cues
% .modality: the modality 'pain' / 'vision'
% .stim_level: the level of the stimulus intensity
% .stim_level_scale_calibration: the expected rating based on the
% calibration task only
% .weighted_expectation: the expectation based on the weighted cue mean

xdata.s(strcmp(xdata.modality, 'pain')) = repmat(x(1),height(xdata(strcmp(xdata.modality, 'pain'),:)),1);
xdata.s(strcmp(xdata.modality, 'vision')) = repmat(x(2),height(xdata(strcmp(xdata.modality, 'vision'),:)),1);

xdata.PE = xdata.rating - xdata.weighted_expectation;
xdata.w(1) = x(3);
for trial_ind = 2:height(xdata)
    xdata.w(trial_ind) = xdata.w(trial_ind-1) - x(4) .* xdata.PE(trial_ind) ./ 100; % the /100 is because the PE is in 0-100 scale, but a is in 0-1
    if xdata.w(trial_ind) < 0
        xdata.w(trial_ind:end) = 0;
        break
    end
end

xdata.predicted_rating = (1-xdata.w) .* xdata.stim_level_scale_calibration .* xdata.s + xdata.w .* xdata.weighted_expectation;
predicted_perception = xdata.predicted_rating;

end