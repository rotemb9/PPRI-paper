function [predicted_perception,xdata] = perception_model_expect_learn_mod(x, xdata)
% perception model - expectation and learning, but different
% weighting per modality
% w(t+1) = w(t) - a * PE; rating(t) = (1-w) * intensity * s + w * cue_signal(t)
% but with a seperate w and a seperate a for vision and for pain
% 6 free parameters: s_p, s_v, w0_p (initial weighting of expectations for pain), w0_v
% (initial w for vision), a_p (learning rate for pain), a_v (learning rate for
% vision).
% this function calculates the perception based on the modality, the
% intensity of the stimulus, and the weighted mean of the cue
% (weights, represented by k and b values, are based on the optimization of the expectation task data per
% participant and modality) with a learning rate for expectation effects
% (based on prediction errors)
% input x is an array with the parameters s_p, s_v, a_p, a_v, w_p and w_v (in that order)
% xdata is a table with the relevant data:
% .vas1:.vas10: values of the 10 cues
% .modality: the modality 'pain' / 'vision'
% .stim_level: the level of the stimulus intensity
% .weighted_expectation: the expectation based on the weighted cue mean

xdata.PE = xdata.rating - xdata.weighted_expectation;

xdata.s(strcmp(xdata.modality, 'pain')) = repmat(x(1),height(xdata(strcmp(xdata.modality, 'pain'),:)),1);
xdata.s(strcmp(xdata.modality, 'vision')) = repmat(x(2),height(xdata(strcmp(xdata.modality, 'vision'),:)),1);

% compute w for pain trials (w_p)
xdata_pain = xdata(strcmp(xdata.modality, 'pain'),:);
xdata_pain.w(1) = x(3);
a_p = x(5);
for trial_ind = 2:height(xdata_pain)
    xdata_pain.w(trial_ind) = xdata_pain.w(trial_ind-1) - a_p .* xdata_pain.PE(trial_ind) ./ 100; % the /100 is because the PE is in 0-100 scale, but a is in 0-1
    if xdata_pain.w(trial_ind) < 0
       xdata_pain.w(trial_ind:end) = 0;
       break;
    end
end

% compute w for vision trials (w_v)
xdata_vision = xdata(strcmp(xdata.modality, 'vision'),:);
xdata_vision.w(1) = x(4);
a_v = x(6);
for trial_ind = 2:height(xdata_vision)
    xdata_vision.w(trial_ind) = xdata_vision.w(trial_ind-1) - a_v .* xdata_vision.PE(trial_ind) ./ 100; % the /100 is because the PE is in 0-100 scale, but a is in 0-1
    if xdata_vision.w(trial_ind) < 0
        xdata_vision.w(trial_ind:end) = 0;
        break;
    end
end

% combine data from pain and vision
xdata.w(strcmp(xdata.modality, 'pain')) = xdata_pain.w;
xdata.w(strcmp(xdata.modality, 'vision')) = xdata_vision.w;

% predict perception
xdata.predicted_rating = (1-xdata.w) .* xdata.stim_level_scale_calibration .* xdata.s + xdata.w .* xdata.weighted_expectation;
predicted_perception = xdata.predicted_rating;

end