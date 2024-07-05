function [expectation, weights] = compute_expectation_weights(x, xdata)
% this function calculates the expectation based on weighted mean, with
% weights depending on k (x(1)) and b (x(2)), and the cues (xdata)

rowmin = min(xdata, [], 2);
rowmax = max(xdata, [], 2);
xdata_rescaled = rescale(xdata, 'InputMin', rowmin, 'InputMax', rowmax);
xdata_rescaled_demeaned = xdata_rescaled - mean(xdata_rescaled,2);
logistic_term = @(x, b, x0) 1 ./ (1 + exp(-b * (x - x0)));
power_term = @(x, k) (sign(x) .* abs(x) .^ k) ./ x;
weights_power = power_term(xdata_rescaled_demeaned,x(1));
weights_power = weights_power ./ sum(weights_power,2);
weights_logistic = logistic_term(xdata_rescaled_demeaned, x(2), 0);
weights = weights_power + weights_logistic;
weights = weights ./ sum(weights,2);
expectation = sum(xdata .* weights,2);