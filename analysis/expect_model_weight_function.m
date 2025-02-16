%% visualize the weight function (based on the expectation model) across participants
main_path = pwd;
processed_data_path = fullfile(main_path, '..', 'data_for_analysis', 'processed_data');
expect_model_params = readtable(fullfile(processed_data_path, 'expectation_model_params_measures.csv'));
% pain
k_values_pain = expect_model_params.k(strcmp(expect_model_params.modalities_all,'pain'));
b_values_pain = expect_model_params.b(strcmp(expect_model_params.modalities_all,'pain'));
median_k_pain = median(k_values_pain);
median_b_pain = median(b_values_pain);
% vision
k_values_vision = expect_model_params.k(strcmp(expect_model_params.modalities_all,'vision'));
b_values_vision = expect_model_params.b(strcmp(expect_model_params.modalities_all,'vision'));
median_k_vision = median(k_values_vision);
median_b_vision = median(b_values_vision);

% compute weight functions of individual participants
x = linspace(20,60,10);
% pain
for ind = 1:length(k_values_pain)
    [expectation_pain_ind(ind, :), weights_pain_ind(ind, :)] = compute_expectation_weights([k_values_pain(ind), b_values_pain(ind)], x);
end
% vision
for ind = 1:length(k_values_vision)
    [expectation_vision_ind(ind, :), weights_vision_ind(ind, :)] = compute_expectation_weights([k_values_vision(ind), b_values_vision(ind)], x);
end

%% visualize weight function
%% group level + individual plots
% pain
[expectation_pain, weights_pain] = compute_expectation_weights([median_k_pain, median_b_pain], x);
subplot(1,2,1);
hold on
% Overlay individual pain weights (thin, black, and transparent)
for i = 1:size(weights_pain_ind, 1)
    plot(x, weights_pain_ind(i,:), 'Color', [0.8, 0.8, 0.8], 'LineWidth', 0.3);
end
% add group level line (based on median k and b)
plot(x, weights_pain, 'k', 'LineWidth', 3);

% Add horizontal reference line
yline(0.1, '--');

% Adjust axes and labels
ylim([0, 0.2]);
xlabel('V');
ylabel('weight');
title('Pain');

hold off




% vision
[expectation_vision, weights_vision] = compute_expectation_weights([median_k_vision, median_b_vision], x);
subplot(1,2,2);
hold on
% Overlay individual vision weights (thin, black, and transparent)
for i = 1:size(weights_vision_ind, 1)
    plot(x, weights_vision_ind(i,:), 'Color', [0.8, 0.8, 0.8], 'LineWidth', 0.3);
end
% add group level line (based on median k and b)
plot(x, weights_vision, 'k', 'LineWidth', 3);

% Add horizontal reference line
yline(0.1, '--');

% Adjust axes and labels
ylim([0, 0.2]);
xlabel('V');
ylabel('weight');
title('Visual perception');

hold off