%% visualize the weight function (based on the expectation model) across participants
main_path = pwd;
processed_data_path = fullfile(main_path, '..', 'data_for_analysis', 'processed_data');
expect_model_params = readtable(fullfile(processed_data_path, 'expectation_model_params_measures.csv'));
median_k_pain = median(expect_model_params.k(strcmp(expect_model_params.modalities_all,'pain')));
median_b_pain = median(expect_model_params.b(strcmp(expect_model_params.modalities_all,'pain')));
median_k_vision = median(expect_model_params.k(strcmp(expect_model_params.modalities_all,'vision')));
median_b_vision = median(expect_model_params.b(strcmp(expect_model_params.modalities_all,'vision')));

%% visualize weight function
%% group level
x = linspace(20,60,10);
% pain
[expectation_pain, weights_pain] = compute_expectation_weights([median_k_pain, median_b_pain], x);
subplot(1,2,1);
plot(x, weights_pain);
yline(0.1, '--');
ylim([0, 0.2]);
xlabel('V');
ylabel('weight');
title('Pain');

% vision
[expectation_vision, weights_vision] = compute_expectation_weights([median_k_vision, median_b_vision], x);
subplot(1,2,2);
plot(x, weights_vision);
yline(0.1, '--');
title('Visual perception');
xlabel('V');
ylabel('weight');
ylim([0, 0.2]);