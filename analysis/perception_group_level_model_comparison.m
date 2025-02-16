% group level model comparison

main_dir = 'data_for_analysis/processed_data';
output_dir = 'data_for_analysis/processed_data';
% get the optimization table with all the parameters and sse per model per
% participant
optimization_table_filename = fullfile(main_dir, 'perception_models_params_measures.csv');
optimization_table = readtable(optimization_table_filename);

% get the perception data with trial level predictions per model
percept_data_filename = fullfile(main_dir, 'task-expectpercept_all_subjs_with_pred_after_exclusions.csv');
percept_data = readtable(percept_data_filename);

% create a table for the group level measures and stats per model
model = (1:5)';
name = {'baseline', 'expect', 'expect_learn', 'expect_by_mod', 'expect_learn_by_mode'}';
models_group_level = table(model, name);

% each participant had 144 trials, so to get from ss to mse we divide the
% ss by 144
n_trials_per_sub = 144;
models_group_level.n_trials_per_sub(:) = n_trials_per_sub;

% mean SSE across participants
models_group_level.sse(models_group_level.model == 1) = mean(optimization_table.model1_ss);
models_group_level.sse(models_group_level.model == 2) = mean(optimization_table.model2_ss);
models_group_level.sse(models_group_level.model == 3) = mean(optimization_table.model3_ss);
models_group_level.sse(models_group_level.model == 4) = mean(optimization_table.model2mod_ss);
models_group_level.sse(models_group_level.model == 5) = mean(optimization_table.model3mod_ss);

% mean MSE across participants
models_group_level.mse = models_group_level.sse / n_trials_per_sub;

n = 45; % n subjects
models_group_level.n(:) = n;

% num params
models_group_level.k = [2, 3, 4, 4, 6]';

% AIC
models_group_level.AIC = 2 * models_group_level.k + n * log(models_group_level.sse / n);

% number of participants for whom this is the best model
models_group_level.num_subs_best(1) = sum(strcmp(optimization_table.best_model, 'model1'));
models_group_level.num_subs_best(2) = sum(strcmp(optimization_table.best_model, 'model2'));
models_group_level.num_subs_best(3) = sum(strcmp(optimization_table.best_model, 'model3'));
models_group_level.num_subs_best(4) = sum(strcmp(optimization_table.best_model, 'model2mod'));
models_group_level.num_subs_best(5) = sum(strcmp(optimization_table.best_model, 'model3mod'));

models_group_level.perc_sample_best = models_group_level.num_subs_best / n * 100;

% group level model comparison with f tests
first_model = [1, 2, 2, 4]'; % the simpler model for each comparisons
second_model = [2, 3, 4, 5]'; % the more complex model for each comparison
model_comparison_group = table(first_model, second_model);
for ind = 1:length(first_model)
    cur_first_model = first_model(ind);
    cur_second_model = second_model(ind);
    [model_comparison_group.F(ind), model_comparison_group.p(ind)] = model_comparison_test(models_group_level.sse(cur_first_model), models_group_level.sse(cur_second_model), models_group_level.k(cur_first_model), models_group_level.k(cur_second_model), n);
end

% save model comparison tables

writetable(models_group_level, fullfile(output_dir, 'percept_models_group_level.csv'));
writetable(model_comparison_group, fullfile(output_dir, 'percept_model_comparison_group.csv'));

% get stats for the scaling factor (for paper revision - comment 3.9)
optimization_table.scaling_factor_pain(strcmp(optimization_table.best_model, 'model1')) = optimization_table.model1_s_p(strcmp(optimization_table.best_model, 'model1'));
optimization_table.scaling_factor_vision(strcmp(optimization_table.best_model, 'model1')) = optimization_table.model1_s_v(strcmp(optimization_table.best_model, 'model1'));
optimization_table.scaling_factor_pain(strcmp(optimization_table.best_model, 'model2')) = optimization_table.model2_s_p(strcmp(optimization_table.best_model, 'model2'));
optimization_table.scaling_factor_vision(strcmp(optimization_table.best_model, 'model2')) = optimization_table.model2_s_v(strcmp(optimization_table.best_model, 'model2'));
optimization_table.scaling_factor_pain(strcmp(optimization_table.best_model, 'model3')) = optimization_table.model3_s_p(strcmp(optimization_table.best_model, 'model3'));
optimization_table.scaling_factor_vision(strcmp(optimization_table.best_model, 'model3')) = optimization_table.model3_s_v(strcmp(optimization_table.best_model, 'model3'));
optimization_table.scaling_factor_pain(strcmp(optimization_table.best_model, 'model2mod')) = optimization_table.model2mod_s_p(strcmp(optimization_table.best_model, 'model2mod'));
optimization_table.scaling_factor_vision(strcmp(optimization_table.best_model, 'model2mod')) = optimization_table.model2mod_s_v(strcmp(optimization_table.best_model, 'model2mod'));
optimization_table.scaling_factor_pain(strcmp(optimization_table.best_model, 'model3mod')) = optimization_table.model3mod_s_p(strcmp(optimization_table.best_model, 'model3mod'));
optimization_table.scaling_factor_vision(strcmp(optimization_table.best_model, 'model3mod')) = optimization_table.model3mod_s_v(strcmp(optimization_table.best_model, 'model3mod'));

mean(optimization_table.scaling_factor_pain);
std(optimization_table.scaling_factor_pain);
mean(optimization_table.scaling_factor_vision);
std(optimization_table.scaling_factor_vision);
% different from 1?
% pain
[h,p,~,stats] = ttest(optimization_table.scaling_factor_pain, 1);
% vision
[h,p,~,stats] = ttest(optimization_table.scaling_factor_vision, 1);
% compare between modalities
[h,p,~,stats] = ttest(optimization_table.scaling_factor_pain, optimization_table.scaling_factor_vision);
