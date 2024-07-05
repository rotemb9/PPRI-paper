%%  expectation mode with 2 free parameters:
%% b: weighting of values that are below vs. above the mean (based on alogistic function) (b=0 no overweighting; b<0 overweighting of values below the mean; b>0 overweighting of values above the mean)
%% k: weighting of outliers vs. inliers (based on an exponent) (k =1 means no overweighting; k<1 overweighting of inliers; k>1 overweighting of outliers)
%% k cannot be negative
%% this function uses the optimizaton from 'optimize_expectation_weighting.m'
%% by reading in the optimization table, and then saves the predicted expectations each trial of the expectation and cued perception tasks

raw_data_path = fullfile('data_for_analysis', 'raw_data');
processed_data_path = fullfile('data_for_analysis', 'processed_data');
% load optimized parameters for the expectation model for each participant
% and modality
expect_params_filename = fullfile(processed_data_path,'expectation_model_params_measures.csv');
expect_params = readtable(expect_params_filename);

% get expectation task data
expect_data_filename = fullfile(raw_data_path,'task-expect_all_subjs.csv');
expect_data = readtable(expect_data_filename);
expect_data = expect_data(:, 2:end);

vas_columns_expect = find(contains(expect_data.Properties.VariableNames, 'vas_val'));
% compute simple mean
expect_data.vas_mean = mean(expect_data{:,vas_columns_expect},2);

% cued perception task data
percept_data_filename = fullfile(raw_data_path,'task-expectpercept_all_subjs_with_exclusions.csv');
percept_data = readtable(percept_data_filename);
percept_data = percept_data(:, 2:end);

vas_columns_percept = find(contains(percept_data.Properties.VariableNames, 'vas_val'));
% compute simple mean
percept_data.vas_mean = mean(percept_data{:,vas_columns_percept},2);

% get all participants and modalities
participants = unique(expect_data.participant);
modalities = unique(expect_data.modality);

% compute predicted expectations for each participant and modality
% based on the optimized parameters from the input table
% and visualize
expect_data_with_pred = expect_data;
percept_data_with_pred = percept_data;

for sub_ind = 1:length(participants)
    cur_sub = participants{sub_ind};
    disp(cur_sub);
    for modality_ind = 1:length(modalities)
        modality = modalities{modality_ind};
        disp(modality);
        % get k and b for the participant and modality
        cur_k = expect_params.k(strcmp(expect_params.participants_all, cur_sub) & strcmp(expect_params.modalities_all, modality));
        cur_b = expect_params.b(strcmp(expect_params.participants_all, cur_sub) & strcmp(expect_params.modalities_all, modality));
        % get the expectation data for this participant and modality and make sure
        % trial number is correct
        cur_data = expect_data_with_pred(strcmp(expect_data_with_pred.participant, cur_sub) & strcmp(expect_data_with_pred.modality, modality), :);
        n_trials_expect = height(cur_data);
        disp([num2str(n_trials_expect) ' trials found in expectation data']);
        % get the 10 values data of each cue
        xdata = cur_data{:,vas_columns_expect};
        % predict expectations
        [pred_expect,weights] = compute_expectation_weights_logistic_power_v1([cur_k,cur_b], xdata);
        % make sure predictions and weights are in range
        if any(pred_expect < 0 | pred_expect > 100)
            warning([num2str(sum(pred_expect < 0 | pred_expect > 100)) ' expectations out of range']);
        end
        if any(weights(:) < 0)
            num_trials_neg_weight = sum(any(weights < 0,2));
            warning([num2str(num_trials_neg_weight) ' trials with negative weight']);
            warning(['minimal weight is ' num2str(min(weights(:)))]);
        end
        % add predicted expectations to expect_data_with_pred
        expect_data_with_pred.predicted_expect(strcmp(expect_data_with_pred.participant, cur_sub) & strcmp(expect_data_with_pred.modality, modality)) = pred_expect;
        
        % cued perception task
        % get the perception data for this participant and modality and make sure
        % trial number is correct
        cur_data = percept_data_with_pred(strcmp(percept_data_with_pred.participant, cur_sub) & strcmp(percept_data_with_pred.modality, modality), :);
        n_trials_percept = height(cur_data);
        disp([num2str(n_trials_percept) ' trials found in perception data']);
        % get the 10 values data of each cue
        xdata = cur_data{:,vas_columns_percept};
        % predict expectations
        [pred_expect,weights] = compute_expectation_weights_logistic_power_v1([cur_k,cur_b], xdata);
        % make sure predictions and weights are in range
        if any(pred_expect < 0 | pred_expect > 100)
            warning([num2str(sum(pred_expect < 0 | pred_expect > 100)) ' expectations out of range']);
        end
        if any(weights(:) < 0)
            num_trials_neg_weight = sum(any(weights < 0,2));
            warning([num2str(num_trials_neg_weight) ' trials with negative weight']);
            warning(['minimal weight is ' num2str(min(weights(:)))]);
        end
        % add predicted expectations to percept_data_with_pred
        percept_data_with_pred.predicted_expect(strcmp(percept_data_with_pred.participant, cur_sub) & strcmp(percept_data_with_pred.modality, modality)) = pred_expect;
    end
end

% save the data with predicted expectations
writetable(expect_data_with_pred, fullfile(processed_data, 'task-expect_all_subjs_with_pred.csv'));
writetable(percept_data_with_pred, fullfile(processed_data, 'task-expectpercept_all_subjs_with_pred_and_exclusions.csv'));