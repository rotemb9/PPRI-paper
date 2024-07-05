%%% takes the single level data and computes brain signature scores
% goes over cope images from the single level analysis (/PPRI_BIDS/derivatives/model/single_trial/sub-XX/first_level/sub-XX_expectpercept_run-X.feat/stats/copeX.nii.gz)
% note that odd copes are for the anticipation period and even ones are for
% the stimulus evoked data

run_local = 0;

%% define paths and canlab tools
% add canlab tools to path
addpath(genpath('canlab_tools'));
% define dir paths
behav_data_dir = 'data_for_analysis/raw_data';
single_trial_data_dir = '/PPRI_BIDS/derivatives/model/single_trial';
results_path = 'data_for_analysis/processed_data';

results_filename = fullfile(results_path, 'results_nps_siips');

%% get the behavioral data
behav_data = readtable(fullfile(behav_data_dir, 'task-expectpercept_all_subjs_with_exclusions.csv'));
% make sure each participant has 144 trials (24X6 runs; 72 from each modality).
participants = unique(behav_data.participant);
[total_num_trials, total_num_trials_pain, total_num_trials_vision, total_num_runs] = deal(zeros(length(participants),1));
for sub_ind = 1:length(participants)
    cur_sub = participants{sub_ind};
    total_num_trials(sub_ind) = sum(strcmp(behav_data.participant, cur_sub));
    total_num_trials_pain(sub_ind) = sum(strcmp(behav_data.participant, cur_sub) & strcmp(behav_data.modality,'pain'));
    total_num_trials_vision(sub_ind) = sum(strcmp(behav_data.participant, cur_sub) & strcmp(behav_data.modality,'vision'));
    total_num_runs(sub_ind) = length(unique(behav_data.block(strcmp(behav_data.participant, cur_sub))));
end
if any(total_num_trials ~= 144) || any(total_num_trials_pain ~= 72) || any(total_num_trials_vision ~= 72) || any(total_num_runs ~= 6)
   warning('missing data!'); 
end

%% get the brain data

% run through participants
for sub_ind = 1:length(participants)
    tic;
    cur_sub = participants{sub_ind}; 
    disp(cur_sub);
    sub_brain_path = fullfile(single_trial_data_dir, cur_sub);
    sub_data = behav_data(strcmp(behav_data.participant, cur_sub),:);

    for run_num = 1:6
        run_brain_path = fullfile(sub_brain_path, 'first_level', [cur_sub, '_expectpercept_run-', num2str(run_num), '.feat'], 'stats');
        disp(['run ' num2str(run_num)]);

        for trial_num = 1:24
            disp(['trial ' num2str(trial_num)]);
            disp('analyzing anticipation data');
            % anticipation period
            cope_anticip = 1 + (2 * (trial_num - 1));
            trial_brain_data_path_anticip = fullfile(run_brain_path, ['cope' num2str(cope_anticip) '.nii.gz']);
            if ~isfile(trial_brain_data_path_anticip)
                warning('skipping trial %d of run %d of %s', trial_num, run_num, cur_sub);
                continue
            end
            trial_brain_data_anticip = fmri_data(trial_brain_data_path_anticip, 'noverbose');
            sub_data.nps_anticip(sub_data.block == run_num & sub_data.trial_num == trial_num) = apply_nps(trial_brain_data_anticip,'noverbose');
            sub_data.siips_anticip(sub_data.block == run_num & sub_data.trial_num == trial_num) = apply_siips(trial_brain_data_anticip,'noverbose');
            % stim period
            disp('analyzing stimulus data');
            cope_stim = cope_anticip + 1;
            trial_brain_data_path_stim = fullfile(run_brain_path, ['cope' num2str(cope_stim) '.nii.gz']);
            if ~isfile(trial_brain_data_path_stim)
                warning('skipping trial %d of run %d of %s', trial_num, run_num, cur_sub);
                continue
            end
            trial_brain_data_stim = fmri_data(trial_brain_data_path_stim, 'noverbose');
            sub_data.nps_stim(sub_data.block == run_num & sub_data.trial_num == trial_num) = apply_nps(trial_brain_data_stim,'noverbose');
            sub_data.siips_stim(sub_data.block == run_num & sub_data.trial_num == trial_num) = apply_siips(trial_brain_data_stim,'noverbose');
        end
    end
    if sub_ind == 1
        expectpercept_with_neuromarkers = sub_data;
    else
        expectpercept_with_neuromarkers = [expectpercept_with_neuromarkers; sub_data];
    end
    disp(toc);
    save(results_filename, 'expectpercept_with_neuromarkers');
end
writetable(expectpercept_with_neuromarkers, [results_filename '.csv']);