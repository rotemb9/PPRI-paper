%% define paths
addpath(genpath('canlab_tools'));
single_trial_dir = '/PPRI_BIDS/derivatives/model/single_trial';
results_dir = 'data_for_analysis/processed_data';

%% get the participants table
trials_data = readtable('/data_for_analysis/processed_data/task-expectpercept_all_subjs_with_pred_after_exclusions.csv');

%% loop through participants
participants = unique(trials_data.participant);
for sub_ind = 1:length(participants)
    cur_sub = participants{sub_ind};
    disp(cur_sub);
    subj_dir = fullfile(single_trial_dir, cur_sub, 'first_level');
    
    % loop through runs, to get the design matrix and compute VIFs for each
    for run_ind = 1:6
        cur_run = ['run-' num2str(run_ind)];
        disp(cur_run);
        run_dir = fullfile(subj_dir, [cur_sub, '_expectpercept_', cur_run, '.feat']);
        design_matrix_file = fullfile(run_dir, 'design.mat');
        if ~isfile(design_matrix_file)
           warning(['no design.mat file for ', cur_sub, ' ', cur_run]);
           continue
        end
        design_mat_text = fileread(design_matrix_file);
        strfind(design_mat_text, 'Matrix');
        design_mat = str2num(design_mat_text(strfind(design_mat_text, 'Matrix')+7:end));
        % the design_mat will now have the design matrix
        % there are 48 regressors of interest: 24 for the cue and 24 for the
        % stimulus period. These are columns 1:3:5:...:95 in the matrix. Columns
        % 1:4:95 are for the cue, and columns 3:4:95 are for the stimulus period.
        % Columns 2:2:94 are the temporal derivatives of the regressors of interest.
        % Columns 96-102 are the nuisance regressors: 6 motion
        % regressors and global CSF signal
        cue_regressors = 1:4:95;
        stim_regressors = 3:4:95;
        nuisance_regressors = 96:102;

        % get VIFs for all regressors
        %vifs = getvif(design_mat, 0, 'plot');
        vifs = getvif(design_mat);
        trials_data.vif_stim(strcmp(trials_data.participant, cur_sub) & trials_data.block == run_ind) = vifs(stim_regressors);
        trials_data.vif_cue(strcmp(trials_data.participant, cur_sub) & trials_data.block == run_ind) = vifs(cue_regressors);
    end
end

%% how many VIFs are problematic?
num_cue_vif_gt_5 = sum(trials_data.vif_cue > 5);
num_stim_vif_gt_5 = sum(trials_data.vif_stim > 5);

disp(['there are ' num2str(num_cue_vif_gt_5) ' trials with VIFs > 5 for the cue regressor']);
disp(['there are ' num2str(num_stim_vif_gt_5) ' trials with VIFs > 5 for the stim regressor']);


%% save the data
output_filename = fullfile(results_dir, 'expectpercept_with_vifs.csv');
writetable(trials_data, output_filename);