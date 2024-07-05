%%% takes the single level data and computes average score per ROI
% goes over cope images from the single level analysis (/PPRI_BIDS/derivatives/model/single_trial/sub-XX/first_level/sub-XX_expectpercept_run-X.feat/stats/copeX.nii.gz)
% note that odd copes are for the anticipation period and even ones are for
% the stimulus evoked data


% add canlab tools to path
addpath(genpath('canlab_tools'));
% define dir paths
behav_data_dir = 'data_for_analysis/raw_data';
single_trial_data_dir = '/PPRI_BIDS/derivatives/model/single_trial';
results_path = 'data_for_analysis/processed_data';

results_filename = fullfile(results_path, 'results_ROIs');

%% define rois to test
% pain - based on atlas pain_pathways
nociceptive_rois = {'dpIns', 'aIns', 'aMCC_MPFC', 'Thal_VPLM','Thal_MD', 'Bstem_PAG', 's1_handplus'}; % regions from the pain_pathways atlas
pain_pathways = load_atlas(which('pain_pathways_atlas_obj.mat'));
pain_pathways = pain_pathways.select_atlas_subset(nociceptive_rois);

% higher level - based on atlas canlab2023 (threhsold = 0.2)
canlab2023_atlas_coarse_2mm = load_atlas('canlab2023_coarse_2mm');
canlab2023_atlas_coarse_2mm_thr = canlab2023_atlas_coarse_2mm.threshold(0.2).remove_empty();
higher_level_rois = {'Ctx_11l', 'Ctx_p9_46v_R', 'NAc_shell_like', 'NAc_core_like', 'Ctx_IPS1'};
higher_level = canlab2023_atlas_coarse_2mm_thr.select_atlas_subset(higher_level_rois);
higher_level_left_dlpfc = select_atlas_subset(canlab2023_atlas_coarse_2mm_thr, {'Ctx_8C_L', 'Ctx_46_L'}, 'flatten');
higher_level_left_dlpfc.labels = {'Ctx_8C_L_and_Ctx_46_L'};
higher_level = [higher_level, higher_level_left_dlpfc];

% visual regions - based on atlas canlab2023 (threhsold = 0.2)
visual_rois = {'Thal_Lateral_Geniculate_Nucleus', 'V1', 'V2', 'V3', 'V4', 'MT'};
visual_processing = canlab2023_atlas_coarse_2mm_thr.select_atlas_subset(visual_rois);

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
            % compute ROI averages
            % nociceptive regions
            pain_pathways_mean = extract_roi_averages(trial_brain_data_anticip, fmri_data(pain_pathways), 'unique_mask_values', 'nonorm');
            for i = 1:length(pain_pathways_mean)
                eval(sprintf('sub_data.pain_anticip_%s(sub_data.block == run_num & sub_data.trial_num == trial_num) = pain_pathways_mean(%d).dat;', pain_pathways.labels{i}, i));
            end
            % higher level regions
            higher_level_mean = extract_roi_averages(trial_brain_data_anticip, fmri_data(higher_level), 'unique_mask_values', 'nonorm');
            for i = 1:length(higher_level_mean)
                eval(sprintf('sub_data.higher_anticip_%s(sub_data.block == run_num & sub_data.trial_num == trial_num) = higher_level_mean(%d).dat;', higher_level.labels{i}, i));
            end
            % visual processing rois
            visual_processing_mean = extract_roi_averages(trial_brain_data_anticip, fmri_data(visual_processing), 'unique_mask_values', 'nonorm');
            for i = 1:length(visual_processing_mean)
                eval(sprintf('sub_data.visual_anticip_%s(sub_data.block == run_num & sub_data.trial_num == trial_num) = visual_processing_mean(%d).dat;', visual_processing.labels{i}, i));
            end
          
            % stim period
            disp('analyzing stimulus data');
            cope_stim = cope_anticip + 1;
            trial_brain_data_path_stim = fullfile(run_brain_path, ['cope' num2str(cope_stim) '.nii.gz']);
            if ~isfile(trial_brain_data_path_stim)
                warning('skipping trial %d of run %d of %s', trial_num, run_num, cur_sub);
                continue
            end
            trial_brain_data_stim = fmri_data(trial_brain_data_path_stim, 'noverbose');
            % compute ROI averages
            % nociceptive regions
            pain_pathways_mean = extract_roi_averages(trial_brain_data_stim, fmri_data(pain_pathways), 'unique_mask_values', 'nonorm');
            for i = 1:length(pain_pathways_mean)
                eval(sprintf('sub_data.pain_stim_%s(sub_data.block == run_num & sub_data.trial_num == trial_num) = pain_pathways_mean(%d).dat;', pain_pathways.labels{i}, i));
            end
            % higher level regions
            higher_level_mean = extract_roi_averages(trial_brain_data_stim, fmri_data(higher_level), 'unique_mask_values', 'nonorm');
            for i = 1:length(higher_level_mean)
                eval(sprintf('sub_data.higher_stim_%s(sub_data.block == run_num & sub_data.trial_num == trial_num) = higher_level_mean(%d).dat;', higher_level.labels{i}, i));
            end
            % visual processing rois
            visual_processing_mean = extract_roi_averages(trial_brain_data_stim, fmri_data(visual_processing), 'unique_mask_values', 'nonorm');
            for i = 1:length(visual_processing_mean)
                eval(sprintf('sub_data.visual_stim_%s(sub_data.block == run_num & sub_data.trial_num == trial_num) = visual_processing_mean(%d).dat;', visual_processing.labels{i}, i));
            end

        end
    end
    if sub_ind == 1
        expectpercept_with_rois = sub_data;
    else
        expectpercept_with_rois = [expectpercept_with_rois; sub_data];
    end
    disp(toc);
    save(results_filename, 'expectpercept_with_rois');
end

writetable(expectpercept_with_rois, [results_filename '.csv']);