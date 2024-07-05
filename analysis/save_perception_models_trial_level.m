% this script computes and saves the trial-level predictions of the perception models for each participant, based on an optimization table created by the script "optimize_perception.m".
% The following models are included:
% 1. no expectation no learning (w = 0)
% 2. expectation, no learning (w is a free parameter, no a)
% 3. expectation + learning
% 4. model2 with a modality effect (seperate w per modality)
% 5. model3 with a modality effect (seperate w and seperate a per modality)

raw_data_path = fullfile('data_for_analysis', 'raw_data');
processed_data_path = fullfile('data_for_analysis', 'processed_data');
% load optimized parameters for the perception models for each participant
percept_params_filename = fullfile(processed_data_path, 'perception_models_params_measures.csv');
percept_params = readtable(percept_params_filename);

% read data from the perception task
percept_data_filename = fullfile(processed_data_path, 'task-expectpercept_all_subjs_with_pred_and_exclusions.csv');
percept_data = readtable(percept_data_filename);

% read data from the stimulus response task
stimresp_data_filename = fullfile(raw_data_path, 'task-stimresp_all_subjs.csv');
stimresp_data = readtable(stimresp_data_filename);

% get all participants
participants = unique(percept_data.participant);

for sub_ind = 1:length(participants)
    cur_sub = participants{sub_ind};
    disp('=============================');
    disp(cur_sub);
    disp('=============================');
    disp('estimating stimulus intensity based on calibration');
    
    %% stimulus intensity
    % pain
    subj_pain_calibration_data = stimresp_data(strcmp(stimresp_data.participant, cur_sub) & strcmp(stimresp_data.modality, 'pain'),:);
    mean_47 = mean(subj_pain_calibration_data.rating(subj_pain_calibration_data.stim_value==47));
    percept_data.stim_level_scale_calibration(strcmp(percept_data.participant, cur_sub) & strcmp(percept_data.modality, 'pain') & percept_data.stim_value == 47) = mean_47;
    mean_48 = mean(subj_pain_calibration_data.rating(subj_pain_calibration_data.stim_value==48));
    percept_data.stim_level_scale_calibration(strcmp(percept_data.participant, cur_sub) & strcmp(percept_data.modality, 'pain') & percept_data.stim_value == 48) = mean_48;
    % vision
    subj_vision_calibration_data = stimresp_data(strcmp(stimresp_data.participant, cur_sub) & strcmp(stimresp_data.modality, 'vision'),:);
    mean_05 = mean(subj_vision_calibration_data.rating(subj_vision_calibration_data.stim_value==0.5));
    percept_data.stim_level_scale_calibration(strcmp(percept_data.participant, cur_sub) & strcmp(percept_data.modality, 'vision') & percept_data.stim_value == 0.5) = mean_05;
    % 0.6 is a stimulus value in the perception task, but not in the
    % calibration task. Therefore, I used the calibration value for 0.5 and
    % 0.725 to infer the expected value for 0.6
    mean_06 = mean(subj_vision_calibration_data.rating(subj_vision_calibration_data.stim_value==0.5 | subj_vision_calibration_data.stim_value==0.725));
    percept_data.stim_level_scale_calibration(strcmp(percept_data.participant, cur_sub) & strcmp(percept_data.modality, 'vision') & percept_data.stim_value == 0.6) = mean_06;
    
    %% get params for current participant
    cur_data = percept_data(strcmp(percept_data.participant, cur_sub), :);
    ydata = cur_data.rating;
    xdata = cur_data;
    
    %% model 1
    disp('calculating baseline model (model1: no expectation, no learning)');
    cur_model1_s_p = percept_params.model1_s_p(strcmp(percept_params.participants, cur_sub));
    cur_model1_s_v = percept_params.model1_s_v(strcmp(percept_params.participants, cur_sub));
    predicted_perception1 = perception_model_noexpect_nolearn([cur_model1_s_p, cur_model1_s_v], xdata);
    percept_data.pred_model1(strcmp(percept_data.participant, cur_sub)) = predicted_perception1;
    
    %% model 2
    disp('calculating model2: expectation, no learning; parameters: s_p, s_v, w0');
    cur_model2_s_p = percept_params.model2_s_p(strcmp(percept_params.participants, cur_sub));
    cur_model2_s_v = percept_params.model2_s_v(strcmp(percept_params.participants, cur_sub));
    cur_model2_w0 = percept_params.model2_w(strcmp(percept_params.participants, cur_sub));
    predicted_perception2 = perception_model_expect_nolearn([cur_model2_s_p, cur_model2_s_v, cur_model2_w0], xdata);
    percept_data.pred_model2(strcmp(percept_data.participant, cur_sub)) = predicted_perception2;
    
    %% model 3
    disp('calculating model3: expectation and learning; free parameters: s_p, s_v, w0, a');
    cur_model3_s_p = percept_params.model3_s_p(strcmp(percept_params.participants, cur_sub));
    cur_model3_s_v = percept_params.model3_s_v(strcmp(percept_params.participants, cur_sub));
    cur_model3_w0 = percept_params.model3_w0(strcmp(percept_params.participants, cur_sub));
    cur_model3_a = percept_params.model3_a(strcmp(percept_params.participants, cur_sub));
    predicted_perception3 = perception_model_expect_learn([cur_model3_s_p, cur_model3_s_v, cur_model3_w0, cur_model3_a], xdata);
    percept_data.pred_model3(strcmp(percept_data.participant, cur_sub)) = predicted_perception3;

    %% model 4
    disp('Calculating model 4 (model2mod): s_p, s_v, w_pain, w_vision');
    cur_model4_s_p = percept_params.model2mod_s_p(strcmp(percept_params.participants, cur_sub));
    cur_model4_s_v = percept_params.model2mod_s_v(strcmp(percept_params.participants, cur_sub));
    cur_model4_w_p = percept_params.model2mod_w_pain(strcmp(percept_params.participants, cur_sub));
    cur_model4_w_v = percept_params.model2mod_w_vision(strcmp(percept_params.participants, cur_sub));
    predicted_perception4 = perception_model_expect_nolearn_mod([cur_model4_s_p, cur_model4_s_v, cur_model4_w_p, cur_model4_w_v], xdata);
    percept_data.pred_model4(strcmp(percept_data.participant, cur_sub)) = predicted_perception4;
    
    %% model 5
    disp('Calculating model5 (model3mod): s_p, s_v, w0_pain, w0_vision, a_pain, a_vision');
    cur_model5_s_p = percept_params.model3mod_s_p(strcmp(percept_params.participants, cur_sub));
    cur_model5_s_v = percept_params.model3mod_s_v(strcmp(percept_params.participants, cur_sub));
    cur_model5_w0_p = percept_params.model3mod_w0_p(strcmp(percept_params.participants, cur_sub));
    cur_model5_w0_v = percept_params.model3mod_w0_v(strcmp(percept_params.participants, cur_sub));
    cur_model5_a_p = percept_params.model3mod_a_p(strcmp(percept_params.participants, cur_sub));
    cur_model5_a_v = percept_params.model3mod_a_v(strcmp(percept_params.participants, cur_sub));
    predicted_perception5 = perception_model_expect_learn_mod([cur_model5_s_p, cur_model5_s_v, cur_model5_w0_p, cur_model5_w0_v, cur_model5_a_p, cur_model5_a_v], xdata);
    percept_data.pred_model5(strcmp(percept_data.participant, cur_sub)) = predicted_perception5;
end

% save the data with predicted expectations
output_filename = fullfile(processed_data_path, 'task-expectpercept_all_subjs_with_pred_after_exclusions.csv');
writetable(percept_data, output_filename);
