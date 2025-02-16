% this script optimizes, fits and compares between model for the perception
% task (based on least squares). Expectations are based on weighted mean,
% with parameters k and b as optimized in the optimize_expectation script (or its updates),
% per participant and modality. Intensity rating is based on the data from
% the calibration task of each participant, scaled for each participant and modality (free parameters).
% The following models are included:
% 1. no expectation no learning (w = 0)
% 2. expectation, no learning (w is a free parameter, no a)
% 3. expectation + learning
% 4. model2 with a modality effect (seperate w per modality)
% 5. model3 with a modality effect (seperate w and seperate a per modality)

main_dir = pwd;

% in the optimizations, multiple starting points are used. How many to use?
num_multi = 50;

% read data from the perception task
percept_data_filename = fullfile(main_dir, 'data_for_analysis/raw_data/task-expectpercept_all_subjs_with_exclusions.csv');
percept_data = readtable(percept_data_filename);
% exclude trials based on technical issues and RTs
warning('excluding %d trials due to technical issues, and %d trials due to too fast or too slow RTs', sum(percept_data.exclude_tech_issues), sum(percept_data.exclude_RT & ~percept_data.exclude_tech_issues));
percept_data = percept_data(~percept_data.exclude_tech_issues & ~percept_data.exclude_RT,:);

% read participants' estimated k and b values
params_filename = fullfile(main_dir, 'data_for_analysis/processed_data/optimized_k_and_b_values.txt');
params_data = readtable(params_filename);

% loop through participants and compute expectations based on their
% optimized k and b
participants = unique(percept_data.participant);
percept_data.weighted_expectation = zeros(height(percept_data), 1);

% while looping, also convert the stim_level (stimulus intensity) to the
% 0-100 rating scale for each participant, based on the calibration data
calibration_data_filename = fullfile(main_dir, 'data_for_analysis/raw_data/task-stimresp_all_subjs.csv');
calibration_data = readtable(calibration_data_filename);

vas_columns = 10:19;

optimization_table = table(participants);

for participant_ind = 1:length(participants)
    cur_participant = participants{participant_ind};
    disp('=============================');
    disp(cur_participant);
    disp('=============================');
    disp('estimating stimulus intensity based on calibration');
    
    %% stimulus intensity
    % pain
    subj_pain_calibration_data = calibration_data(strcmp(calibration_data.participant, cur_participant) & strcmp(calibration_data.modality, 'pain'),:);
    mean_47 = mean(subj_pain_calibration_data.rating(subj_pain_calibration_data.stim_value==47));
    percept_data.stim_level_scale_calibration(strcmp(percept_data.participant, cur_participant) & strcmp(percept_data.modality, 'pain') & percept_data.stim_value == 47) = mean_47;
    mean_48 = mean(subj_pain_calibration_data.rating(subj_pain_calibration_data.stim_value==48));
    percept_data.stim_level_scale_calibration(strcmp(percept_data.participant, cur_participant) & strcmp(percept_data.modality, 'pain') & percept_data.stim_value == 48) = mean_48;
    % vision
    subj_vision_calibration_data = calibration_data(strcmp(calibration_data.participant, cur_participant) & strcmp(calibration_data.modality, 'vision'),:);
    mean_05 = mean(subj_vision_calibration_data.rating(subj_vision_calibration_data.stim_value==0.5));
    percept_data.stim_level_scale_calibration(strcmp(percept_data.participant, cur_participant) & strcmp(percept_data.modality, 'vision') & percept_data.stim_value == 0.5) = mean_05;
    % 0.6 is a stimulus value in the perception task, but not in the
    % calibration task. Therefore, I used the calibration value for 0.5 and
    % 0.725 to infer the expected value for 0.6
    mean_06 = mean(subj_vision_calibration_data.rating(subj_vision_calibration_data.stim_value==0.5 | subj_vision_calibration_data.stim_value==0.725));
    percept_data.stim_level_scale_calibration(strcmp(percept_data.participant, cur_participant) & strcmp(percept_data.modality, 'vision') & percept_data.stim_value == 0.6) = mean_06;
      
    
    %% expectations
    disp('computing expectations');
    
    % pain
    xdata = percept_data{strcmp(percept_data.participant, cur_participant) & strcmp(percept_data.modality, 'pain'),vas_columns};
    k = params_data.k(strcmp(params_data.participants_all, cur_participant) & strcmp(params_data.modalities_all, 'pain'));
    b = params_data.b(strcmp(params_data.participants_all, cur_participant) & strcmp(params_data.modalities_all, 'pain'));
    x = [k, b];
    cur_expectations = compute_expectation_weights_logistic_power_v1(x, xdata);
    percept_data.weighted_expectation(strcmp(percept_data.participant, cur_participant) & strcmp(percept_data.modality, 'pain')) = cur_expectations;
    
    
    % vision
    xdata = percept_data{strcmp(percept_data.participant, cur_participant) & strcmp(percept_data.modality, 'vision'),vas_columns};
    k = params_data.k(strcmp(params_data.participants_all, cur_participant) & strcmp(params_data.modalities_all, 'vision'));
    b = params_data.b(strcmp(params_data.participants_all, cur_participant) & strcmp(params_data.modalities_all, 'vision'));
    x = [k, b];
    cur_expectations = compute_expectation_weights_logistic_power_v1(x, xdata);
    percept_data.weighted_expectation(strcmp(percept_data.participant, cur_participant) & strcmp(percept_data.modality, 'vision')) = cur_expectations;

    %% perception models fitting
    disp('fitting models');
    cur_data = percept_data(strcmp(percept_data.participant, cur_participant), :);
    ydata = cur_data.rating;
    xdata = cur_data;
    
    
    %% model1: baseline (no expectation, no learning)
    % rating = stimulus intensity, scaled
    disp('calculating baseline model (model1: no expectation, no learning)');
    % free parameters s are the scaling factors of the calibration rating per participant (fixed during the task per participant)
    % data: s_p for pain and s_v for vision
    disp('with scaling factor- free parameters (s_p for pain and s_v for vision)');
    x0 = [1,1];
    lb = [0,0];
    ub = [5,5];
    problem = createOptimProblem('lsqcurvefit','x0',x0,'objective',@perception_model_noexpect_nolearn,'xdata',xdata,'ydata',ydata,'lb',lb,'ub',ub);
    ms = MultiStart();
    x = run(ms,problem,num_multi);
    x = round(x,5);
    [percept_data.model1_pred(strcmp(percept_data.participant, cur_participant)), new_xdata] = perception_model_noexpect_nolearn(x, xdata);
    percept_data.model1_s(strcmp(percept_data.participant, cur_participant)) = new_xdata.s;
    optimization_table.model1_s_p(participant_ind) = x(1);
    optimization_table.model1_s_v(participant_ind) = x(2);
    optimization_table.model1_r(participant_ind) = corr(new_xdata.rating,new_xdata.predicted_rating);
    optimization_table.model1_ss(participant_ind) = sum((new_xdata.rating - new_xdata.predicted_rating).^2);
    
    %% model2: (expectation, no learning)
    % free parameter w is the weight of expectation per participant, which is fixed during the task (no learning)
    disp('optimizing model2: expectation, no learning; parameters: s_p, s_v, w0');    
    x0 = [1, 1, 0.5];
    lb = [0, 0, 0];
    ub = [5, 5, 1];

    % multiple starting point - use MultiStart
    problem = createOptimProblem('lsqcurvefit','x0',x0,'objective',@perception_model_expect_nolearn,'xdata',xdata,'ydata',ydata,'lb',lb,'ub',ub);
    ms = MultiStart();
    x = run(ms,problem,num_multi);
    x = round(x,5);
    [percept_data.model2_pred(strcmp(percept_data.participant, cur_participant)), new_xdata] = perception_model_expect_nolearn(x, xdata);
    percept_data.model2_s(strcmp(percept_data.participant, cur_participant)) = new_xdata.s;
    percept_data.model2_w(strcmp(percept_data.participant, cur_participant)) = new_xdata.w;
    optimization_table.model2_s_p(participant_ind) = x(1);
    optimization_table.model2_s_v(participant_ind) = x(2);
    optimization_table.model2_w(participant_ind) = x(3);
    optimization_table.model2_r(participant_ind) = corr(new_xdata.rating,new_xdata.predicted_rating);
    optimization_table.model2_ss(participant_ind) = sum((new_xdata.rating - new_xdata.predicted_rating).^2);
    
    %% model3: (expectation + learning)
    % like model2 but w (free parameter at the beginning of the task) is
    % changing based on a learning rate a (free parameter)
    disp('optimizing model3: expectation and learning; free parameters: s_p, s_v, w0, a');
    x0 = [1, 1, 0.5, 0];
    lb = [0, 0, 0,-1];
    ub = [5, 5, 1,1];
    % multiple starting point - use MultiStart
    problem = createOptimProblem('lsqcurvefit','x0',x0,'objective',@perception_model_expect_learn,'xdata',xdata,'ydata',ydata,'lb',lb,'ub',ub);
    ms = MultiStart();
    x = run(ms,problem,num_multi);
    x = round(x,5);
    [percept_data.model3_pred(strcmp(percept_data.participant, cur_participant)), new_xdata] = perception_model_expect_learn(x, xdata);
    percept_data.model3_w(strcmp(percept_data.participant, cur_participant)) = new_xdata.w;
    optimization_table.model3_s_p(participant_ind) = x(1);
    optimization_table.model3_s_v(participant_ind) = x(2);    
    optimization_table.model3_w0(participant_ind) = x(3);
    optimization_table.model3_a(participant_ind) = x(4);
    optimization_table.model3_r(participant_ind) = corr(new_xdata.rating,new_xdata.predicted_rating);
    optimization_table.model3_ss(participant_ind) = sum((new_xdata.rating - new_xdata.predicted_rating).^2);
    
    percept_data.PE(strcmp(percept_data.participant, cur_participant)) = new_xdata.PE;
    
    %% test which model is best and then test what happens if we have seperate w or a per modality?
    
    %% model2mod optimization
    % MODEL2mod: like model2 but seperate w for pain and for vision
    disp('optimizing model2mod: s_p, s_v, w_pain, w_vision');
    x0 = [1, 1, 0.5,0.5];
    lb = [0, 0, 0,0];
    ub = [5, 5, 1,1];
    problem = createOptimProblem('lsqcurvefit','x0',x0,'objective',@perception_model_expect_nolearn_mod,'xdata',xdata,'ydata',ydata,'lb',lb,'ub',ub);
    ms = MultiStart();
    x = run(ms,problem,num_multi);
    x = round(x,5);
    [percept_data.model2mod_pred(strcmp(percept_data.participant, cur_participant)), new_xdata] = perception_model_expect_nolearn_mod(x, xdata);
    percept_data.model2mod_w(strcmp(percept_data.participant, cur_participant)) = new_xdata.w;
    optimization_table.model2mod_s_p(participant_ind) = x(1);
    optimization_table.model2mod_s_v(participant_ind) = x(2);      
    optimization_table.model2mod_w_pain(participant_ind) = x(3);
    optimization_table.model2mod_w_vision(participant_ind) = x(4);
    optimization_table.model2mod_r(participant_ind) = corr(new_xdata.rating,new_xdata.predicted_rating);
    optimization_table.model2mod_ss(participant_ind) = sum((new_xdata.rating - new_xdata.predicted_rating).^2);
    
    %% model3mod optimization
    % MODEL3mod: like model3 but also seperate a for pain and for vision
    disp('optimizing model3mod: s_p, s_v, w0_pain, w0_vision, a_pain, a_vision');
    x0 = [1,1,0.5,0.5,0,0];
    lb = [0,0,0,0,-1,-1];
    ub = [5,5,1,1,1,1];
    problem = createOptimProblem('lsqcurvefit','x0',x0,'objective',@perception_model_expect_learn_mod,'xdata',xdata,'ydata',ydata,'lb',lb,'ub',ub);
    ms = MultiStart();
    [x, multi_error] = run(ms,problem,num_multi);
    x = round(x,5);
    [percept_data.model3mod_pred(strcmp(percept_data.participant, cur_participant)), new_xdata] = perception_model_expect_learn_mod(x, xdata);
    percept_data.model3mod_w(strcmp(percept_data.participant, cur_participant)) = new_xdata.w;
    % if w0 = 0, then a should be 0 as well (it can be a different number
    % in the optimization because if w0 = 0 it doesn't matter what a is)
    if x(3) == 0 % for pain
       x(5) = 0; 
    end
    if x(4) == 0 % for vision
       x(6) = 0; 
    end    
    optimization_table.model3mod_s_p(participant_ind) = x(1);
    optimization_table.model3mod_s_v(participant_ind) = x(2);  
    optimization_table.model3mod_w0_p(participant_ind) = x(3);
    optimization_table.model3mod_w0_v(participant_ind) = x(4);
    optimization_table.model3mod_a_p(participant_ind) = x(5);
    optimization_table.model3mod_a_v(participant_ind) = x(6);
    optimization_table.model3mod_r(participant_ind) = corr(new_xdata.rating,new_xdata.predicted_rating);
    optimization_table.model3mod_ss(participant_ind) = sum((new_xdata.rating - new_xdata.predicted_rating).^2);
end

%% visualizations
models = {'model1', 'model2', 'model3', 'model2mod', 'model3mod'};
model_labels = {'no expect', 'expect', 'expect learn', 'expect by mod', 'expect learn by mod'};

figure;
% ss as a function of model per participant
plot_mat = table2array(optimization_table(:,contains(optimization_table.Properties.VariableNames, '_ss')));
model_names = optimization_table.Properties.VariableNames(contains(optimization_table.Properties.VariableNames, '_ss'));
model_names = erase(model_names, '_ss');
[~,model_names_ind] = ismember(model_names, models);
label_names = model_labels(model_names_ind);
mean_model_ss = mean(plot_mat);
bar_plot = bar(mean_model_ss, 'FaceColor',[0.9 0.9 0.9],'EdgeColor',[0 0 0]);
hold on   

line_plot = plot(plot_mat', '-o');

se_models_ss = std(plot_mat)./sqrt(size(plot_mat,1));
er_bars = errorbar(mean_model_ss, se_models_ss, 'k'); 

xticks(1:size(plot_mat, 2));
xticklabels(label_names);
xtickangle(45)
ylabel('sse');
set(gca,'FontSize',18)
hold off

% fitted parameters from model3mod
figure;
% w pain vs. vision based on model3mod
subplot(1,2,1);
histogram(optimization_table.model3mod_w0_p,50);
hold on
histogram(optimization_table.model3mod_w0_v,50);
title('fitted wo', 'FontSize', 12); xlim([0,1]); ylim([0,40]);
legend({'pain','vision'});

% learning rate pain vs. vision based on model3mod
subplot(1,2,2);
histogram(optimization_table.model3mod_a_p,50);
hold on;
histogram(optimization_table.model3mod_a_v,50);
title('fitted learning rate', 'FontSize', 12); xlim([0,1]); ylim([0,40]);
legend({'pain','vision'});
hold off

%% Model comparison
% model2 vs. model1
ss1 = optimization_table.model1_ss;
ss2 = optimization_table.model2_ss;
n = 144; % number of observations per model
[optimization_table.model_2_vs_1_F, optimization_table.model_2_vs_1_P] = model_comparison_test(ss1, ss2, 2, 3, n);

% model3 vs. model2
ss1 = optimization_table.model2_ss;
ss2 = optimization_table.model3_ss;
n = 144; % number of observations per model
[optimization_table.model_3_vs_2_F, optimization_table.model_3_vs_2_P] = model_comparison_test(ss1, ss2, 3, 4, n);

% model2mod vs. model2
ss1 = optimization_table.model2_ss;
ss2 = optimization_table.model2mod_ss;
n = 144; % number of observations per model
[optimization_table.model_2mod_vs_2_F, optimization_table.model_2mod_vs_2_P] = model_comparison_test(ss1, ss2, 3, 4, n);

% model3mod vs. model3
ss1 = optimization_table.model3_ss;
ss2 = optimization_table.model3mod_ss;
n = 144; % number of observations per model
[optimization_table.model_3mod_vs_3_F, optimization_table.model_3mod_vs_3_P] = model_comparison_test(ss1, ss2, 4, 6, n);

% model3mod vs. model2mod
ss1 = optimization_table.model2mod_ss;
ss2 = optimization_table.model3mod_ss;
n = 144; % number of observations per model
[optimization_table.model_3mod_vs_2mod_F, optimization_table.model_3mod_vs_2mod_P] = model_comparison_test(ss1, ss2, 4, 6, n);


% define best model per participant
optimization_table.best_model(:) = {'model1'};
optimization_table.best_model(optimization_table.model_2_vs_1_P < 0.05) = {'model2'};
optimization_table.best_model(optimization_table.model_2_vs_1_P < 0.05 & optimization_table.model_3_vs_2_P < 0.05) = {'model3'};
optimization_table.best_model(ismember(optimization_table.best_model, 'model2') & optimization_table.model_2mod_vs_2_P < 0.05) = {'model2mod'};
optimization_table.best_model(ismember(optimization_table.best_model, 'model3') & optimization_table.model_3mod_vs_3_P < 0.05) = {'model3mod'};
optimization_table.best_model(ismember(optimization_table.best_model, 'model2mod') & optimization_table.model_3mod_vs_2mod_P < 0.05) = {'model3mod'};
optimization_table.best_model = categorical(optimization_table.best_model);
optimization_table.best_model_label = renamecats(optimization_table.best_model,models,model_labels);
figure;
histogram(optimization_table.best_model_label);
ylabel('Number of participants', 'FontSize', 16);
xlabel('Best model', 'FontSize', 16);

%% parameters comparisons
% weighting (w) based on model2mod
[h, p] = ttest(optimization_table.model2mod_w_pain, optimization_table.model2mod_w_vision);

% initial weighting (w0) based on model3mod
[h, p] = ttest(optimization_table.model3mod_w0_p, optimization_table.model3mod_w0_v);
% learning rate based on model3mod
[h, p] = ttest(optimization_table.model3mod_a_p, optimization_table.model3mod_a_v);

%% save results
save('percept_data_from_optimization', 'percept_data');
save('optimization_table_perception', 'optimization_table');
writetable(optimization_table, 'perception_models_params_measures.csv');