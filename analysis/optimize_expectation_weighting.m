%%  expectation mode with 2 free parameters:
%% b: weighting of values that are below vs. above the mean (based on alogistic function) (b=0 no overweighting; b<0 overweighting of values below the mean; b>0 overweighting of values above the mean)
%% k: weighting of outliers vs. inliers (based on an exponent) (k =1 means no overweighting; k<1 overweighting of inliers; k>1 overweighting of outliers)
%% added bounds for the parameters: k in [0.001, 1000], b in [-1000, 1000]
optimoptions(@lsqcurvefit,'StepTolerance',1e-15);

expect_data_filename = 'data_for_analysis/raw_data/task-expect_all_subjs.csv';
expect_data = readtable(expect_data_filename);
expect_data = expect_data(:, 2:end);

vas_columns = 9:18;

% compute simple mean
expect_data.vas_mean=mean(expect_data{:,vas_columns},2);

participants = unique(expect_data.participant);
modalities = unique(expect_data.modality);

participants_all = repelem(participants, length(modalities),1);
modalities_all = repmat(modalities, length(participants),1);

optimization_table = table(participants_all, modalities_all);
ind = 0;
% optimize a,b,d for each participant and each modality seperately
for participant_ind = 1:length(participants)
    cur_participant = participants{participant_ind};
    disp(cur_participant);
    for modality_ind = 1:length(modalities)
        ind = ind + 1;
        cur_modality = modalities{modality_ind};
        disp(cur_modality);
        if ~strcmp(optimization_table.participants_all(ind), cur_participant) || ~strcmp(optimization_table.modalities_all(ind), cur_modality)
            warning('wrong order of participants or modalities');
        end
        cur_data = expect_data(strcmp(expect_data.participant, cur_participant) & strcmp(expect_data.modality, cur_modality), :);
        ydata = cur_data.rating;
        xdata = cur_data{:,vas_columns};
        %  optimize k (x(1)) and b (x(2))
        x0 = [1,0]; % starting point is 1 for k and 0 for b
        lb = [0.001, -1000]; % k is bounded at 0, b is not bounded
        ub = [1000, 1000]; % 1000 for both
        problem = createOptimProblem('lsqcurvefit','x0',x0,'lb', lb,'ub',ub,'objective',@compute_expectation_weights_logistic_power_v1,'xdata',xdata,'ydata',ydata);
        ms = MultiStart();
        num_multi = 100;
        x = run(ms,problem,num_multi);
        optimization_table.participants_all{ind} = cur_participant;
        optimization_table.modalities_all{ind} = cur_modality;
        optimization_table.k(ind) = x(1);
        optimization_table.b(ind) = x(2);
        disp(x);
    end
end

pain_values = optimization_table(strcmp(optimization_table.modalities_all,'pain'),:);
vision_values = optimization_table(strcmp(optimization_table.modalities_all,'vision'),:);

% means and wilcockson tests
% pain
median(pain_values.k)
[p,h] = signrank(pain_values.k, 1);
median(pain_values.b)
[p,h] = signrank(pain_values.b, 0);

% vision
median(vision_values.k)
[p,h] = signrank(vision_values.k, 1);
median(vision_values.b)
[p,h] = signrank(vision_values.b, 0);

% pain vs. vision
[p,h] = signrank(pain_values.k, vision_values.k);
[p,h] = signrank(pain_values.b, vision_values.b);

[rho, p] = corr(pain_values.k, vision_values.k,'Type','Spearman');
[rho, p] = corr(pain_values.b, vision_values.b,'Type','Spearman');

% plot the median weighting function across the sample
x = linspace(40,60,10);
x_rescaled = rescale(x, 0, 1);
x_rescaled_demeaned = x_rescaled - mean(x_rescaled);
logistic_term = @(x, b, x0) 1 ./ (1 + exp(-b * (x - x0)));
power_term = @(x, k) (sign(x) .* abs(x) .^ k) ./ x;


% pain
k_pain = median(pain_values.k);
b_pain = median(pain_values.b);
weights_power = power_term(x_rescaled_demeaned,k_pain);
weights_power = weights_power ./ sum(weights_power);
weights_logistic = logistic_term(x_rescaled_demeaned, b_pain, 0);
weights_pain = weights_power + weights_logistic;
weights_pain = weights_pain ./ sum(weights_pain);
figure('Name', 'Weighting_function');
subplot(2,1,1);
plot(x, weights_pain, 'k');
ylim([0,0.2]);
xlim([min(x),max(x)]);
xlabel('X', 'FontSize', 18);
ylabel('Weight', 'FontSize', 18);
title('Pain median weight function', 'FontSize', 18);
yline(0.1, '--k');
% vision
k_vision = median(vision_values.k);
b_vision = median(vision_values.b);
weights_power = power_term(x_rescaled_demeaned,k_vision);
weights_power = weights_power ./ sum(weights_power);
weights_logistic = logistic_term(x_rescaled_demeaned, b_vision, 0);
weights_vision = weights_power + weights_logistic;
weights_vision = weights_vision ./ sum(weights_vision);
subplot(2,1,2);
plot(x, weights_vision, 'k');
ylim([0,0.2]);
xlim([min(x),max(x)]);
xlabel('X', 'FontSize', 18);
ylabel('Weight', 'FontSize', 18);
title('Vision median weight function', 'FontSize', 18);
yline(0.1, '--k');


%% scatter with k,b per participant and modality
figure('Name', 'clusters');
num_clusters = 4;
subplot(1,2,1);
params_pain = pain_values{:,3:4};
idx_pain = kmeans(params_pain, num_clusters);
scatter(params_pain(:,1), params_pain(:,2),30, idx_pain, 'filled');
xlabel('k'); ylabel('b'); title(['pain; kmeans clustering, ' num2str(num_clusters) ' clusters']);
subplot(1,2,2);
params_vision = vision_values{:,3:4};
idx_vision = kmeans(params_vision, num_clusters);
scatter(params_vision(:,1), params_vision(:,2),30, idx_vision, 'filled');
xlabel('k'); ylabel('b'); title(['vision; kmeans clustering, ' num2str(num_clusters) ' clusters']);
colormap(cool);