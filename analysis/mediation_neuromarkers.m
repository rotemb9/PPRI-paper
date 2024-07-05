% organize input for multilevel mediation analysis with NPS/SIIPS
% create variables (for each modality seperately) with NPS, SIIPS, cue (cue-based expectation
% value), cue mean (low/high), pain rating, stimulus intensity.
% then use these variables in mediation analyses.
% NPS and SIIPS values are z-scored across all trials, so that the scores
% would be comparable, but between-subject effects will remain

%% Note: need to add canlab tools and spm12 to path for the mediation analysis below to work properly

dat = readtable('data_for_analysis/processed_data/results_nps_siips.csv');
% recode cue mean and intensity as high / low (-1, 1)
dat.cue_mean_level(dat.cue_mean == 0.3) = -1;
dat.cue_mean_level(dat.cue_mean == 0.7) = 1;
dat.stim_level(dat.stim_level == 1) = -1;
dat.stim_level(dat.stim_level == 2) = 1;

% remove trials that are marked to exclude based on technical issues
warning('excluding %d trials due to technical issues', sum(dat.exclude_tech_issues));
dat = dat(~dat.exclude_tech_issues, :);

% remove trials with nan values for neuromarkers (most likely means there
% was no imaging data available)
dat.exclude(isnan(dat.nps_stim) | isnan(dat.siips_stim) | isnan(dat.nps_anticip) | isnan(dat.siips_anticip)) = 1;
warning(['excluding ', num2str(sum(dat.exclude)), ' trials with NA values']);
dat = dat(~dat.exclude, :);
disp('there are no imaging data for these trials because of technical issues as well');

% remove trials with invalid RTs
warning('excluding %d trials due to invalid RTs', sum(dat.exclude_RT));
dat = dat(~dat.exclude_RT, :);

% remove trials with VIF > 5
dat.exclude(dat.vif_stim > 5 | dat.vif_cue > 5) = 1;
warning('excluding %d trials with VIFs > 5', sum(dat.exclude));
dat = dat(~dat.exclude, :);

% remove trials with NPS/SIIPS over 3.5 SDs from the mean
% or nan VALUES
% ONLY FOR STIM PERIOD, BECAUSE THE MEDIATION DOES NOT USE THE ANTICIPATION
num_sds = 3.5;

dat.exclude(:) = false;
mean_nps_stim = mean(dat.nps_stim(~isnan(dat.nps_stim)));
sd_nps_stim = std(dat.nps_stim(~isnan(dat.nps_stim)));
mean_siips_stim = mean(dat.siips_stim(~isnan(dat.siips_stim)));
sd_siips_stim = std(dat.siips_stim(~isnan(dat.siips_stim)));

dat.exclude((dat.nps_stim < (mean_nps_stim - num_sds *  sd_nps_stim))  | (dat.nps_stim > (mean_nps_stim + num_sds *  sd_nps_stim))) = 1;
dat.exclude((dat.siips_stim < (mean_siips_stim - num_sds *  sd_siips_stim))  | (dat.siips_stim > (mean_siips_stim + num_sds *  sd_siips_stim))) = 1;

warning(['excluded ', num2str(sum(dat.exclude)), ' trials with NPS/SIIPS outliers (stim period only) based on ', num2str(num_sds), ' SDs, out of ', num2str(height(dat)), ' trials']);
dat = dat(~dat.exclude, :);

% z score each neuromarker and rating, across all trials
% since pain and vision are used in seperate mediation models, z score
% within each modality
% pain
dat.nps_stim_z(strcmp(dat.modality, 'pain')) = zscore(dat.nps_stim(strcmp(dat.modality, 'pain')));
dat.nps_anticip_z(strcmp(dat.modality, 'pain')) = zscore(dat.nps_anticip(strcmp(dat.modality, 'pain')));
dat.siips_stim_z(strcmp(dat.modality, 'pain')) = zscore(dat.siips_stim(strcmp(dat.modality, 'pain')));
dat.siips_anticip_z(strcmp(dat.modality, 'pain')) = zscore(dat.siips_anticip(strcmp(dat.modality, 'pain')));
dat.rating_z(strcmp(dat.modality, 'pain')) = zscore(dat.rating(strcmp(dat.modality, 'pain')));
dat.weighted_expectation_z(strcmp(dat.modality, 'pain')) = zscore(dat.weighted_expectation(strcmp(dat.modality, 'pain')));

% vision
dat.nps_stim_z(strcmp(dat.modality, 'vision')) = zscore(dat.nps_stim(strcmp(dat.modality, 'vision')));
dat.nps_anticip_z(strcmp(dat.modality, 'vision')) = zscore(dat.nps_anticip(strcmp(dat.modality, 'vision')));
dat.siips_stim_z(strcmp(dat.modality, 'vision')) = zscore(dat.siips_stim(strcmp(dat.modality, 'vision')));
dat.siips_anticip_z(strcmp(dat.modality, 'vision')) = zscore(dat.siips_anticip(strcmp(dat.modality, 'vision')));
dat.rating_z(strcmp(dat.modality, 'vision')) = zscore(dat.rating(strcmp(dat.modality, 'vision')));
dat.weighted_expectation_z(strcmp(dat.modality, 'vision')) = zscore(dat.weighted_expectation(strcmp(dat.modality, 'vision')));

participants = unique(dat.participant);

[weighted_expectation_pain, weighted_expectation_z_pain, weighted_expectation_vision, weighted_expectation_z_vision, cue_mean_pain, cue_mean_vision, intensity_pain, intensity_vision, rating_pain, rating_z_pain, rating_vision, rating_z_vision] = deal(cell(size(participants)));
[nps_stim_pain, nps_stim_z_pain, nps_stim_vision, nps_stim_z_vision, siips_stim_pain, siips_stim_z_pain, siips_stim_vision, siips_stim_z_vision] = deal(cell(size(participants)));

for sub_ind = 1:length(participants)
    cur_sub = participants{sub_ind};
    disp(cur_sub);
    cur_data = dat(strcmp(dat.participant, cur_sub), :);
    % each variable should be a cell array, with one cell per participant, containing a
    % vector of values
    weighted_expectation_pain{sub_ind} = cur_data.weighted_expectation(strcmp(cur_data.modality, 'pain'));
    weighted_expectation_vision{sub_ind} = cur_data.weighted_expectation(strcmp(cur_data.modality, 'vision'));
    weighted_expectation_z_pain{sub_ind} = cur_data.weighted_expectation_z(strcmp(cur_data.modality, 'pain'));
    weighted_expectation_z_vision{sub_ind} = cur_data.weighted_expectation_z(strcmp(cur_data.modality, 'vision'));
    cue_mean_pain{sub_ind} = cur_data.cue_mean_level(strcmp(cur_data.modality, 'pain'));
    cue_mean_vision{sub_ind} = cur_data.cue_mean_level(strcmp(cur_data.modality, 'vision'));
    intensity_pain{sub_ind} = cur_data.stim_level(strcmp(cur_data.modality, 'pain'));
    intensity_vision{sub_ind} = cur_data.stim_level(strcmp(cur_data.modality, 'vision'));
    rating_pain{sub_ind} = cur_data.rating(strcmp(cur_data.modality, 'pain'));
    rating_vision{sub_ind} = cur_data.rating(strcmp(cur_data.modality, 'vision'));
    rating_z_pain{sub_ind} = cur_data.rating_z(strcmp(cur_data.modality, 'pain'));
    rating_z_vision{sub_ind} = cur_data.rating_z(strcmp(cur_data.modality, 'vision'));
    nps_stim_pain{sub_ind} = cur_data.nps_stim(strcmp(cur_data.modality, 'pain'));
    nps_stim_vision{sub_ind} = cur_data.nps_stim(strcmp(cur_data.modality, 'vision'));
    nps_stim_z_pain{sub_ind} = cur_data.nps_stim_z(strcmp(cur_data.modality, 'pain'));
    nps_stim_z_vision{sub_ind} = cur_data.nps_stim_z(strcmp(cur_data.modality, 'vision'));
    siips_stim_pain{sub_ind} = cur_data.siips_stim(strcmp(cur_data.modality, 'pain'));
    siips_stim_vision{sub_ind} = cur_data.siips_stim(strcmp(cur_data.modality, 'vision'));
    siips_stim_z_pain{sub_ind} = cur_data.siips_stim_z(strcmp(cur_data.modality, 'pain'));
    siips_stim_z_vision{sub_ind} = cur_data.siips_stim_z(strcmp(cur_data.modality, 'vision'));         
end

%% pain
% expectation --> NPS --> rating (intensity as covariate)
[paths, stats] = mediation(weighted_expectation_z_pain, rating_z_pain, nps_stim_z_pain, 'covs', intensity_pain, 'names', {'expectation','rating','NPS'}, 'boot','verbose', 'doplots', 'dosave','bootsamples', 10000);
mediation_path_diagram(stats)
% expectation --> SIIPS --> rating (intensity as covariate)
[paths, stats] = mediation(weighted_expectation_z_pain, rating_z_pain, siips_stim_z_pain, 'covs', intensity_pain, 'names', {'expectation','rating','SIIPS'}, 'boot','verbose', 'doplots', 'dosave','bootsamples', 10000);
mediation_path_diagram(stats)
% intensity --> NPS --> rating (expectation as covariate)
[paths, stats] = mediation(intensity_pain, rating_z_pain, nps_stim_z_pain, 'covs', weighted_expectation_z_pain, 'names', {'intensity','rating','NPS'}, 'boot','verbose', 'doplots', 'dosave','bootsamples', 10000);
mediation_path_diagram(stats)
% intensity --> SIIPS --> rating (expectation as covariate)
[paths, stats] = mediation(intensity_pain, rating_z_pain, siips_stim_z_pain, 'covs', weighted_expectation_z_pain, 'names', {'intensity','rating','SIIPS'}, 'boot','verbose', 'doplots', 'dosave','bootsamples', 10000);
mediation_path_diagram(stats)

%% vision
% expectation --> NPS --> rating (intensity as covariate)
[paths, stats] = mediation(weighted_expectation_z_vision, rating_z_vision, nps_stim_z_vision, 'covs', intensity_vision, 'names', {'expectation','rating','NPS'}, 'boot','verbose', 'doplots', 'dosave','bootsamples', 10000);
mediation_path_diagram(stats)
% expectation --> SIIPS --> rating (intensity as covariate)
[paths, stats] = mediation(weighted_expectation_z_vision, rating_z_vision, siips_stim_z_vision, 'covs', intensity_vision, 'names', {'expectation','rating','SIIPS'}, 'boot','verbose', 'doplots', 'dosave','bootsamples', 10000);
mediation_path_diagram(stats)
% intensity --> NPS --> rating (expectation as covariate)
[paths, stats] = mediation(intensity_vision, rating_z_vision, nps_stim_z_vision, 'covs', weighted_expectation_z_vision, 'names', {'intensity','rating','NPS'}, 'boot','verbose', 'doplots', 'dosave','bootsamples', 10000);
mediation_path_diagram(stats)
% intensity --> SIIPS --> rating (expectation as covariate)
[paths, stats] = mediation(intensity_vision, rating_z_vision, siips_stim_z_vision, 'covs', weighted_expectation_z_vision, 'names', {'intensity','rating','SIIPS'}, 'boot','verbose', 'doplots', 'dosave','bootsamples', 10000);
mediation_path_diagram(stats)