function [F, P] = model_comparison_test(ss1, ss2, nparam1, nparam2, n)
% model comparison for nested models
% varify that nparam2 > nparam1
if nparam1 >= nparam2
    error('models are not nested');
end

ss_diff = ss1 - ss2;
df_diff = nparam2 - nparam1;
ms_diff = ss_diff ./ df_diff;
df_res_full = n - nparam2;
ms_res_full = ss2 ./ df_res_full;
F = ms_diff ./ ms_res_full;
P = 1 - fcdf(F, df_diff, df_res_full);

end