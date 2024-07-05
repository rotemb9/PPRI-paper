x = linspace(20,60,10);
x_rescaled = rescale(x, 0, 1);
x_rescaled_demeaned = x_rescaled - mean(x_rescaled);

% for the paper figure:
b_values = [-10, -1, 0, 1, 10];
k_values = [0.01, 0.2, 1, 5, 100];

colors_to_use = jet(length(k_values));
% for paper figure
colors_to_use = [0,0,1; % blue
                 0,0.75,1; % light blue
                 0,0,0; % black
                 1,0.75,0; % orange
                 1,0,0]; % red

y_lim = [0, 0.25];
% for the paper figure:
n_rows = 1;
n_cols = length(b_values);

figure('Name','Weights');
[k, b] = ndgrid(k_values, b_values);
b = b(:); k = k(:);
expectations_table = table(b, k);
n = length(x);
logistic_term = @(x, b, x0) 1 ./ (1 + exp(-b * (x - x0)));
power_term = @(x, k) (sign(x) .* abs(x) .^ k) ./ x;
ind = 1;
for b_ind = 1:length(b_values)
    subplot(n_rows, n_cols, b_ind);
    b = b_values(b_ind);
    for k_ind = 1:length(k_values)
        k = k_values(k_ind);
        weights_power = power_term(x_rescaled_demeaned,k);
        weights_power = weights_power ./ sum(weights_power);
        weights_logistic = logistic_term(x_rescaled_demeaned, b, 0);
        weights = weights_power + weights_logistic;
        weights = weights ./ sum(weights);
        expectations_table.mean(ind) = mean(x);
        expectations_table.median(ind) = median(x);
        expectations_table.min(ind) = min(x);
        expectations_table.max(ind) = max(x);
        expectation = x * weights';
        % show warning if expectation out of x range
        if expectation < min(x) || expectation > max(x)
            warning('expectation out of x range! a = %d, b = %d, d = %d', a, b, k);
        end
        if any(weights<0)
            warning('negative weight! %d a = %d, b = %d, d = %d',round(min(weights),4), round(a,4), round(b,4), round(k,4));
        end
        expectations_table.expectation(ind) = expectation;
        if k==1
            colors_to_use(k_ind,:) = [0,0,0];
            plot(x, weights, 'color', colors_to_use(k_ind, :), 'LineWidth',2);
        else
            plot(x, weights, 'color', colors_to_use(k_ind, :));
        end
        hold on
        ind = ind+1;
    end
    ylim(y_lim);
    yticks(y_lim(1):0.05:y_lim(2));
    xline(mean(x), 'k--', 'LineWidth',2);
    title(['b = ', num2str(b)], 'FontSize', 12);
    xlabel('V', 'FontSize', 12);
    ylabel('weights', 'FontSize', 12);
    xtickangle(45)
end

leg = legend(string(k_values),'Location', 'south', 'FontSize', 12);
title(leg, 'k', 'FontSize', 12);

% expectation heat map
figure('Name','Expectations');
heatmap(expectations_table,'k','b','ColorVariable','expectation');
xlabel('k');
ylabel('b');
colormap('jet');
title(['Expectation as a function of b and k; mean x = ' num2str(mean(x))]);
axis([min(x), max(x)]);