# Assignment 05 - Problem 03
# Statistics Problem Solution in Python

import numpy as np
from scipy import stats
from scipy.stats import anderson, bartlett

# Try to import matplotlib for plotting, but continue if not available
try:
    import matplotlib.pyplot as plt
    matplotlib_available = True
except ImportError:
    print("Warning: matplotlib not available, skipping plot generation")
    matplotlib_available = False

print("Assignment 05 - Problem 03 Solution")
print("=" * 40)

# Data from Problem #1
group1 = [4.2, 6.1, 3.4]
group2 = [4.5, 2.7, 2.3, 2.3]
group3 = [1.2, -0.3, 0.4]

print("Data from Problem #1:")
print("Group 1:", group1, f"(n1 = {len(group1)})")
print("Group 2:", group2, f"(n2 = {len(group2)})")
print("Group 3:", group3, f"(n3 = {len(group3)})")

# Part (a) - Find the p-value for the hypothesis test in Problem #1(a)
print("\nPart (a) - P-value for One-Way ANOVA")
print("-" * 40)

# Perform One-Way ANOVA
f_statistic, p_value_anova = stats.f_oneway(group1, group2, group3)
print(f"F-statistic: {f_statistic:.4f}")
print(f"P-value: {p_value_anova:.6f}")

# Part (b) - Normal probability plot of residuals and Anderson-Darling test
print("\nPart (b) - Normal Probability Plot & Anderson-Darling Test")
print("-" * 40)

# Prepare data for residuals calculation
all_data = group1 + group2 + group3
group_labels = np.array([1] * len(group1) + [2] * len(group2) + [3] * len(group3))
grand_mean = np.mean(all_data)

# Calculate group means
group_means = {
    1: np.mean(group1),
    2: np.mean(group2),
    3: np.mean(group3)
}

# Calculate residuals (observed - fitted value)
residuals = []
for i, value in enumerate(all_data):
    group = group_labels[i]
    fitted_value = group_means[group]
    residual = value - fitted_value
    residuals.append(residual)

residuals = np.array(residuals)

# Create normal probability plot (QQ Plot) if matplotlib is available
if matplotlib_available:
    plt.figure(figsize=(10, 6))
    stats.probplot(residuals, plot=plt)
    plt.title('Normal Probability Plot of Residuals')
    plt.grid(True)
    plt.savefig('assignment05_problem03.png')
    plt.close()
else:
    print("Skipping plot generation as matplotlib is not available")

# Perform Anderson-Darling test for normality
ad_result = anderson(residuals)
ad_statistic = ad_result.statistic
ad_critical_values = ad_result.critical_values
ad_significance_levels = [15, 10, 5, 2.5, 1]

# Calculate p-value using approximation (since scipy doesn't provide p-value directly)
# We'll estimate the p-value based on the critical values
if ad_statistic < ad_critical_values[0]:  # If less than 15% critical value
    ad_p_value = 0.15  # p > 0.15
elif ad_statistic > ad_critical_values[-1]:  # If greater than 1% critical value
    ad_p_value = 0.01  # p < 0.01
else:
    # Find the two critical values that bracket the statistic
    for i in range(len(ad_critical_values) - 1):
        if ad_critical_values[i] <= ad_statistic <= ad_critical_values[i + 1]:
            # Linear interpolation to estimate p-value
            lower_sig = ad_significance_levels[i] / 100
            upper_sig = ad_significance_levels[i + 1] / 100
            lower_crit = ad_critical_values[i]
            upper_crit = ad_critical_values[i + 1]
            
            # Interpolate
            ad_p_value = lower_sig + (upper_sig - lower_sig) * (ad_statistic - lower_crit) / (upper_crit - lower_crit)
            break

print(f"Anderson-Darling Statistic: {ad_statistic:.4f}")
print(f"Estimated p-value: {ad_p_value:.6f}")

# For more accurate p-value, we can use additional normality tests
_, shapiro_p = stats.shapiro(residuals)
print(f"Shapiro-Wilk p-value: {shapiro_p:.6f}")

# Part (c) - Bartlett's test for equality of variances
print("\nPart (c) - Bartlett's Test for Equality of Variances")
print("-" * 40)

# Perform Bartlett's test
bartlett_stat, bartlett_p = bartlett(group1, group2, group3)
print(f"Bartlett's Statistic: {bartlett_stat:.4f}")
print(f"P-value: {bartlett_p:.6f}")

print("\n" + "="*60)
print("FINAL ANSWERS FOR SUBMISSION:")
print("="*60)

print(f"Part (a): P-value for One-Way ANOVA = {p_value_anova:.6f}")
print(f"Part (b): P-value for Anderson-Darling test = {ad_p_value:.6f}")
print(f"Part (c): P-value for Bartlett's test = {bartlett_p:.6f}")
print("="*60)
