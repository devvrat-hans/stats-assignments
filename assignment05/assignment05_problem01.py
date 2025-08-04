# Assignment 05 - Problem 01
# Statistics Problem Solution in Python
# Three-Group Comparison Analysis

import numpy as np
import scipy.stats as stats
from scipy.stats import f_oneway
import pandas as pd

print("Assignment 05 - Problem 01 Solution")
print("=" * 40)

print("\n" + "="*60)
print("ASSIGNMENT 05 - PROBLEM 01 - ACTUAL SOLUTION")
print("="*60)

# Real data from the problem image - ACTUAL VALUES
# Group 1: 3 values, Group 2: 4 values, Group 3: 3 values
group1 = [4.2, 6.1, 3.4]
group2 = [4.5, 2.7, 2.3, 2.3]
group3 = [1.2, -0.3, 0.4]

print("Data from assignment05_problem01_data.png:")
print("Group 1:", group1, f"(n1 = {len(group1)})")
print("Group 2:", group2, f"(n2 = {len(group2)})")
print("Group 3:", group3, f"(n3 = {len(group3)})")

# Calculate descriptive statistics for each group
mean1, mean2, mean3 = np.mean(group1), np.mean(group2), np.mean(group3)
std1, std2, std3 = np.std(group1, ddof=1), np.std(group2, ddof=1), np.std(group3, ddof=1)
n1, n2, n3 = len(group1), len(group2), len(group3)

print(f"\nDescriptive Statistics:")
print(f"Group 1: Mean = {mean1:.4f}, SD = {std1:.4f}, n = {n1}")
print(f"Group 2: Mean = {mean2:.4f}, SD = {std2:.4f}, n = {n2}")
print(f"Group 3: Mean = {mean3:.4f}, SD = {std3:.4f}, n = {n3}")

# Problem parameters
alpha = 0.05

print("\nPart (a) - One-Way ANOVA")
print("-" * 30)
print("H₀: μ₁ = μ₂ = μ₃ (all group means are equal)")
print("H₁: At least one group mean is different")
print(f"Significance level: α = {alpha}")

# Perform One-Way ANOVA
f_statistic, p_value_anova = f_oneway(group1, group2, group3)

# Calculate ANOVA components manually for verification
all_data = group1 + group2 + group3
grand_mean = np.mean(all_data)
total_n = len(all_data)

# Between-group sum of squares (SSB)
ssb = n1 * (mean1 - grand_mean)**2 + n2 * (mean2 - grand_mean)**2 + n3 * (mean3 - grand_mean)**2

# Within-group sum of squares (SSW)
ssw1 = sum((x - mean1)**2 for x in group1)
ssw2 = sum((x - mean2)**2 for x in group2)
ssw3 = sum((x - mean3)**2 for x in group3)
ssw = ssw1 + ssw2 + ssw3

# Degrees of freedom
df_between = 3 - 1  # k - 1
df_within = total_n - 3  # N - k
df_total = total_n - 1

# Mean squares
msb = ssb / df_between
msw = ssw / df_within

# F-statistic
f_calc = msb / msw

print(f"\nANOVA Results:")
print(f"F-statistic: {f_statistic:.4f}")
print(f"P-value: {p_value_anova:.4f}")
print(f"Degrees of freedom: {df_between}, {df_within}")

# Critical F-value
f_critical = stats.f.ppf(1 - alpha, df_between, df_within)
print(f"Critical F-value: {f_critical:.4f}")

# Decision for Part (a)
if p_value_anova < alpha:
    decision_a = "Reject H₀"
    conclusion_a = "There is sufficient evidence that at least one group mean is different"
else:
    decision_a = "Fail to reject H₀"
    conclusion_a = "There is insufficient evidence that the group means are different"

print(f"\nDecision: {decision_a}")
print(f"Conclusion: {conclusion_a}")

print("\nPart (b) - ANOVA Components: SS(treatment) and SS(error)")
print("-" * 50)

# Calculate Sum of Squares components
print("Sum of Squares Calculations:")
print(f"SS(treatment) = SS(between groups) = {ssb:.4f}")
print(f"SS(error) = SS(within groups) = {ssw:.4f}")
print(f"SS(total) = {ssb + ssw:.4f}")

print(f"\nANOVA Table:")
print(f"Source         df    SS        MS        F       p-value")
print(f"Treatment      {df_between:2d}    {ssb:7.3f}   {msb:7.3f}   {f_calc:7.4f}   {p_value_anova:.4f}")
print(f"Error          {df_within:2d}    {ssw:7.3f}   {msw:7.3f}")
print(f"Total          {df_total:2d}    {ssb + ssw:7.3f}")

print("\nPart (b) - Pairwise Comparisons (if ANOVA is significant)")
print("-" * 50)

# Perform pairwise t-tests
t_stat_12, p_value_12 = stats.ttest_ind(group1, group2, equal_var=True)
t_stat_13, p_value_13 = stats.ttest_ind(group1, group3, equal_var=True)
t_stat_23, p_value_23 = stats.ttest_ind(group2, group3, equal_var=True)

print("Pairwise t-test results:")
print(f"Group 1 vs Group 2: t = {t_stat_12:.4f}, p = {p_value_12:.4f}")
print(f"Group 1 vs Group 3: t = {t_stat_13:.4f}, p = {p_value_13:.4f}")
print(f"Group 2 vs Group 3: t = {t_stat_23:.4f}, p = {p_value_23:.4f}")

# Bonferroni correction
alpha_bonferroni = alpha / 3
print(f"\nWith Bonferroni correction (α = {alpha_bonferroni:.4f}):")
print(f"Group 1 vs Group 2: {'Significant' if p_value_12 < alpha_bonferroni else 'Not significant'}")
print(f"Group 1 vs Group 3: {'Significant' if p_value_13 < alpha_bonferroni else 'Not significant'}")
print(f"Group 2 vs Group 3: {'Significant' if p_value_23 < alpha_bonferroni else 'Not significant'}")

print("\nPart (c) - Effect Size and Confidence Intervals")
print("-" * 45)

# Calculate eta-squared (effect size for ANOVA)
eta_squared = ssb / (ssb + ssw)
print(f"Eta-squared (η²): {eta_squared:.4f}")

if eta_squared < 0.01:
    effect_interpretation = "Small effect size"
elif eta_squared < 0.06:
    effect_interpretation = "Medium effect size"
elif eta_squared < 0.14:
    effect_interpretation = "Large effect size"
else:
    effect_interpretation = "Very large effect size"

print(f"Effect size interpretation: {effect_interpretation}")

# Confidence intervals for each group mean
confidence_level = 0.95
for i, (group, mean, std, n) in enumerate([(group1, mean1, std1, n1), 
                                           (group2, mean2, std2, n2), 
                                           (group3, mean3, std3, n3)], 1):
    df_group = n - 1
    t_crit = stats.t.ppf(1 - alpha/2, df_group)
    margin_error = t_crit * (std / np.sqrt(n))
    ci_lower = mean - margin_error
    ci_upper = mean + margin_error
    print(f"Group {i}: 95% CI = ({ci_lower:.4f}, {ci_upper:.4f})")

print("\n" + "="*60)
print("FINAL ANSWERS FOR SUBMISSION:")
print("="*60)

print(f"Part (a): {decision_a}")
print(f"         F-statistic = {f_statistic:.4f}")
print(f"         P-value = {p_value_anova:.4f}")
if p_value_anova < alpha:
    print(f"         Since p-value ({p_value_anova:.4f}) < α ({alpha}), we {decision_a}")
else:
    print(f"         Since p-value ({p_value_anova:.4f}) > α ({alpha}), we {decision_a}")

print(f"\nPart (b): SS(treatment) = {ssb:.4f}")
print(f"         SS(error) = {ssw:.4f}")
print(f"         Pairwise comparisons:")
print(f"         Group 1 vs 2: p = {p_value_12:.4f}")
print(f"         Group 1 vs 3: p = {p_value_13:.4f}")
print(f"         Group 2 vs 3: p = {p_value_23:.4f}")

print(f"\nPart (c): Effect size η² = {eta_squared:.4f} ({effect_interpretation})")

print("\nMULTIPLE CHOICE ANSWERS:")
print("Part (a): (G) Reject H0 since 11.0335 > 4.7374")
print("         This matches our F-statistic and critical value exactly!")
print("Part (c): Option corresponding to the calculated effect size")

print("="*60)
