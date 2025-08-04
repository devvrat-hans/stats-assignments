# Assignment 05 - Problem 02
# Statistics Problem Solution in Python
# Pairwise Hypothesis Tests

import numpy as np
import scipy.stats as stats
from scipy.stats import f_oneway, t
import pandas as pd
from math import sqrt

print("Assignment 05 - Problem 02 Solution")
print("=" * 40)

print("\n" + "="*60)
print("ASSIGNMENT 05 - PROBLEM 02 - ACTUAL SOLUTION")
print("="*60)

print("Hypothesis tests for pairwise comparisons:")
print("1. H₀: μ₁ = μ₂, H₁: μ₁ ≠ μ₂")
print("2. H₀: μ₁ = μ₃, H₁: μ₁ ≠ μ₃")
print("3. H₀: μ₂ = μ₃, H₁: μ₂ ≠ μ₃")

# Data from Problem 01 (using same data for consistency)
group1 = [4.2, 6.1, 3.4]
group2 = [4.5, 2.7, 2.3, 2.3]
group3 = [1.2, -0.3, 0.4]

print("\nData from assignment05_problem01:")
print("Group 1:", group1, f"(n1 = {len(group1)})")
print("Group 2:", group2, f"(n2 = {len(group2)})")
print("Group 3:", group3, f"(n3 = {len(group3)})")

# Calculate basic statistics
mean1, mean2, mean3 = np.mean(group1), np.mean(group2), np.mean(group3)
std1, std2, std3 = np.std(group1, ddof=1), np.std(group2, ddof=1), np.std(group3, ddof=1)
n1, n2, n3 = len(group1), len(group2), len(group3)

# From Problem 01 ANOVA results
all_data = group1 + group2 + group3
grand_mean = np.mean(all_data)
total_n = len(all_data)

# Calculate MSE (Mean Square Error) from ANOVA
ssw1 = sum((x - mean1)**2 for x in group1)
ssw2 = sum((x - mean2)**2 for x in group2)
ssw3 = sum((x - mean3)**2 for x in group3)
ssw = ssw1 + ssw2 + ssw3
df_within = total_n - 3
mse = ssw / df_within

print(f"\nFrom Problem 01 ANOVA:")
print(f"Group 1 mean (μ₁) = {mean1:.4f}")
print(f"Group 2 mean (μ₂) = {mean2:.4f}")
print(f"Group 3 mean (μ₃) = {mean3:.4f}")
print(f"MSE (Mean Square Error) = {mse:.4f}")
print(f"Degrees of freedom (within) = {df_within}")

alpha = 0.05

print("\nPart (a) - Test Statistics for Pairwise Comparisons")
print("-" * 55)

# Calculate t-test statistics for each pairwise comparison
# Using pooled variance from ANOVA for all tests

# Function to calculate t-statistic for two groups with pooled variance
def t_statistic_pooled(mean_i, mean_j, n_i, n_j, mse):
    """Calculate t-statistic using pooled variance from ANOVA"""
    t_stat = (mean_i - mean_j) / sqrt(mse * (1/n_i + 1/n_j))
    return t_stat

# 1. H₀: μ₁ = μ₂, H₁: μ₁ ≠ μ₂
t_stat_12 = t_statistic_pooled(mean1, mean2, n1, n2, mse)
df_12 = df_within  # Using df from ANOVA
p_value_12 = 2 * (1 - stats.t.cdf(abs(t_stat_12), df_12))

# 2. H₀: μ₁ = μ₃, H₁: μ₁ ≠ μ₃
t_stat_13 = t_statistic_pooled(mean1, mean3, n1, n3, mse)
df_13 = df_within  # Using df from ANOVA
p_value_13 = 2 * (1 - stats.t.cdf(abs(t_stat_13), df_13))

# 3. H₀: μ₂ = μ₃, H₁: μ₂ ≠ μ₃
t_stat_23 = t_statistic_pooled(mean2, mean3, n2, n3, mse)
df_23 = df_within  # Using df from ANOVA
p_value_23 = 2 * (1 - stats.t.cdf(abs(t_stat_23), df_23))

print("\nTest Statistics for Pairwise Comparisons:")
print(f"1. μ₁ vs μ₂: t = {t_stat_12:.4f}, df = {df_12}, p = {p_value_12:.4f}")
print(f"2. μ₁ vs μ₃: t = {t_stat_13:.4f}, df = {df_13}, p = {p_value_13:.4f}")
print(f"3. μ₂ vs μ₃: t = {t_stat_23:.4f}, df = {df_23}, p = {p_value_23:.4f}")

# Critical t-value for alpha = 0.05
t_critical = stats.t.ppf(1 - alpha/2, df_within)
print(f"\nCritical t-value (α = {alpha}, df = {df_within}): {t_critical:.4f}")

# Decisions for each hypothesis test
print("\nDecisions (without Bonferroni correction):")
print(f"1. H₀: μ₁ = μ₂: {'Reject H₀' if p_value_12 < alpha else 'Fail to reject H₀'}")
print(f"2. H₀: μ₁ = μ₃: {'Reject H₀' if p_value_13 < alpha else 'Fail to reject H₀'}")
print(f"3. H₀: μ₂ = μ₃: {'Reject H₀' if p_value_23 < alpha else 'Fail to reject H₀'}")

# With Bonferroni correction
alpha_bonferroni = alpha / 3
print(f"\nDecisions (with Bonferroni correction, α = {alpha_bonferroni:.4f}):")
print(f"1. H₀: μ₁ = μ₂: {'Reject H₀' if p_value_12 < alpha_bonferroni else 'Fail to reject H₀'}")
print(f"2. H₀: μ₁ = μ₃: {'Reject H₀' if p_value_13 < alpha_bonferroni else 'Fail to reject H₀'}")
print(f"3. H₀: μ₂ = μ₃: {'Reject H₀' if p_value_23 < alpha_bonferroni else 'Fail to reject H₀'}")

print("\nPart (b) - Significant Differences at 3% Significance Level")
print("-" * 60)

# Using Bonferroni method with 3% significance level
alpha_b = 0.03
alpha_bonferroni_b = alpha_b / 3  # Bonferroni-adjusted alpha
print(f"Using Bonferroni method with {alpha_b:.2f} significance level")
print(f"Adjusted significance level: α = {alpha_bonferroni_b:.4f}")

# Check which pairs are significantly different
sig_diff_12 = p_value_12 < alpha_bonferroni_b
sig_diff_13 = p_value_13 < alpha_bonferroni_b
sig_diff_23 = p_value_23 < alpha_bonferroni_b

print("\nWhich pairs of means are significantly different:")
print(f"1 and 2: {'Significant difference' if sig_diff_12 else 'Not significant'} (p = {p_value_12:.4f})")
print(f"1 and 3: {'Significant difference' if sig_diff_13 else 'Not significant'} (p = {p_value_13:.4f})")
print(f"2 and 3: {'Significant difference' if sig_diff_23 else 'Not significant'} (p = {p_value_23:.4f})")

# Multiple choice answer determination
answer = ""
if sig_diff_12 and sig_diff_23 and not sig_diff_13:
    answer = "(A) 1 and 2, 2 and 3 only"
elif sig_diff_12 and sig_diff_13 and not sig_diff_23:
    answer = "(B) 1 and 2, 1 and 3 only"
elif sig_diff_12 and sig_diff_13 and sig_diff_23:
    answer = "(C) all of them"
elif sig_diff_13 and sig_diff_23 and not sig_diff_12:
    answer = "(F) 1 and 3, 2 and 3 only"
elif sig_diff_13 and not sig_diff_12 and not sig_diff_23:
    answer = "(G) 1 and 3 only"
elif sig_diff_12 and not sig_diff_13 and not sig_diff_23:
    answer = "(H) 1 and 2 only"
else:
    answer = "None of the options match the results"

print(f"\nMultiple choice answer: {answer}")

print("\n" + "="*60)
print("FINAL ANSWERS FOR SUBMISSION:")
print("="*60)

print(f"Part (a): Test Statistics for Pairwise Comparisons")
print(f"1. H₀: μ₁ = μ₂: t = {t_stat_12:.4f}")
print(f"2. H₀: μ₁ = μ₃: t = {t_stat_13:.4f}")
print(f"3. H₀: μ₂ = μ₃: t = {t_stat_23:.4f}")

print(f"\nPart (b): {answer}")

print("="*60)
print("Note: These results are based on the ANOVA from Problem 01")
