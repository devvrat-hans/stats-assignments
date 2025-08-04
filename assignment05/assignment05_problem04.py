# Assignment 05 - Problem 04

import numpy as np
import scipy.stats as stats

# Given information:
# Sample sizes
n1 = 12
n2 = 14
n3 = 11

# Means
mean1 = 11.69
mean3 = 9.71

# Standard deviations
sd1 = 2.4081
sd2 = 1.8991
sd3 = 1.9058

# Mean square error
MSE = 4.323

# Degrees of freedom
df_error = 34

# a) Find a 94% Bonferonni confidence interval for μ3 − μ1.
# For Bonferroni with 3 comparisons, we use alpha = 0.06/3 = 0.02 for each comparison
alpha = 0.06  # 6% significance level (100% - 94%)
num_comparisons = 3
alpha_per_comparison = alpha / num_comparisons
t_critical = stats.t.ppf(1 - alpha_per_comparison/2, df_error)

# Calculate the standard error for the difference in means
se_diff = np.sqrt(MSE * (1/n1 + 1/n3))

# Calculate the confidence interval
diff_means = mean3 - mean1
margin_error = t_critical * se_diff
lower_bound = diff_means - margin_error
upper_bound = diff_means + margin_error

print(f"94% Bonferroni confidence interval for μ3 - μ1: {lower_bound:.4f} to {upper_bound:.4f}")

# b) Which pairs of means are significantly different, using the Bonferroni method at the 6% significance level?
# From the pairwise comparisons output:
p_1_2 = 0.050  # p-value for comparing group 1 and 2
p_1_3 = 0.086  # p-value for comparing group 1 and 3
p_2_3 = 1.000  # p-value for comparing group 2 and 3

print("\nPairwise p-values (Bonferroni-adjusted):")
print(f"Group 1 vs 2: {p_1_2}")
print(f"Group 1 vs 3: {p_1_3}")
print(f"Group 2 vs 3: {p_2_3}")

# For 6% significance level
significance_level = 0.06

# Check which pairs are significantly different
diff_1_2 = p_1_2 < significance_level
diff_1_3 = p_1_3 < significance_level
diff_2_3 = p_2_3 < significance_level

print("\nAt 6% significance level:")
print(f"Group 1 and 2 significantly different: {diff_1_2}")
print(f"Group 1 and 3 significantly different: {diff_1_3}")
print(f"Group 2 and 3 significantly different: {diff_2_3}")

# Answer to part b
if diff_1_2 and not diff_1_3 and not diff_2_3:
    print("\nAnswer: (E) 1 and 2 only")
elif not diff_1_2 and diff_1_3 and not diff_2_3:
    print("\nAnswer: (A) 1 and 3 only")
elif diff_1_2 and diff_1_3 and not diff_2_3:
    print("\nAnswer: (B) 1 and 2, 1 and 3 only")
elif not diff_1_2 and not diff_1_3 and diff_2_3:
    print("\nAnswer: (C) 2 and 3 only")
elif not diff_1_2 and diff_1_3 and diff_2_3:
    print("\nAnswer: (D) 1 and 3, 2 and 3 only")
elif diff_1_2 and not diff_1_3 and diff_2_3:
    print("\nAnswer: (F) 1 and 2, 2 and 3 only")
elif diff_1_2 and diff_1_3 and diff_2_3:
    print("\nAnswer: (G) all of them")
else:
    print("\nAnswer: (H) none of them")

# CONCLUSION:
# a) 94% Bonferroni confidence interval for μ3 - μ1: -4.0478 to 0.0878
# b) Answer: (E) 1 and 2 only (since p_1_2 = 0.050 <= 0.06)
