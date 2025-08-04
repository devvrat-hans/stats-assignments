# Assignment 06 - Problem 01

import numpy as np
from scipy.stats import chi2

# Given data
observed = np.array([342, 83, 37])  # black, brown, pale
total = np.sum(observed)  # Total number of seeds

# According to theory (dominant epistasis)
expected_proportions = np.array([12/16, 3/16, 1/16])
expected = expected_proportions * total

# Print the data
print("Observed counts:")
print(f"Black: {observed[0]}, Brown: {observed[1]}, Pale: {observed[2]}")
print("\nExpected counts based on theory:")
print(f"Black: {expected[0]:.2f}, Brown: {expected[1]:.2f}, Pale: {expected[2]:.2f}")

# (a) Calculate the chi-square test statistic
chi_square = np.sum((observed - expected)**2 / expected)
print(f"\n(a) Chi-square test statistic: {chi_square:.3f}")

# (b) Find the critical value
# Degrees of freedom = number of categories - 1 = 3 - 1 = 2
df = len(observed) - 1
alpha = 0.01  # 1% significance level
critical_value = chi2.ppf(1 - alpha, df)
print(f"(b) Critical value at 1% significance level: {critical_value:.3f}")

# (c) Make a conclusion
if chi_square > critical_value:
    print("\nConclusion: Reject H0. The observed frequencies contradict the theory.")
    print("Answer: (A) We conclude that the data is consistent with the theory since the answer in (a) is greater than the answer in (b).")
else:
    print("\nConclusion: Do not reject H0. The data is consistent with the theory.")
    print("Answer: (G) We cannot conclude that the observed frequencies contradict the theory since the answer in (a) is less than or equal to the answer in (b).")

# Calculate p-value
p_value = 1 - chi2.cdf(chi_square, df)
print(f"\nP-value: {p_value:.6f}")

# CONCLUSIONS:
# a) Chi-square test statistic: 2.496
# b) Critical value at 1% significance level: 9.210
# c) Answer: (G) We cannot conclude that the observed frequencies contradict the theory since the answer in (a) is less than or equal to the answer in (b).
