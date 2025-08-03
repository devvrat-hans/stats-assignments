'''
Problem #1:
(a) Suppose that we identify 167 women 50 to 54 years of age who have both a mother and a sister with a history 
of breast cancer. 18 of these women themselves have developed breast cancer at some time in their lives. If 
we assume that the proportion of breast cancer cases in women whose mothers have had breast cancer is 8%, 
does having a sister with the disease increase the risk? Find the p-value.

(b) At the 10% significance level, what is the conclusion of the above hypothesis test?
'''

import numpy as np
from scipy import stats

# Problem 1(a): Calculate the p-value

# Given information
n = 167  # sample size
x = 18   # number of women with breast cancer
p0 = 0.08  # assumed proportion (null hypothesis)

# Calculate the sample proportion
p_hat = x / n
print(f"Sample proportion: {p_hat:.4f}")

# Perform hypothesis test for proportion
# H0: p = 0.08 (no increased risk)
# H1: p > 0.08 (increased risk due to sister)

# Calculate the test statistic (z-score)
z_score = (p_hat - p0) / np.sqrt(p0 * (1 - p0) / n)
print(f"z-score: {z_score:.4f}")

# Calculate the p-value for a one-tailed (right-tailed) test
p_value = 1 - stats.norm.cdf(z_score)
print(f"p-value: {p_value:.4f}")

# Problem 1(b): Conclusion at 10% significance level
alpha = 0.10
print("\nAt 10% significance level:")
if p_value < alpha:
    print(f"Since p-value ({p_value:.4f}) < α ({alpha}), we reject the null hypothesis.")
    print("We conclude that having a sister with the disease increases the risk.")
else:
    print(f"Since p-value ({p_value:.4f}) ≥ α ({alpha}), we fail to reject the null hypothesis.")
    print("We cannot conclude that having a sister with the disease increases the risk.")
