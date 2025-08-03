'''
Problem #4: In a 1868 paper, German physician Carl Wunderlich reported based on over a million body temperature readings
that the mean body temperature for healthy adults is 98.6° F. However, it is now commonly believed that the
mean body temperature of a healthy adult is less than what was reported in that paper. To test this hypothesis a
researcher measures the following body temperatures from a random sample of healthy adults.

98.4, 98.7, 98.8, 98.4, 97.6, 98.2, 98.3

(a) Find the value of the test statistic.
(b) Find the 5% critical value.
(c) Find the p-value for the relevant hypothesis test.
(d) Since the sample size is less than 30, the above analysis requires that the population follows a normal
distribution. What method could be used to check this assumption?
'''

import numpy as np
import matplotlib.pyplot as plt
from scipy import stats

# Problem 4: One-sample t-test and checking normality

# Given information
temperatures = np.array([98.4, 98.7, 98.8, 98.4, 97.6, 98.2, 98.3])
claimed_mean = 98.6  # Wunderlich's claimed mean temperature
alpha = 0.05  # significance level

# Sample statistics
sample_mean = np.mean(temperatures)
sample_std = np.std(temperatures, ddof=1)  # using n-1 for sample standard deviation
n = len(temperatures)

print("Problem 4: One-sample t-test and checking normality")
print("==================================================")

print(f"\nSample data: {temperatures}")
print(f"Sample size: {n}")
print(f"Sample mean: {sample_mean:.4f}")
print(f"Sample standard deviation: {sample_std:.4f}")
print(f"Hypothesized population mean: {claimed_mean}")

# (a) Calculate the test statistic (t-score)
t_score = (sample_mean - claimed_mean) / (sample_std / np.sqrt(n))
print(f"\n(a) Test statistic (t-score): {t_score:.4f}")

# (b) Calculate the critical value for a one-tailed (left-tailed) test
# degrees of freedom = n - 1
df = n - 1
t_critical = stats.t.ppf(alpha, df)  # left-tailed
print(f"(b) Critical value (left-tailed, alpha = {alpha}): {t_critical:.3f}")

# (c) Calculate the p-value for a one-tailed (left-tailed) test
p_value = stats.t.cdf(t_score, df)
print(f"(c) p-value: {p_value:.4f}")

# Alternative using t-test function
t_stat, p_val = stats.ttest_1samp(temperatures, claimed_mean)
# Convert to one-tailed p-value (since the alternative is "less than")
p_val_one_tailed = p_val / 2  # divide by 2 if t_stat is negative
if t_stat > 0:
    p_val_one_tailed = 1 - p_val / 2

print(f"    Using scipy.stats.ttest_1samp (one-tailed): {p_val_one_tailed:.4f}")

# Drawing a conclusion
print("\nConclusion:")
if p_value < alpha:
    print(f"Since p-value ({p_value:.4f}) < α ({alpha}), we reject the null hypothesis.")
    print("We conclude that the mean body temperature is less than 98.6°F.")
else:
    print(f"Since p-value ({p_value:.4f}) ≥ α ({alpha}), we fail to reject the null hypothesis.")
    print("We cannot conclude that the mean body temperature is less than 98.6°F.")

# (d) Checking the normality assumption
print("\n(d) Methods to check normality assumption for a small sample:")
print("1. Normal Probability Plot (Q-Q Plot) - visual inspection")
print("2. Shapiro-Wilk test - statistical test for normality")
print("3. Anderson-Darling test - statistical test for normality")
print("4. Kolmogorov-Smirnov test - statistical test for normality")

# Perform Shapiro-Wilk test (appropriate for small samples)
shapiro_stat, shapiro_p = stats.shapiro(temperatures)
print(f"\nShapiro-Wilk test result:")
print(f"W = {shapiro_stat:.4f}, p-value = {shapiro_p:.4f}")
if shapiro_p < 0.05:
    print("Since p-value < 0.05, we reject the null hypothesis of normality.")
    print("The data does not appear to follow a normal distribution.")
else:
    print("Since p-value ≥ 0.05, we fail to reject the null hypothesis of normality.")
    print("The data appears to follow a normal distribution.")

# Create Q-Q plot code
print("\nTo visually check normality, create a Q-Q plot.")
print("In a Q-Q plot, if points fall close to a straight line, the data follows an approximately normal distribution.")
