'''
Problem #3: The health of the bear population in Yellowstone National Park is monitored by periodic measurements taken
from anesthetized bears. A sample of 42 bears has a mean weight of 188.4 lb.

At α = .04, can it be concluded that the average weight of a bear in Yellowstone National Park is different from
187 lb? Note that the standard deviation of the weight of a bear is known to be 8.4 lb.

(a) Find the value of the test statistic for the above hypothesis.
(b) Find the critical value.
(c) Find the p-value.
(d) What is the correct way to draw a conclusion regarding the above hypothesis test?
'''

import numpy as np
from scipy import stats

# Problem 3: Hypothesis test for population mean with known standard deviation

# Given information
sample_mean = 188.4  # sample mean weight
population_mean = 187  # hypothesized population mean weight
population_sd = 8.4  # known population standard deviation
n = 42  # sample size
alpha = 0.04  # significance level

# (a) Calculate the test statistic (z-score)
z_score = (sample_mean - population_mean) / (population_sd / np.sqrt(n))
print(f"(a) Test statistic (z-score): {z_score:.2f}")

# (b) Calculate the critical value for a two-tailed test
critical_value = stats.norm.ppf(1 - alpha/2)
print(f"(b) Critical value (two-tailed, alpha = {alpha}): ±{critical_value:.2f}")

# (c) Calculate the p-value for a two-tailed test
p_value = 2 * (1 - stats.norm.cdf(abs(z_score)))
print(f"(c) p-value: {p_value:.4f}")

# (d) Drawing a conclusion
print("\n(d) Conclusion:")
print("Comparing test statistic to critical value:")
if abs(z_score) > critical_value:
    print(f"Since |{z_score:.2f}| > {critical_value:.2f}, we reject the null hypothesis.")
else:
    print(f"Since |{z_score:.2f}| ≤ {critical_value:.2f}, we fail to reject the null hypothesis.")

print("\nComparing p-value to alpha:")
if p_value < alpha:
    print(f"Since p-value ({p_value:.4f}) < α ({alpha}), we reject the null hypothesis.")
    print("We conclude that the average weight of a bear is different from 187 lb.")
else:
    print(f"Since p-value ({p_value:.4f}) ≥ α ({alpha}), we fail to reject the null hypothesis.")
    print("We cannot conclude that the average weight of a bear is different from 187 lb.")
