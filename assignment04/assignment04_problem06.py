'''
Problem #6: An observational study of Alzheimer's disease (AD) obtained data from 9 AD patients exhibiting moderate
dementia and selected a group of 7 control individuals without AD. AD is a progressive neurodegenerative
disease of the elderly and advancing age is known to be a primary risk factor in AD diagnosis. Therefore, it was
crucial for the study's credibility to examine whether the ages in the AD group might be significantly different
than in the control group. The ages of the subjects in years are summarized in the R Output below.

Variable    n    Mean    sd    Minimum    Q1 Median    Q3 Maximum
Alzheimers  9    72.51   9.57  77.00      79.25 87.00  92.25 93.00
Control     7    66.60   12.86 54.00      56.00 65.00  82.00 89.00

We want to test if the average age in the Alzheimer's group is significantly different than the control group.
Assume that the population variances are equal.

(a) What is the null hypothesis?
(b) Find the value of the test statistic.
(c) Find the 5% critical value.
(d) What is the conclusion of the hypothesis test?
'''

import numpy as np
from scipy import stats

# Problem 6: Two-sample t-test with equal variances

print("Problem #6: Two-sample t-test (Alzheimer's study)")
print("================================================")

# Given information from the R Output
alzheimers_n = 9
alzheimers_mean = 72.51
alzheimers_sd = 9.57

control_n = 7
control_mean = 66.60
control_sd = 12.86

# Significance level
alpha = 0.05

# (a) Null hypothesis
print("\n(a) Null hypothesis:")
print("   H₀: μ₁ = μ₂ (The mean age in the Alzheimer's group equals the mean age in the control group)")
print("   H₁: μ₁ ≠ μ₂ (The mean age in the Alzheimer's group differs from the mean age in the control group)")

# (b) Calculate the test statistic
# Pooled standard deviation (assuming equal variances)
pooled_var = ((alzheimers_n - 1) * alzheimers_sd**2 + (control_n - 1) * control_sd**2) / (alzheimers_n + control_n - 2)
pooled_sd = np.sqrt(pooled_var)

# t-statistic
t_stat = (alzheimers_mean - control_mean) / (pooled_sd * np.sqrt(1/alzheimers_n + 1/control_n))
print(f"\n(b) Test statistic: {t_stat:.3f}")

# (c) Critical value (two-tailed test)
df = alzheimers_n + control_n - 2  # degrees of freedom
t_critical = stats.t.ppf(1 - alpha/2, df)
print(f"\n(c) Critical value (two-tailed, alpha = {alpha}): ±{t_critical:.3f}")

# (d) Conclusion
p_value = 2 * (1 - stats.t.cdf(abs(t_stat), df))  # two-tailed p-value

print("\n(d) Conclusion:")
print(f"   t-statistic: {t_stat:.3f}")
print(f"   Critical value: ±{t_critical:.3f}")
print(f"   p-value: {p_value:.4f}")

if abs(t_stat) > t_critical:
    print(f"   Since |{t_stat:.3f}| > {t_critical:.3f}, we reject the null hypothesis.")
    print("   There is a significant difference in mean age between the Alzheimer's group and the control group.")
else:
    print(f"   Since |{t_stat:.3f}| ≤ {t_critical:.3f}, we fail to reject the null hypothesis.")
    print("   There is not enough evidence of a difference in mean age between the two groups.")

if p_value < alpha:
    print(f"\n   Since p-value ({p_value:.4f}) < α ({alpha}), we reject the null hypothesis.")
else:
    print(f"\n   Since p-value ({p_value:.4f}) ≥ α ({alpha}), we fail to reject the null hypothesis.")
