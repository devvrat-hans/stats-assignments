# Assignment 05 - Problem 06

import numpy as np
import scipy.stats as stats
import matplotlib.pyplot as plt

# Given data from Problem #5:
x = np.array([2, 5, 9, 8, 14])
y = np.array([9, 7, 6, 4, 0])

# Regression coefficients from Problem #5
b0 = 10.6847
b1 = -0.7217

# a) Calculate the residuals
predicted_y = b0 + b1 * x
residuals = y - predicted_y

# Format residuals to 4 decimals
residuals_formatted = [f"{res:.4f}" for res in residuals]
print(f"a) Residuals: {', '.join(residuals_formatted)}")

# b) Calculate the residual sum of squares SS(error)
ss_error = np.sum(residuals**2)
print(f"b) SS(error): {ss_error:.4f}")

# c) Find the test statistic for H0: ρ = 0 vs H1: ρ ≠ 0
# Correlation coefficient
r = np.corrcoef(x, y)[0, 1]
# Test statistic: t = r * sqrt(n-2) / sqrt(1-r^2)
n = len(x)
t_stat = r * np.sqrt(n-2) / np.sqrt(1-r**2)
print(f"c) Test statistic: {t_stat:.2f}")

# d) Find the 1% critical value for the hypothesis test
# Two-tailed test with alpha = 0.01 and df = n-2
alpha = 0.01
df = n - 2
critical_value = stats.t.ppf(1-alpha/2, df)
print(f"d) Critical value: {critical_value:.3f}")

# e) Find the p-value for the hypothesis test
# Two-tailed test
p_value = 2 * stats.t.sf(abs(t_stat), df)
print(f"e) p-value: {p_value:.6f}")

# Plot residuals
plt.figure(figsize=(10, 6))
plt.scatter(predicted_y, residuals, color='blue')
plt.axhline(y=0, color='red', linestyle='-')
plt.title('Residual Plot')
plt.xlabel('Predicted Values')
plt.ylabel('Residuals')
plt.grid(True)
plt.savefig('/Users/devvrathans/stats-assignment/assignment05/assignment05_problem06.png')
plt.close()

# CONCLUSIONS:
# a) Residuals: -0.2413, -0.0762, 1.8106, -0.9111, -0.5809
# b) SS(error): 4.5099
# c) Test statistic: -5.30
# d) Critical value: 5.841
# e) p-value: 0.0131
