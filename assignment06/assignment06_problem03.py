# Assignment 6 - Problem 3
# A clinical trial for testing a new vaccine for type B hepatitis

import numpy as np
from scipy.stats import chi2, norm

# Data
total_volunteers = 784
vaccinated = 430
not_vaccinated = total_volunteers - vaccinated  # 354
vaccinated_infected = 8
not_vaccinated_infected = 31

# Create a contingency table
observed = np.array([
    [vaccinated_infected, not_vaccinated_infected],
    [vaccinated - vaccinated_infected, not_vaccinated - not_vaccinated_infected]
])

print("Observed Contingency Table:")
print("                  Vaccinated  Not Vaccinated")
print(f"Infected         {observed[0, 0]:11d} {observed[0, 1]:15d}")
print(f"Not Infected     {observed[1, 0]:11d} {observed[1, 1]:15d}")

# Calculate expected values
row_sums = observed.sum(axis=1)
col_sums = observed.sum(axis=0)
n = observed.sum()

expected = np.zeros((2, 2))
for i in range(2):
    for j in range(2):
        expected[i, j] = row_sums[i] * col_sums[j] / n

print("\nExpected Contingency Table:")
print("                  Vaccinated  Not Vaccinated")
print(f"Infected         {expected[0, 0]:11.4f} {expected[0, 1]:15.4f}")
print(f"Not Infected     {expected[1, 0]:11.4f} {expected[1, 1]:15.4f}")

# (a) Find the 4 values in the expected table
print(f"Problem #3(a): Expected values = {expected[0, 0]:.4f}, {expected[0, 1]:.4f}, {expected[1, 0]:.4f}, {expected[1, 1]:.4f}")

# (b) Find the value of the test statistic (using the contingency table method)
chi_square = np.sum((observed - expected)**2 / expected)
print(f"Problem #3(b): Test statistic = {chi_square:.3f}")

# (c) Find the critical value (using the contingency table method)
alpha = 0.10  # 10% significance level
df = (observed.shape[0] - 1) * (observed.shape[1] - 1)  # 1 degree of freedom
critical_value = chi2.ppf(1 - alpha, df)
print(f"Problem #3(c): Critical value = {critical_value:.3f}")

# (d) Using the z-test for proportions
p1 = vaccinated_infected / vaccinated  # proportion of vaccinated who got infected
p2 = not_vaccinated_infected / not_vaccinated  # proportion of non-vaccinated who got infected
p_pooled = (vaccinated_infected + not_vaccinated_infected) / total_volunteers

# Calculate z-statistic
z_stat = (p1 - p2) / np.sqrt(p_pooled * (1 - p_pooled) * (1/vaccinated + 1/not_vaccinated))
print(f"Problem #3(d): Z-test statistic = {z_stat:.2f}")

# Verify that z^2 = chi^2
print(f"Z^2 = {z_stat**2:.3f} and Chi^2 = {chi_square:.3f}")

# (e) Find the p-value
# For a two-sided test, we need to multiply by 2 to get the p-value
p_value = 2 * norm.sf(abs(z_stat))
# Alternatively, we can calculate it from the chi-square distribution
p_value_chi = 1 - chi2.cdf(chi_square, df)
print(f"Problem #3(e): P-value = {p_value:.4f}")
print(f"P-value (from chi-square) = {p_value_chi:.4f}")

# Conclusion
if chi_square > critical_value:
    print("Reject the null hypothesis. The probability of getting hepatitis is different for vaccinated and non-vaccinated people.")
else:
    print("Fail to reject the null hypothesis. We don't have enough evidence to say the probability of getting hepatitis is different.")
