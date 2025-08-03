import numpy as np
import matplotlib.pyplot as plt
from scipy import stats

# Given data
data = [8, 8, 12, 9, 10, 7, 8, 11, 8]

# (a) Calculate numerical values for x-axis
# Sort the data
sorted_data = np.sort(data)
n = len(data)

# Calculate plotting positions for normal probability plot
# Using (i - 0.5) / n formula
positions = [(i + 1 - 0.5) / n for i in range(n)]

# Calculate the corresponding z-scores (quantiles of the standard normal distribution)
z_scores = stats.norm.ppf(positions)

# First five values for x-axis (sorted data)
first_five_x = sorted_data[:5]

# (b) Calculate numerical values for y-axis (the z-scores)
first_five_y = z_scores[:5]

print("Problem 2 (a) - First five values for x-axis:", first_five_x)
print("Problem 2 (b) - First five values for y-axis (rounded to 2 decimals):", 
      np.round(first_five_y, 2))

# (c) Perform Anderson-Darling test
result = stats.anderson(data, dist='norm')
print("Problem 2 (c) - Anderson-Darling test:")
print(f"A^2 = {result.statistic}")
print(f"Critical values: {result.critical_values}")
print(f"Significance levels: {result.significance_level}")

# p-value calculation (approximate)
# For Anderson-Darling test, we can use approximation formulas
# or compare with critical values
if result.statistic < result.critical_values[2]:  # 5% significance level
    print("p-value > 0.05")
else:
    print("p-value < 0.05")

# (d) Plot the normal probability plot for visualization
plt.figure(figsize=(10, 6))
stats.probplot(data, plot=plt)
plt.title('Normal Probability Plot')
plt.grid(True)
plt.savefig('normal_probability_plot.png')

# Print the answer options for part (d)
print("\nProblem 2 (d) - Is it reasonable to assume the data comes from a normal distribution?")
print("(A) Yes, because the p-value is greater than 0.10.")
print("(B) No, because the p-value is greater than 0.10.")
print("(C) Yes, because the sample size is less than 30.")
print("(D) Yes, because the points do not fall close to a straight line.")
print("(E) Yes, because the p-value is less than .05.")
print("(F) No, because the points fall close to a straight line.")
print("(G) No, because the p-value is less than .05.")
print("(H) The results are inconclusive, since 0.05 ≤ p-value ≤ 0.10.")
