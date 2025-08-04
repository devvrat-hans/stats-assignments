# Assignment 05 - Problem 05

import numpy as np
import matplotlib.pyplot as plt
from scipy import stats

# Given data:
x = np.array([2, 5, 9, 8, 14])
y = np.array([9, 7, 6, 4, 0])

# a) Determine the least squares regression line.
# Formula: y = b0 + b1*x
# Use linear regression to calculate coefficients
slope, intercept, r_value, p_value, std_err = stats.linregress(x, y)

b1 = slope       # slope
b0 = intercept   # intercept

print(f"Least squares regression line: y = {b0:.4f} + {b1:.4f} * x")

# b) Draw the least squares regression line on a scatterplot and find 
# which points are above the regression line.

# Calculate predicted y values
predicted_y = b0 + b1 * x

# Find which points are above the regression line
above_line = y > predicted_y
points_above = np.where(above_line)[0]

# Print which points are above the regression line
print("\nPoints above the regression line:")
for i in points_above:
    print(f"Point {i+1}: ({x[i]}, {y[i]})")

# Calculate the sum of y-values for points above the regression line
sum_y_above = np.sum(y[above_line])
print(f"\nSum of y-values for points above the regression line: {sum_y_above}")

# Plot
plt.figure(figsize=(10, 6))
plt.scatter(x, y, color='black', label='Data Points')
plt.scatter(x[above_line], y[above_line], color='green', label='Points Above Line')

# Plot the regression line
regression_line = b0 + b1 * np.array([min(x), max(x)])
plt.plot([min(x), max(x)], regression_line, color='red', label='Regression Line')

plt.title('Scatter Plot with Regression Line')
plt.xlabel('x')
plt.ylabel('y')
plt.legend()
plt.grid(True)
plt.savefig('/Users/devvrathans/stats-assignment/assignment05/assignment05_problem05.png')
plt.close()

# CONCLUSION:
# a) Least squares regression line: y = 10.6847 + -0.7217 * x
# b) Sum of y-values for points above the regression line: 6
