#!/usr/bin/env python3
# Assignment 3 - Questions 4 and 5 Solutions (Python)

import pandas as pd
import numpy as np
from scipy import stats

# Read the data
data = pd.read_csv('/Users/devvrathans/stats-assignment-01/data/kumaa25_class.txt', sep=',')

# Display basic information about the dataset
print("Dataset information:")
print(f"Number of rows: {len(data)}")
print(f"Columns: {data.columns.tolist()}")
print("\n")

#################################
# Question 4
# Find a 91% confidence interval for the proportion of people who live at least 5km from campus
#################################

print("Problem #4 Solution (Python)")
print("=" * 50)

# Filter out rows with missing distance values
distance_data = data['distance'].dropna()

# Count how many people live at least 5 km from campus
at_least_5km = (distance_data >= 5).sum()
total_people = len(distance_data)
proportion = at_least_5km / total_people

# Calculate 91% confidence interval for proportion
confidence_level = 0.91
z_score = stats.norm.ppf((1 + confidence_level) / 2)
margin_error = z_score * np.sqrt((proportion * (1 - proportion)) / total_people)

ci_lower = proportion - margin_error
ci_upper = proportion + margin_error

print(f"Total people with distance data: {total_people}")
print(f"People living at least 5 km from campus: {at_least_5km}")
print(f"Proportion: {proportion:.4f}")
print(f"91% Confidence Interval: ({ci_lower:.4f}, {ci_upper:.4f})")
print("\n")

#################################
# Question 5
# (a) Find a 97% confidence interval for the average height (in inches)
# (b) Determine which statement is true regarding part (a)
#################################

print("Problem #5 Solution (Python)")
print("=" * 50)

# Filter out rows with missing height values
height_data = data['height'].dropna()

# Calculate mean and standard deviation
height_mean = height_data.mean()
height_std = height_data.std(ddof=1)  # Using sample standard deviation
n = len(height_data)

# Calculate 97% confidence interval using t-distribution
confidence_level = 0.97
t_score = stats.t.ppf((1 + confidence_level) / 2, df=n-1)
margin_error = t_score * (height_std / np.sqrt(n))

ci_lower = height_mean - margin_error
ci_upper = height_mean + margin_error

print(f"Sample size: {n}")
print(f"Mean height: {height_mean:.4f} inches")
print(f"Standard deviation: {height_std:.4f} inches")
print(f"97% Confidence Interval: ({ci_lower:.4f}, {ci_upper:.4f})")

# Analyze the statements for question 5b
print("\nAnalysis of statements for 5(b):")
print("(A) The population must be normal.")
print("   - Not necessarily true. When sample size is large, we can use CLT.")
print("(B) The population standard deviation σ must be known.")
print("   - False. We estimated it with the sample standard deviation.")
print("(C) The population must follow a t-distribution.")
print("   - False. The t-distribution is used for the sampling distribution,")
print("     not for the population distribution.")
print("(D) The population cannot follow a t-distribution.")
print("   - Not necessarily true. The confidence interval calculation")
print("     doesn't require this constraint.")
print("(E) The population does not need to be normal because the sample size is greater than 30.")
print("   - True. By the Central Limit Theorem, with n > 30, we can assume")
print("     the sampling distribution of the mean is approximately normal.")
print("(F) The population mean must be inside the confidence interval.")
print("   - False. The confidence interval is a random interval, and we cannot")
print("     be certain that it contains the true population mean.")

print("\nThe correct answer is (E).")
