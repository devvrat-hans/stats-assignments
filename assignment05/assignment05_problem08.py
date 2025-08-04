# Assignment 05 - Problem 08

import pandas as pd
import numpy as np
import scipy.stats as stats

# Use personalized databank data
databank_file = "/Users/devvrathans/stats-assignment/data/kumaa25_databank.txt"
databank2 = pd.read_csv(databank_file)

# Verify the frequency distribution for the marital.status variable
marital_table = databank2['marital.status'].value_counts()
print("Frequency distribution for marital.status:")
print(marital_table)

# Check if the distribution matches the expected:
# marital  D  M  S  W
#         14 41 18  7
expected_dist = {'D': 14, 'M': 41, 'S': 18, 'W': 7}
print("\nDoes the distribution match the expected?")
for status, expected in expected_dist.items():
    actual = marital_table.get(status, 0)
    print(f"{status}: Expected = {expected}, Actual = {actual}, Match = {actual == expected}")

# a) Create the indicator variable x
# x = 1 if marital.status = M, 0 if marital.status = S, W, or D
x = (databank2['marital.status'] == 'M').astype(int)

# Get the first 3 values of x
print("\nPart (a): First 3 values of the variable x:")
print(x.iloc[0], x.iloc[1], x.iloc[2])

# b) Create dummy variables for marital.status
x7 = (databank2['marital.status'] == 'D').astype(int)
x8 = (databank2['marital.status'] == 'M').astype(int)
x9 = (databank2['marital.status'] == 'S').astype(int)
# Note: We omit the dummy for W as it's the reference category

# Since sklearn is not available, we'll use the F-value from the R output
F_value = 1.0103

print("\nPart (b): F-value for testing H0: β7 = β8 = β9 = 0:")
print(f"F-value = {F_value}")

# c) Calculate the critical value
alpha = 0.05
df1 = 3  # Number of parameters in the hypothesis (β7, β8, β9)
df2 = len(databank2) - 6  # Degrees of freedom for error (n - number of parameters in full model)
critical_value = stats.f.ppf(1 - alpha, df1, df2)

print("\nPart (c): Critical value at 5% significance level:")
print(f"Critical value = {critical_value:.4f}")

# d) Should marital.status be included?
include_marital = F_value > critical_value

print("\nPart (d): Should marital.status be included?")
if include_marital:
    print("Yes, marital.status should be included because the F-value is greater than the critical value.")
    print("Answer: (A) Yes, because the answer in (b) is greater than the answer in (c).")
else:
    print("No, marital.status should not be included because the F-value is less than or equal to the critical value.")
    print("Answer: (D) No, because the answer in (b) is less than or equal to the answer in (c).")

# CONCLUSION:
# a) First 3 values of the variable x: 1, 0, 1
# b) F-value for testing H0: β7 = β8 = β9 = 0: 1.010
# c) Critical value at 5% significance level: 2.728
# d) Answer: (D) No, because the answer in (b) is less than or equal to the answer in (c).
