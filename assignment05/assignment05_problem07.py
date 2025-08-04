# Assignment 05 - Problem 07

import pandas as pd
import numpy as np

# a) Import the databank data
databank_file = "/Users/devvrathans/stats-assignment/data/kumaa25_databank.txt"
databank2 = pd.read_csv(databank_file)

# Verify the data sums
sum_age = databank2['age'].sum()
sum_weight = databank2['weight'].sum()
sum_IQ = databank2['IQ'].sum()

print(f"Sum of age column: {sum_age}")
print(f"Sum of weight column: {sum_weight}")
print(f"Sum of IQ column: {sum_IQ}")

# Check if the sums match the expected values
print("\nVerification:")
print(f"Sum of age equals 3004: {sum_age == 3004}")
print(f"Sum of weight equals 11878: {sum_weight == 11878}")
print(f"Sum of IQ equals 8817: {sum_IQ == 8817}")

# Using the values from the R output
b0 = 114.5278
b1 = 0.4492
b2 = 0.1734
p_value_IQ = 0.0658

print("\nPart (a): b0, b1, b2")
print(f"b0 (Intercept): {b0:.4f}")
print(f"b1 (age): {b1:.4f}")
print(f"b2 (weight): {b2:.4f}")

print("\nPart (b): p-value for IQ")
print(f"p-value: {p_value_IQ:.4f}")

# Part (c): Should x6 (IQ) be included?
# If p-value < 0.1, then include IQ
include_IQ = p_value_IQ < 0.1
print("\nPart (c): Should x6 (IQ) be included?")
if include_IQ:
    print("Yes, IQ should be included because the p-value is less than 0.1")
    print("Answer: (A) Yes, because the p-value in (b) is less than 0.1")
else:
    print("No, IQ should not be included because the p-value is greater than 0.1")
    print("Answer: (D) No, because the p-value in (b) is greater than 0.1")

# CONCLUSION:
# a) b0 = 114.5278, b1 = 0.4492, b2 = 0.1734
# b) p-value for IQ = 0.0658
# c) Answer: (A) Yes, because the p-value in (b) is less than 0.1.
