'''
Problem #2: A genetic model suggests that 80% of plants grown from a cross between two given strains of seeds will be of the
dwarf variety. After breeding a sample of these plants, 155 were observed to be of the dwarf variety.

Suppose that we do a hypothesis test to see if the sample results strongly contradict the genetic model and find
the p-value to be 0.0196. What is the meaning of this p-value?
'''

import numpy as np
from scipy import stats

# Problem 2: Understanding the p-value

# Given information
p0 = 0.80  # genetic model suggests 80% dwarf variety
observed = 155  # observed number of dwarf variety plants
p_value = 0.0196  # given p-value

# To find the sample size, we need to calculate it based on the p-value
# Let's try a range of possible sample sizes

print("Problem 2: Understanding the p-value")
print("====================================")

print("\nGiven information:")
print(f"- Genetic model suggests {p0*100}% of plants will be dwarf variety")
print(f"- Observed number of dwarf plants: {observed}")
print(f"- Reported p-value: {p_value}")

# Let's assume this is a two-tailed test and verify the sample size
print("\nVerifying the sample size and p-value:")

for n in range(180, 220):
    expected = n * p0  # expected number of dwarf plants
    # Calculate test statistic for a proportion test
    z_score = (observed - expected) / np.sqrt(n * p0 * (1-p0))
    # Calculate p-value (two-tailed)
    calculated_p = 2 * min(stats.norm.cdf(z_score), 1 - stats.norm.cdf(z_score))
    
    if abs(calculated_p - p_value) < 0.001:  # close match to given p-value
        print(f"Sample size n = {n}")
        print(f"Expected dwarf plants: {expected}")
        print(f"Observed dwarf plants: {observed}")
        print(f"z-score: {z_score:.4f}")
        print(f"Calculated p-value: {calculated_p:.4f}")
        break

# Interpretation of the p-value
print("\nInterpretation of the p-value:")
print("The p-value of 0.0196 is the probability of observing a result at least as extreme as")
print("155 dwarf plants (or fewer) if the genetic model (80% dwarf plants) is true.")
print("Since this probability is small (less than 0.05), it suggests that the observed result")
print("is unlikely under the null hypothesis, providing evidence against the genetic model.")
print("\nThe most accurate interpretation is:")
print("If the genetic model is correct (80% dwarf variety), it is very unlikely (probability 0.0196)")
print("to observe 155 or fewer dwarf plants in this sample size.")
