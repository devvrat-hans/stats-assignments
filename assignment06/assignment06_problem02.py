# Assignment 06 - Problem 02

import numpy as np
import pandas as pd

# Given data - handedness of biological offspring by parental handedness
observed = np.array([
    [290, 37],  # Right × Right: Right-Handed, Left-Handed
    [29, 8],    # Right × Left: Right-Handed, Left-Handed
    [30, 6]     # Left × Right: Right-Handed, Left-Handed
])

# Create a DataFrame for better display
observed_df = pd.DataFrame(
    observed,
    index=["Right × Right", "Right × Left", "Left × Right"],
    columns=["Right-Handed", "Left-Handed"]
)

# Print the observed table
print("Observed frequencies:")
print(observed_df)

# (a) Find the values in cells (2,2) and (3,2) of the expected table
# Calculate row and column totals
row_totals = observed.sum(axis=1)
col_totals = observed.sum(axis=0)
total = observed.sum()

# Calculate expected frequencies
expected = np.zeros((3, 2))
for i in range(3):
    for j in range(2):
        expected[i, j] = row_totals[i] * col_totals[j] / total

# Create a DataFrame for better display
expected_df = pd.DataFrame(
    expected,
    index=["Right × Right", "Right × Left", "Left × Right"],
    columns=["Right-Handed", "Left-Handed"]
)

# Print the expected table
print("\nExpected frequencies:")
print(expected_df)

print("\n(a) Values in cells (2,2) and (3,2) of the expected table:")
print(f"Cell (2,2) (Right × Left, Left-Handed): {expected[1, 1]:.3f}")
print(f"Cell (3,2) (Left × Right, Left-Handed): {expected[2, 1]:.3f}")

# (b) Can a chi-square analysis be performed on the above table?
# Check if at least 80% of the expected frequencies are >= 5
# and all expected frequencies are >= 1
min_expected = expected.min()
num_cells_ge_5 = np.sum(expected >= 5)
percentage_ge_5 = num_cells_ge_5 / expected.size * 100

print("\n(b) Can a chi-square analysis be performed on the above table?")
print(f"Minimum expected frequency: {min_expected:.3f}")
print(f"Number of cells with expected frequency >= 5: {num_cells_ge_5} ({percentage_ge_5:.1f}%)")

if min_expected >= 1 and percentage_ge_5 >= 80:
    print("Yes, chi-square analysis can be performed.")
else:
    print("No, chi-square analysis cannot be performed because not all conditions are met.")

# (c) Combine the last two rows to create a new 2×2 table
new_observed = np.zeros((2, 2))
new_observed[0, :] = observed[0, :]  # First row remains the same
new_observed[1, :] = observed[1:, :].sum(axis=0)  # Combine the last two rows

# Create a DataFrame for better display
new_observed_df = pd.DataFrame(
    new_observed,
    index=["Right × Right", "At least one parent Left"],
    columns=["Right-Handed", "Left-Handed"]
)

# Print the new observed table
print("\n(c) New 2×2 observed table (combining the last two rows):")
print(new_observed_df)

# Calculate expected frequencies for the new table
new_row_totals = new_observed.sum(axis=1)
new_col_totals = new_observed.sum(axis=0)
new_total = new_observed.sum()

new_expected = np.zeros((2, 2))
for i in range(2):
    for j in range(2):
        new_expected[i, j] = new_row_totals[i] * new_col_totals[j] / new_total

# Create a DataFrame for better display
new_expected_df = pd.DataFrame(
    new_expected,
    index=["Right × Right", "At least one parent Left"],
    columns=["Right-Handed", "Left-Handed"]
)

# Print the new expected table
print("\nNew 2×2 expected table:")
print(new_expected_df)

# Calculate the chi-square test statistic for the new table
chi_square = np.sum((new_observed - new_expected)**2 / new_expected)
print(f"\nChi-square test statistic: {chi_square:.2f}")

# Calculate the critical value for the test
from scipy.stats import chi2
alpha = 0.05  # 5% significance level
df = (new_observed.shape[0] - 1) * (new_observed.shape[1] - 1)  # 1 degree of freedom
critical_value = chi2.ppf(1 - alpha, df)
print(f"Critical value (5% significance level): {critical_value:.3f}")

# CONCLUSIONS:
# a) Values in cells (2,2) and (3,2) of the expected table:
#    Cell (2,2) (Right × Left, Left-Handed): 4.718
#    Cell (3,2) (Left × Right, Left-Handed): 4.590
# b) No, chi-square analysis cannot be performed because not all conditions are met.
# c) New 2×2 observed table (combining the last two rows):
#    Right × Right: 290, 37
#    At least one parent Left: 59, 14
# d) Test statistic: 3.32, Critical value: 3.841
# e) Do not reject the hypothesis of independence since the answer in (c) is less than or equal to the answer in (d)
