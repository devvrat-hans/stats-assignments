# Assignment 06 - Problem 02

# Given data - handedness of biological offspring by parental handedness
observed <- matrix(c(
  290, 37,  # Right × Right: Right-Handed, Left-Handed
  29, 8,    # Right × Left: Right-Handed, Left-Handed
  30, 6     # Left × Right: Right-Handed, Left-Handed
), nrow = 3, byrow = TRUE)

# Add column and row names for clarity
colnames(observed) <- c("Right-Handed", "Left-Handed")
rownames(observed) <- c("Right × Right", "Right × Left", "Left × Right")

# Print the observed table
cat("Observed frequencies:\n")
print(observed)

# (a) Find the values in cells (2,2) and (3,2) of the expected table
# Calculate row and column totals
row_totals <- rowSums(observed)
col_totals <- colSums(observed)
total <- sum(observed)

# Calculate expected frequencies
expected <- matrix(0, nrow = 3, ncol = 2)
for (i in 1:3) {
  for (j in 1:2) {
    expected[i, j] <- row_totals[i] * col_totals[j] / total
  }
}

# Add column and row names for clarity
colnames(expected) <- c("Right-Handed", "Left-Handed")
rownames(expected) <- c("Right × Right", "Right × Left", "Left × Right")

# Print the expected table
cat("\nExpected frequencies:\n")
print(expected)

cat("\n(a) Values in cells (2,2) and (3,2) of the expected table:\n")
cat("Cell (2,2) (Right × Left, Left-Handed):", expected[2, 2], "\n")
cat("Cell (3,2) (Left × Right, Left-Handed):", expected[3, 2], "\n")

# (b) Can a chi-square analysis be performed on the above table?
# Check if at least 80% of the expected frequencies are >= 5
# and all expected frequencies are >= 1
min_expected <- min(expected)
num_cells_ge_5 <- sum(expected >= 5)
percentage_ge_5 <- num_cells_ge_5 / length(expected) * 100

cat("\n(b) Can a chi-square analysis be performed on the above table?\n")
cat("Minimum expected frequency:", min_expected, "\n")
cat("Number of cells with expected frequency >= 5:", num_cells_ge_5, 
    "(", percentage_ge_5, "%)\n")

if (min_expected >= 1 && percentage_ge_5 >= 80) {
  cat("Yes, chi-square analysis can be performed.\n")
} else {
  cat("No, chi-square analysis cannot be performed because not all conditions are met.\n")
}

# (c) Combine the last two rows to create a new 2×2 table
new_observed <- matrix(0, nrow = 2, ncol = 2)
new_observed[1, ] <- observed[1, ]  # First row remains the same
new_observed[2, ] <- colSums(observed[2:3, ])  # Combine the last two rows

# Add column and row names for clarity
colnames(new_observed) <- c("Right-Handed", "Left-Handed")
rownames(new_observed) <- c("Right × Right", "At least one parent Left")

# Print the new observed table
cat("\n(c) New 2×2 observed table (combining the last two rows):\n")
print(new_observed)

# Calculate expected frequencies for the new table
new_row_totals <- rowSums(new_observed)
new_col_totals <- colSums(new_observed)
new_total <- sum(new_observed)

new_expected <- matrix(0, nrow = 2, ncol = 2)
for (i in 1:2) {
  for (j in 1:2) {
    new_expected[i, j] <- new_row_totals[i] * new_col_totals[j] / new_total
  }
}

# Add column and row names for clarity
colnames(new_expected) <- c("Right-Handed", "Left-Handed")
rownames(new_expected) <- c("Right × Right", "At least one parent Left")

# Print the new expected table
cat("\nNew 2×2 expected table:\n")
print(new_expected)

# Calculate the chi-square test statistic for the new table
chi_square <- sum((new_observed - new_expected)^2 / new_expected)
cat("\nChi-square test statistic:", sprintf("%.2f", chi_square), "\n")

# Calculate the critical value for the test
alpha <- 0.05  # 5% significance level
df <- (nrow(new_observed) - 1) * (ncol(new_observed) - 1)  # 1 degree of freedom
critical_value <- qchisq(1 - alpha, df)
cat("Critical value (5% significance level):", sprintf("%.3f", critical_value), "\n")

# CONCLUSIONS:
# a) Values in cells (2,2) and (3,2) of the expected table:
#    Cell (2,2) (Right × Left, Left-Handed): 4.718
#    Cell (3,2) (Left × Right, Left-Handed): 4.590
# b) No, chi-square analysis cannot be performed because not all conditions are met.
# c) New 2×2 observed table (combining the last two rows):
# d) Test statistic: 3.32, Critical value: 3.841
# e) Do not reject the hypothesis of independence since the answer in (c) is less than or equal to the answer in (d)
#    Right × Right: 290, 37
#    At least one parent Left: 59, 14
