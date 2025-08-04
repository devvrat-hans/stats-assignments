# Assignment 6 - Problem 3
# A clinical trial for testing a new vaccine for type B hepatitis

# Data
total_volunteers <- 784
vaccinated <- 430
not_vaccinated <- total_volunteers - vaccinated  # 354
vaccinated_infected <- 8
not_vaccinated_infected <- 31

# Create a contingency table
observed <- matrix(c(vaccinated_infected, not_vaccinated_infected,
                     vaccinated - vaccinated_infected, not_vaccinated - not_vaccinated_infected),
                   nrow = 2, byrow = TRUE)

# Display the observed contingency table
rownames(observed) <- c("Infected", "Not Infected")
colnames(observed) <- c("Vaccinated", "Not Vaccinated")
print("Observed Contingency Table:")
print(observed)

# Calculate expected values
expected <- matrix(0, nrow = 2, ncol = 2)
row_sums <- rowSums(observed)
col_sums <- colSums(observed)
n <- sum(observed)

for (i in 1:2) {
  for (j in 1:2) {
    expected[i, j] <- row_sums[i] * col_sums[j] / n
  }
}

# Display the expected contingency table
print("Expected Contingency Table:")
print(expected)

# (a) Find the 4 values in the expected table
cat("Problem #3(a): Expected values =", sprintf("%.4f", expected[1, 1]), ",", 
    sprintf("%.4f", expected[1, 2]), ",", sprintf("%.4f", expected[2, 1]), ",", 
    sprintf("%.4f", expected[2, 2]), "\n")

# (b) Find the value of the test statistic (using the contingency table method)
chi_square <- sum((observed - expected)^2 / expected)
cat("Problem #3(b): Test statistic =", sprintf("%.3f", chi_square), "\n")

# (c) Find the critical value (using the contingency table method)
alpha <- 0.10  # 10% significance level
df <- (nrow(observed) - 1) * (ncol(observed) - 1)  # 1 degree of freedom
critical_value <- qchisq(1 - alpha, df)
cat("Problem #3(c): Critical value =", sprintf("%.3f", critical_value), "\n")

# (d) Using the z-test for proportions
p1 <- vaccinated_infected / vaccinated  # proportion of vaccinated who got infected
p2 <- not_vaccinated_infected / not_vaccinated  # proportion of non-vaccinated who got infected
p_pooled <- (vaccinated_infected + not_vaccinated_infected) / total_volunteers

# Calculate z-statistic
z_stat <- (p1 - p2) / sqrt(p_pooled * (1 - p_pooled) * (1/vaccinated + 1/not_vaccinated))
cat("Problem #3(d): Z-test statistic =", sprintf("%.2f", z_stat), "\n")

# Verify that z^2 = chi^2
cat("Z^2 =", z_stat^2, "and Chi^2 =", chi_square, "\n")

# (e) Find the p-value
# For a two-sided test, we need to multiply by 2 to get the p-value
p_value <- 2 * pnorm(abs(z_stat), lower.tail = FALSE)
# Alternatively, we can calculate it from the chi-square distribution
p_value_chi <- 1 - pchisq(chi_square, df)
cat("Problem #3(e): P-value =", sprintf("%.4f", p_value), "\n")
cat("P-value (from chi-square) =", sprintf("%.4f", p_value_chi), "\n")

# Conclusion
if (chi_square > critical_value) {
  cat("Reject the null hypothesis. The probability of getting hepatitis is different for vaccinated and non-vaccinated people.\n")
} else {
  cat("Fail to reject the null hypothesis. We don't have enough evidence to say the probability of getting hepatitis is different.\n")
}
