# Assignment 06 - Problem 01

# Given data
observed <- c(black = 342, brown = 83, pale = 37)
total <- sum(observed)  # Total number of seeds

# According to theory (dominant epistasis)
expected_proportions <- c(black = 12/16, brown = 3/16, pale = 1/16)
expected <- expected_proportions * total

# Print the data
cat("Observed counts:\n")
print(observed)
cat("\nExpected counts based on theory:\n")
print(expected)

# (a) Calculate the chi-square test statistic
chi_square <- sum((observed - expected)^2 / expected)
cat("\n(a) Chi-square test statistic:", chi_square, "\n")

# (b) Find the critical value
# Degrees of freedom = number of categories - 1 = 3 - 1 = 2
df <- length(observed) - 1
alpha <- 0.01  # 1% significance level
critical_value <- qchisq(1 - alpha, df)
cat("(b) Critical value at 1% significance level:", critical_value, "\n")

# (c) Make a conclusion
if (chi_square > critical_value) {
  cat("\nConclusion: Reject H0. The observed frequencies contradict the theory.\n")
  cat("Answer: (A) We conclude that the data is consistent with the theory since the answer in (a) is greater than the answer in (b).\n")
} else {
  cat("\nConclusion: Do not reject H0. The data is consistent with the theory.\n")
  cat("Answer: (G) We cannot conclude that the observed frequencies contradict the theory since the answer in (a) is less than or equal to the answer in (b).\n")
}

# Calculate p-value
p_value <- 1 - pchisq(chi_square, df)
cat("\nP-value:", p_value, "\n")

# CONCLUSIONS:
# a) Chi-square test statistic: 2.496
# b) Critical value at 1% significance level: 9.210
# c) Answer: (G) We cannot conclude that the observed frequencies contradict the theory since the answer in (a) is less than or equal to the answer in (b).
