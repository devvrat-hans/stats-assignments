# Problem #1:
# (a) Suppose that we identify 167 women 50 to 54 years of age who have both a mother and a sister with a history 
# of breast cancer. 18 of these women themselves have developed breast cancer at some time in their lives. If 
# we assume that the proportion of breast cancer cases in women whose mothers have had breast cancer is 8%, 
# does having a sister with the disease increase the risk? Find the p-value.
#
# (b) At the 10% significance level, what is the conclusion of the above hypothesis test?

# Problem 1(a): Calculate the p-value

# Given information
n <- 167  # sample size
x <- 18   # number of women with breast cancer
p0 <- 0.08  # assumed proportion (null hypothesis)

# Calculate the sample proportion
p_hat <- x / n
cat("Sample proportion:", round(p_hat, 4), "\n")

# Perform hypothesis test for proportion
# H0: p = 0.08 (no increased risk)
# H1: p > 0.08 (increased risk due to sister)

# Calculate the test statistic (z-score)
z_score <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)
cat("z-score:", round(z_score, 4), "\n")

# Calculate the p-value for a one-tailed (right-tailed) test
p_value <- 1 - pnorm(z_score)
cat("p-value:", round(p_value, 4), "\n")

# Alternative method using prop.test
# Note: prop.test uses a slightly different method, so results may vary slightly
prop_test_result <- prop.test(x, n, p = p0, alternative = "greater", correct = FALSE)
cat("p-value using prop.test:", round(prop_test_result$p.value, 4), "\n")

# Problem 1(b): Conclusion at 10% significance level
alpha <- 0.10
cat("\nAt 10% significance level:\n")
if (p_value < alpha) {
  cat("Since p-value (", round(p_value, 4), ") < α (", alpha, "), we reject the null hypothesis.\n", sep="")
  cat("We conclude that having a sister with the disease increases the risk.\n")
} else {
  cat("Since p-value (", round(p_value, 4), ") ≥ α (", alpha, "), we fail to reject the null hypothesis.\n", sep="")
  cat("We cannot conclude that having a sister with the disease increases the risk.\n")
}
