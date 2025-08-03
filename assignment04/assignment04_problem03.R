# Problem #3: The health of the bear population in Yellowstone National Park is monitored by periodic measurements taken
# from anesthetized bears. A sample of 42 bears has a mean weight of 188.4 lb.
#
# At α = .04, can it be concluded that the average weight of a bear in Yellowstone National Park is different from
# 187 lb? Note that the standard deviation of the weight of a bear is known to be 8.4 lb.
#
# (a) Find the value of the test statistic for the above hypothesis.
# (b) Find the critical value.
# (c) Find the p-value.
# (d) What is the correct way to draw a conclusion regarding the above hypothesis test?

# Problem 3: Hypothesis test for population mean with known standard deviation

# Given information
sample_mean <- 188.4  # sample mean weight
population_mean <- 187  # hypothesized population mean weight
population_sd <- 8.4  # known population standard deviation
n <- 42  # sample size
alpha <- 0.04  # significance level

# (a) Calculate the test statistic (z-score)
z_score <- (sample_mean - population_mean) / (population_sd / sqrt(n))
cat("(a) Test statistic (z-score):", round(z_score, 2), "\n")

# (b) Calculate the critical value for a two-tailed test
critical_value <- qnorm(1 - alpha/2)
cat("(b) Critical value (two-tailed, alpha =", alpha, "): ±", round(critical_value, 2), "\n")

# (c) Calculate the p-value for a two-tailed test
p_value <- 2 * (1 - pnorm(abs(z_score)))
cat("(c) p-value:", round(p_value, 4), "\n")

# (d) Drawing a conclusion
cat("\n(d) Conclusion:\n")
cat("Comparing test statistic to critical value:\n")
if (abs(z_score) > critical_value) {
  cat("Since |", round(z_score, 2), "| > ", round(critical_value, 2), 
      ", we reject the null hypothesis.\n", sep="")
} else {
  cat("Since |", round(z_score, 2), "| ≤ ", round(critical_value, 2), 
      ", we fail to reject the null hypothesis.\n", sep="")
}

cat("\nComparing p-value to alpha:\n")
if (p_value < alpha) {
  cat("Since p-value (", round(p_value, 4), ") < α (", alpha, "), we reject the null hypothesis.\n", sep="")
  cat("We conclude that the average weight of a bear is different from 187 lb.\n")
} else {
  cat("Since p-value (", round(p_value, 4), ") ≥ α (", alpha, "), we fail to reject the null hypothesis.\n", sep="")
  cat("We cannot conclude that the average weight of a bear is different from 187 lb.\n")
}
