# Problem #4: In a 1868 paper, German physician Carl Wunderlich reported based on over a million body temperature readings
# that the mean body temperature for healthy adults is 98.6° F. However, it is now commonly believed that the
# mean body temperature of a healthy adult is less than what was reported in that paper. To test this hypothesis a
# researcher measures the following body temperatures from a random sample of healthy adults.
#
# 98.4, 98.7, 98.8, 98.4, 97.6, 98.2, 98.3
#
# (a) Find the value of the test statistic.
# (b) Find the 5% critical value.
# (c) Find the p-value for the relevant hypothesis test.
# (d) Since the sample size is less than 30, the above analysis requires that the population follows a normal
# distribution. What method could be used to check this assumption?

# Problem 4: One-sample t-test and checking normality

# Given information
temperatures <- c(98.4, 98.7, 98.8, 98.4, 97.6, 98.2, 98.3)
claimed_mean <- 98.6  # Wunderlich's claimed mean temperature
alpha <- 0.05  # significance level

# Sample statistics
sample_mean <- mean(temperatures)
sample_sd <- sd(temperatures)
n <- length(temperatures)

cat("Problem 4: One-sample t-test and checking normality\n")
cat("==================================================\n")

cat("\nSample data:", temperatures, "\n")
cat("Sample size:", n, "\n")
cat("Sample mean:", round(sample_mean, 4), "\n")
cat("Sample standard deviation:", round(sample_sd, 4), "\n")
cat("Hypothesized population mean:", claimed_mean, "\n")

# (a) Calculate the test statistic (t-score)
t_score <- (sample_mean - claimed_mean) / (sample_sd / sqrt(n))
cat("\n(a) Test statistic (t-score):", round(t_score, 4), "\n")

# (b) Calculate the critical value for a one-tailed (left-tailed) test
# degrees of freedom = n - 1
df <- n - 1
t_critical <- qt(alpha, df)  # left-tailed
cat("(b) Critical value (left-tailed, alpha =", alpha, "):", round(t_critical, 3), "\n")

# (c) Calculate the p-value for a one-tailed (left-tailed) test
p_value <- pt(t_score, df)
cat("(c) p-value:", round(p_value, 4), "\n")

# Alternative using t.test function
t_test_result <- t.test(temperatures, mu = claimed_mean, alternative = "less")
cat("    Using t.test function:", round(t_test_result$p.value, 4), "\n")

# Drawing a conclusion
cat("\nConclusion:\n")
if (p_value < alpha) {
  cat("Since p-value (", round(p_value, 4), ") < α (", alpha, "), we reject the null hypothesis.\n", sep="")
  cat("We conclude that the mean body temperature is less than 98.6°F.\n")
} else {
  cat("Since p-value (", round(p_value, 4), ") ≥ α (", alpha, "), we fail to reject the null hypothesis.\n", sep="")
  cat("We cannot conclude that the mean body temperature is less than 98.6°F.\n")
}

# (d) Checking the normality assumption
cat("\n(d) Methods to check normality assumption for a small sample:\n")
cat("1. Normal Probability Plot (Q-Q Plot) - visual inspection\n")
cat("2. Shapiro-Wilk test - statistical test for normality\n")
cat("3. Anderson-Darling test - statistical test for normality\n")
cat("4. Kolmogorov-Smirnov test - statistical test for normality\n")

# Perform Shapiro-Wilk test (appropriate for small samples)
shapiro_test <- shapiro.test(temperatures)
cat("\nShapiro-Wilk test result:\n")
cat("W =", round(shapiro_test$statistic, 4), ", p-value =", round(shapiro_test$p.value, 4), "\n")
if (shapiro_test$p.value < 0.05) {
  cat("Since p-value < 0.05, we reject the null hypothesis of normality.\n")
  cat("The data does not appear to follow a normal distribution.\n")
} else {
  cat("Since p-value ≥ 0.05, we fail to reject the null hypothesis of normality.\n")
  cat("The data appears to follow a normal distribution.\n")
}

# QQ Plot code
cat("\nCode to create a QQ plot in R:\n")
cat('# Create QQ plot\n')
cat('qqnorm(temperatures, main="Normal Q-Q Plot for Body Temperatures")\n')
cat('qqline(temperatures, col="red")\n')
cat('\nIn a Q-Q plot, if points fall close to a straight line, the data follows an approximately normal distribution.\n')
