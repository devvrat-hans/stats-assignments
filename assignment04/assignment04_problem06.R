# Problem #6: An observational study of Alzheimer's disease (AD) obtained data from 9 AD patients exhibiting moderate
# dementia and selected a group of 7 control individuals without AD. AD is a progressive neurodegenerative
# disease of the elderly and advancing age is known to be a primary risk factor in AD diagnosis. Therefore, it was
# crucial for the study's credibility to examine whether the ages in the AD group might be significantly different
# than in the control group. The ages of the subjects in years are summarized in the R Output below.
#
# Variable    n    Mean    sd    Minimum    Q1 Median    Q3 Maximum
# Alzheimers  9    72.51   9.57  77.00      79.25 87.00  92.25 93.00
# Control     7    66.60   12.86 54.00      56.00 65.00  82.00 89.00
#
# We want to test if the average age in the Alzheimer's group is significantly different than the control group.
# Assume that the population variances are equal.
#
# (a) What is the null hypothesis?
# (b) Find the value of the test statistic.
# (c) Find the 5% critical value.
# (d) What is the conclusion of the hypothesis test?

# Problem 6: Two-sample t-test with equal variances

cat("Problem #6: Two-sample t-test (Alzheimer's study)\n")
cat("================================================\n")

# Given information from the R Output
alzheimers_n <- 9
alzheimers_mean <- 72.51
alzheimers_sd <- 9.57

control_n <- 7
control_mean <- 66.60
control_sd <- 12.86

# Significance level
alpha <- 0.05

# (a) Null hypothesis
cat("\n(a) Null hypothesis:\n")
cat("   H₀: μ₁ = μ₂ (The mean age in the Alzheimer's group equals the mean age in the control group)\n")
cat("   H₁: μ₁ ≠ μ₂ (The mean age in the Alzheimer's group differs from the mean age in the control group)\n")

# (b) Calculate the test statistic
# Pooled standard deviation (assuming equal variances)
pooled_var <- ((alzheimers_n - 1) * alzheimers_sd^2 + (control_n - 1) * control_sd^2) / 
              (alzheimers_n + control_n - 2)
pooled_sd <- sqrt(pooled_var)

# t-statistic
t_stat <- (alzheimers_mean - control_mean) / (pooled_sd * sqrt(1/alzheimers_n + 1/control_n))
cat("\n(b) Test statistic:", round(t_stat, 3), "\n")

# (c) Critical value (two-tailed test)
df <- alzheimers_n + control_n - 2  # degrees of freedom
t_critical <- qt(1 - alpha/2, df)
cat("\n(c) Critical value (two-tailed, alpha =", alpha, "): ±", round(t_critical, 3), "\n")

# (d) Conclusion
p_value <- 2 * (1 - pt(abs(t_stat), df))  # two-tailed p-value

cat("\n(d) Conclusion:\n")
cat("   t-statistic:", round(t_stat, 3), "\n")
cat("   Critical value: ±", round(t_critical, 3), "\n")
cat("   p-value:", round(p_value, 4), "\n")

if (abs(t_stat) > t_critical) {
  cat("   Since |", round(t_stat, 3), "| > ", round(t_critical, 3), ", we reject the null hypothesis.\n", sep="")
  cat("   There is a significant difference in mean age between the Alzheimer's group and the control group.\n")
} else {
  cat("   Since |", round(t_stat, 3), "| ≤ ", round(t_critical, 3), ", we fail to reject the null hypothesis.\n", sep="")
  cat("   There is not enough evidence of a difference in mean age between the two groups.\n")
}

if (p_value < alpha) {
  cat("\n   Since p-value (", round(p_value, 4), ") < α (", alpha, "), we reject the null hypothesis.\n", sep="")
} else {
  cat("\n   Since p-value (", round(p_value, 4), ") ≥ α (", alpha, "), we fail to reject the null hypothesis.\n", sep="")
}

# Using R's built-in t.test function (for verification)
cat("\nUsing R's t.test function for verification:\n")
cat("# This code would be used if we had the actual data:\n")
cat("# t.test(alzheimers_data, control_data, var.equal = TRUE)\n")
cat("\n# Since we only have summary statistics, we can use the following approach:\n")
cat("# t.test formula: t = (x1 - x2) / (sp * sqrt(1/n1 + 1/n2))\n")
cat("# where sp^2 = ((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2)\n")
