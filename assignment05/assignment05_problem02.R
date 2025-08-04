# Assignment 05 - Problem 02
# Statistics Problem Solution in R
# Pairwise Hypothesis Tests

cat("Assignment 05 - Problem 02 Solution\n")
cat("=====================================\n")

# Load required libraries
if (!require(stats)) install.packages("stats")
library(stats)

cat("\n", rep("=", 60), "\n")
cat("ASSIGNMENT 05 - PROBLEM 02 - ACTUAL SOLUTION\n")
cat(rep("=", 60), "\n")

cat("Hypothesis tests for pairwise comparisons:\n")
cat("1. H₀: μ₁ = μ₂, H₁: μ₁ ≠ μ₂\n")
cat("2. H₀: μ₁ = μ₃, H₁: μ₁ ≠ μ₃\n")
cat("3. H₀: μ₂ = μ₃, H₁: μ₂ ≠ μ₃\n")

# Data from Problem 01 (using same data for consistency)
group1 <- c(4.2, 6.1, 3.4)
group2 <- c(4.5, 2.7, 2.3, 2.3)
group3 <- c(1.2, -0.3, 0.4)

cat("\nData from assignment05_problem01:\n")
cat("Group 1:", group1, sprintf("(n1 = %d)\n", length(group1)))
cat("Group 2:", group2, sprintf("(n2 = %d)\n", length(group2)))
cat("Group 3:", group3, sprintf("(n3 = %d)\n", length(group3)))

# Calculate basic statistics
mean1 <- mean(group1)
mean2 <- mean(group2)
mean3 <- mean(group3)
sd1 <- sd(group1)
sd2 <- sd(group2)
sd3 <- sd(group3)
n1 <- length(group1)
n2 <- length(group2)
n3 <- length(group3)

# From Problem 01 ANOVA results
all_data <- c(group1, group2, group3)
grand_mean <- mean(all_data)
total_n <- length(all_data)

# Prepare data for ANOVA
group_labels <- c(rep("Group1", length(group1)), 
                  rep("Group2", length(group2)), 
                  rep("Group3", length(group3)))
df <- data.frame(values = all_data, groups = as.factor(group_labels))

# Perform ANOVA to get MSE
anova_result <- aov(values ~ groups, data = df)
anova_summary <- summary(anova_result)

# Extract MSE and df
mse <- anova_summary[[1]]$`Mean Sq`[2]  # Mean Square Error (within groups)
df_within <- anova_summary[[1]]$Df[2]   # Degrees of freedom (within)

cat("\nFrom Problem 01 ANOVA:\n")
cat("Group 1 mean (μ₁) =", round(mean1, 4), "\n")
cat("Group 2 mean (μ₂) =", round(mean2, 4), "\n")
cat("Group 3 mean (μ₃) =", round(mean3, 4), "\n")
cat("MSE (Mean Square Error) =", round(mse, 4), "\n")
cat("Degrees of freedom (within) =", df_within, "\n")

alpha <- 0.05

cat("\nPart (a) - Test Statistics for Pairwise Comparisons\n")
cat(rep("-", 55), "\n")

# Function to calculate t-statistic for two groups with pooled variance
t_statistic_pooled <- function(mean_i, mean_j, n_i, n_j, mse) {
  t_stat <- (mean_i - mean_j) / sqrt(mse * (1/n_i + 1/n_j))
  return(t_stat)
}

# 1. H₀: μ₁ = μ₂, H₁: μ₁ ≠ μ₂
t_stat_12 <- t_statistic_pooled(mean1, mean2, n1, n2, mse)
df_12 <- df_within  # Using df from ANOVA
p_value_12 <- 2 * pt(abs(t_stat_12), df_12, lower.tail = FALSE)

# 2. H₀: μ₁ = μ₃, H₁: μ₁ ≠ μ₃
t_stat_13 <- t_statistic_pooled(mean1, mean3, n1, n3, mse)
df_13 <- df_within  # Using df from ANOVA
p_value_13 <- 2 * pt(abs(t_stat_13), df_13, lower.tail = FALSE)

# 3. H₀: μ₂ = μ₃, H₁: μ₂ ≠ μ₃
t_stat_23 <- t_statistic_pooled(mean2, mean3, n2, n3, mse)
df_23 <- df_within  # Using df from ANOVA
p_value_23 <- 2 * pt(abs(t_stat_23), df_23, lower.tail = FALSE)

cat("\nTest Statistics for Pairwise Comparisons:\n")
cat("1. μ₁ vs μ₂: t =", round(t_stat_12, 4), ", df =", df_12, ", p =", round(p_value_12, 4), "\n")
cat("2. μ₁ vs μ₃: t =", round(t_stat_13, 4), ", df =", df_13, ", p =", round(p_value_13, 4), "\n")
cat("3. μ₂ vs μ₃: t =", round(t_stat_23, 4), ", df =", df_23, ", p =", round(p_value_23, 4), "\n")

# Critical t-value for alpha = 0.05
t_critical <- qt(1 - alpha/2, df_within)
cat("\nCritical t-value (α =", alpha, ", df =", df_within, "):", round(t_critical, 4), "\n")

# Decisions for each hypothesis test
cat("\nDecisions (without Bonferroni correction):\n")
cat("1. H₀: μ₁ = μ₂:", ifelse(p_value_12 < alpha, "Reject H₀", "Fail to reject H₀"), "\n")
cat("2. H₀: μ₁ = μ₃:", ifelse(p_value_13 < alpha, "Reject H₀", "Fail to reject H₀"), "\n")
cat("3. H₀: μ₂ = μ₃:", ifelse(p_value_23 < alpha, "Reject H₀", "Fail to reject H₀"), "\n")

# With Bonferroni correction
alpha_bonferroni <- alpha / 3
cat("\nDecisions (with Bonferroni correction, α =", round(alpha_bonferroni, 4), "):\n")
cat("1. H₀: μ₁ = μ₂:", ifelse(p_value_12 < alpha_bonferroni, "Reject H₀", "Fail to reject H₀"), "\n")
cat("2. H₀: μ₁ = μ₃:", ifelse(p_value_13 < alpha_bonferroni, "Reject H₀", "Fail to reject H₀"), "\n")
cat("3. H₀: μ₂ = μ₃:", ifelse(p_value_23 < alpha_bonferroni, "Reject H₀", "Fail to reject H₀"), "\n")

cat("\nPart (b) - Significant Differences at 3% Significance Level\n")
cat(rep("-", 60), "\n")

# Using Bonferroni method with 3% significance level
alpha_b <- 0.03
alpha_bonferroni_b <- alpha_b / 3  # Bonferroni-adjusted alpha
cat("Using Bonferroni method with", alpha_b, "significance level\n")
cat("Adjusted significance level: α =", round(alpha_bonferroni_b, 4), "\n")

# Check which pairs are significantly different
sig_diff_12 <- p_value_12 < alpha_bonferroni_b
sig_diff_13 <- p_value_13 < alpha_bonferroni_b
sig_diff_23 <- p_value_23 < alpha_bonferroni_b

cat("\nWhich pairs of means are significantly different:\n")
cat("1 and 2:", ifelse(sig_diff_12, "Significant difference", "Not significant"), "(p =", round(p_value_12, 4), ")\n")
cat("1 and 3:", ifelse(sig_diff_13, "Significant difference", "Not significant"), "(p =", round(p_value_13, 4), ")\n")
cat("2 and 3:", ifelse(sig_diff_23, "Significant difference", "Not significant"), "(p =", round(p_value_23, 4), ")\n")

# Multiple choice answer determination
answer <- ""
if (sig_diff_12 && sig_diff_23 && !sig_diff_13) {
  answer <- "(A) 1 and 2, 2 and 3 only"
} else if (sig_diff_12 && sig_diff_13 && !sig_diff_23) {
  answer <- "(B) 1 and 2, 1 and 3 only"
} else if (sig_diff_12 && sig_diff_13 && sig_diff_23) {
  answer <- "(C) all of them"
} else if (sig_diff_13 && sig_diff_23 && !sig_diff_12) {
  answer <- "(F) 1 and 3, 2 and 3 only"
} else if (sig_diff_13 && !sig_diff_12 && !sig_diff_23) {
  answer <- "(G) 1 and 3 only"
} else if (sig_diff_12 && !sig_diff_13 && !sig_diff_23) {
  answer <- "(H) 1 and 2 only"
} else {
  answer <- "None of the options match the results"
}

cat("\nMultiple choice answer:", answer, "\n")

cat("\n", rep("=", 60), "\n")
cat("FINAL ANSWERS FOR SUBMISSION:\n")
cat(rep("=", 60), "\n")

cat("Part (a): Test Statistics for Pairwise Comparisons\n")
cat("1. H₀: μ₁ = μ₂: t =", round(t_stat_12, 4), "\n")
cat("2. H₀: μ₁ = μ₃: t =", round(t_stat_13, 4), "\n")
cat("3. H₀: μ₂ = μ₃: t =", round(t_stat_23, 4), "\n")

cat("\nPart (b):", answer, "\n")

cat(rep("=", 60), "\n")
cat("Note: These results are based on the ANOVA from Problem 01\n")
