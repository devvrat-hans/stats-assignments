# Assignment 05 - Problem 01
# Statistics Problem Solution in R
# Three-Group Comparison Analysis

cat("Assignment 05 - Problem 01 Solution\n")
cat("=====================================\n")

# Load required libraries
if (!require(stats)) install.packages("stats")
library(stats)

cat("\n", rep("=", 60), "\n")
cat("ASSIGNMENT 05 - PROBLEM 01 - ACTUAL SOLUTION\n")
cat(rep("=", 60), "\n")

# Real data from the problem image - ACTUAL VALUES
# Group 1: 3 values, Group 2: 4 values, Group 3: 3 values
group1 <- c(4.2, 6.1, 3.4)
group2 <- c(4.5, 2.7, 2.3, 2.3)
group3 <- c(1.2, -0.3, 0.4)

cat("Data from assignment05_problem01_data.png:\n")
cat("Group 1:", group1, sprintf("(n1 = %d)\n", length(group1)))
cat("Group 2:", group2, sprintf("(n2 = %d)\n", length(group2)))
cat("Group 3:", group3, sprintf("(n3 = %d)\n", length(group3)))

# Calculate descriptive statistics for each group
mean1 <- mean(group1)
mean2 <- mean(group2)
mean3 <- mean(group3)
sd1 <- sd(group1)
sd2 <- sd(group2)
sd3 <- sd(group3)
n1 <- length(group1)
n2 <- length(group2)
n3 <- length(group3)

cat("\nDescriptive Statistics:\n")
cat("Group 1: Mean =", round(mean1, 4), ", SD =", round(sd1, 4), ", n =", n1, "\n")
cat("Group 2: Mean =", round(mean2, 4), ", SD =", round(sd2, 4), ", n =", n2, "\n")
cat("Group 3: Mean =", round(mean3, 4), ", SD =", round(sd3, 4), ", n =", n3, "\n")

# Problem parameters
alpha <- 0.05

cat("\nPart (a) - One-Way ANOVA\n")
cat(rep("-", 30), "\n")
cat("H₀: μ₁ = μ₂ = μ₃ (all group means are equal)\n")
cat("H₁: At least one group mean is different\n")
cat("Significance level: α =", alpha, "\n")

# Prepare data for ANOVA
all_data <- c(group1, group2, group3)
group_labels <- c(rep("Group1", length(group1)), 
                  rep("Group2", length(group2)), 
                  rep("Group3", length(group3)))

# Create data frame
df <- data.frame(values = all_data, groups = as.factor(group_labels))

# Perform One-Way ANOVA
anova_result <- aov(values ~ groups, data = df)
anova_summary <- summary(anova_result)

# Extract ANOVA statistics
f_statistic <- anova_summary[[1]]$`F value`[1]
p_value_anova <- anova_summary[[1]]$`Pr(>F)`[1]
df_between <- anova_summary[[1]]$Df[1]
df_within <- anova_summary[[1]]$Df[2]

cat("\nANOVA Results:\n")
cat("F-statistic:", round(f_statistic, 4), "\n")
cat("P-value:", round(p_value_anova, 4), "\n")
cat("Degrees of freedom:", df_between, ",", df_within, "\n")

# Critical F-value
f_critical <- qf(1 - alpha, df_between, df_within)
cat("Critical F-value:", round(f_critical, 4), "\n")

# Decision for Part (a)
if (p_value_anova < alpha) {
  decision_a <- "Reject H₀"
  conclusion_a <- "There is sufficient evidence that at least one group mean is different"
} else {
  decision_a <- "Fail to reject H₀"
  conclusion_a <- "There is insufficient evidence that the group means are different"
}

cat("\nDecision:", decision_a, "\n")
cat("Conclusion:", conclusion_a, "\n")

# Display full ANOVA table
cat("\nFull ANOVA Table:\n")
print(anova_summary)

cat("\nPart (b) - ANOVA Components: SS(treatment) and SS(error)\n")
cat(rep("-", 50), "\n")

# Extract Sum of Squares components
ss_treatment <- anova_summary[[1]]$`Sum Sq`[1]
ss_error <- anova_summary[[1]]$`Sum Sq`[2]
ss_total <- ss_treatment + ss_error

cat("Sum of Squares Calculations:\n")
cat("SS(treatment) = SS(between groups) =", round(ss_treatment, 3), "\n")
cat("SS(error) = SS(within groups) =", round(ss_error, 3), "\n")
cat("SS(total) =", round(ss_total, 3), "\n")

cat("\nANOVA Table:\n")
cat("Source         df    SS        MS        F       p-value\n")
cat("Treatment      ", sprintf("%2d", df_between), "   ", sprintf("%7.3f", ss_treatment), "  ", 
    sprintf("%7.3f", ss_treatment/df_between), "  ", sprintf("%7.4f", f_statistic), "  ", 
    sprintf("%.4f", p_value_anova), "\n")
cat("Error          ", sprintf("%2d", df_within), "   ", sprintf("%7.3f", ss_error), "  ", 
    sprintf("%7.3f", ss_error/df_within), "\n")
cat("Total          ", sprintf("%2d", df_between + df_within), "   ", sprintf("%7.3f", ss_total), "\n")

cat("\nPart (b) - Pairwise Comparisons (if ANOVA is significant)\n")
cat(rep("-", 50), "\n")

# Perform pairwise t-tests
t_test_12 <- t.test(group1, group2, var.equal = TRUE)
t_test_13 <- t.test(group1, group3, var.equal = TRUE)
t_test_23 <- t.test(group2, group3, var.equal = TRUE)

cat("Pairwise t-test results:\n")
cat("Group 1 vs Group 2: t =", round(t_test_12$statistic, 4), ", p =", round(t_test_12$p.value, 4), "\n")
cat("Group 1 vs Group 3: t =", round(t_test_13$statistic, 4), ", p =", round(t_test_13$p.value, 4), "\n")
cat("Group 2 vs Group 3: t =", round(t_test_23$statistic, 4), ", p =", round(t_test_23$p.value, 4), "\n")

# Bonferroni correction
alpha_bonferroni <- alpha / 3
cat("\nWith Bonferroni correction (α =", round(alpha_bonferroni, 4), "):\n")
cat("Group 1 vs Group 2:", ifelse(t_test_12$p.value < alpha_bonferroni, "Significant", "Not significant"), "\n")
cat("Group 1 vs Group 3:", ifelse(t_test_13$p.value < alpha_bonferroni, "Significant", "Not significant"), "\n")
cat("Group 2 vs Group 3:", ifelse(t_test_23$p.value < alpha_bonferroni, "Significant", "Not significant"), "\n")

cat("\nPart (c) - Effect Size and Confidence Intervals\n")
cat(rep("-", 45), "\n")

# Calculate eta-squared (effect size for ANOVA)
ss_between <- anova_summary[[1]]$`Sum Sq`[1]
ss_within <- anova_summary[[1]]$`Sum Sq`[2]
eta_squared <- ss_between / (ss_between + ss_within)

cat("Eta-squared (η²):", round(eta_squared, 4), "\n")

if (eta_squared < 0.01) {
  effect_interpretation <- "Small effect size"
} else if (eta_squared < 0.06) {
  effect_interpretation <- "Medium effect size"
} else if (eta_squared < 0.14) {
  effect_interpretation <- "Large effect size"
} else {
  effect_interpretation <- "Very large effect size"
}

cat("Effect size interpretation:", effect_interpretation, "\n")

# Confidence intervals for each group mean
confidence_level <- 0.95
groups <- list(group1, group2, group3)
means <- c(mean1, mean2, mean3)
sds <- c(sd1, sd2, sd3)
ns <- c(n1, n2, n3)

for (i in 1:3) {
  df_group <- ns[i] - 1
  t_crit <- qt(1 - alpha/2, df_group)
  margin_error <- t_crit * (sds[i] / sqrt(ns[i]))
  ci_lower <- means[i] - margin_error
  ci_upper <- means[i] + margin_error
  cat("Group", i, ": 95% CI = (", round(ci_lower, 4), ", ", round(ci_upper, 4), ")\n")
}

cat("\n", rep("=", 60), "\n")
cat("FINAL ANSWERS FOR SUBMISSION:\n")
cat(rep("=", 60), "\n")

cat("Part (a):", decision_a, "\n")
cat("         F-statistic =", round(f_statistic, 4), "\n")
cat("         P-value =", round(p_value_anova, 4), "\n")
if (p_value_anova < alpha) {
  cat("         Since p-value (", round(p_value_anova, 4), ") < α (", alpha, "), we", decision_a, "\n")
} else {
  cat("         Since p-value (", round(p_value_anova, 4), ") > α (", alpha, "), we", decision_a, "\n")
}

cat("\nPart (b): SS(treatment) =", round(ss_treatment, 3), "\n")
cat("         SS(error) =", round(ss_error, 3), "\n")
cat("         Pairwise comparisons:\n")
cat("         Group 1 vs 2: p =", round(t_test_12$p.value, 4), "\n")
cat("         Group 1 vs 3: p =", round(t_test_13$p.value, 4), "\n")
cat("         Group 2 vs 3: p =", round(t_test_23$p.value, 4), "\n")

cat("\nPart (c): Effect size η² =", round(eta_squared, 4), " (", effect_interpretation, ")\n")

cat("\nMULTIPLE CHOICE ANSWERS:\n")
cat("Part (a): (G) Reject H0 since 11.0335 > 4.7374\n")
cat("         This matches our F-statistic and critical value exactly!\n")
cat("Part (c): Option corresponding to the calculated effect size\n")

cat(rep("=", 60), "\n")
