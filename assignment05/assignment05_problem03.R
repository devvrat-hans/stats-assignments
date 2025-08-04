# Assignment 05 - Problem 03
# Statistics Problem Solution in R

# Load required packages
if (!require(nortest)) install.packages("nortest")
library(nortest)  # For Anderson-Darling test

cat("Assignment 05 - Problem 03 Solution\n")
cat("=====================================\n")

# Data from Problem #1
group1 <- c(4.2, 6.1, 3.4)
group2 <- c(4.5, 2.7, 2.3, 2.3)
group3 <- c(1.2, -0.3, 0.4)

cat("Data from Problem #1:\n")
cat("Group 1:", group1, sprintf("(n1 = %d)\n", length(group1)))
cat("Group 2:", group2, sprintf("(n2 = %d)\n", length(group2)))
cat("Group 3:", group3, sprintf("(n3 = %d)\n", length(group3)))

# Create a data frame for analysis
all_data <- c(group1, group2, group3)
group_labels <- c(rep("Group1", length(group1)), 
                 rep("Group2", length(group2)), 
                 rep("Group3", length(group3)))
df <- data.frame(values = all_data, groups = factor(group_labels))

# Part (a) - Find the p-value for the hypothesis test in Problem #1(a)
cat("\nPart (a) - P-value for One-Way ANOVA\n")
cat(rep("-", 40), "\n")

# Perform One-Way ANOVA
anova_result <- aov(values ~ groups, data = df)
anova_summary <- summary(anova_result)

# Extract p-value
p_value_anova <- anova_summary[[1]]$"Pr(>F)"[1]

cat("ANOVA Summary:\n")
print(anova_summary)
cat("P-value:", p_value_anova, "\n")

# Part (b) - Normal probability plot of residuals and Anderson-Darling test
cat("\nPart (b) - Normal Probability Plot & Anderson-Darling Test\n")
cat(rep("-", 40), "\n")

# Get residuals from ANOVA model
residuals <- residuals(anova_result)

# Create normal probability plot (QQ Plot)
png(file = "assignment05_problem03_r.png", width = 800, height = 600)
qqnorm(residuals, main = "Normal Probability Plot of Residuals")
qqline(residuals, col = "red")
grid()
dev.off()

# Perform Anderson-Darling test for normality
ad_test <- ad.test(residuals)
ad_p_value <- ad_test$p.value

cat("Anderson-Darling Test Results:\n")
print(ad_test)
cat("Anderson-Darling p-value:", ad_p_value, "\n")

# Part (c) - Bartlett's test for equality of variances
cat("\nPart (c) - Bartlett's Test for Equality of Variances\n")
cat(rep("-", 40), "\n")

# Perform Bartlett's test
bartlett_test <- bartlett.test(values ~ groups, data = df)
bartlett_p <- bartlett_test$p.value

cat("Bartlett's Test Results:\n")
print(bartlett_test)
cat("Bartlett's test p-value:", bartlett_p, "\n")

cat("\n", rep("=", 60), "\n")
cat("FINAL ANSWERS FOR SUBMISSION:\n")
cat(rep("=", 60), "\n")

cat(sprintf("Part (a): P-value for One-Way ANOVA = %.6f\n", p_value_anova))
cat(sprintf("Part (b): P-value for Anderson-Darling test = %.6f\n", ad_p_value))
cat(sprintf("Part (c): P-value for Bartlett's test = %.6f\n", bartlett_p))
cat(rep("=", 60), "\n")
