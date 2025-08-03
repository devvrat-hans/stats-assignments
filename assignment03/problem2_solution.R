# Problem 2 solution in R

# Given data
data <- c(8, 8, 12, 9, 10, 7, 8, 11, 8)

# (a) Calculate numerical values for x-axis
sorted_data <- sort(data)
n <- length(data)

# Calculate plotting positions for normal probability plot
positions <- (1:n - 0.5) / n

# Calculate the corresponding z-scores
z_scores <- qnorm(positions)

# First five values for x-axis (sorted data)
first_five_x <- sorted_data[1:5]

# (b) Calculate numerical values for y-axis (the z-scores)
first_five_y <- z_scores[1:5]

cat("Problem 2 (a) - First five values for x-axis:", first_five_x, "\n")
cat("Problem 2 (b) - First five values for y-axis (rounded to 2 decimals):", 
    round(first_five_y, 2), "\n")

# (c) Perform normal probability plot and Anderson-Darling test
library(nortest)

# Perform Anderson-Darling test
ad_test <- ad.test(data)

cat("\nProblem 2 (c) - Anderson-Darling test:\n")
print(ad_test)

# Create normal probability plot
png("normal_probability_plot_r.png", width=800, height=600)
qqnorm(data, main="Normal Q-Q Plot")
qqline(data)
dev.off()

# Print conclusion about normality
cat("\nProblem 2 (d) - Is it reasonable to assume the data comes from a normal distribution?\n")
if (ad_test$p.value > 0.10) {
  cat("Based on the Anderson-Darling test (p-value =", round(ad_test$p.value, 3), "), it is reasonable to assume normality.\n")
} else if (ad_test$p.value > 0.05) {
  cat("The results are inconclusive (p-value =", round(ad_test$p.value, 3), ").\n")
} else {
  cat("Based on the Anderson-Darling test (p-value =", round(ad_test$p.value, 3), "), the data may not follow a normal distribution.\n")
}
