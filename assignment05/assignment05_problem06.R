# Assignment 05 - Problem 06

# Given data from Problem #5:
x <- c(2, 5, 9, 8, 14)
y <- c(9, 7, 6, 4, 0)

# Regression coefficients from Problem #5
b0 <- 10.6847
b1 <- -0.7217

# a) Calculate the residuals
predicted_y <- b0 + b1 * x
residuals <- y - predicted_y

# Format residuals to 4 decimals
residuals_formatted <- format(residuals, digits=6)
cat("a) Residuals: ", paste(residuals_formatted, collapse=", "), "\n")

# b) Calculate the residual sum of squares SS(error)
ss_error <- sum(residuals^2)
cat("b) SS(error): ", format(ss_error, digits=6), "\n")

# c) Find the test statistic for H0: ρ = 0 vs H1: ρ ≠ 0
# Correlation coefficient
r <- cor(x, y)
# Test statistic: t = r * sqrt(n-2) / sqrt(1-r^2)
n <- length(x)
t_stat <- r * sqrt(n-2) / sqrt(1-r^2)
cat("c) Test statistic: ", format(t_stat, digits=4), "\n")

# d) Find the 1% critical value for the hypothesis test
# Two-tailed test with alpha = 0.01 and df = n-2
alpha <- 0.01
df <- n - 2
critical_value <- qt(1-alpha/2, df)
cat("d) Critical value: ", format(critical_value, digits=5), "\n")

# e) Find the p-value for the hypothesis test
# Two-tailed test
p_value <- 2 * pt(-abs(t_stat), df)
cat("e) p-value: ", format(p_value, digits=6), "\n")

# Create a linear model to verify our calculations
model <- lm(y ~ x)
summary(model)

# CONCLUSIONS:
# a) Residuals: -0.2413, -0.0762, 1.8106, -0.9111, -0.5809
# b) SS(error): 4.5099
# c) Test statistic: -5.30
# d) Critical value: 5.841
# e) p-value: 0.0131
