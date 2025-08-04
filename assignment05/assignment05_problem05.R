# Assignment 05 - Problem 05

# Given data:
x <- c(2, 5, 9, 8, 14)
y <- c(9, 7, 6, 4, 0)

# a) Determine the least squares regression line.
# Formula: y = b0 + b1*x
# Calculate the coefficients

# Calculate means
mean_x <- mean(x)
mean_y <- mean(y)

# Calculate b1 (slope)
numerator <- sum((x - mean_x) * (y - mean_y))
denominator <- sum((x - mean_x)^2)
b1 <- numerator / denominator

# Calculate b0 (intercept)
b0 <- mean_y - b1 * mean_x

# Print the least squares regression line
cat("Least squares regression line: y =", b0, "+", b1, "* x\n")

# b) Draw the least squares regression line on a scatterplot and find 
# which points are above the regression line.

# Calculate predicted y values
predicted_y <- b0 + b1 * x

# Find which points are above the regression line
above_line <- y > predicted_y
points_above <- which(above_line)

# Print which points are above the regression line
cat("Points above the regression line:\n")
for (i in points_above) {
  cat("Point", i, ": (", x[i], ",", y[i], ")\n")
}

# Calculate the sum of y-values for points above the regression line
sum_y_above <- sum(y[above_line])
cat("Sum of y-values for points above the regression line:", sum_y_above, "\n")

# Plot
plot(x, y, main="Scatter Plot with Regression Line", 
     xlab="x", ylab="y", pch=19)
abline(b0, b1, col="red")

# Highlight points above the regression line
points(x[above_line], y[above_line], col="green", pch=19)

# Add a legend
legend("topright", legend=c("Data Points", "Points Above Line", "Regression Line"), 
       col=c("black", "green", "red"), pch=c(19, 19, NA), lty=c(NA, NA, 1))

# CONCLUSION:
# a) Least squares regression line: y = 10.6847 + -0.7217 * x
# b) Sum of y-values for points above the regression line: 6
