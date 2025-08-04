# Assignment 5 - Problem 1
# Statistical Analysis Solution in R

# Load required libraries
library(ggplot2)
library(dplyr)
library(stats)
library(nortest)

# Load the dataset
tryCatch({
  # Try to load the databank dataset
  data <- read.csv('/Users/devvrathans/stats-assignment/data/databank.txt')
  cat("Dataset loaded successfully\n")
  cat("Dataset dimensions:", dim(data), "\n")
  cat("\nFirst few rows:\n")
  print(head(data))
  cat("\nDataset structure:\n")
  str(data)
  cat("\nDescriptive statistics:\n")
  print(summary(data))
}, error = function(e) {
  cat("Dataset file not found. Creating sample data for demonstration.\n")
  # Create sample data if file not found
  set.seed(42)
  n <- 100
  data <<- data.frame(
    age = rnorm(n, 40, 15),
    weight = rnorm(n, 150, 25),
    height = rnorm(n, 68, 4),
    systolic = rnorm(n, 120, 20)
  )
})

# Problem 1: Statistical analysis
# This is a template that can be adapted based on the specific problem requirements

# Example: Hypothesis testing
# H0: μ = 120 (null hypothesis)
# H1: μ ≠ 120 (alternative hypothesis)

if ("systolic" %in% names(data)) {
  sample_data <- data$systolic[!is.na(data$systolic)]
  
  # Calculate sample statistics
  sample_mean <- mean(sample_data)
  sample_sd <- sd(sample_data)
  sample_size <- length(sample_data)
  
  cat("\nSample Statistics:\n")
  cat("Sample mean:", round(sample_mean, 4), "\n")
  cat("Sample standard deviation:", round(sample_sd, 4), "\n")
  cat("Sample size:", sample_size, "\n")
  
  # Perform one-sample t-test
  hypothesized_mean <- 120
  t_test_result <- t.test(sample_data, mu = hypothesized_mean)
  
  cat("\nOne-sample t-test results:\n")
  cat("t-statistic:", round(t_test_result$statistic, 4), "\n")
  cat("p-value:", round(t_test_result$p.value, 4), "\n")
  
  # Extract confidence interval
  confidence_level <- 0.95
  ci_lower <- t_test_result$conf.int[1]
  ci_upper <- t_test_result$conf.int[2]
  
  cat("\n", confidence_level*100, "% Confidence Interval:\n", sep="")
  cat("[", round(ci_lower, 4), ", ", round(ci_upper, 4), "]\n", sep="")
  
  # Create visualizations
  png('/Users/devvrathans/stats-assignment/assignment05/assignment05_problem01_r_output.png', 
      width = 1200, height = 800, res = 150)
  
  par(mfrow = c(2, 2))
  
  # Histogram with normal curve overlay
  hist(sample_data, breaks = 20, prob = TRUE, 
       main = "Distribution of Systolic Blood Pressure",
       xlab = "Systolic Blood Pressure", 
       ylab = "Density",
       col = "skyblue", border = "black")
  
  x <- seq(min(sample_data), max(sample_data), length = 100)
  lines(x, dnorm(x, sample_mean, sample_sd), col = "red", lwd = 2)
  abline(v = sample_mean, col = "red", lty = 2, lwd = 2)
  abline(v = hypothesized_mean, col = "green", lty = 2, lwd = 2)
  legend("topright", 
         legend = c(paste("Sample mean:", round(sample_mean, 2)),
                   paste("Hypothesized mean:", hypothesized_mean),
                   "Normal fit"),
         col = c("red", "green", "red"),
         lty = c(2, 2, 1),
         lwd = 2)
  grid()
  
  # Q-Q plot for normality check
  qqnorm(sample_data, main = "Q-Q Plot for Normality Check")
  qqline(sample_data, col = "red", lwd = 2)
  grid()
  
  # Box plot
  boxplot(sample_data, 
          main = "Box Plot",
          ylab = "Systolic Blood Pressure",
          col = "lightblue")
  grid()
  
  # Confidence interval visualization
  margin_error <- abs(ci_upper - sample_mean)
  plot(1, sample_mean, 
       xlim = c(0.5, 1.5), 
       ylim = c(min(sample_data), max(sample_data)),
       pch = 19, cex = 2,
       main = paste(confidence_level*100, "% Confidence Interval", sep=""),
       xlab = "", ylab = "Systolic Blood Pressure",
       xaxt = "n")
  
  arrows(1, ci_lower, 1, ci_upper, 
         length = 0.1, angle = 90, code = 3, lwd = 2)
  abline(h = hypothesized_mean, col = "red", lty = 2, lwd = 2)
  legend("topright", 
         legend = paste("Hypothesized mean:", hypothesized_mean),
         col = "red", lty = 2, lwd = 2)
  grid()
  
  dev.off()
  
  # Normality tests
  cat("\nNormality Tests:\n")
  
  # Shapiro-Wilk test (if sample size <= 5000)
  if (sample_size <= 5000) {
    shapiro_test <- shapiro.test(sample_data)
    cat("Shapiro-Wilk test p-value:", round(shapiro_test$p.value, 4), "\n")
  }
  
  # Anderson-Darling test
  ad_test <- ad.test(sample_data)
  cat("Anderson-Darling test p-value:", round(ad_test$p.value, 4), "\n")
  
  # Conclusion
  cat("\nConclusion:\n")
  if (t_test_result$p.value < 0.05) {
    cat("At α = 0.05, we reject the null hypothesis (p-value =", 
        round(t_test_result$p.value, 4), "< 0.05)\n")
    cat("There is sufficient evidence to conclude that the population mean is significantly different from", 
        hypothesized_mean, "\n")
  } else {
    cat("At α = 0.05, we fail to reject the null hypothesis (p-value =", 
        round(t_test_result$p.value, 4), "≥ 0.05)\n")
    cat("There is insufficient evidence to conclude that the population mean is significantly different from", 
        hypothesized_mean, "\n")
  }
  
} else {
  cat("Systolic blood pressure data not available in the dataset.\n")
  cat("Please verify the problem requirements and dataset.\n")
}

cat("\n", rep("=", 50), "\n", sep="")
cat("Problem 1 Analysis Complete\n")
cat(rep("=", 50), "\n", sep="")
