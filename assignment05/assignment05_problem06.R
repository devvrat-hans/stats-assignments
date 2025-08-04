# Assignment 5 - Problem 6
# Statistical Analysis Solution in R

# Load required libraries
library(ggplot2)
library(dplyr)
library(stats)
library(nortest)  # for additional normality tests
library(fitdistrplus)  # for distribution fitting

# Load the dataset
tryCatch({
  # Try to load the databank dataset
  data <- read.csv('/Users/devvrathans/stats-assignment/data/databank.txt')
  cat("Dataset loaded successfully\n")
  cat("Dataset dimensions:", dim(data), "\n")
  cat("\nFirst few rows:\n")
  print(head(data))
}, error = function(e) {
  cat("Dataset file not found. Creating sample data for demonstration.\n")
  # Create sample data if file not found
  set.seed(42)
  n <- 100
  data <<- data.frame(
    age = rnorm(n, 40, 15),
    weight = rnorm(n, 150, 25),
    systolic = rnorm(n, 120, 20),
    success = rbinom(n, 1, 0.3),  # Binary outcome
    counts = rpois(n, 5)  # Count data
  )
})

# Problem 6: Probability Distributions and Goodness of Fit
# Example: Testing if data follows a specific distribution

# Case 1: Testing if a continuous variable follows normal distribution
if ("systolic" %in% names(data)) {
  clean_data <- data$systolic[!is.na(data$systolic)]
  
  cat("\nGoodness of Fit Test: Normal Distribution\n")
  cat("Variable: Systolic Blood Pressure\n")
  cat("Sample size:", length(clean_data), "\n")
  
  # Sample statistics
  sample_mean <- mean(clean_data)
  sample_sd <- sd(clean_data)
  sample_skewness <- e1071::skewness(clean_data)
  sample_kurtosis <- e1071::kurtosis(clean_data)
  
  cat("\nSample Statistics:\n")
  cat("Mean:", round(sample_mean, 4), "\n")
  cat("Standard deviation:", round(sample_sd, 4), "\n")
  cat("Skewness:", round(sample_skewness, 4), "\n")
  cat("Kurtosis:", round(sample_kurtosis, 4), "\n")
  
  # Normality tests
  cat("\nNormality Tests:\n")
  
  # Shapiro-Wilk test
  if (length(clean_data) <= 5000) {
    shapiro_test <- shapiro.test(clean_data)
    cat("Shapiro-Wilk test: W =", round(shapiro_test$statistic, 4), 
        ", p-value =", round(shapiro_test$p.value, 4), "\n")
  }
  
  # Anderson-Darling test
  ad_test <- ad.test(clean_data)
  cat("Anderson-Darling test: AD =", round(ad_test$statistic, 4), 
      ", p-value =", round(ad_test$p.value, 4), "\n")
  
  # Kolmogorov-Smirnov test
  ks_test <- ks.test(clean_data, "pnorm", sample_mean, sample_sd)
  cat("Kolmogorov-Smirnov test: KS =", round(ks_test$statistic, 4), 
      ", p-value =", round(ks_test$p.value, 4), "\n")
  
  # Cramer-von Mises test
  cvm_test <- cvm.test(clean_data)
  cat("Cramer-von Mises test: CvM =", round(cvm_test$statistic, 4), 
      ", p-value =", round(cvm_test$p.value, 4), "\n")
  
  # Lilliefors test
  lillie_test <- lillie.test(clean_data)
  cat("Lilliefors test: D =", round(lillie_test$statistic, 4), 
      ", p-value =", round(lillie_test$p.value, 4), "\n")
  
  # Pearson chi-square goodness of fit test
  cat("\nChi-square Goodness of Fit Test:\n")
  
  # Create bins
  num_bins <- min(10, floor(sqrt(length(clean_data))))
  breaks <- seq(min(clean_data), max(clean_data), length.out = num_bins + 1)
  observed_freq <- table(cut(clean_data, breaks = breaks, include.lowest = TRUE))
  
  # Calculate expected frequencies under normal distribution
  expected_prob <- diff(pnorm(breaks, sample_mean, sample_sd))
  expected_freq <- expected_prob * length(clean_data)
  
  # Combine bins with expected frequency < 5
  while (any(expected_freq < 5) && length(expected_freq) > 2) {
    min_idx <- which.min(expected_freq)
    if (min_idx == 1) {
      # Combine with next bin
      observed_freq[2] <- observed_freq[2] + observed_freq[1]
      expected_freq[2] <- expected_freq[2] + expected_freq[1]
      observed_freq <- observed_freq[-1]
      expected_freq <- expected_freq[-1]
    } else if (min_idx == length(expected_freq)) {
      # Combine with previous bin
      observed_freq[length(observed_freq) - 1] <- observed_freq[length(observed_freq) - 1] + observed_freq[length(observed_freq)]
      expected_freq[length(expected_freq) - 1] <- expected_freq[length(expected_freq) - 1] + expected_freq[length(expected_freq)]
      observed_freq <- observed_freq[-length(observed_freq)]
      expected_freq <- expected_freq[-length(expected_freq)]
    } else {
      # Combine with adjacent bin with smaller frequency
      if (expected_freq[min_idx - 1] < expected_freq[min_idx + 1]) {
        observed_freq[min_idx - 1] <- observed_freq[min_idx - 1] + observed_freq[min_idx]
        expected_freq[min_idx - 1] <- expected_freq[min_idx - 1] + expected_freq[min_idx]
        observed_freq <- observed_freq[-min_idx]
        expected_freq <- expected_freq[-min_idx]
      } else {
        observed_freq[min_idx + 1] <- observed_freq[min_idx + 1] + observed_freq[min_idx]
        expected_freq[min_idx + 1] <- expected_freq[min_idx + 1] + expected_freq[min_idx]
        observed_freq <- observed_freq[-min_idx]
        expected_freq <- expected_freq[-min_idx]
      }
    }
  }
  
  # Perform chi-square test
  chi2_test <- chisq.test(as.numeric(observed_freq), p = expected_freq/sum(expected_freq))
  
  cat("Number of bins after combining:", length(observed_freq), "\n")
  cat("Chi-square statistic:", round(chi2_test$statistic, 4), "\n")
  cat("p-value:", round(chi2_test$p.value, 4), "\n")
  cat("Degrees of freedom:", length(observed_freq) - 1 - 2, "\n")
  
  # Create comprehensive visualization
  png('/Users/devvrathans/stats-assignment/assignment05/assignment05_problem06_r_output.png', 
      width = 1500, height = 1200, res = 150)
  
  par(mfrow = c(3, 3))
  
  # Histogram with normal overlay
  hist(clean_data, breaks = 30, prob = TRUE, 
       main = "Histogram with Normal Distribution Overlay",
       xlab = "Systolic Blood Pressure", ylab = "Density",
       col = "skyblue", border = "black")
  
  x <- seq(min(clean_data), max(clean_data), length = 100)
  lines(x, dnorm(x, sample_mean, sample_sd), col = "red", lwd = 2)
  legend("topright", legend = "Normal fit", col = "red", lwd = 2)
  grid()
  
  # Q-Q plot
  qqnorm(clean_data, main = "Q-Q Plot (Normal)")
  qqline(clean_data, col = "red", lwd = 2)
  grid()
  
  # P-P plot
  sorted_data <- sort(clean_data)
  n <- length(sorted_data)
  empirical_cdf <- (1:n) / n
  theoretical_cdf <- pnorm(sorted_data, sample_mean, sample_sd)
  
  plot(theoretical_cdf, empirical_cdf, 
       main = "P-P Plot (Normal)",
       xlab = "Theoretical CDF", ylab = "Empirical CDF",
       pch = 19, cex = 0.6)
  abline(0, 1, col = "red", lwd = 2)
  grid()
  
  # Chi-square goodness of fit visualization
  barplot(rbind(as.numeric(observed_freq), expected_freq), 
          beside = TRUE, 
          main = "Chi-square Goodness of Fit",
          xlab = "Bin", ylab = "Frequency",
          legend.text = c("Observed", "Expected"),
          col = c("lightblue", "orange"))
  grid()
  
  # Box plot
  boxplot(clean_data, 
          main = "Box Plot",
          ylab = "Systolic Blood Pressure",
          col = "lightgreen")
  grid()
  
  # Density plot with normal overlay
  plot(density(clean_data), 
       main = "Density Plot with Normal Overlay",
       xlab = "Systolic Blood Pressure", ylab = "Density",
       lwd = 2)
  lines(x, dnorm(x, sample_mean, sample_sd), col = "red", lwd = 2)
  legend("topright", 
         legend = c("Empirical", "Normal fit"), 
         col = c("black", "red"), lwd = 2)
  grid()
  
  # Empirical CDF vs theoretical CDF
  plot(sorted_data, empirical_cdf, type = "s",
       main = "Empirical vs Theoretical CDF",
       xlab = "Systolic Blood Pressure", ylab = "CDF",
       lwd = 2)
  lines(sorted_data, theoretical_cdf, col = "red", lwd = 2)
  legend("bottomright", 
         legend = c("Empirical", "Theoretical"), 
         col = c("black", "red"), lwd = 2)
  grid()
  
  # Residuals plot (empirical - theoretical CDF)
  residuals <- empirical_cdf - theoretical_cdf
  plot(sorted_data, residuals,
       main = "CDF Residuals",
       xlab = "Systolic Blood Pressure", ylab = "Empirical - Theoretical",
       pch = 19, cex = 0.6)
  abline(h = 0, col = "red", lty = 2)
  grid()
  
  # Detrended Q-Q plot
  theoretical_quantiles <- qnorm(ppoints(length(clean_data)), sample_mean, sample_sd)
  empirical_quantiles <- sort(clean_data)
  plot(theoretical_quantiles, empirical_quantiles - theoretical_quantiles,
       main = "Detrended Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Deviation from Line",
       pch = 19, cex = 0.6)
  abline(h = 0, col = "red", lty = 2)
  grid()
  
  dev.off()
  
  # Summary of normality tests
  cat("\nSummary of Normality Tests:\n")
  alpha <- 0.05
  tests_failed <- 0
  
  if (length(clean_data) <= 5000 && shapiro_test$p.value < alpha) {
    cat("- Shapiro-Wilk test: FAILED (p =", round(shapiro_test$p.value, 4), "<", alpha, ")\n")
    tests_failed <- tests_failed + 1
  } else if (length(clean_data) <= 5000) {
    cat("- Shapiro-Wilk test: PASSED (p =", round(shapiro_test$p.value, 4), "≥", alpha, ")\n")
  }
  
  if (ad_test$p.value < alpha) {
    cat("- Anderson-Darling test: FAILED (p =", round(ad_test$p.value, 4), "<", alpha, ")\n")
    tests_failed <- tests_failed + 1
  } else {
    cat("- Anderson-Darling test: PASSED (p =", round(ad_test$p.value, 4), "≥", alpha, ")\n")
  }
  
  if (ks_test$p.value < alpha) {
    cat("- Kolmogorov-Smirnov test: FAILED (p =", round(ks_test$p.value, 4), "<", alpha, ")\n")
    tests_failed <- tests_failed + 1
  } else {
    cat("- Kolmogorov-Smirnov test: PASSED (p =", round(ks_test$p.value, 4), "≥", alpha, ")\n")
  }
  
  if (chi2_test$p.value < alpha) {
    cat("- Chi-square goodness of fit: FAILED (p =", round(chi2_test$p.value, 4), "<", alpha, ")\n")
    tests_failed <- tests_failed + 1
  } else {
    cat("- Chi-square goodness of fit: PASSED (p =", round(chi2_test$p.value, 4), "≥", alpha, ")\n")
  }
}

# Case 2: Testing discrete distributions
if ("counts" %in% names(data)) {
  cat("\n", rep("=", 50), "\n", sep="")
  cat("Testing Poisson Distribution for Count Data\n")
  cat(rep("=", 50), "\n", sep="")
  
  count_data <- data$counts[!is.na(data$counts)]
  
  cat("Sample size:", length(count_data), "\n")
  cat("Sample mean:", round(mean(count_data), 4), "\n")
  cat("Sample variance:", round(var(count_data), 4), "\n")
  cat("Variance-to-mean ratio:", round(var(count_data)/mean(count_data), 4), "\n")
  
  # For Poisson distribution, variance should equal mean
  sample_mean <- mean(count_data)
  
  # Chi-square goodness of fit for Poisson
  observed_counts <- table(count_data)
  max_val <- max(count_data)
  
  # Calculate expected counts under Poisson distribution
  expected_counts <- numeric(max_val + 1)
  for (k in 0:max_val) {
    expected_counts[k + 1] <- length(count_data) * dpois(k, sample_mean)
  }
  
  # Align observed and expected counts
  all_values <- 0:max_val
  obs_aligned <- numeric(length(all_values))
  names(obs_aligned) <- as.character(all_values)
  
  for (i in 1:length(observed_counts)) {
    val <- names(observed_counts)[i]
    obs_aligned[val] <- observed_counts[i]
  }
  
  # Combine bins with expected count < 5
  while (length(expected_counts) > 1 && tail(expected_counts, 1) < 5) {
    if (length(expected_counts) > 1) {
      expected_counts[length(expected_counts) - 1] <- expected_counts[length(expected_counts) - 1] + tail(expected_counts, 1)
      obs_aligned[length(obs_aligned) - 1] <- obs_aligned[length(obs_aligned) - 1] + tail(obs_aligned, 1)
      expected_counts <- expected_counts[-length(expected_counts)]
      obs_aligned <- obs_aligned[-length(obs_aligned)]
    }
  }
  
  # Perform chi-square test
  chi2_test_poisson <- chisq.test(obs_aligned, p = expected_counts/sum(expected_counts))
  
  cat("\nPoisson Goodness of Fit Test:\n")
  cat("Chi-square statistic:", round(chi2_test_poisson$statistic, 4), "\n")
  cat("p-value:", round(chi2_test_poisson$p.value, 4), "\n")
  cat("Degrees of freedom:", length(obs_aligned) - 1 - 1, "\n")
}

# Conclusion
cat("\n", rep("=", 50), "\n", sep="")
cat("CONCLUSIONS\n")
cat(rep("=", 50), "\n", sep="")

if ("systolic" %in% names(data)) {
  cat("\nNormal Distribution Test for Systolic BP:\n")
  if (exists("tests_failed")) {
    if (tests_failed == 0) {
      cat("The data appears to follow a normal distribution (all tests passed)\n")
    } else if (tests_failed <= 2) {
      cat("The data shows some deviation from normality (some tests failed)\n")
    } else {
      cat("The data significantly deviates from normal distribution (most tests failed)\n")
    }
  }
}

if ("counts" %in% names(data)) {
  cat("\nPoisson Distribution Test for Count Data:\n")
  if (exists("chi2_test_poisson")) {
    if (chi2_test_poisson$p.value >= 0.05) {
      cat("The count data appears to follow a Poisson distribution\n")
    } else {
      cat("The count data does not follow a Poisson distribution\n")
    }
  }
}

cat("\n", rep("=", 50), "\n", sep="")
cat("Problem 6 Analysis Complete\n")
cat(rep("=", 50), "\n", sep="")
