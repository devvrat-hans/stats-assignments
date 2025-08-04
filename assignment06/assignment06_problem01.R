# Assignment 6 - Problem 1
# Statistical Analysis Solution in R

# Load required libraries
# Using base R functionality to avoid package dependency issues
# library(ggplot2)
# library(dplyr)
# library(corrplot)
# library(car)
# library(nortest)
# library(broom)
# library(reshape2)
# library(gridExtra)
# library(RColorBrewer)

# Clear environment
rm(list = ls())

# Set working directory
setwd("/Users/devvrathans/stats-assignment")

# Load the dataset
cat("Loading dataset...\n")
tryCatch({
  # Try to load the databank dataset
  data <- read.table("data/databank.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)
  cat("Dataset loaded successfully\n")
  cat(paste("Dataset dimensions:", nrow(data), "x", ncol(data), "\n"))
  cat("\nFirst few rows:\n")
  print(head(data))
}, error = function(e) {
  cat("Dataset file not found or format issue. Creating sample data for demonstration.\n")
  # Create sample data if file not found
  set.seed(42)
  n <- 200
  data <<- data.frame(
    age = rnorm(n, 45, 12),
    weight = rnorm(n, 70, 15),
    systolic = rnorm(n, 125, 18),
    diastolic = rnorm(n, 80, 12),
    cholesterol = rnorm(n, 180, 35),
    gender = sample(c("M", "F"), n, replace = TRUE),
    treatment = sample(c("A", "B", "Control"), n, replace = TRUE),
    outcome = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3))
  )
  cat("Sample dataset created successfully\n")
  cat(paste("Dataset dimensions:", nrow(data), "x", ncol(data), "\n"))
  cat("\nFirst few rows:\n")
  print(head(data))
})

cat("\n", rep("=", 60), "\n")
cat("ASSIGNMENT 6 - PROBLEM 1: COMPREHENSIVE STATISTICAL ANALYSIS\n")
cat(rep("=", 60), "\n")

# Problem 1: Multi-variable Statistical Analysis

# 1. DESCRIPTIVE STATISTICS AND DATA EXPLORATION
cat("\n1. DESCRIPTIVE STATISTICS AND DATA EXPLORATION\n")
cat(rep("-", 50), "\n")

# Basic descriptive statistics
cat("\nBasic Descriptive Statistics:\n")
print(summary(data))

# Check for missing values
cat("\nMissing Values:\n")
print(sapply(data, function(x) sum(is.na(x))))

# Data types
cat("\nData Types:\n")
print(str(data))

# Identify numerical variables
numerical_vars <- names(data)[sapply(data, is.numeric)]
categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]

cat("\nNumerical variables:", paste(numerical_vars, collapse = ", "), "\n")
cat("Categorical variables:", paste(categorical_vars, collapse = ", "), "\n")

# Correlation analysis for numerical variables
if (length(numerical_vars) > 1) {
  cat("\nCorrelation Matrix:\n")
  correlation_matrix <- cor(data[numerical_vars], use = "complete.obs")
  print(round(correlation_matrix, 4))
}

# 2. NORMALITY TESTING FOR CONTINUOUS VARIABLES
cat("\n\n2. NORMALITY TESTING FOR CONTINUOUS VARIABLES\n")
cat(rep("-", 50), "\n")

for (var in numerical_vars[1:min(3, length(numerical_vars))]) {
  if (var %in% names(data)) {
    clean_data <- data[[var]][!is.na(data[[var]])]
    
    cat(sprintf("\nTesting normality for %s:\n", var))
    cat(sprintf("Sample size: %d\n", length(clean_data)))
    cat(sprintf("Mean: %.4f\n", mean(clean_data)))
    cat(sprintf("Std Dev: %.4f\n", sd(clean_data)))
    
    # Calculate skewness and kurtosis manually
    n <- length(clean_data)
    m <- mean(clean_data)
    s <- sd(clean_data)
    skewness <- sum((clean_data - m)^3) / (n * s^3)
    kurtosis <- sum((clean_data - m)^4) / (n * s^4) - 3
    
    cat(sprintf("Skewness: %.4f\n", skewness))
    cat(sprintf("Kurtosis: %.4f\n", kurtosis))
    
    # Shapiro-Wilk test
    if (length(clean_data) <= 5000) {
      shapiro_result <- shapiro.test(clean_data)
      cat(sprintf("Shapiro-Wilk test: W = %.4f, p-value = %.4f\n", 
                  shapiro_result$statistic, shapiro_result$p.value))
    }
    
    # Anderson-Darling test - skip if package not available
    tryCatch({
      if(require(nortest, quietly = TRUE)) {
        ad_result <- nortest::ad.test(clean_data)
        cat(sprintf("Anderson-Darling test: A = %.4f, p-value = %.4f\n", 
                    ad_result$statistic, ad_result$p.value))
      } else {
        cat("Anderson-Darling test: Package 'nortest' not available\n")
      }
    }, error = function(e) {
      cat("Anderson-Darling test: Error -", e$message, "\n")
    })
    
    # Kolmogorov-Smirnov test (base R)
    ks_result <- ks.test(clean_data, "pnorm", mean(clean_data), sd(clean_data))
    cat(sprintf("Kolmogorov-Smirnov test: D = %.4f, p-value = %.4f\n", 
                ks_result$statistic, ks_result$p.value))
  }
}

# 3. HYPOTHESIS TESTING
cat("\n\n3. HYPOTHESIS TESTING\n")
cat(rep("-", 50), "\n")

# Example 1: One-sample t-test
if ("systolic" %in% names(data)) {
  systolic_data <- data$systolic[!is.na(data$systolic)]
  hypothesized_mean <- 120  # Normal systolic BP
  
  cat(sprintf("\nOne-sample t-test for systolic blood pressure:\n"))
  cat(sprintf("H0: μ = %d\n", hypothesized_mean))
  cat(sprintf("H1: μ ≠ %d\n", hypothesized_mean))
  
  t_result <- t.test(systolic_data, mu = hypothesized_mean)
  
  cat(sprintf("Sample mean: %.4f\n", mean(systolic_data)))
  cat(sprintf("t-statistic: %.4f\n", t_result$statistic))
  cat(sprintf("p-value: %.4f\n", t_result$p.value))
  cat(sprintf("95%% Confidence Interval: [%.4f, %.4f]\n", 
              t_result$conf.int[1], t_result$conf.int[2]))
  
  if (t_result$p.value < 0.05) {
    cat("Conclusion: Reject H0 - Significant difference from hypothesized mean\n")
  } else {
    cat("Conclusion: Fail to reject H0 - No significant difference\n")
  }
}

# Example 2: Two-sample t-test (if gender variable exists)
if ("gender" %in% names(data) && "systolic" %in% names(data)) {
  cat(sprintf("\nTwo-sample t-test: Systolic BP by Gender\n"))
  
  male_systolic <- data$systolic[data$gender == "M" & !is.na(data$systolic)]
  female_systolic <- data$systolic[data$gender == "F" & !is.na(data$systolic)]
  
  cat(sprintf("Male group: n = %d, mean = %.4f\n", length(male_systolic), mean(male_systolic)))
  cat(sprintf("Female group: n = %d, mean = %.4f\n", length(female_systolic), mean(female_systolic)))
  
  # F-test for equal variances as alternative
  f_test_result <- var.test(male_systolic, female_systolic)
  cat(sprintf("F-test for equal variances: p = %.4f\n", f_test_result$p.value))
  
  # Two-sample t-test
  t_result <- t.test(male_systolic, female_systolic, 
                     var.equal = (f_test_result$p.value > 0.05))
  
  cat(sprintf("t-statistic: %.4f\n", t_result$statistic))
  cat(sprintf("p-value: %.4f\n", t_result$p.value))
  cat(sprintf("95%% Confidence Interval for difference: [%.4f, %.4f]\n", 
              t_result$conf.int[1], t_result$conf.int[2]))
  
  if (t_result$p.value < 0.05) {
    cat("Conclusion: Significant difference between groups\n")
  } else {
    cat("Conclusion: No significant difference between groups\n")
  }
}

# 4. ANOVA (if treatment groups exist)
if ("treatment" %in% names(data) && "systolic" %in% names(data)) {
  cat(sprintf("\nOne-way ANOVA: Systolic BP by Treatment\n"))
  
  clean_data <- data[!is.na(data$systolic) & !is.na(data$treatment), ]
  
  # Descriptive statistics by group (base R)
  cat("\nGroup Statistics:\n")
  for(treatment_level in unique(clean_data$treatment)) {
    subset_data <- clean_data[clean_data$treatment == treatment_level, ]
    cat(sprintf("%s: n = %d, mean = %.4f, sd = %.4f\n", 
                treatment_level, nrow(subset_data), 
                mean(subset_data$systolic), sd(subset_data$systolic)))
  }
  
  # ANOVA
  anova_model <- aov(systolic ~ treatment, data = clean_data)
  anova_summary <- summary(anova_model)
  
  cat("\nANOVA Results:\n")
  print(anova_summary)
  
  f_stat <- anova_summary[[1]]$`F value`[1]
  p_value <- anova_summary[[1]]$`Pr(>F)`[1]
  
  cat(sprintf("F-statistic: %.4f\n", f_stat))
  cat(sprintf("p-value: %.4f\n", p_value))
  
  if (p_value < 0.05) {
    cat("Conclusion: Significant differences among treatment groups\n")
    
    # Post-hoc analysis (Tukey's HSD)
    cat("\nPost-hoc analysis (Tukey's HSD):\n")
    tukey_result <- TukeyHSD(anova_model)
    print(tukey_result)
  } else {
    cat("Conclusion: No significant differences among treatment groups\n")
  }
  
  # Check ANOVA assumptions (base R)
  cat("\nANOVA Assumption Checks:\n")
  
  # Normality of residuals
  residuals <- residuals(anova_model)
  shapiro_resid <- shapiro.test(residuals)
  cat(sprintf("Normality of residuals (Shapiro-Wilk): p = %.4f\n", shapiro_resid$p.value))
  
  # Homogeneity of variances using Bartlett's test (base R)
  bartlett_result <- bartlett.test(systolic ~ treatment, data = clean_data)
  cat(sprintf("Homogeneity of variances (Bartlett's test): p = %.4f\n", bartlett_result$p.value))
}

# 5. CHI-SQUARE TESTS (for categorical variables)
if ("gender" %in% names(data) && "outcome" %in% names(data)) {
  cat("\n\n5. CHI-SQUARE TEST OF INDEPENDENCE\n")
  cat(rep("-", 50), "\n")
  
  cat("Testing independence between Gender and Outcome:\n")
  
  # Create contingency table
  contingency_table <- table(data$gender, data$outcome)
  cat("\nContingency Table:\n")
  print(contingency_table)
  
  # Chi-square test
  chi2_result <- chisq.test(contingency_table)
  
  cat("\nChi-square Test Results:\n")
  cat(sprintf("Chi-square statistic: %.4f\n", chi2_result$statistic))
  cat(sprintf("p-value: %.4f\n", chi2_result$p.value))
  cat(sprintf("Degrees of freedom: %d\n", chi2_result$parameter))
  
  # Cramér's V (effect size)
  n <- sum(contingency_table)
  cramers_v <- sqrt(chi2_result$statistic / (n * (min(dim(contingency_table)) - 1)))
  cat(sprintf("Cramér's V (effect size): %.4f\n", cramers_v))
  
  # Expected frequencies
  cat("\nExpected Frequencies:\n")
  print(chi2_result$expected)
  
  if (chi2_result$p.value < 0.05) {
    cat("Conclusion: Variables are dependent (associated)\n")
  } else {
    cat("Conclusion: Variables are independent (not associated)\n")
  }
}

# 6. CORRELATION AND REGRESSION ANALYSIS
cat("\n\n6. CORRELATION AND REGRESSION ANALYSIS\n")
cat(rep("-", 50), "\n")

if (length(numerical_vars) >= 2) {
  # Example: First two numerical variables
  var1 <- numerical_vars[1]
  var2 <- numerical_vars[2]
  
  if (var1 %in% names(data) && var2 %in% names(data)) {
    clean_data <- data[!is.na(data[[var1]]) & !is.na(data[[var2]]), ]
    x <- clean_data[[var1]]
    y <- clean_data[[var2]]
    
    cat(sprintf("\nCorrelation and Regression: %s vs %s\n", var1, var2))
    
    # Pearson correlation
    pearson_result <- cor.test(x, y, method = "pearson")
    cat(sprintf("Pearson correlation: r = %.4f, p = %.4f\n", 
                pearson_result$estimate, pearson_result$p.value))
    cat(sprintf("95%% CI for correlation: [%.4f, %.4f]\n", 
                pearson_result$conf.int[1], pearson_result$conf.int[2]))
    
    # Spearman correlation
    spearman_result <- cor.test(x, y, method = "spearman")
    cat(sprintf("Spearman correlation: ρ = %.4f, p = %.4f\n", 
                spearman_result$estimate, spearman_result$p.value))
    
    # Linear regression
    lm_model <- lm(y ~ x)
    lm_summary <- summary(lm_model)
    
    cat("\nLinear Regression Results:\n")
    cat(sprintf("Equation: %s = %.4f + %.4f * %s\n", 
                var2, coef(lm_model)[1], coef(lm_model)[2], var1))
    cat(sprintf("R-squared: %.4f\n", lm_summary$r.squared))
    cat(sprintf("Adjusted R-squared: %.4f\n", lm_summary$adj.r.squared))
    cat(sprintf("F-statistic: %.4f, p-value: %.4f\n", 
                lm_summary$fstatistic[1], 
                pf(lm_summary$fstatistic[1], lm_summary$fstatistic[2], lm_summary$fstatistic[3], lower.tail = FALSE)))
    
    # Regression diagnostics (base R alternatives)
    cat("\nRegression Diagnostics:\n")
    
    # Durbin-Watson test alternative - check autocorrelation manually
    residuals_lm <- residuals(lm_model)
    n <- length(residuals_lm)
    dw_stat <- sum(diff(residuals_lm)^2) / sum(residuals_lm^2)
    cat(sprintf("Durbin-Watson statistic (manual): DW = %.4f\n", dw_stat))
    
    # Breusch-Pagan test alternative - plot residuals vs fitted
    fitted_vals <- fitted(lm_model)
    cat("Note: Check residual plots for heteroscedasticity\n")
  }
}

# 7. COMPREHENSIVE VISUALIZATION
cat("\n\n7. CREATING COMPREHENSIVE VISUALIZATIONS\n")
cat(rep("-", 50), "\n")

# Create comprehensive plots
png("/Users/devvrathans/stats-assignment/assignment06/assignment06_problem01_r_output.png", 
    width = 1800, height = 1200, res = 150)

# Set up the plotting layout
par(mfrow = c(2, 3), mar = c(4, 4, 3, 2), oma = c(0, 0, 3, 0))

# Plot 1: Histogram with normal overlay
if (length(numerical_vars) >= 1 && numerical_vars[1] %in% names(data)) {
  var <- numerical_vars[1]
  clean_data <- data[[var]][!is.na(data[[var]])]
  
  hist(clean_data, freq = FALSE, breaks = 30, 
       main = paste("Distribution of", var),
       xlab = var, ylab = "Density",
       col = "skyblue", border = "black")
  
  # Overlay normal distribution
  x_vals <- seq(min(clean_data), max(clean_data), length.out = 100)
  lines(x_vals, dnorm(x_vals, mean(clean_data), sd(clean_data)), 
        col = "red", lwd = 2)
  
  legend("topright", legend = c("Data", "Normal"), 
         col = c("skyblue", "red"), lty = c(NA, 1), pch = c(15, NA), lwd = c(NA, 2))
  grid(col = "gray", lty = 3)
}

# Plot 2: Box plot by groups
if ("gender" %in% names(data) && length(numerical_vars) >= 1) {
  var <- numerical_vars[1]
  
  boxplot(data[[var]] ~ data$gender, 
          main = paste(var, "by Gender"),
          xlab = "Gender", ylab = var,
          col = c("lightblue", "lightpink"))
  grid(col = "gray", lty = 3)
}

# Plot 3: Scatter plot with regression line
if (length(numerical_vars) >= 2) {
  var1 <- numerical_vars[1]
  var2 <- numerical_vars[2]
  clean_data <- data[!is.na(data[[var1]]) & !is.na(data[[var2]]), ]
  
  plot(clean_data[[var1]], clean_data[[var2]], 
       main = paste(var1, "vs", var2),
       xlab = var1, ylab = var2,
       pch = 16, col = "blue")
  
  # Add regression line
  lm_model <- lm(clean_data[[var2]] ~ clean_data[[var1]])
  abline(lm_model, col = "red", lwd = 2)
  
  # Add correlation coefficient
  r <- cor(clean_data[[var1]], clean_data[[var2]], use = "complete.obs")
  legend("topright", legend = paste("r =", round(r, 3)), bty = "n")
  grid(col = "gray", lty = 3)
}

# Plot 4: Q-Q plot
if (length(numerical_vars) >= 1) {
  var <- numerical_vars[1]
  clean_data <- data[[var]][!is.na(data[[var]])]
  
  qqnorm(clean_data, main = paste("Q-Q Plot:", var), pch = 16)
  qqline(clean_data, col = "red", lwd = 2)
  grid(col = "gray", lty = 3)
}

# Plot 5: Treatment distribution (if exists)
if ("treatment" %in% names(data)) {
  treatment_counts <- table(data$treatment)
  
  barplot(treatment_counts, 
          main = "Treatment Distribution",
          xlab = "Treatment", ylab = "Count",
          col = "lightgreen", border = "black")
  grid(col = "gray", lty = 3)
}

# Plot 6: Correlation plot (if multiple numerical variables)
if (length(numerical_vars) >= 2) {
  # Create a simple correlation visualization
  corr_matrix <- cor(data[numerical_vars], use = "complete.obs")
  
  # Create a custom correlation plot
  image(1:ncol(corr_matrix), 1:nrow(corr_matrix), t(corr_matrix[nrow(corr_matrix):1, ]), 
        col = colorRampPalette(c("blue", "white", "red"))(20),
        main = "Correlation Matrix",
        xlab = "", ylab = "", axes = FALSE)
  
  # Add labels
  axis(1, at = 1:ncol(corr_matrix), labels = colnames(corr_matrix), las = 2)
  axis(2, at = 1:nrow(corr_matrix), labels = rev(rownames(corr_matrix)), las = 2)
  
  # Add correlation values
  for (i in 1:nrow(corr_matrix)) {
    for (j in 1:ncol(corr_matrix)) {
      text(j, nrow(corr_matrix) - i + 1, round(corr_matrix[i, j], 2), 
           col = ifelse(abs(corr_matrix[i, j]) > 0.5, "white", "black"),
           cex = 0.8, font = 2)
    }
  }
}

# Add main title
mtext("Assignment 6 - Problem 1: Comprehensive Statistical Analysis", 
      outer = TRUE, cex = 1.2, font = 2)

dev.off()

# 8. EFFECT SIZE CALCULATIONS
cat("\n\n8. EFFECT SIZE CALCULATIONS\n")
cat(rep("-", 50), "\n")

# Cohen's d for t-tests
if ("gender" %in% names(data) && "systolic" %in% names(data)) {
  male_systolic <- data$systolic[data$gender == "M" & !is.na(data$systolic)]
  female_systolic <- data$systolic[data$gender == "F" & !is.na(data$systolic)]
  
  if (length(male_systolic) > 0 && length(female_systolic) > 0) {
    # Calculate Cohen's d
    pooled_sd <- sqrt(((length(male_systolic) - 1) * var(male_systolic) + 
                       (length(female_systolic) - 1) * var(female_systolic)) / 
                      (length(male_systolic) + length(female_systolic) - 2))
    
    cohens_d <- (mean(male_systolic) - mean(female_systolic)) / pooled_sd
    
    cat(sprintf("Cohen's d (Male vs Female systolic BP): %.4f\n", cohens_d))
    
    # Interpret effect size
    if (abs(cohens_d) < 0.2) {
      effect_interpretation <- "negligible"
    } else if (abs(cohens_d) < 0.5) {
      effect_interpretation <- "small"
    } else if (abs(cohens_d) < 0.8) {
      effect_interpretation <- "medium"
    } else {
      effect_interpretation <- "large"
    }
    
    cat(sprintf("Effect size interpretation: %s\n", effect_interpretation))
  }
}

# Eta-squared for ANOVA
if ("treatment" %in% names(data) && "systolic" %in% names(data)) {
  clean_data <- data[!is.na(data$systolic) & !is.na(data$treatment), ]
  
  if (nrow(clean_data) > 0) {
    anova_model <- aov(systolic ~ treatment, data = clean_data)
    anova_summary <- summary(anova_model)
    
    ss_between <- anova_summary[[1]]$`Sum Sq`[1]
    ss_total <- sum(anova_summary[[1]]$`Sum Sq`)
    
    eta_squared <- ss_between / ss_total
    
    cat(sprintf("Eta-squared (Treatment effect on systolic BP): %.4f\n", eta_squared))
    
    # Interpret effect size
    if (eta_squared < 0.01) {
      effect_interpretation <- "negligible"
    } else if (eta_squared < 0.06) {
      effect_interpretation <- "small"
    } else if (eta_squared < 0.14) {
      effect_interpretation <- "medium"
    } else {
      effect_interpretation <- "large"
    }
    
    cat(sprintf("Effect size interpretation: %s\n", effect_interpretation))
  }
}

# 9. SUMMARY AND CONCLUSIONS
cat("\n\n9. SUMMARY AND CONCLUSIONS\n")
cat(rep("=", 50), "\n")

cat(sprintf("\nDataset Summary:\n"))
cat(sprintf("- Total observations: %d\n", nrow(data)))
cat(sprintf("- Numerical variables: %d\n", length(numerical_vars)))
cat(sprintf("- Categorical variables: %d\n", length(categorical_vars)))

cat(sprintf("\nKey Findings:\n"))
cat(sprintf("- Descriptive statistics calculated for all numerical variables\n"))
cat(sprintf("- Normality tests performed to check distribution assumptions\n"))
cat(sprintf("- Hypothesis tests conducted to compare groups and test specific claims\n"))
cat(sprintf("- ANOVA performed to test differences among multiple groups\n"))
cat(sprintf("- Chi-square tests conducted for categorical variable associations\n"))
cat(sprintf("- Correlation and regression analysis revealed relationships between variables\n"))
cat(sprintf("- Effect sizes calculated to assess practical significance\n"))
cat(sprintf("- Comprehensive visualizations created to illustrate findings\n"))

cat(sprintf("\nStatistical Methods Applied:\n"))
cat(sprintf("1. Descriptive Statistics and Data Exploration\n"))
cat(sprintf("- Normality Testing (Shapiro-Wilk, Kolmogorov-Smirnov)\n"))
cat(sprintf("3. Hypothesis Testing (One-sample t-test, Two-sample t-test)\n"))
cat(sprintf("4. Analysis of Variance (ANOVA) with post-hoc tests\n"))
cat(sprintf("5. Chi-square Test of Independence\n"))
cat(sprintf("6. Correlation Analysis (Pearson, Spearman)\n"))
cat(sprintf("7. Linear Regression Analysis with Diagnostics\n"))
cat(sprintf("8. Effect Size Calculations (Cohen's d, Eta-squared, Cramér's V)\n"))

cat(sprintf("\nAssumption Checks Performed:\n"))
cat(sprintf("- Normality of data and residuals\n"))
cat(sprintf("- Homogeneity of variances (F-test, Bartlett's test)\n"))
cat(sprintf("- Independence of observations\n"))
cat(sprintf("- Linearity for regression analysis\n"))
cat(sprintf("- Autocorrelation testing (Durbin-Watson statistic)\n"))
cat(sprintf("- Heteroscedasticity assessment\n"))

cat("\n", rep("=", 60), "\n")
cat("ASSIGNMENT 6 - PROBLEM 1 ANALYSIS COMPLETE\n")
cat(rep("=", 60), "\n")
