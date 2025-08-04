# Assignment 5 - Problem 3
# Statistical Analysis Solution in R

# Load required libraries
library(ggplot2)
library(dplyr)
library(stats)
library(car)
library(nortest)
library(corrplot)

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
    height = rnorm(n, 68, 4),
    systolic = rnorm(n, 120, 20),
    serum.chol = rnorm(n, 200, 40)
  )
})

# Problem 3: Correlation and Regression Analysis
# Example: Analyzing relationship between variables

if ("systolic" %in% names(data) && "age" %in% names(data)) {
  # Clean data - remove missing values
  clean_data <- data[complete.cases(data[c("systolic", "age")]), ]
  x <- clean_data$age
  y <- clean_data$systolic
  
  cat("\nCorrelation and Regression Analysis\n")
  cat("Variables: Age (X) vs Systolic Blood Pressure (Y)\n")
  cat("Sample size:", nrow(clean_data), "\n")
  
  # Calculate correlation coefficients
  pearson_test <- cor.test(x, y, method = "pearson")
  spearman_test <- cor.test(x, y, method = "spearman")
  
  cat("\nCorrelation Analysis:\n")
  cat("Pearson correlation coefficient:", round(pearson_test$estimate, 4), 
      "(p-value:", round(pearson_test$p.value, 4), ")\n")
  cat("Spearman correlation coefficient:", round(spearman_test$estimate, 4), 
      "(p-value:", round(spearman_test$p.value, 4), ")\n")
  
  # Linear regression
  lm_model <- lm(y ~ x)
  model_summary <- summary(lm_model)
  
  slope <- model_summary$coefficients[2, 1]
  intercept <- model_summary$coefficients[1, 1]
  r_squared <- model_summary$r.squared
  
  cat("\nLinear Regression Analysis:\n")
  cat("Regression equation: Y =", round(intercept, 4), "+", round(slope, 4), "X\n")
  cat("R-squared:", round(r_squared, 4), "\n")
  cat("Slope:", round(slope, 4), "\n")
  cat("Intercept:", round(intercept, 4), "\n")
  
  # Confidence intervals
  conf_int <- confint(lm_model)
  cat("95% CI for slope: [", round(conf_int[2, 1], 4), ",", round(conf_int[2, 2], 4), "]\n")
  
  # Model diagnostics
  cat("\nModel Summary:\n")
  print(model_summary)
  
  # ANOVA table
  cat("\nANOVA Table:\n")
  print(anova(lm_model))
  
  # Create comprehensive visualization
  png('/Users/devvrathans/stats-assignment/assignment05/assignment05_problem03_r_output.png', 
      width = 1500, height = 1200, res = 150)
  
  par(mfrow = c(3, 3))
  
  # Scatter plot with regression line
  plot(x, y, main = paste("Scatter Plot with Regression Line\nR² =", round(r_squared, 4)),
       xlab = "Age", ylab = "Systolic Blood Pressure",
       pch = 19, col = "blue", cex = 0.8)
  abline(lm_model, col = "red", lwd = 2)
  
  # Add regression equation
  equation <- paste("y =", round(intercept, 2), "+", round(slope, 2), "x")
  legend("topleft", legend = equation, bty = "n", cex = 1.2)
  grid()
  
  # Residual plots (built-in diagnostic plots)
  plot(lm_model, which = 1)  # Residuals vs Fitted
  plot(lm_model, which = 2)  # Q-Q plot
  plot(lm_model, which = 3)  # Scale-Location
  plot(lm_model, which = 4)  # Cook's distance
  plot(lm_model, which = 5)  # Residuals vs Leverage
  
  # Additional plots
  # Histogram of residuals
  residuals <- residuals(lm_model)
  hist(residuals, breaks = 20, prob = TRUE, 
       main = "Distribution of Residuals",
       xlab = "Residuals", ylab = "Density",
       col = "lightblue", border = "black")
  
  # Add normal curve
  x_norm <- seq(min(residuals), max(residuals), length = 100)
  lines(x_norm, dnorm(x_norm, mean(residuals), sd(residuals)), col = "red", lwd = 2)
  grid()
  
  # Correlation matrix plot (if more variables available)
  if (ncol(clean_data) > 2) {
    numeric_data <- clean_data[sapply(clean_data, is.numeric)]
    if (ncol(numeric_data) > 1) {
      cor_matrix <- cor(numeric_data, use = "complete.obs")
      corrplot(cor_matrix, method = "color", type = "upper", 
               title = "Correlation Matrix", mar = c(0, 0, 1, 0))
    }
  }
  
  # Prediction intervals
  new_data <- data.frame(x = seq(min(x), max(x), length.out = 100))
  pred_intervals <- predict(lm_model, newdata = new_data, interval = "prediction")
  conf_intervals <- predict(lm_model, newdata = new_data, interval = "confidence")
  
  plot(x, y, main = "Prediction and Confidence Intervals",
       xlab = "Age", ylab = "Systolic Blood Pressure",
       pch = 19, col = "blue", cex = 0.8)
  
  # Add regression line
  lines(new_data$x, pred_intervals[, 1], col = "red", lwd = 2)
  
  # Add confidence intervals
  lines(new_data$x, conf_intervals[, 2], col = "green", lty = 2, lwd = 2)
  lines(new_data$x, conf_intervals[, 3], col = "green", lty = 2, lwd = 2)
  
  # Add prediction intervals
  lines(new_data$x, pred_intervals[, 2], col = "orange", lty = 3, lwd = 2)
  lines(new_data$x, pred_intervals[, 3], col = "orange", lty = 3, lwd = 2)
  
  legend("topleft", 
         legend = c("Regression Line", "95% Confidence", "95% Prediction"),
         col = c("red", "green", "orange"),
         lty = c(1, 2, 3), lwd = 2)
  grid()
  
  # Influence measures
  influence_measures <- influence.measures(lm_model)
  plot(cooks.distance(lm_model), 
       main = "Cook's Distance",
       xlab = "Observation", ylab = "Cook's Distance",
       pch = 19, col = "red")
  abline(h = 4/length(x), col = "blue", lty = 2)
  grid()
  
  dev.off()
  
  # Additional diagnostic tests
  cat("\nDiagnostic Tests:\n")
  
  # Durbin-Watson test for autocorrelation
  dw_test <- durbinWatsonTest(lm_model)
  cat("Durbin-Watson test statistic:", round(dw_test$dw, 4), "\n")
  cat("Durbin-Watson p-value:", round(dw_test$p, 4), "\n")
  
  # Shapiro-Wilk test for normality of residuals
  if (length(residuals) <= 5000) {
    shapiro_test <- shapiro.test(residuals)
    cat("Shapiro-Wilk test for residuals normality: statistic =", 
        round(shapiro_test$statistic, 4), ", p-value =", round(shapiro_test$p.value, 4), "\n")
  }
  
  # Breusch-Pagan test for heteroscedasticity
  bp_test <- ncvTest(lm_model)
  cat("Breusch-Pagan test for heteroscedasticity: Chi-square =", 
      round(bp_test$ChiSquare, 4), ", p-value =", round(bp_test$p, 4), "\n")
  
  # Sample predictions
  cat("\nSample Predictions:\n")
  sample_ages <- c(25, 40, 60)
  for (age in sample_ages) {
    pred_data <- data.frame(x = age)
    prediction <- predict(lm_model, newdata = pred_data, interval = "prediction")
    cat("Age", age, ": Predicted systolic BP =", round(prediction[1], 2), 
        ", 95% PI: [", round(prediction[2], 2), ",", round(prediction[3], 2), "]\n")
  }
  
  # Conclusion
  cat("\nConclusion:\n")
  alpha <- 0.05
  if (pearson_test$p.value < alpha) {
    cat("At α =", alpha, ", there is significant correlation between age and systolic blood pressure (p-value =", 
        round(pearson_test$p.value, 4), ")\n")
    if (pearson_test$estimate > 0) {
      cat("The correlation is positive - as age increases, systolic blood pressure tends to increase\n")
    } else {
      cat("The correlation is negative - as age increases, systolic blood pressure tends to decrease\n")
    }
  } else {
    cat("At α =", alpha, ", there is no significant correlation between age and systolic blood pressure (p-value =", 
        round(pearson_test$p.value, 4), ")\n")
  }
  
  # Strength of correlation
  cat("\nCorrelation Strength:\n")
  abs_corr <- abs(pearson_test$estimate)
  if (abs_corr < 0.3) {
    cat("Weak correlation\n")
  } else if (abs_corr < 0.7) {
    cat("Moderate correlation\n")
  } else {
    cat("Strong correlation\n")
  }
  
} else if ("weight" %in% names(data) && "height" %in% names(data)) {
  # Alternative analysis: Weight vs Height
  cat("\nAlternative Analysis: Weight vs Height Relationship\n")
  
  clean_data <- data[complete.cases(data[c("weight", "height")]), ]
  x <- clean_data$height
  y <- clean_data$weight
  
  # Correlation analysis
  pearson_test <- cor.test(x, y, method = "pearson")
  cat("Pearson correlation (height vs weight):", round(pearson_test$estimate, 4), 
      "(p-value:", round(pearson_test$p.value, 4), ")\n")
  
  # Simple linear regression
  lm_model <- lm(y ~ x)
  model_summary <- summary(lm_model)
  
  cat("R-squared:", round(model_summary$r.squared, 4), "\n")
  cat("Regression equation: Weight =", round(model_summary$coefficients[1, 1], 4), 
      "+", round(model_summary$coefficients[2, 1], 4), "× Height\n")
  
} else {
  cat("Required variables not available in the dataset.\n")
  cat("Please verify the problem requirements and dataset.\n")
}

cat("\n", rep("=", 50), "\n", sep="")
cat("Problem 3 Analysis Complete\n")
cat(rep("=", 50), "\n", sep="")
