# Assignment 5 - Problem 5
# Statistical Analysis Solution in R

# Load required libraries
library(ggplot2)
library(dplyr)
library(stats)
library(car)
library(multcomp)  # for multiple comparisons
library(agricolae)  # for additional post-hoc tests

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
  n <- 120
  data <<- data.frame(
    age = rnorm(n, 40, 15),
    weight = rnorm(n, 150, 25),
    systolic = rnorm(n, 120, 20),
    serum.chol = rnorm(n, 200, 40),
    exercise = sample(0:3, n, replace = TRUE),
    ed.level = sample(0:3, n, replace = TRUE)
  )
})

# Problem 5: Analysis of Variance (ANOVA)
# Example: Comparing means across multiple groups

if ("systolic" %in% names(data) && "exercise" %in% names(data)) {
  # Clean data - remove missing values
  clean_data <- data[complete.cases(data[c("systolic", "exercise")]), ]
  
  # Convert exercise to factor
  clean_data$exercise <- factor(clean_data$exercise)
  
  cat("\nOne-Way ANOVA Analysis\n")
  cat("Dependent Variable: Systolic Blood Pressure\n")
  cat("Independent Variable: Exercise Level\n")
  cat("Sample size:", nrow(clean_data), "\n")
  
  # Descriptive statistics by group
  cat("\nDescriptive Statistics by Exercise Level:\n")
  group_stats <- aggregate(systolic ~ exercise, data = clean_data, 
                          function(x) c(n = length(x), mean = mean(x), sd = sd(x), 
                                       min = min(x), max = max(x)))
  print(group_stats)
  
  # Overall statistics
  overall_mean <- mean(clean_data$systolic)
  overall_sd <- sd(clean_data$systolic)
  total_n <- nrow(clean_data)
  
  cat("\nOverall Statistics:\n")
  cat("Overall mean:", round(overall_mean, 4), "\n")
  cat("Overall standard deviation:", round(overall_sd, 4), "\n")
  cat("Total sample size:", total_n, "\n")
  
  # Perform one-way ANOVA
  anova_model <- aov(systolic ~ exercise, data = clean_data)
  anova_summary <- summary(anova_model)
  
  cat("\nOne-Way ANOVA Results:\n")
  print(anova_summary)
  
  # Extract key statistics
  f_statistic <- anova_summary[[1]]$`F value`[1]
  p_value <- anova_summary[[1]]$`Pr(>F)`[1]
  
  cat("\nKey Statistics:\n")
  cat("F-statistic:", round(f_statistic, 4), "\n")
  cat("p-value:", round(p_value, 4), "\n")
  
  # Calculate effect size (eta-squared)
  ss_between <- anova_summary[[1]]$`Sum Sq`[1]
  ss_total <- sum(anova_summary[[1]]$`Sum Sq`)
  eta_squared <- ss_between / ss_total
  
  cat("Effect Size (η²):", round(eta_squared, 4), "\n")
  
  # Test assumptions
  cat("\nAssumption Testing:\n")
  
  # 1. Test for normality within each group
  cat("\n1. Normality Tests (Shapiro-Wilk):\n")
  normality_results <- by(clean_data$systolic, clean_data$exercise, 
                         function(x) if(length(x) <= 5000) shapiro.test(x) else NULL)
  
  normality_violated <- FALSE
  for (i in 1:length(normality_results)) {
    if (!is.null(normality_results[[i]])) {
      exercise_level <- names(normality_results)[i]
      result <- normality_results[[i]]
      cat("Exercise level", exercise_level, ": W =", round(result$statistic, 4), 
          ", p-value =", round(result$p.value, 4), "\n")
      if (result$p.value < 0.05) {
        normality_violated <- TRUE
      }
    }
  }
  
  # 2. Test for homogeneity of variances
  cat("\n2. Homogeneity of Variance Tests:\n")
  
  # Levene's test (more robust)
  levene_result <- leveneTest(systolic ~ exercise, data = clean_data)
  cat("Levene's test: F =", round(levene_result$`F value`[1], 4), 
      ", p-value =", round(levene_result$`Pr(>F)`[1], 4), "\n")
  
  # Bartlett's test (assumes normality)
  bartlett_result <- bartlett.test(systolic ~ exercise, data = clean_data)
  cat("Bartlett's test: χ² =", round(bartlett_result$statistic, 4), 
      ", p-value =", round(bartlett_result$p.value, 4), "\n")
  
  # Create comprehensive visualization
  png('/Users/devvrathans/stats-assignment/assignment05/assignment05_problem05_r_output.png', 
      width = 1500, height = 1200, res = 150)
  
  par(mfrow = c(3, 3))
  
  # Box plot
  boxplot(systolic ~ exercise, data = clean_data,
          main = "Box Plot by Exercise Level",
          xlab = "Exercise Level", ylab = "Systolic Blood Pressure",
          col = rainbow(length(unique(clean_data$exercise))))
  grid()
  
  # Violin plot using vioplot if available, otherwise use density plots
  if (require(vioplot, quietly = TRUE)) {
    vioplot(systolic ~ exercise, data = clean_data,
            main = "Violin Plot by Exercise Level",
            xlab = "Exercise Level", ylab = "Systolic Blood Pressure")
  } else {
    # Alternative: density plots
    plot(density(clean_data$systolic), main = "Density Plot by Exercise Level",
         xlab = "Systolic Blood Pressure", ylab = "Density")
    colors <- rainbow(length(unique(clean_data$exercise)))
    for (i in 1:length(unique(clean_data$exercise))) {
      level <- unique(clean_data$exercise)[i]
      subset_data <- clean_data$systolic[clean_data$exercise == level]
      lines(density(subset_data), col = colors[i], lwd = 2)
    }
    legend("topright", legend = paste("Exercise", unique(clean_data$exercise)), 
           col = colors, lwd = 2)
  }
  grid()
  
  # Strip chart with means
  stripchart(systolic ~ exercise, data = clean_data, 
             method = "jitter", vertical = TRUE,
             main = "Strip Plot with Group Means",
             xlab = "Exercise Level", ylab = "Systolic Blood Pressure",
             col = rainbow(length(unique(clean_data$exercise))), pch = 19)
  
  # Add group means
  group_means <- tapply(clean_data$systolic, clean_data$exercise, mean)
  for (i in 1:length(group_means)) {
    segments(i - 0.2, group_means[i], i + 0.2, group_means[i], 
             col = "red", lwd = 3)
  }
  grid()
  
  # Means plot with error bars
  group_means <- tapply(clean_data$systolic, clean_data$exercise, mean)
  group_sems <- tapply(clean_data$systolic, clean_data$exercise, 
                      function(x) sd(x)/sqrt(length(x)))
  
  exercise_levels <- as.numeric(names(group_means))
  plot(exercise_levels, group_means, type = "b", pch = 19, 
       main = "Group Means with Standard Error",
       xlab = "Exercise Level", ylab = "Mean Systolic Blood Pressure",
       ylim = c(min(group_means - group_sems), max(group_means + group_sems)))
  
  arrows(exercise_levels, group_means - group_sems, 
         exercise_levels, group_means + group_sems,
         length = 0.1, angle = 90, code = 3)
  grid()
  
  # Q-Q plot of residuals
  qqnorm(residuals(anova_model), main = "Q-Q Plot of Residuals")
  qqline(residuals(anova_model), col = "red", lwd = 2)
  grid()
  
  # Residuals vs fitted values
  plot(fitted(anova_model), residuals(anova_model),
       main = "Residuals vs Fitted Values",
       xlab = "Fitted Values", ylab = "Residuals",
       pch = 19)
  abline(h = 0, col = "red", lty = 2)
  grid()
  
  # Scale-Location plot
  plot(fitted(anova_model), sqrt(abs(residuals(anova_model))),
       main = "Scale-Location Plot",
       xlab = "Fitted Values", ylab = "√|Residuals|",
       pch = 19)
  grid()
  
  # Histogram of residuals
  hist(residuals(anova_model), breaks = 20, prob = TRUE,
       main = "Distribution of Residuals",
       xlab = "Residuals", ylab = "Density",
       col = "lightblue", border = "black")
  
  # Add normal curve
  x_norm <- seq(min(residuals(anova_model)), max(residuals(anova_model)), length = 100)
  lines(x_norm, dnorm(x_norm, mean(residuals(anova_model)), sd(residuals(anova_model))), 
        col = "red", lwd = 2)
  grid()
  
  # Cook's distance
  cook_d <- cooks.distance(anova_model)
  plot(cook_d, main = "Cook's Distance",
       xlab = "Observation", ylab = "Cook's Distance",
       pch = 19, col = "red")
  abline(h = 4/length(cook_d), col = "blue", lty = 2)
  grid()
  
  dev.off()
  
  # Post-hoc tests if ANOVA is significant
  if (p_value < 0.05) {
    cat("\nPost-hoc Analysis:\n")
    cat("Since ANOVA is significant, performing multiple comparisons...\n")
    
    # Tukey's HSD
    tukey_result <- TukeyHSD(anova_model)
    cat("\nTukey's HSD Test:\n")
    print(tukey_result)
    
    # Bonferroni correction
    pairwise_result <- pairwise.t.test(clean_data$systolic, clean_data$exercise, 
                                      p.adjust.method = "bonferroni")
    cat("\nPairwise t-tests with Bonferroni correction:\n")
    print(pairwise_result)
    
    # LSD test (if agricolae package is available)
    if (require(agricolae, quietly = TRUE)) {
      lsd_result <- LSD.test(anova_model, "exercise")
      cat("\nLSD Test:\n")
      print(lsd_result$groups)
    }
  }
  
  # Additional diagnostic tests
  cat("\nAdditional Diagnostic Tests:\n")
  
  # Durbin-Watson test for independence of residuals
  dw_test <- durbinWatsonTest(anova_model)
  cat("Durbin-Watson test: D-W =", round(dw_test$dw, 4), 
      ", p-value =", round(dw_test$p, 4), "\n")
  
  # Outlier detection
  outliers <- which(abs(residuals(anova_model)) > 2 * sd(residuals(anova_model)))
  if (length(outliers) > 0) {
    cat("Potential outliers (|residual| > 2SD):", length(outliers), "observations\n")
  } else {
    cat("No major outliers detected\n")
  }
  
  # Conclusion
  cat("\nConclusion:\n")
  alpha <- 0.05
  if (p_value < alpha) {
    cat("At α =", alpha, ", we reject the null hypothesis (p-value =", 
        round(p_value, 4), "<", alpha, ")\n")
    cat("There is sufficient evidence to conclude that there are significant differences in systolic blood pressure among exercise levels\n")
  } else {
    cat("At α =", alpha, ", we fail to reject the null hypothesis (p-value =", 
        round(p_value, 4), "≥", alpha, ")\n")
    cat("There is insufficient evidence to conclude that there are significant differences in systolic blood pressure among exercise levels\n")
  }
  
  # Effect size interpretation
  cat("\nEffect Size Interpretation (η²):\n")
  if (eta_squared < 0.01) {
    cat("Small effect\n")
  } else if (eta_squared < 0.06) {
    cat("Medium effect\n")
  } else if (eta_squared < 0.14) {
    cat("Large effect\n")
  } else {
    cat("Very large effect\n")
  }
  
  # Assumption summary
  cat("\nAssumption Summary:\n")
  if (normality_violated) {
    cat("- Normality assumption may be violated for some groups\n")
  } else {
    cat("- Normality assumption appears to be met\n")
  }
  
  if (levene_result$`Pr(>F)`[1] < 0.05) {
    cat("- Homogeneity of variance assumption is violated (consider Welch's ANOVA)\n")
    
    # Perform Welch's ANOVA as alternative
    welch_result <- oneway.test(systolic ~ exercise, data = clean_data, var.equal = FALSE)
    cat("\nWelch's ANOVA (for unequal variances):\n")
    cat("F =", round(welch_result$statistic, 4), ", p-value =", round(welch_result$p.value, 4), "\n")
  } else {
    cat("- Homogeneity of variance assumption appears to be met\n")
  }
  
} else if ("serum.chol" %in% names(data) && "ed.level" %in% names(data)) {
  # Alternative analysis: Serum cholesterol by education level
  cat("\nAlternative Analysis: Serum Cholesterol by Education Level\n")
  
  clean_data <- data[complete.cases(data[c("serum.chol", "ed.level")]), ]
  clean_data$ed.level <- factor(clean_data$ed.level)
  
  # Perform one-way ANOVA
  anova_model <- aov(serum.chol ~ ed.level, data = clean_data)
  anova_summary <- summary(anova_model)
  
  cat("One-way ANOVA results:\n")
  print(anova_summary)
  
  # Group statistics
  cat("\nGroup Statistics:\n")
  group_stats <- aggregate(serum.chol ~ ed.level, data = clean_data, 
                          function(x) c(n = length(x), mean = mean(x), sd = sd(x)))
  print(group_stats)
  
} else {
  cat("Required variables not available in the dataset.\n")
  cat("Please verify the problem requirements and dataset.\n")
}

cat("\n", rep("=", 50), "\n", sep="")
cat("Problem 5 Analysis Complete\n")
cat(rep("=", 50), "\n", sep="")
