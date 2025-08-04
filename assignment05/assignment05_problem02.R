# Assignment 5 - Problem 2
# Statistical Analysis Solution in R

# Load required libraries
library(ggplot2)
library(dplyr)
library(stats)
library(car)  # for Levene's test
library(nortest)
library(tidyr)

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
    gender = sample(c('M', 'F'), n, replace = TRUE),
    exercise = sample(0:3, n, replace = TRUE),
    systolic = rnorm(n, 120, 20)
  )
})

# Problem 2: Two-sample comparison or ANOVA
# This could be comparing groups, testing independence, etc.

# Example: Comparing systolic blood pressure between genders
if ("systolic" %in% names(data) && "gender" %in% names(data)) {
  # Clean data
  clean_data <- data[complete.cases(data[c("systolic", "gender")]), ]
  
  # Separate groups
  male_systolic <- clean_data$systolic[clean_data$gender == "M"]
  female_systolic <- clean_data$systolic[clean_data$gender == "F"]
  
  cat("\nGroup Statistics:\n")
  cat("Male group - n:", length(male_systolic), 
      ", mean:", round(mean(male_systolic), 4), 
      ", std:", round(sd(male_systolic), 4), "\n")
  cat("Female group - n:", length(female_systolic), 
      ", mean:", round(mean(female_systolic), 4), 
      ", std:", round(sd(female_systolic), 4), "\n")
  
  # Perform two-sample t-test (Welch's t-test - assumes unequal variances)
  t_test_result <- t.test(male_systolic, female_systolic, var.equal = FALSE)
  
  cat("\nTwo-sample t-test results:\n")
  cat("t-statistic:", round(t_test_result$statistic, 4), "\n")
  cat("p-value:", round(t_test_result$p.value, 4), "\n")
  cat("95% CI for difference:", round(t_test_result$conf.int, 4), "\n")
  
  # Test for equal variances (Levene's test)
  levene_result <- leveneTest(systolic ~ factor(gender), data = clean_data)
  cat("\nLevene's test for equal variances:\n")
  cat("F-statistic:", round(levene_result$`F value`[1], 4), "\n")
  cat("p-value:", round(levene_result$`Pr(>F)`[1], 4), "\n")
  
  # Effect size (Cohen's d)
  pooled_sd <- sqrt(((length(male_systolic) - 1) * var(male_systolic) + 
                    (length(female_systolic) - 1) * var(female_systolic)) / 
                   (length(male_systolic) + length(female_systolic) - 2))
  cohens_d <- (mean(male_systolic) - mean(female_systolic)) / pooled_sd
  cat("\nEffect size (Cohen's d):", round(cohens_d, 4), "\n")
  
  # Create comprehensive visualization
  png('/Users/devvrathans/stats-assignment/assignment05/assignment05_problem02_r_output.png', 
      width = 1500, height = 1000, res = 150)
  
  par(mfrow = c(2, 3))
  
  # Box plot comparison
  boxplot(systolic ~ gender, data = clean_data,
          main = "Box Plot Comparison by Gender",
          xlab = "Gender", ylab = "Systolic Blood Pressure",
          col = c("lightblue", "lightpink"))
  grid()
  
  # Histogram comparison
  hist(male_systolic, breaks = 15, prob = TRUE, 
       col = rgb(0, 0, 1, 0.5), border = "blue",
       main = "Distribution Comparison by Gender",
       xlab = "Systolic Blood Pressure", ylab = "Density",
       xlim = range(c(male_systolic, female_systolic)))
  hist(female_systolic, breaks = 15, prob = TRUE, 
       col = rgb(1, 0, 0, 0.5), border = "red", add = TRUE)
  legend("topright", legend = c("Male", "Female"), 
         fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))
  grid()
  
  # Q-Q plots for normality
  qqnorm(male_systolic, main = "Q-Q Plot - Male Group")
  qqline(male_systolic, col = "red", lwd = 2)
  grid()
  
  qqnorm(female_systolic, main = "Q-Q Plot - Female Group")
  qqline(female_systolic, col = "red", lwd = 2)
  grid()
  
  # Density plots
  plot(density(male_systolic), col = "blue", lwd = 2, 
       main = "Density Plot Comparison",
       xlab = "Systolic Blood Pressure",
       xlim = range(c(male_systolic, female_systolic)))
  lines(density(female_systolic), col = "red", lwd = 2)
  legend("topright", legend = c("Male", "Female"), 
         col = c("blue", "red"), lwd = 2)
  grid()
  
  # Strip chart with means
  stripchart(systolic ~ gender, data = clean_data, 
             method = "jitter", vertical = TRUE,
             main = "Strip Chart with Means",
             xlab = "Gender", ylab = "Systolic Blood Pressure",
             col = c("blue", "red"), pch = 19)
  
  # Add mean lines
  male_mean <- mean(male_systolic)
  female_mean <- mean(female_systolic)
  segments(0.8, male_mean, 1.2, male_mean, col = "blue", lwd = 3)
  segments(1.8, female_mean, 2.2, female_mean, col = "red", lwd = 3)
  
  legend("topright", 
         legend = c(paste("Male mean:", round(male_mean, 2)),
                   paste("Female mean:", round(female_mean, 2))),
         col = c("blue", "red"), lwd = 3)
  grid()
  
  dev.off()
  
  # Additional normality tests
  cat("\nNormality Tests:\n")
  
  # Shapiro-Wilk tests for normality (if sample size <= 5000)
  if (length(male_systolic) <= 5000) {
    shapiro_male <- shapiro.test(male_systolic)
    cat("Male group - Shapiro-Wilk p-value:", round(shapiro_male$p.value, 4), "\n")
  }
  
  if (length(female_systolic) <= 5000) {
    shapiro_female <- shapiro.test(female_systolic)
    cat("Female group - Shapiro-Wilk p-value:", round(shapiro_female$p.value, 4), "\n")
  }
  
  # Anderson-Darling tests
  ad_male <- ad.test(male_systolic)
  ad_female <- ad.test(female_systolic)
  cat("Male group - Anderson-Darling p-value:", round(ad_male$p.value, 4), "\n")
  cat("Female group - Anderson-Darling p-value:", round(ad_female$p.value, 4), "\n")
  
  # Conclusion
  cat("\nConclusion:\n")
  alpha <- 0.05
  if (t_test_result$p.value < alpha) {
    cat("At α =", alpha, ", we reject the null hypothesis (p-value =", 
        round(t_test_result$p.value, 4), "<", alpha, ")\n")
    cat("There is sufficient evidence to conclude that there is a significant difference in systolic blood pressure between males and females\n")
  } else {
    cat("At α =", alpha, ", we fail to reject the null hypothesis (p-value =", 
        round(t_test_result$p.value, 4), "≥", alpha, ")\n")
    cat("There is insufficient evidence to conclude that there is a significant difference in systolic blood pressure between males and females\n")
  }
  
  # Interpretation of effect size
  cat("\nEffect Size Interpretation:\n")
  if (abs(cohens_d) < 0.2) {
    cat("Small effect size\n")
  } else if (abs(cohens_d) < 0.5) {
    cat("Small to medium effect size\n")
  } else if (abs(cohens_d) < 0.8) {
    cat("Medium to large effect size\n")
  } else {
    cat("Large effect size\n")
  }
  
} else if ("exercise" %in% names(data)) {
  # Alternative analysis: ANOVA for exercise groups
  cat("\nAlternative Analysis: ANOVA for Exercise Groups\n")
  
  clean_data <- data[complete.cases(data[c("systolic", "exercise")]), ]
  
  # Perform one-way ANOVA
  anova_result <- aov(systolic ~ factor(exercise), data = clean_data)
  anova_summary <- summary(anova_result)
  
  cat("One-way ANOVA results:\n")
  print(anova_summary)
  
  # Group statistics
  cat("\nGroup Statistics:\n")
  group_stats <- aggregate(systolic ~ exercise, data = clean_data, 
                          function(x) c(n = length(x), mean = mean(x), sd = sd(x)))
  print(group_stats)
  
  # Create visualization for ANOVA
  png('/Users/devvrathans/stats-assignment/assignment05/assignment05_problem02_anova_r_output.png', 
      width = 1200, height = 800, res = 150)
  
  par(mfrow = c(2, 2))
  
  # Box plot by exercise level
  boxplot(systolic ~ exercise, data = clean_data,
          main = "Systolic BP by Exercise Level",
          xlab = "Exercise Level", ylab = "Systolic Blood Pressure",
          col = rainbow(length(unique(clean_data$exercise))))
  grid()
  
  # Means plot
  means <- tapply(clean_data$systolic, clean_data$exercise, mean)
  plot(names(means), means, type = "b", pch = 19, 
       main = "Mean Systolic BP by Exercise Level",
       xlab = "Exercise Level", ylab = "Mean Systolic BP")
  grid()
  
  # Residuals vs fitted
  plot(anova_result, which = 1)
  
  # Q-Q plot of residuals
  plot(anova_result, which = 2)
  
  dev.off()
  
  # Post-hoc tests if significant
  if (anova_summary[[1]]$`Pr(>F)`[1] < 0.05) {
    cat("\nPost-hoc pairwise comparisons (Tukey HSD):\n")
    tukey_result <- TukeyHSD(anova_result)
    print(tukey_result)
  }
  
} else {
  cat("Required variables not available in the dataset.\n")
  cat("Please verify the problem requirements and dataset.\n")
}

cat("\n", rep("=", 50), "\n", sep="")
cat("Problem 2 Analysis Complete\n")
cat(rep("=", 50), "\n", sep="")
