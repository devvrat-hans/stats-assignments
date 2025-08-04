# Assignment 5 - Problem 7
# Statistical Analysis Solution in R

# Load required libraries
library(ggplot2)
library(dplyr)
library(stats)

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
    serum.chol = rnorm(n, 200, 40),
    exercise = sample(0:3, n, replace = TRUE),
    gender = sample(c('M', 'F'), n, replace = TRUE),
    before = rnorm(n, 120, 20)
  )
  # Create 'after' correlated with 'before' for realistic paired data
  data$after <<- data$before + rnorm(n, -5, 10)
})

# Problem 7: Non-parametric Tests
# When assumptions for parametric tests are violated, use non-parametric alternatives

cat("\nNon-parametric Statistical Tests\n")
cat(rep("=", 50), "\n", sep="")

# Case 1: Non-parametric alternative to paired t-test (Wilcoxon signed-rank test)
if ("before" %in% names(data) && "after" %in% names(data)) {
  # Clean data - remove missing values
  paired_data <- data[complete.cases(data[c("before", "after")]), ]
  before <- paired_data$before
  after <- paired_data$after
  differences <- after - before
  
  cat("\n1. WILCOXON SIGNED-RANK TEST (Paired Data)\n")
  cat("Non-parametric alternative to paired t-test\n")
  cat("H0: Median difference = 0\n")
  cat("H1: Median difference ≠ 0\n")
  cat("Sample size:", nrow(paired_data), "\n")
  
  # Descriptive statistics
  cat("\nDescriptive Statistics:\n")
  cat("Before - Mean:", round(mean(before), 4), ", Median:", round(median(before), 4), "\n")
  cat("After - Mean:", round(mean(after), 4), ", Median:", round(median(after), 4), "\n")
  cat("Differences - Mean:", round(mean(differences), 4), ", Median:", round(median(differences), 4), "\n")
  
  # Perform Wilcoxon signed-rank test
  wilcoxon_test <- wilcox.test(before, after, paired = TRUE, alternative = "two.sided")
  
  cat("\nWilcoxon Signed-Rank Test Results:\n")
  cat("Test statistic (V):", wilcoxon_test$statistic, "\n")
  cat("p-value:", round(wilcoxon_test$p.value, 4), "\n")
  
  # For comparison, also perform paired t-test
  t_test_paired <- t.test(before, after, paired = TRUE)
  cat("\nFor comparison - Paired t-test:\n")
  cat("t-statistic:", round(t_test_paired$statistic, 4), "\n")
  cat("p-value:", round(t_test_paired$p.value, 4), "\n")
  
  # Effect size for Wilcoxon (r = Z / sqrt(N))
  n_pairs <- length(differences)
  z_score <- qnorm(1 - wilcoxon_test$p.value/2)  # Approximate z-score
  effect_size_r <- z_score / sqrt(n_pairs)
  cat("Effect size (r):", round(effect_size_r, 4), "\n")
}

# Case 2: Non-parametric alternative to independent t-test (Mann-Whitney U test)
if ("systolic" %in% names(data) && "gender" %in% names(data)) {
  cat("\n2. MANN-WHITNEY U TEST (Independent Groups)\n")
  cat("Non-parametric alternative to independent t-test\n")
  cat("H0: Distributions are identical\n")
  cat("H1: Distributions differ in location\n")
  
  # Clean data
  clean_data <- data[complete.cases(data[c("systolic", "gender")]), ]
  male_systolic <- clean_data$systolic[clean_data$gender == "M"]
  female_systolic <- clean_data$systolic[clean_data$gender == "F"]
  
  cat("\nSample sizes: Male =", length(male_systolic), ", Female =", length(female_systolic), "\n")
  
  # Descriptive statistics
  cat("\nDescriptive Statistics:\n")
  cat("Male - Mean:", round(mean(male_systolic), 4), ", Median:", round(median(male_systolic), 4), "\n")
  cat("Female - Mean:", round(mean(female_systolic), 4), ", Median:", round(median(female_systolic), 4), "\n")
  
  # Perform Mann-Whitney U test (Wilcoxon rank-sum test in R)
  mw_test <- wilcox.test(male_systolic, female_systolic, alternative = "two.sided")
  
  cat("\nMann-Whitney U Test Results:\n")
  cat("W statistic:", mw_test$statistic, "\n")
  cat("p-value:", round(mw_test$p.value, 4), "\n")
  
  # For comparison, also perform independent t-test
  t_test_ind <- t.test(male_systolic, female_systolic)
  cat("\nFor comparison - Independent t-test:\n")
  cat("t-statistic:", round(t_test_ind$statistic, 4), "\n")
  cat("p-value:", round(t_test_ind$p.value, 4), "\n")
  
  # Effect size for Mann-Whitney (r = Z / sqrt(N))
  n_total <- length(male_systolic) + length(female_systolic)
  z_score_u <- qnorm(1 - mw_test$p.value/2)
  effect_size_r_u <- z_score_u / sqrt(n_total)
  cat("Effect size (r):", round(effect_size_r_u, 4), "\n")
}

# Case 3: Non-parametric alternative to one-way ANOVA (Kruskal-Wallis test)
if ("systolic" %in% names(data) && "exercise" %in% names(data)) {
  cat("\n3. KRUSKAL-WALLIS TEST (Multiple Groups)\n")
  cat("Non-parametric alternative to one-way ANOVA\n")
  cat("H0: All group distributions are identical\n")
  cat("H1: At least one group differs\n")
  
  # Clean data
  clean_data <- data[complete.cases(data[c("systolic", "exercise")]), ]
  clean_data$exercise <- factor(clean_data$exercise)
  
  # Group statistics
  group_stats <- aggregate(systolic ~ exercise, data = clean_data, 
                          function(x) c(n = length(x), mean = mean(x), median = median(x)))
  
  cat("\nNumber of groups:", length(unique(clean_data$exercise)), "\n")
  cat("Group sizes:", table(clean_data$exercise), "\n")
  
  # Descriptive statistics by group
  cat("\nDescriptive Statistics by Exercise Level:\n")
  print(group_stats)
  
  # Perform Kruskal-Wallis test
  kw_test <- kruskal.test(systolic ~ exercise, data = clean_data)
  
  cat("\nKruskal-Wallis Test Results:\n")
  cat("Chi-square statistic:", round(kw_test$statistic, 4), "\n")
  cat("p-value:", round(kw_test$p.value, 4), "\n")
  cat("Degrees of freedom:", kw_test$parameter, "\n")
  
  # For comparison, also perform one-way ANOVA
  anova_test <- aov(systolic ~ exercise, data = clean_data)
  anova_summary <- summary(anova_test)
  f_stat <- anova_summary[[1]]$`F value`[1]
  f_p <- anova_summary[[1]]$`Pr(>F)`[1]
  
  cat("\nFor comparison - One-way ANOVA:\n")
  cat("F-statistic:", round(f_stat, 4), "\n")
  cat("p-value:", round(f_p, 4), "\n")
  
  # Effect size for Kruskal-Wallis (eta-squared analog)
  n_total <- nrow(clean_data)
  k <- length(unique(clean_data$exercise))
  eta_squared_kw <- (kw_test$statistic - k + 1) / (n_total - k)
  cat("Effect size (η² analog):", round(eta_squared_kw, 4), "\n")
}

# Create comprehensive visualization
png('/Users/devvrathans/stats-assignment/assignment05/assignment05_problem07_r_output.png', 
    width = 1500, height = 1200, res = 150)

par(mfrow = c(2, 3))

# Visualization for paired data (if available)
if ("before" %in% names(data) && "after" %in% names(data)) {
  # Scatter plot
  plot(before, after, 
       main = "Before vs After (Paired Data)",
       xlab = "Before", ylab = "After",
       pch = 19, col = "blue", cex = 0.8)
  abline(a = 0, b = 1, col = "red", lty = 2)
  grid()
  
  # Histogram of differences
  hist(differences, breaks = 20, 
       main = "Distribution of Differences",
       xlab = "Difference (After - Before)", ylab = "Frequency",
       col = "skyblue", border = "black")
  abline(v = median(differences), col = "red", lty = 2, lwd = 2)
  legend("topright", legend = paste("Median:", round(median(differences), 2)), 
         col = "red", lty = 2)
  grid()
}

# Visualization for independent groups (if available)
if ("systolic" %in% names(data) && "gender" %in% names(data)) {
  # Box plot
  boxplot(systolic ~ gender, data = clean_data,
          main = "Box Plot by Gender",
          xlab = "Gender", ylab = "Systolic Blood Pressure",
          col = c("lightblue", "lightpink"))
  grid()
  
  # Density plots
  male_density <- density(male_systolic)
  female_density <- density(female_systolic)
  
  plot(male_density, 
       main = "Distribution by Gender",
       xlab = "Systolic Blood Pressure", ylab = "Density",
       col = "blue", lwd = 2,
       xlim = range(c(male_systolic, female_systolic)))
  lines(female_density, col = "red", lwd = 2)
  legend("topright", legend = c("Male", "Female"), 
         col = c("blue", "red"), lwd = 2)
  grid()
}

# Visualization for multiple groups (if available)
if ("systolic" %in% names(data) && "exercise" %in% names(data)) {
  # Box plot
  boxplot(systolic ~ exercise, data = clean_data,
          main = "Box Plot by Exercise Level",
          xlab = "Exercise Level", ylab = "Systolic Blood Pressure",
          col = rainbow(length(unique(clean_data$exercise))))
  grid()
  
  # Group medians plot
  group_medians <- tapply(clean_data$systolic, clean_data$exercise, median)
  exercise_levels <- as.numeric(names(group_medians))
  
  plot(exercise_levels, group_medians, type = "b", 
       main = "Group Medians",
       xlab = "Exercise Level", ylab = "Median Systolic BP",
       pch = 19, cex = 1.5, lwd = 2)
  grid()
}

dev.off()

# Additional non-parametric tests and considerations
cat("\n4. ADDITIONAL NON-PARAMETRIC CONSIDERATIONS\n")
cat(rep("=", 50), "\n", sep="")

# Sign test (simpler alternative to Wilcoxon)
if ("before" %in% names(data) && "after" %in% names(data)) {
  cat("\nSign Test (alternative to Wilcoxon):\n")
  positive_diff <- sum(differences > 0)
  negative_diff <- sum(differences < 0)
  zero_diff <- sum(differences == 0)
  
  cat("Positive differences:", positive_diff, "\n")
  cat("Negative differences:", negative_diff, "\n")
  cat("Zero differences:", zero_diff, "(excluded from test)\n")
  
  # Sign test using binomial distribution
  n_non_zero <- positive_diff + negative_diff
  sign_p <- 2 * pbinom(min(positive_diff, negative_diff), n_non_zero, 0.5)
  cat("Sign test p-value:", round(sign_p, 4), "\n")
}

# Post-hoc tests for Kruskal-Wallis (if significant)
if ("systolic" %in% names(data) && "exercise" %in% names(data)) {
  if (exists("kw_test") && kw_test$p.value < 0.05) {
    cat("\nPost-hoc Analysis for Kruskal-Wallis (Dunn's test approximation):\n")
    
    # Pairwise Wilcoxon tests with Bonferroni correction
    pairwise_result <- pairwise.wilcox.test(clean_data$systolic, clean_data$exercise, 
                                           p.adjust.method = "bonferroni")
    cat("Pairwise Wilcoxon tests with Bonferroni correction:\n")
    print(pairwise_result$p.value)
  }
}

# Assumptions and when to use non-parametric tests
cat("\nWhen to Use Non-parametric Tests:\n")
cat("1. Data is not normally distributed\n")
cat("2. Ordinal data or ranked data\n")
cat("3. Small sample sizes\n")
cat("4. Presence of outliers\n")
cat("5. Unequal variances that cannot be corrected\n")

# Advantages and disadvantages
cat("\nAdvantages of Non-parametric Tests:\n")
cat("- No assumption of normality\n")
cat("- Robust to outliers\n")
cat("- Can handle ordinal data\n")
cat("- Often simpler to compute\n")

cat("\nDisadvantages of Non-parametric Tests:\n")
cat("- Generally less powerful than parametric tests\n")
cat("- May require larger sample sizes\n")
cat("- Limited availability of post-hoc tests\n")
cat("- Less informative about effect magnitude\n")

# Summary of all test results
cat("\n5. SUMMARY OF TEST RESULTS\n")
cat(rep("=", 50), "\n", sep="")

if ("before" %in% names(data) && "after" %in% names(data)) {
  cat("\nPaired Data Analysis:\n")
  if (exists("wilcoxon_test")) {
    cat("Wilcoxon signed-rank test: p =", round(wilcoxon_test$p.value, 4), "\n")
    cat("Paired t-test: p =", round(t_test_paired$p.value, 4), "\n")
    
    if (wilcoxon_test$p.value < 0.05) {
      cat("Conclusion: Significant difference detected (non-parametric)\n")
    } else {
      cat("Conclusion: No significant difference detected (non-parametric)\n")
    }
  }
}

if ("systolic" %in% names(data) && "gender" %in% names(data)) {
  cat("\nIndependent Groups Analysis:\n")
  if (exists("mw_test")) {
    cat("Mann-Whitney U test: p =", round(mw_test$p.value, 4), "\n")
    cat("Independent t-test: p =", round(t_test_ind$p.value, 4), "\n")
    
    if (mw_test$p.value < 0.05) {
      cat("Conclusion: Significant difference between groups (non-parametric)\n")
    } else {
      cat("Conclusion: No significant difference between groups (non-parametric)\n")
    }
  }
}

if ("systolic" %in% names(data) && "exercise" %in% names(data)) {
  cat("\nMultiple Groups Analysis:\n")
  if (exists("kw_test")) {
    cat("Kruskal-Wallis test: p =", round(kw_test$p.value, 4), "\n")
    cat("One-way ANOVA: p =", round(f_p, 4), "\n")
    
    if (kw_test$p.value < 0.05) {
      cat("Conclusion: Significant differences among groups (non-parametric)\n")
    } else {
      cat("Conclusion: No significant differences among groups (non-parametric)\n")
    }
  }
}

cat("\n", rep("=", 50), "\n", sep="")
cat("Problem 7 Analysis Complete\n")
cat(rep("=", 50), "\n", sep="")
