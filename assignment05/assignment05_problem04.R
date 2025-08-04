# Assignment 5 - Problem 4
# Statistical Analysis Solution in R

# Load required libraries
library(ggplot2)
library(dplyr)
library(stats)
library(vcd)  # for advanced categorical data analysis
library(corrplot)
library(RColorBrewer)

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
    gender = sample(c('M', 'F'), n, replace = TRUE),
    smoking.status = sample(0:2, n, replace = TRUE),
    exercise = sample(0:3, n, replace = TRUE),
    marital.status = sample(c('S', 'M', 'D', 'W'), n, replace = TRUE),
    ed.level = sample(0:3, n, replace = TRUE)
  )
})

# Problem 4: Chi-square tests and categorical data analysis
# Example: Testing independence between categorical variables

if ("gender" %in% names(data) && "smoking.status" %in% names(data)) {
  # Clean data - remove missing values
  clean_data <- data[complete.cases(data[c("gender", "smoking.status")]), ]
  
  cat("\nChi-square Test of Independence\n")
  cat("Variables: Gender vs Smoking Status\n")
  cat("Sample size:", nrow(clean_data), "\n")
  
  # Create contingency table
  contingency_table <- table(clean_data$gender, clean_data$smoking.status)
  cat("\nContingency Table:\n")
  print(contingency_table)
  
  # Add row and column totals
  contingency_with_margins <- addmargins(contingency_table)
  cat("\nContingency Table with Totals:\n")
  print(contingency_with_margins)
  
  # Perform chi-square test
  chi2_test <- chisq.test(contingency_table)
  
  cat("\nChi-square Test Results:\n")
  cat("Chi-square statistic:", round(chi2_test$statistic, 4), "\n")
  cat("p-value:", round(chi2_test$p.value, 4), "\n")
  cat("Degrees of freedom:", chi2_test$parameter, "\n")
  
  cat("\nExpected Frequencies:\n")
  print(round(chi2_test$expected, 4))
  
  # Check assumptions
  cat("\nAssumption Check:\n")
  min_expected <- min(chi2_test$expected)
  cells_less_than_5 <- sum(chi2_test$expected < 5)
  total_cells <- length(chi2_test$expected)
  percentage_less_than_5 <- (cells_less_than_5 / total_cells) * 100
  
  cat("Minimum expected frequency:", round(min_expected, 4), "\n")
  cat("Cells with expected frequency < 5:", cells_less_than_5, "out of", total_cells, 
      "(", round(percentage_less_than_5, 1), "%)\n")
  
  if (min_expected >= 5 && percentage_less_than_5 <= 20) {
    cat("Chi-square test assumptions are satisfied\n")
  } else {
    cat("Warning: Chi-square test assumptions may be violated\n")
  }
  
  # Calculate effect size (Cramér's V)
  n <- sum(contingency_table)
  cramers_v <- sqrt(chi2_test$statistic / (n * (min(dim(contingency_table)) - 1)))
  cat("\nEffect size (Cramér's V):", round(cramers_v, 4), "\n")
  
  # Create comprehensive visualization
  png('/Users/devvrathans/stats-assignment/assignment05/assignment05_problem04_r_output.png', 
      width = 1500, height = 1200, res = 150)
  
  par(mfrow = c(3, 3))
  
  # Grouped bar chart
  barplot(contingency_table, beside = TRUE, 
          main = "Grouped Bar Chart: Gender vs Smoking Status",
          xlab = "Smoking Status", ylab = "Count",
          legend.text = TRUE, 
          col = c("lightblue", "lightpink"),
          args.legend = list(title = "Gender"))
  grid()
  
  # Stacked bar chart
  barplot(contingency_table, 
          main = "Stacked Bar Chart: Gender vs Smoking Status",
          xlab = "Smoking Status", ylab = "Count",
          legend.text = TRUE,
          col = c("lightblue", "lightpink"),
          args.legend = list(title = "Gender"))
  grid()
  
  # Mosaic plot
  mosaicplot(contingency_table, 
             main = "Mosaic Plot: Gender vs Smoking Status",
             color = c("lightblue", "lightpink", "lightgreen"))
  
  # Spine plot
  spineplot(contingency_table, 
            main = "Spine Plot: Gender vs Smoking Status")
  
  # Heatmap-like visualization of observed frequencies
  color_palette <- colorRampPalette(c("white", "blue"))(100)
  image(1:ncol(contingency_table), 1:nrow(contingency_table), 
        t(contingency_table), 
        col = color_palette,
        main = "Heatmap: Observed Frequencies",
        xlab = "Smoking Status", ylab = "Gender",
        axes = FALSE)
  
  # Add text annotations
  for (i in 1:nrow(contingency_table)) {
    for (j in 1:ncol(contingency_table)) {
      text(j, i, contingency_table[i, j], cex = 1.2)
    }
  }
  axis(1, at = 1:ncol(contingency_table), labels = colnames(contingency_table))
  axis(2, at = 1:nrow(contingency_table), labels = rownames(contingency_table))
  
  # Heatmap of expected frequencies
  image(1:ncol(contingency_table), 1:nrow(contingency_table), 
        t(chi2_test$expected), 
        col = colorRampPalette(c("white", "orange"))(100),
        main = "Heatmap: Expected Frequencies",
        xlab = "Smoking Status", ylab = "Gender",
        axes = FALSE)
  
  # Add text annotations
  for (i in 1:nrow(contingency_table)) {
    for (j in 1:ncol(contingency_table)) {
      text(j, i, round(chi2_test$expected[i, j], 1), cex = 1.2)
    }
  }
  axis(1, at = 1:ncol(contingency_table), labels = colnames(contingency_table))
  axis(2, at = 1:nrow(contingency_table), labels = rownames(contingency_table))
  
  # Residuals visualization
  residuals <- (contingency_table - chi2_test$expected) / sqrt(chi2_test$expected)
  image(1:ncol(contingency_table), 1:nrow(contingency_table), 
        t(residuals), 
        col = colorRampPalette(c("red", "white", "blue"))(100),
        main = "Standardized Residuals",
        xlab = "Smoking Status", ylab = "Gender",
        axes = FALSE)
  
  # Add text annotations
  for (i in 1:nrow(contingency_table)) {
    for (j in 1:ncol(contingency_table)) {
      text(j, i, round(residuals[i, j], 2), cex = 1.2)
    }
  }
  axis(1, at = 1:ncol(contingency_table), labels = colnames(contingency_table))
  axis(2, at = 1:nrow(contingency_table), labels = rownames(contingency_table))
  
  # Proportion plots
  prop_table <- prop.table(contingency_table, 1)  # Row proportions
  barplot(t(prop_table), beside = FALSE,
          main = "Proportions within Gender",
          xlab = "Gender", ylab = "Proportion",
          legend.text = TRUE,
          col = rainbow(ncol(contingency_table)),
          args.legend = list(title = "Smoking Status"))
  
  prop_table_col <- prop.table(contingency_table, 2)  # Column proportions
  barplot(prop_table_col, beside = FALSE,
          main = "Proportions within Smoking Status",
          xlab = "Smoking Status", ylab = "Proportion",
          legend.text = TRUE,
          col = c("lightblue", "lightpink"),
          args.legend = list(title = "Gender"))
  
  dev.off()
  
  # Additional analysis - Detailed percentages
  cat("\nColumn Percentages (within smoking status):\n")
  col_percentages <- prop.table(contingency_table, 2) * 100
  print(round(col_percentages, 2))
  
  # Row percentages
  cat("\nRow Percentages (within gender):\n")
  row_percentages <- prop.table(contingency_table, 1) * 100
  print(round(row_percentages, 2))
  
  # Post-hoc analysis if significant
  if (chi2_test$p.value < 0.05) {
    cat("\nPost-hoc Analysis (Standardized Residuals):\n")
    cat("Values > |2| indicate significant deviations from independence\n")
    print(round(residuals, 3))
    
    # Find significant cells
    significant_cells <- which(abs(residuals) > 2, arr.ind = TRUE)
    if (nrow(significant_cells) > 0) {
      cat("\nSignificant deviations (|residual| > 2):\n")
      for (i in 1:nrow(significant_cells)) {
        row_idx <- significant_cells[i, 1]
        col_idx <- significant_cells[i, 2]
        gender <- rownames(contingency_table)[row_idx]
        smoking <- colnames(contingency_table)[col_idx]
        residual <- residuals[row_idx, col_idx]
        cat("Gender", gender, ", Smoking", smoking, ": residual =", round(residual, 3), "\n")
      }
    }
  }
  
  # Alternative tests if assumptions are violated
  if (min_expected < 5 || percentage_less_than_5 > 20) {
    cat("\nFisher's Exact Test (alternative when assumptions violated):\n")
    if (nrow(contingency_table) == 2 && ncol(contingency_table) == 2) {
      fisher_test <- fisher.test(contingency_table)
      cat("Fisher's exact test p-value:", round(fisher_test$p.value, 4), "\n")
      cat("Odds ratio:", round(fisher_test$estimate, 4), "\n")
      cat("95% CI for odds ratio: [", round(fisher_test$conf.int[1], 4), ",", 
          round(fisher_test$conf.int[2], 4), "]\n")
    }
  }
  
  # Conclusion
  cat("\nConclusion:\n")
  alpha <- 0.05
  if (chi2_test$p.value < alpha) {
    cat("At α =", alpha, ", we reject the null hypothesis (p-value =", 
        round(chi2_test$p.value, 4), "<", alpha, ")\n")
    cat("There is sufficient evidence to conclude that gender and smoking status are dependent/associated\n")
  } else {
    cat("At α =", alpha, ", we fail to reject the null hypothesis (p-value =", 
        round(chi2_test$p.value, 4), "≥", alpha, ")\n")
    cat("There is insufficient evidence to conclude that gender and smoking status are dependent/associated\n")
  }
  
  # Effect size interpretation
  cat("\nEffect Size Interpretation (Cramér's V):\n")
  if (cramers_v < 0.1) {
    cat("Negligible association\n")
  } else if (cramers_v < 0.3) {
    cat("Small association\n")
  } else if (cramers_v < 0.5) {
    cat("Medium association\n")
  } else {
    cat("Large association\n")
  }
  
} else if ("exercise" %in% names(data) && "marital.status" %in% names(data)) {
  # Alternative analysis: Exercise vs Marital Status
  cat("\nAlternative Analysis: Exercise vs Marital Status\n")
  
  clean_data <- data[complete.cases(data[c("exercise", "marital.status")]), ]
  
  # Create contingency table
  contingency_table <- table(clean_data$exercise, clean_data$marital.status)
  cat("\nContingency Table:\n")
  print(contingency_table)
  
  # Perform chi-square test
  chi2_test <- chisq.test(contingency_table)
  
  cat("\nChi-square Test Results:\n")
  cat("Chi-square statistic:", round(chi2_test$statistic, 4), "\n")
  cat("p-value:", round(chi2_test$p.value, 4), "\n")
  cat("Degrees of freedom:", chi2_test$parameter, "\n")
  
  # Effect size
  n <- sum(contingency_table)
  cramers_v <- sqrt(chi2_test$statistic / (n * (min(dim(contingency_table)) - 1)))
  cat("Effect size (Cramér's V):", round(cramers_v, 4), "\n")
  
} else if ("gender" %in% names(data)) {
  # Goodness of fit test for single categorical variable
  cat("\nGoodness of Fit Test: Gender Distribution\n")
  
  gender_counts <- table(data$gender)
  cat("\nObserved frequencies:\n")
  print(gender_counts)
  
  # Test against equal proportions
  expected_equal <- rep(sum(gender_counts) / length(gender_counts), length(gender_counts))
  
  chi2_test <- chisq.test(gender_counts, p = rep(1/length(gender_counts), length(gender_counts)))
  
  cat("\nGoodness of Fit Test (against equal proportions):\n")
  cat("Chi-square statistic:", round(chi2_test$statistic, 4), "\n")
  cat("p-value:", round(chi2_test$p.value, 4), "\n")
  cat("Degrees of freedom:", chi2_test$parameter, "\n")
  
} else {
  cat("Required categorical variables not available in the dataset.\n")
  cat("Please verify the problem requirements and dataset.\n")
}

cat("\n", rep("=", 50), "\n", sep="")
cat("Problem 4 Analysis Complete\n")
cat(rep("=", 50), "\n", sep="")
