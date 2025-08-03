#!/usr/bin/env Rscript
# Assignment 3 - Questions 4 and 5 Solutions (R)

# Read the data
data <- read.csv('/Users/devvrathans/stats-assignment-01/data/kumaa25_class.txt', sep=',', header=TRUE)

# Display basic information about the dataset
cat("Dataset information:\n")
cat("Number of rows:", nrow(data), "\n")
cat("Columns:", paste(names(data), collapse=", "), "\n\n")

#################################
# Question 4
# Find a 91% confidence interval for the proportion of people who live at least 5km from campus
#################################

cat("Problem #4 Solution (R)\n")
cat(paste(rep("=", 50), collapse=""), "\n")

# Filter out rows with missing distance values
distance_data <- data$distance[!is.na(data$distance)]

# Count how many people live at least 5 km from campus
at_least_5km <- sum(distance_data >= 5)
total_people <- length(distance_data)
proportion <- at_least_5km / total_people

# Calculate 91% confidence interval for proportion
confidence_level <- 0.91
z_score <- qnorm((1 + confidence_level) / 2)
margin_error <- z_score * sqrt((proportion * (1 - proportion)) / total_people)

ci_lower <- proportion - margin_error
ci_upper <- proportion + margin_error

cat("Total people with distance data:", total_people, "\n")
cat("People living at least 5 km from campus:", at_least_5km, "\n")
cat("Proportion:", format(proportion, digits=4), "\n")
cat("91% Confidence Interval: (", format(ci_lower, digits=4), ", ", format(ci_upper, digits=4), ")\n\n", sep="")
cat("91% Confidence Interval: (", format(ci_lower), ", ", format(ci_upper), ")\n\n", sep="")

# Alternative method using prop.test
prop_test <- prop.test(x = at_least_5km, n = total_people, conf.level = 0.91, correct = FALSE)
cat("Using prop.test function:\n")
cat("91% Confidence Interval: (", 
    format(prop_test$conf.int[1], digits=4), ", ", 
    format(prop_test$conf.int[2], digits=4), ")\n\n", sep="")

cat("91% Confidence Interval: (", 
    format(prop_test$conf.int[1]), ", ", 
    format(prop_test$conf.int[2]), ")\n\n", sep="")

#################################
# Question 5
# (a) Find a 97% confidence interval for the average height (in inches)
# (b) Determine which statement is true regarding part (a)
#################################

cat("Problem #5 Solution (R)\n")
cat(paste(rep("=", 50), collapse=""), "\n")

# Filter out rows with missing height values
height_data <- data$height[!is.na(data$height)]

# Calculate mean and standard deviation
height_mean <- mean(height_data)
height_sd <- sd(height_data)
n <- length(height_data)

# Calculate 97% confidence interval using t-distribution
confidence_level <- 0.97
t_score <- qt((1 + confidence_level) / 2, df = n - 1)
margin_error <- t_score * (height_sd / sqrt(n))

ci_lower <- height_mean - margin_error
ci_upper <- height_mean + margin_error

cat("Sample size:", n, "\n")
cat("Mean height:", format(height_mean, digits=4), "inches\n")
cat("Standard deviation:", format(height_sd, digits=4), "inches\n")
cat("97% Confidence Interval: (", format(ci_lower, digits=4), ", ", format(ci_upper, digits=4), ")\n", sep="")
cat("97% Confidence Interval: (", format(ci_lower), ", ", format(ci_upper), ")\n", sep="")

# Alternative method using t.test
t_test <- t.test(height_data, conf.level = 0.97)
cat("Using t.test function:\n")
cat("97% Confidence Interval: (", 
    format(t_test$conf.int[1], digits=4), ", ", 
    format(t_test$conf.int[2], digits=4), ")\n\n", sep="")
cat("97% Confidence Interval: (", 
    format(t_test$conf.int[1]), ", ", 
    format(t_test$conf.int[2]), ")\n\n", sep="")

# Analyze the statements for question 5b
cat("Analysis of statements for 5(b):\n")
cat("(A) The population must be normal.\n")
cat("   - Not necessarily true. When sample size is large, we can use CLT.\n")
cat("(B) The population standard deviation σ must be known.\n")
cat("   - False. We estimated it with the sample standard deviation.\n")
cat("(C) The population must follow a t-distribution.\n")
cat("   - False. The t-distribution is used for the sampling distribution,\n")
cat("     not for the population distribution.\n")
cat("(D) The population cannot follow a t-distribution.\n")
cat("   - Not necessarily true. The confidence interval calculation\n")
cat("     doesn't require this constraint.\n")
cat("(E) The population does not need to be normal because the sample size is greater than 30.\n")
cat("   - True. By the Central Limit Theorem, with n > 30, we can assume\n")
cat("     the sampling distribution of the mean is approximately normal.\n")
cat("(F) The population mean must be inside the confidence interval.\n")
cat("   - False. The confidence interval is a random interval, and we cannot\n")
cat("     be certain that it contains the true population mean.\n\n")

cat("The correct answer is (E).")
