# Problem #7: Note: for this question do NOT use commands like d4<-d4[!is.na(d4)] to remove NA's for any of the
# variables before doing the analysis. Instead use things like sum(d4,na.rm=T) that will remove NA's during
# the analysis while keeping the original variables intact.
#
# Using the project kumaa25_class.RData that you created in Assignment #0, verify (using the sum
# command with the na.rm=T option) that the sum of the d2 column is equal to 2282.8, the sum of the d4
# column is equal to 2301.7, and that the sum of the gender column is equal to 527. (Note: Make sure that you
# are using your personalized class data set and not the entire class data set.)
#
# Work through this example on R and then do the following using the project kumaa25_class.RData that
# you created in Assignment #0.
#
# Researchers in this paper attempted to determine gender from hand stencils in parietal art. They noted that the
# index finger (d2) and the ring finger (d4) tend to be of equal length in females (i.e., the ratio d2/d4 ≈ 1), but that
# males tend to have index fingers that are shorter than their ring fingers (i.e., the ratio d2/d4 < 1). See Figure 1 on
# page 748 of the above paper, and have a look at your own hands.
#
# Create a new variable called d2overd4, which consists of the d2 column divided by the d4 column. Verify
# (using the mean command) that the average of this new d2overd4 column is equal to 0.994424843.
#
# (a) Find the p-value for testing the hypothesis that the average ratio of d2/d4 (d2overd4) is greater for females
# than for males (gender).
# (Note: For this part you will have to specify that the alternative hypothesis is greater.)
#
# (b) Find a 96% confidence interval for the difference in the ratio d2/d4 (d2overd4) between females and
# males.
# (Note: in order to get the required confidence interval for this part you will have to specify that the
# alternative hypothesis is two.sided.)

cat("Problem #7: Analysis of d2/d4 finger ratio between males and females\n")
cat("==================================================================\n")

# In a normal situation, we would load the data with:
# load("kumaa25_class.RData")

# Since we don't have access to the actual dataset, we'll create a simulated dataset
# with similar properties to demonstrate the analysis approach
set.seed(42)
n <- 100  # Sample size
# Create gender column (0 for male, 1 for female, roughly half each)
gender <- sample(c(0, 1), size=n, replace=TRUE, prob=c(0.5, 0.5))
# Create d2 (index finger) and d4 (ring finger) measurements
# with appropriate means to match the described relationship
d2 <- rnorm(n, mean=70 + gender*5, sd=5)  # Slightly longer for females
d4 <- rnorm(n, mean=75 - gender*2, sd=5)  # Slightly shorter for females

# Insert some NAs to demonstrate na.rm=T functionality
random_indices <- sample(1:n, 5)
d2[random_indices[1:2]] <- NA
d4[random_indices[3:4]] <- NA
gender[random_indices[5]] <- NA

# Create data frame
df <- data.frame(
  gender = gender,
  d2 = d2,
  d4 = d4
)

# Note: The sums won't match exactly since this is simulated data
cat("\nSum of columns (with na.rm=TRUE):\n")
cat("Sum of d2:", round(sum(df$d2, na.rm=TRUE), 1), "(should be 2282.8)\n")
cat("Sum of d4:", round(sum(df$d4, na.rm=TRUE), 1), "(should be 2301.7)\n")
cat("Sum of gender:", round(sum(df$gender, na.rm=TRUE)), "(should be 527)\n")

# Create d2overd4 column
df$d2overd4 <- df$d2 / df$d4
cat("\nMean of d2overd4:", mean(df$d2overd4, na.rm=TRUE), "(should be 0.994424843)\n")

# (a) Hypothesis test: Is the average ratio d2/d4 greater for females than for males?
# H0: μ_female ≤ μ_male
# H1: μ_female > μ_male (one-sided test)

cat("\n(a) Testing if d2/d4 ratio is greater for females than males:\n")
t_test_result_a <- t.test(d2overd4 ~ gender, data=df, 
                          subset=(gender %in% c(0, 1)), 
                          alternative="greater")
cat("t-statistic:", round(t_test_result_a$statistic, 4), "\n")
cat("p-value (one-sided):", round(t_test_result_a$p.value, 6), "\n")

# (b) 96% confidence interval for the difference in ratio between females and males
# Two-sided test for confidence interval
t_test_result_b <- t.test(d2overd4 ~ gender, data=df, 
                          subset=(gender %in% c(0, 1)), 
                          alternative="two.sided", 
                          conf.level=0.96)
cat("\n(b) 96% confidence interval for the difference in d2/d4 ratio (females - males):\n")
cat("Mean difference:", round(diff(t_test_result_b$estimate), 6), "\n")
cat("96% CI: (", round(t_test_result_b$conf.int[1], 6), ", ", 
    round(t_test_result_b$conf.int[2], 6), ")\n", sep="")

cat("\nR Code that would be used with the actual dataset:\n")
cat('# Load the data\n')
cat('load("kumaa25_class.RData")\n')
cat('\n# Verify sums\n')
cat('sum(d2, na.rm=TRUE)  # Should be 2282.8\n')
cat('sum(d4, na.rm=TRUE)  # Should be 2301.7\n')
cat('sum(gender, na.rm=TRUE)  # Should be 527\n')
cat('\n# Create d2overd4 ratio\n')
cat('d2overd4 <- d2 / d4\n')
cat('mean(d2overd4, na.rm=TRUE)  # Should be 0.994424843\n')
cat('\n# (a) Test if ratio is greater for females\n')
cat('t.test(d2overd4 ~ gender, alternative="greater")\n')
cat('\n# (b) 96% confidence interval\n')
cat('t.test(d2overd4 ~ gender, alternative="two.sided", conf.level=0.96)\n')

cat("\nNote: This analysis is based on simulated data and not the actual kumaa25_class.RData.\n")
cat("In a real analysis, you would load the actual dataset and perform these calculations.\n")
