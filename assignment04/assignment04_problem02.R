# Problem #2: A genetic model suggests that 80% of plants grown from a cross between two given strains of seeds will be of the
# dwarf variety. After breeding a sample of these plants, 155 were observed to be of the dwarf variety.
#
# Suppose that we do a hypothesis test to see if the sample results strongly contradict the genetic model and find
# the p-value to be 0.0196. What is the meaning of this p-value?

# Problem 2: Understanding the p-value

# Given information
p0 <- 0.80  # genetic model suggests 80% dwarf variety
observed <- 155  # observed number of dwarf variety plants
p_value <- 0.0196  # given p-value

# To find the sample size, we need to calculate it based on the p-value
# Let's try a range of possible sample sizes

cat("Problem 2: Understanding the p-value\n")
cat("====================================\n")

cat("\nGiven information:\n")
cat("- Genetic model suggests", p0*100, "% of plants will be dwarf variety\n")
cat("- Observed number of dwarf plants:", observed, "\n")
cat("- Reported p-value:", p_value, "\n")

# Let's assume this is a two-tailed test and verify the sample size
cat("\nVerifying the sample size and p-value:\n")

for (n in 180:220) {
  expected <- n * p0  # expected number of dwarf plants
  # Calculate test statistic for a proportion test
  z_score <- (observed - expected) / sqrt(n * p0 * (1-p0))
  # Calculate p-value (two-tailed)
  calculated_p <- 2 * min(pnorm(z_score), 1 - pnorm(z_score))
  
  if (abs(calculated_p - p_value) < 0.001) {  # close match to given p-value
    cat("Sample size n =", n, "\n")
    cat("Expected dwarf plants:", expected, "\n")
    cat("Observed dwarf plants:", observed, "\n")
    cat("z-score:", round(z_score, 4), "\n")
    cat("Calculated p-value:", round(calculated_p, 4), "\n")
    break
  }
}

# Alternative method using prop.test
# We can verify using prop.test once we have the sample size
if (exists("n")) {
  prop_test <- prop.test(observed, n, p = p0, alternative = "two.sided", correct = FALSE)
  cat("\nUsing prop.test:\n")
  cat("p-value:", round(prop_test$p.value, 4), "\n")
}

# Interpretation of the p-value
cat("\nInterpretation of the p-value:\n")
cat("The p-value of 0.0196 is the probability of observing a result at least as extreme as\n")
cat("155 dwarf plants (or fewer) if the genetic model (80% dwarf plants) is true.\n")
cat("Since this probability is small (less than 0.05), it suggests that the observed result\n")
cat("is unlikely under the null hypothesis, providing evidence against the genetic model.\n")
cat("\nThe most accurate interpretation is:\n")
cat("If the genetic model is correct (80% dwarf variety), it is very unlikely (probability 0.0196)\n")
cat("to observe 155 or fewer dwarf plants in this sample size.\n")
