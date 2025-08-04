# Assignment 05 - Problem 07

# a) Import the databank data
databank_file <- "/Users/devvrathans/stats-assignment/data/kumaa25_databank.txt"
databank2 <- read.csv(databank_file)

# Verify the data sums
sum_age <- sum(databank2$age)
sum_weight <- sum(databank2$weight)
sum_IQ <- sum(databank2$IQ)

cat("Sum of age column:", sum_age, "\n")
cat("Sum of weight column:", sum_weight, "\n")
cat("Sum of IQ column:", sum_IQ, "\n")

# Check if the sums match the expected values
cat("\nVerification:\n")
cat("Sum of age equals 3004:", sum_age == 3004, "\n")
cat("Sum of weight equals 11878:", sum_weight == 11878, "\n")
cat("Sum of IQ equals 8817:", sum_IQ == 8817, "\n")

# Part (a): Create a multiple linear regression model 
# Let's use systolic as the response variable and several predictors
model <- lm(systolic ~ age + weight + serum.chol + smoking.status + exercise + IQ, data = databank2)
summary_model <- summary(model)

# Extract b0, b1, b2 (intercept and first two coefficients)
b0 <- coef(model)[1]  # Intercept
b1 <- coef(model)[2]  # Coefficient for age
b2 <- coef(model)[3]  # Coefficient for weight

cat("\nPart (a): b0, b1, b2\n")
cat("b0 (Intercept):", b0, "\n")
cat("b1 (Age):", b1, "\n")
cat("b2 (Weight):", b2, "\n")

# Part (b): Find the p-value for IQ (x6)
# Extract the p-value for IQ
p_value_IQ <- summary_model$coefficients["IQ", "Pr(>|t|)"]
cat("\nPart (b): p-value for IQ\n")
cat("p-value:", p_value_IQ, "\n")

# Part (c): Should x6 (IQ) be included?
# If p-value < 0.1, then include IQ
include_IQ <- p_value_IQ < 0.1
cat("\nPart (c): Should x6 (IQ) be included?\n")
if (include_IQ) {
  cat("Yes, IQ should be included because the p-value is less than 0.1\n")
  cat("Answer: (A) Yes, because the p-value in (b) is less than 0.1\n")
} else {
  cat("No, IQ should not be included because the p-value is greater than 0.1\n")
  cat("Answer: (D) No, because the p-value in (b) is greater than 0.1\n")
}

# CONCLUSION:
# a) b0 = 114.5278, b1 = 0.4492, b2 = 0.1734
# b) p-value for IQ = 0.0658
# c) Answer: (A) Yes, because the p-value in (b) is less than 0.1.
