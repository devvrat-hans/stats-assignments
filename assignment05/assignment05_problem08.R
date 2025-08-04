# Assignment 05 - Problem 08

# Use personalized databank data
databank_file <- "/Users/devvrathans/stats-assignment/data/kumaa25_databank.txt"
databank2 <- read.csv(databank_file)

# Verify the frequency distribution for the marital.status variable
marital_table <- table(databank2$marital.status)
cat("Frequency distribution for marital.status:\n")
print(marital_table)

# Check if the distribution matches the expected:
# marital  D  M  S  W
#         14 41 18  7
expected_dist <- c(D = 14, M = 41, S = 18, W = 7)
cat("\nDoes the distribution match the expected?\n")
for (status in names(expected_dist)) {
  cat(status, ": Expected =", expected_dist[status], 
      ", Actual =", marital_table[status], 
      ", Match =", marital_table[status] == expected_dist[status], "\n")
}

# a) Create the indicator variable x
# x = 1 if marital.status = M, 0 if marital.status = S, W, or D
x <- ifelse(databank2$marital.status == "M", 1, 0)

# Get the first 3 values of x
cat("\nPart (a): First 3 values of the variable x:\n")
cat(x[1], x[2], x[3], "\n")

# b) Create dummy variables for marital.status
x7 <- ifelse(databank2$marital.status == "D", 1, 0)
x8 <- ifelse(databank2$marital.status == "M", 1, 0)
x9 <- ifelse(databank2$marital.status == "S", 1, 0)
# Note: We omit the dummy for W as it's the reference category

# Create a new dataframe with these variables
df <- data.frame(
  systolic = databank2$systolic,
  weight = databank2$weight,
  age = databank2$age,
  x7 = x7,
  x8 = x8,
  x9 = x9
)

# Fit the full model: y = β0 + β1 x1 + β2 x2 + β7 x7 + β8 x8 + β9 x9
full_model <- lm(systolic ~ weight + age + x7 + x8 + x9, data = df)

# Fit the reduced model: y = β0 + β1 x1 + β2 x2
reduced_model <- lm(systolic ~ weight + age, data = df)

# Perform the F-test
anova_result <- anova(reduced_model, full_model)
F_value <- anova_result$F[2]

cat("\nPart (b): F-value for testing H0: β7 = β8 = β9 = 0:\n")
cat("F-value =", F_value, "\n")

# c) Find the critical value
alpha <- 0.05
df1 <- 3  # Number of parameters in the hypothesis (β7, β8, β9)
df2 <- nrow(df) - length(coef(full_model))  # Degrees of freedom for error
critical_value <- qf(1 - alpha, df1, df2)

cat("\nPart (c): Critical value at 5% significance level:\n")
cat("Critical value =", critical_value, "\n")

# d) Should marital.status be included?
include_marital <- F_value > critical_value

cat("\nPart (d): Should marital.status be included?\n")
if (include_marital) {
  cat("Yes, marital.status should be included because the F-value is greater than the critical value.\n")
  cat("Answer: (A) Yes, because the answer in (b) is greater than the answer in (c).\n")
} else {
  cat("No, marital.status should not be included because the F-value is less than or equal to the critical value.\n")
  cat("Answer: (D) No, because the answer in (b) is less than or equal to the answer in (c).\n")
}

# Check the p-values for x7, x8, and x9
summary_model <- summary(full_model)
p_value_x7 <- summary_model$coefficients["x7", "Pr(>|t|)"]
p_value_x8 <- summary_model$coefficients["x8", "Pr(>|t|)"]
p_value_x9 <- summary_model$coefficients["x9", "Pr(>|t|)"]

cat("\nP-values for marital status variables:\n")
cat("p-value for x7 (D):", p_value_x7, "\n")
cat("p-value for x8 (M):", p_value_x8, "\n")
cat("p-value for x9 (S):", p_value_x9, "\n")

# CONCLUSION:
# a) First 3 values of the variable x: 1, 0, 1
# b) F-value for testing H0: β7 = β8 = β9 = 0: 1.010
# c) Critical value at 5% significance level: 2.728
# d) Answer: (D) No, because the answer in (b) is less than or equal to the answer in (c).
