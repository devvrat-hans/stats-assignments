# Problem #5: Suppose that we want to see if the probability of being left-handed is different for men than it is for women. And
# suppose that 455 students in this class each test this hypothesis at the 1% significance level using their own
# personalized class data set.
#
# (a) If the probability of being left-handed is in fact different for men than for women, how many students (on
# average) would fail to reject the null hypothesis?
# (Assume that each student does the analysis correctly.)
#
# (b) If the probability of being left-handed is in fact the same for men as it is for women, how many students (on
# average) would reject the null hypothesis, and falsely conclude that the probability of being left-handed is
# different for men than for women?
# (Assume that each student does the analysis correctly.)

# Problem 5: Understanding Type I and Type II errors in hypothesis testing

cat("Problem #5: Type I and Type II errors in hypothesis testing\n")
cat("==========================================================\n")

# Given information
total_students <- 455
significance_level <- 0.01  # 1% significance level

# Part (a): If the alternative hypothesis is true, how many fail to reject H0?
# This is related to Type II error (β), but we don't have the specific power
# Without specific information about the effect size and sample sizes,
# we can't calculate the exact number, but we can explain the concept

cat("\n(a) If the probability of being left-handed is different for men and women (H1 is true):\n")
cat("   This question is asking about the Type II error rate (β) - failing to reject H0 when H1 is true.\n")
cat("   Without knowing the specific effect size and sample sizes for each student's test,\n")
cat("   we can't calculate the exact Type II error rate.\n")
cat("   If we knew the power (1-β) of the test, the answer would be: total_students × β\n")
cat("   Since we don't have this information, we can only provide a conceptual explanation.\n")

# Part (b): If the null hypothesis is true, how many would reject H0?
# This is related to Type I error (α)
# The probability of Type I error equals the significance level (α)
type_I_errors <- total_students * significance_level
cat("\n(b) If the probability is the same for men and women (H0 is true):\n")
cat("   Type I error rate (α) =", significance_level, "\n")
cat("   Expected number of students who would reject H0 (Type I errors) =", total_students, "×", 
    significance_level, "=", type_I_errors, "≈", round(type_I_errors), "students\n")

cat("\nExplanation:\n")
cat("- Type I error: Rejecting the null hypothesis when it is actually true (false positive)\n")
cat("- Type II error: Failing to reject the null hypothesis when it is actually false (false negative)\n")
cat("- The significance level (α) directly controls the Type I error rate\n")
cat("- When H0 is true, approximately 1% of students will falsely reject it (due to α = 0.01)\n")
cat("- Therefore, approximately 455 × 0.01 = 4.55 ≈ 5 students would make a Type I error\n")

# From the options in the image, the answer appears to be:
# (a) Part (a) choices: Most likely option (I) 23 or option (A) 23 would be correct if β ≈ 0.05
cat("\nFrom the multiple choice options in the image:\n")
cat("For part (a), the answer would depend on the power of the test.\n")
cat("For part (b), the answer is 5 (about 5 students would make Type I errors).\n")
