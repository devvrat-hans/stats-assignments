'''
Problem #5: Suppose that we want to see if the probability of being left-handed is different for men than it is for women. And
suppose that 455 students in this class each test this hypothesis at the 1% significance level using their own
personalized class data set.

(a) If the probability of being left-handed is in fact different for men than for women, how many students (on
average) would fail to reject the null hypothesis?
(Assume that each student does the analysis correctly.)

(b) If the probability of being left-handed is in fact the same for men as it is for women, how many students (on
average) would reject the null hypothesis, and falsely conclude that the probability of being left-handed is
different for men than for women?
(Assume that each student does the analysis correctly.)
'''

import numpy as np

# Problem 5: Understanding Type I and Type II errors in hypothesis testing

print("Problem #5: Type I and Type II errors in hypothesis testing")
print("==========================================================")

# Given information
total_students = 455
significance_level = 0.01  # 1% significance level

# Part (a): If the alternative hypothesis is true, how many fail to reject H0?
# This is related to Type II error (β), but we don't have the specific power
# Without specific information about the effect size and sample sizes,
# we can't calculate the exact number, but we can explain the concept

print("\n(a) If the probability of being left-handed is different for men and women (H1 is true):")
print("   This question is asking about the Type II error rate (β) - failing to reject H0 when H1 is true.")
print("   Without knowing the specific effect size and sample sizes for each student's test,")
print("   we can't calculate the exact Type II error rate.")
print("   If we knew the power (1-β) of the test, the answer would be: total_students × β")
print("   Since we don't have this information, we can only provide a conceptual explanation.")

# Part (b): If the null hypothesis is true, how many would reject H0?
# This is related to Type I error (α)
# The probability of Type I error equals the significance level (α)
type_I_errors = total_students * significance_level
print("\n(b) If the probability is the same for men and women (H0 is true):")
print(f"   Type I error rate (α) = {significance_level}")
print(f"   Expected number of students who would reject H0 (Type I errors) = {total_students} × {significance_level} = {type_I_errors} ≈ {int(round(type_I_errors))} students")

print("\nExplanation:")
print("- Type I error: Rejecting the null hypothesis when it is actually true (false positive)")
print("- Type II error: Failing to reject the null hypothesis when it is actually false (false negative)")
print("- The significance level (α) directly controls the Type I error rate")
print("- When H0 is true, approximately 1% of students will falsely reject it (due to α = 0.01)")
print("- Therefore, approximately 455 × 0.01 = 4.55 ≈ 5 students would make a Type I error")
