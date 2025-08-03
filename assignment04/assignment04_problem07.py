'''
Problem #7: Note: for this question do NOT use commands like d4<-d4[!is.na(d4)] to remove NA's for any of the
variables before doing the analysis. Instead use things like sum(d4,na.rm=T) that will remove NA's during
the analysis while keeping the original variables intact.

Using the project kumaa25_class.RData that you created in Assignment #0, verify (using the sum
command with the na.rm=T option) that the sum of the d2 column is equal to 2282.8, the sum of the d4
column is equal to 2301.7, and that the sum of the gender column is equal to 527. (Note: Make sure that you
are using your personalized class data set and not the entire class data set.)

Work through this example on R and then do the following using the project kumaa25_class.RData that
you created in Assignment #0.

Researchers in this paper attempted to determine gender from hand stencils in parietal art. They noted that the
index finger (d2) and the ring finger (d4) tend to be of equal length in females (i.e., the ratio d2/d4 ≈ 1), but that
males tend to have index fingers that are shorter than their ring fingers (i.e., the ratio d2/d4 < 1). See Figure 1 on
page 748 of the above paper, and have a look at your own hands.

Create a new variable called d2overd4, which consists of the d2 column divided by the d4 column. Verify
(using the mean command) that the average of this new d2overd4 column is equal to 0.994424843.

(a) Find the p-value for testing the hypothesis that the average ratio of d2/d4 (d2overd4) is greater for females
than for males (gender).
(Note: For this part you will have to specify that the alternative hypothesis is greater.)

(b) Find a 96% confidence interval for the difference in the ratio d2/d4 (d2overd4) between females and
males.
(Note: in order to get the required confidence interval for this part you will have to specify that the
alternative hypothesis is two.sided.)
'''

import pandas as pd
import numpy as np
from scipy import stats
import warnings
warnings.filterwarnings('ignore')

print("Problem #7: Analysis of d2/d4 finger ratio between males and females")
print("==================================================================")

# Note: In a real situation, we would need to load the actual kumaa25_class.RData file
# Since we can't access the specific dataset in this case, we'll create a simulated dataset
# with similar properties to demonstrate the analysis approach.

# Create a simulated dataset based on the given information
# This is for demonstration purposes only
np.random.seed(42)
n = 100  # Sample size
# Create gender column (0 for male, 1 for female, roughly half each)
gender = np.random.choice([0, 1], size=n, p=[0.5, 0.5])
# Create d2 (index finger) and d4 (ring finger) measurements
# with appropriate means to match the described relationship
d2 = np.random.normal(loc=70 + gender*5, scale=5, size=n)  # Slightly longer for females
d4 = np.random.normal(loc=75 - gender*2, scale=5, size=n)  # Slightly shorter for females

# Insert some NAs to demonstrate na.rm=T functionality
random_indices = np.random.choice(n, 5, replace=False)
d2[random_indices[:2]] = np.nan
d4[random_indices[2:4]] = np.nan
# For gender, we need to set missing values differently since gender is integer type
# and np.nan is a float, which can't be assigned to integer array
gender = gender.astype(float)  # Convert to float to allow NaN values
gender[random_indices[4:]] = np.nan

# Create pandas DataFrame
df = pd.DataFrame({
    'gender': gender,
    'd2': d2,
    'd4': d4
})

# Note: The sums won't match exactly since this is simulated data
print("\nSum of columns (with na.rm=TRUE):")
print(f"Sum of d2: {np.nansum(df['d2']):.1f} (should be 2282.8)")
print(f"Sum of d4: {np.nansum(df['d4']):.1f} (should be 2301.7)")
print(f"Sum of gender: {np.nansum(df['gender']):.0f} (should be 527)")

# Create d2overd4 column
df['d2overd4'] = df['d2'] / df['d4']
print(f"\nMean of d2overd4: {np.nanmean(df['d2overd4']):.9f} (should be 0.994424843)")

# (a) Hypothesis test: Is the average ratio d2/d4 greater for females than for males?
# H0: μ_female ≤ μ_male
# H1: μ_female > μ_male (one-sided test)

# Extract d2overd4 values for females and males
female_ratios = df.loc[df['gender'] == 1, 'd2overd4'].dropna()
male_ratios = df.loc[df['gender'] == 0, 'd2overd4'].dropna()

print("\n(a) Testing if d2/d4 ratio is greater for females than males:")
print(f"Number of females: {len(female_ratios)}")
print(f"Number of males: {len(male_ratios)}")
print(f"Mean ratio for females: {np.mean(female_ratios):.6f}")
print(f"Mean ratio for males: {np.mean(male_ratios):.6f}")

# Perform t-test (one-sided)
t_stat, p_value = stats.ttest_ind(female_ratios, male_ratios, equal_var=True, alternative='greater')
print(f"t-statistic: {t_stat:.6f}")
print(f"p-value (one-sided): {p_value:.6f}")

# (b) 96% confidence interval for the difference in ratio between females and males
# Two-sided test for confidence interval
alpha = 0.04  # For a 96% confidence interval
t_stat, p_value_two_sided = stats.ttest_ind(female_ratios, male_ratios, equal_var=True)
df_pooled = len(female_ratios) + len(male_ratios) - 2
pooled_std = np.sqrt(((len(female_ratios) - 1) * np.var(female_ratios, ddof=1) + 
                     (len(male_ratios) - 1) * np.var(male_ratios, ddof=1)) / df_pooled)

# Standard error of difference between means
se_diff = pooled_std * np.sqrt(1/len(female_ratios) + 1/len(male_ratios))

# Difference in means
mean_diff = np.mean(female_ratios) - np.mean(male_ratios)

# Critical t-value for 96% confidence interval
t_critical = stats.t.ppf(1 - alpha/2, df_pooled)

# Calculate confidence interval
ci_lower = mean_diff - t_critical * se_diff
ci_upper = mean_diff + t_critical * se_diff

print(f"\n(b) 96% confidence interval for the difference in d2/d4 ratio (females - males):")
print(f"Mean difference: {mean_diff:.6f}")
print(f"Standard error of difference: {se_diff:.6f}")
print(f"t-critical (96% CI): {t_critical:.6f}")
print(f"96% CI: ({ci_lower:.6f}, {ci_upper:.6f})")

print("\nNote: This analysis is based on simulated data and not the actual kumaa25_class.RData.")
print("In a real analysis, you would load the actual dataset and perform these calculations.")
