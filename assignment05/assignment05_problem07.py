# Assignment 5 - Problem 7
# Statistical Analysis Solution in Python

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import scipy.stats as stats
from scipy.stats import norm, t, chi2, f, wilcoxon, mannwhitneyu, kruskal
import seaborn as sns

# Load the dataset
try:
    # Try to load the databank dataset
    data = pd.read_csv('/Users/devvrathans/stats-assignment/data/databank.txt')
    print("Dataset loaded successfully")
    print(f"Dataset shape: {data.shape}")
    print("\nFirst few rows:")
    print(data.head())
    
except FileNotFoundError:
    print("Dataset file not found. Creating sample data for demonstration.")
    # Create sample data if file not found
    np.random.seed(42)
    n = 100
    data = pd.DataFrame({
        'age': np.random.normal(40, 15, n),
        'weight': np.random.normal(150, 25, n),
        'systolic': np.random.normal(120, 20, n),
        'serum.chol': np.random.normal(200, 40, n),
        'exercise': np.random.choice([0, 1, 2, 3], n),
        'gender': np.random.choice(['M', 'F'], n),
        'before': np.random.normal(120, 20, n),
        'after': np.random.normal(115, 18, n)  # Slightly lower for paired test
    })
    # Make 'after' correlated with 'before' for realistic paired data
    data['after'] = data['before'] + np.random.normal(-5, 10, n)

# Problem 7: Non-parametric Tests
# When assumptions for parametric tests are violated, use non-parametric alternatives

print(f"\nNon-parametric Statistical Tests")
print(f"="*50)

# Case 1: Non-parametric alternative to paired t-test (Wilcoxon signed-rank test)
if 'before' in data.columns and 'after' in data.columns:
    # Clean data - remove missing values
    paired_data = data[['before', 'after']].dropna()
    before = paired_data['before']
    after = paired_data['after']
    differences = after - before
    
    print(f"\n1. WILCOXON SIGNED-RANK TEST (Paired Data)")
    print(f"Non-parametric alternative to paired t-test")
    print(f"H0: Median difference = 0")
    print(f"H1: Median difference ≠ 0")
    print(f"Sample size: {len(paired_data)}")
    
    # Descriptive statistics
    print(f"\nDescriptive Statistics:")
    print(f"Before - Mean: {np.mean(before):.4f}, Median: {np.median(before):.4f}")
    print(f"After - Mean: {np.mean(after):.4f}, Median: {np.median(after):.4f}")
    print(f"Differences - Mean: {np.mean(differences):.4f}, Median: {np.median(differences):.4f}")
    
    # Perform Wilcoxon signed-rank test
    wilcoxon_stat, wilcoxon_p = wilcoxon(before, after, alternative='two-sided')
    
    print(f"\nWilcoxon Signed-Rank Test Results:")
    print(f"Test statistic: {wilcoxon_stat:.4f}")
    print(f"p-value: {wilcoxon_p:.4f}")
    
    # For comparison, also perform paired t-test
    t_stat, t_p = stats.ttest_rel(before, after)
    print(f"\nFor comparison - Paired t-test:")
    print(f"t-statistic: {t_stat:.4f}")
    print(f"p-value: {t_p:.4f}")
    
    # Effect size for Wilcoxon (r = Z / sqrt(N))
    n_pairs = len(differences)
    z_score = stats.norm.ppf(1 - wilcoxon_p/2)  # Approximate z-score
    effect_size_r = z_score / np.sqrt(n_pairs)
    print(f"Effect size (r): {effect_size_r:.4f}")

# Case 2: Non-parametric alternative to independent t-test (Mann-Whitney U test)
if 'systolic' in data.columns and 'gender' in data.columns:
    print(f"\n2. MANN-WHITNEY U TEST (Independent Groups)")
    print(f"Non-parametric alternative to independent t-test")
    print(f"H0: Distributions are identical")
    print(f"H1: Distributions differ in location")
    
    # Clean data
    clean_data = data[['systolic', 'gender']].dropna()
    male_systolic = clean_data[clean_data['gender'] == 'M']['systolic']
    female_systolic = clean_data[clean_data['gender'] == 'F']['systolic']
    
    print(f"\nSample sizes: Male = {len(male_systolic)}, Female = {len(female_systolic)}")
    
    # Descriptive statistics
    print(f"\nDescriptive Statistics:")
    print(f"Male - Mean: {np.mean(male_systolic):.4f}, Median: {np.median(male_systolic):.4f}")
    print(f"Female - Mean: {np.mean(female_systolic):.4f}, Median: {np.median(female_systolic):.4f}")
    
    # Perform Mann-Whitney U test
    u_stat, u_p = mannwhitneyu(male_systolic, female_systolic, alternative='two-sided')
    
    print(f"\nMann-Whitney U Test Results:")
    print(f"U statistic: {u_stat:.4f}")
    print(f"p-value: {u_p:.4f}")
    
    # For comparison, also perform independent t-test
    t_stat_ind, t_p_ind = stats.ttest_ind(male_systolic, female_systolic)
    print(f"\nFor comparison - Independent t-test:")
    print(f"t-statistic: {t_stat_ind:.4f}")
    print(f"p-value: {t_p_ind:.4f}")
    
    # Effect size for Mann-Whitney (r = Z / sqrt(N))
    n_total = len(male_systolic) + len(female_systolic)
    z_score_u = stats.norm.ppf(1 - u_p/2)
    effect_size_r_u = z_score_u / np.sqrt(n_total)
    print(f"Effect size (r): {effect_size_r_u:.4f}")

# Case 3: Non-parametric alternative to one-way ANOVA (Kruskal-Wallis test)
if 'systolic' in data.columns and 'exercise' in data.columns:
    print(f"\n3. KRUSKAL-WALLIS TEST (Multiple Groups)")
    print(f"Non-parametric alternative to one-way ANOVA")
    print(f"H0: All group distributions are identical")
    print(f"H1: At least one group differs")
    
    # Clean data
    clean_data = data[['systolic', 'exercise']].dropna()
    
    # Group data by exercise level
    groups = [group['systolic'].values for name, group in clean_data.groupby('exercise')]
    group_names = [str(name) for name, group in clean_data.groupby('exercise')]
    
    print(f"\nNumber of groups: {len(groups)}")
    print(f"Group sizes: {[len(group) for group in groups]}")
    
    # Descriptive statistics by group
    print(f"\nDescriptive Statistics by Exercise Level:")
    for i, (name, group) in enumerate(zip(group_names, groups)):
        print(f"Exercise {name} - n: {len(group)}, Mean: {np.mean(group):.4f}, Median: {np.median(group):.4f}")
    
    # Perform Kruskal-Wallis test
    kw_stat, kw_p = kruskal(*groups)
    
    print(f"\nKruskal-Wallis Test Results:")
    print(f"H statistic: {kw_stat:.4f}")
    print(f"p-value: {kw_p:.4f}")
    print(f"Degrees of freedom: {len(groups) - 1}")
    
    # For comparison, also perform one-way ANOVA
    f_stat_anova, f_p_anova = stats.f_oneway(*groups)
    print(f"\nFor comparison - One-way ANOVA:")
    print(f"F-statistic: {f_stat_anova:.4f}")
    print(f"p-value: {f_p_anova:.4f}")
    
    # Effect size for Kruskal-Wallis (eta-squared analog)
    n_total = sum(len(group) for group in groups)
    eta_squared_kw = (kw_stat - len(groups) + 1) / (n_total - len(groups))
    print(f"Effect size (η² analog): {eta_squared_kw:.4f}")

# Create comprehensive visualization
plt.figure(figsize=(15, 12))

# Visualization for paired data (if available)
if 'before' in data.columns and 'after' in data.columns:
    plt.subplot(2, 3, 1)
    plt.scatter(before, after, alpha=0.6)
    plt.plot([min(before.min(), after.min()), max(before.max(), after.max())], 
             [min(before.min(), after.min()), max(before.max(), after.max())], 'r--')
    plt.xlabel('Before')
    plt.ylabel('After')
    plt.title('Before vs After (Paired Data)')
    plt.grid(True, alpha=0.3)
    
    plt.subplot(2, 3, 2)
    plt.hist(differences, bins=20, alpha=0.7, color='skyblue', edgecolor='black')
    plt.axvline(np.median(differences), color='red', linestyle='--', 
                label=f'Median: {np.median(differences):.2f}')
    plt.xlabel('Difference (After - Before)')
    plt.ylabel('Frequency')
    plt.title('Distribution of Differences')
    plt.legend()
    plt.grid(True, alpha=0.3)

# Visualization for independent groups (if available)
if 'systolic' in data.columns and 'gender' in data.columns:
    plt.subplot(2, 3, 3)
    plt.boxplot([male_systolic, female_systolic], labels=['Male', 'Female'])
    plt.ylabel('Systolic Blood Pressure')
    plt.title('Box Plot by Gender')
    plt.grid(True, alpha=0.3)
    
    plt.subplot(2, 3, 4)
    plt.hist(male_systolic, alpha=0.7, label='Male', bins=15, density=True)
    plt.hist(female_systolic, alpha=0.7, label='Female', bins=15, density=True)
    plt.xlabel('Systolic Blood Pressure')
    plt.ylabel('Density')
    plt.title('Distribution by Gender')
    plt.legend()
    plt.grid(True, alpha=0.3)

# Visualization for multiple groups (if available)
if 'systolic' in data.columns and 'exercise' in data.columns:
    plt.subplot(2, 3, 5)
    plt.boxplot(groups, labels=group_names)
    plt.xlabel('Exercise Level')
    plt.ylabel('Systolic Blood Pressure')
    plt.title('Box Plot by Exercise Level')
    plt.grid(True, alpha=0.3)
    
    plt.subplot(2, 3, 6)
    group_medians = [np.median(group) for group in groups]
    plt.plot(range(len(group_names)), group_medians, 'o-', linewidth=2, markersize=8)
    plt.xticks(range(len(group_names)), group_names)
    plt.xlabel('Exercise Level')
    plt.ylabel('Median Systolic BP')
    plt.title('Group Medians')
    plt.grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig('/Users/devvrathans/stats-assignment/assignment05/assignment05_problem07_python_output.png', dpi=300, bbox_inches='tight')
plt.show()

# Additional non-parametric tests and considerations
print(f"\n4. ADDITIONAL NON-PARAMETRIC CONSIDERATIONS")
print(f"="*50)

# Sign test (simpler alternative to Wilcoxon)
if 'before' in data.columns and 'after' in data.columns:
    print(f"\nSign Test (alternative to Wilcoxon):")
    positive_diff = np.sum(differences > 0)
    negative_diff = np.sum(differences < 0)
    zero_diff = np.sum(differences == 0)
    
    print(f"Positive differences: {positive_diff}")
    print(f"Negative differences: {negative_diff}")
    print(f"Zero differences: {zero_diff} (excluded from test)")
    
    # Sign test using binomial distribution
    n_non_zero = positive_diff + negative_diff
    sign_p = 2 * stats.binom.cdf(min(positive_diff, negative_diff), n_non_zero, 0.5)
    print(f"Sign test p-value: {sign_p:.4f}")

# Assumptions and when to use non-parametric tests
print(f"\nWhen to Use Non-parametric Tests:")
print(f"1. Data is not normally distributed")
print(f"2. Ordinal data or ranked data")
print(f"3. Small sample sizes")
print(f"4. Presence of outliers")
print(f"5. Unequal variances that cannot be corrected")

# Advantages and disadvantages
print(f"\nAdvantages of Non-parametric Tests:")
print(f"- No assumption of normality")
print(f"- Robust to outliers")
print(f"- Can handle ordinal data")
print(f"- Often simpler to compute")

print(f"\nDisadvantages of Non-parametric Tests:")
print(f"- Generally less powerful than parametric tests")
print(f"- May require larger sample sizes")
print(f"- Limited availability of post-hoc tests")
print(f"- Less informative about effect magnitude")

# Summary of all test results
print(f"\n5. SUMMARY OF TEST RESULTS")
print(f"="*50)

if 'before' in data.columns and 'after' in data.columns:
    print(f"\nPaired Data Analysis:")
    print(f"Wilcoxon signed-rank test: p = {wilcoxon_p:.4f}")
    print(f"Paired t-test: p = {t_p:.4f}")
    
    if wilcoxon_p < 0.05:
        print("Conclusion: Significant difference detected (non-parametric)")
    else:
        print("Conclusion: No significant difference detected (non-parametric)")

if 'systolic' in data.columns and 'gender' in data.columns:
    print(f"\nIndependent Groups Analysis:")
    print(f"Mann-Whitney U test: p = {u_p:.4f}")
    print(f"Independent t-test: p = {t_p_ind:.4f}")
    
    if u_p < 0.05:
        print("Conclusion: Significant difference between groups (non-parametric)")
    else:
        print("Conclusion: No significant difference between groups (non-parametric)")

if 'systolic' in data.columns and 'exercise' in data.columns:
    print(f"\nMultiple Groups Analysis:")
    print(f"Kruskal-Wallis test: p = {kw_p:.4f}")
    print(f"One-way ANOVA: p = {f_p_anova:.4f}")
    
    if kw_p < 0.05:
        print("Conclusion: Significant differences among groups (non-parametric)")
    else:
        print("Conclusion: No significant differences among groups (non-parametric)")

print("\n" + "="*50)
print("Problem 7 Analysis Complete")
print("="*50)
