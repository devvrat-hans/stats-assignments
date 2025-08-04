# Assignment 5 - Problem 2
# Statistical Analysis Solution in Python

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import scipy.stats as stats
from scipy.stats import norm, t, chi2, f, ttest_ind, ttest_rel
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
        'gender': np.random.choice(['M', 'F'], n),
        'exercise': np.random.choice([0, 1, 2, 3], n),
        'systolic': np.random.normal(120, 20, n)
    })

# Problem 2: Two-sample comparison or ANOVA
# This could be comparing groups, testing independence, etc.

# Example: Comparing systolic blood pressure between genders
if 'systolic' in data.columns and 'gender' in data.columns:
    # Clean data
    clean_data = data[['systolic', 'gender']].dropna()
    
    # Separate groups
    male_systolic = clean_data[clean_data['gender'] == 'M']['systolic']
    female_systolic = clean_data[clean_data['gender'] == 'F']['systolic']
    
    print(f"\nGroup Statistics:")
    print(f"Male group - n: {len(male_systolic)}, mean: {np.mean(male_systolic):.4f}, std: {np.std(male_systolic, ddof=1):.4f}")
    print(f"Female group - n: {len(female_systolic)}, mean: {np.mean(female_systolic):.4f}, std: {np.std(female_systolic, ddof=1):.4f}")
    
    # Perform two-sample t-test (assuming unequal variances)
    t_stat, p_value = ttest_ind(male_systolic, female_systolic, equal_var=False)
    
    print(f"\nTwo-sample t-test results:")
    print(f"t-statistic: {t_stat:.4f}")
    print(f"p-value: {p_value:.4f}")
    
    # Test for equal variances (Levene's test)
    levene_stat, levene_p = stats.levene(male_systolic, female_systolic)
    print(f"\nLevene's test for equal variances:")
    print(f"Test statistic: {levene_stat:.4f}")
    print(f"p-value: {levene_p:.4f}")
    
    # Effect size (Cohen's d)
    pooled_std = np.sqrt(((len(male_systolic) - 1) * np.var(male_systolic, ddof=1) + 
                         (len(female_systolic) - 1) * np.var(female_systolic, ddof=1)) / 
                        (len(male_systolic) + len(female_systolic) - 2))
    cohens_d = (np.mean(male_systolic) - np.mean(female_systolic)) / pooled_std
    print(f"\nEffect size (Cohen's d): {cohens_d:.4f}")
    
    # Create comprehensive visualization
    plt.figure(figsize=(15, 10))
    
    # Box plot comparison
    plt.subplot(2, 3, 1)
    data_for_box = [male_systolic, female_systolic]
    plt.boxplot(data_for_box, labels=['Male', 'Female'])
    plt.ylabel('Systolic Blood Pressure')
    plt.title('Box Plot Comparison by Gender')
    plt.grid(True, alpha=0.3)
    
    # Histogram comparison
    plt.subplot(2, 3, 2)
    plt.hist(male_systolic, alpha=0.7, label='Male', bins=15, density=True)
    plt.hist(female_systolic, alpha=0.7, label='Female', bins=15, density=True)
    plt.xlabel('Systolic Blood Pressure')
    plt.ylabel('Density')
    plt.title('Distribution Comparison by Gender')
    plt.legend()
    plt.grid(True, alpha=0.3)
    
    # Q-Q plots for normality
    plt.subplot(2, 3, 3)
    stats.probplot(male_systolic, dist="norm", plot=plt)
    plt.title('Q-Q Plot - Male Group')
    plt.grid(True, alpha=0.3)
    
    plt.subplot(2, 3, 4)
    stats.probplot(female_systolic, dist="norm", plot=plt)
    plt.title('Q-Q Plot - Female Group')
    plt.grid(True, alpha=0.3)
    
    # Violin plot
    plt.subplot(2, 3, 5)
    data_long = pd.melt(clean_data, id_vars=['gender'], value_vars=['systolic'])
    sns.violinplot(data=data_long, x='gender', y='value')
    plt.ylabel('Systolic Blood Pressure')
    plt.title('Violin Plot by Gender')
    plt.grid(True, alpha=0.3)
    
    # Strip plot with means
    plt.subplot(2, 3, 6)
    sns.stripplot(data=data_long, x='gender', y='value', alpha=0.6, size=4)
    
    # Add mean lines
    male_mean = np.mean(male_systolic)
    female_mean = np.mean(female_systolic)
    plt.hlines(male_mean, -0.2, 0.2, colors='red', linestyle='--', linewidth=2, label=f'Male mean: {male_mean:.2f}')
    plt.hlines(female_mean, 0.8, 1.2, colors='blue', linestyle='--', linewidth=2, label=f'Female mean: {female_mean:.2f}')
    
    plt.ylabel('Systolic Blood Pressure')
    plt.title('Strip Plot with Means')
    plt.legend()
    plt.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig('/Users/devvrathans/stats-assignment/assignment05/assignment05_problem02_python_output.png', dpi=300, bbox_inches='tight')
    plt.show()
    
    # Additional tests
    print(f"\nNormality Tests:")
    
    # Shapiro-Wilk tests for normality
    if len(male_systolic) <= 5000:
        shapiro_male = stats.shapiro(male_systolic)
        print(f"Male group - Shapiro-Wilk p-value: {shapiro_male.pvalue:.4f}")
    
    if len(female_systolic) <= 5000:
        shapiro_female = stats.shapiro(female_systolic)
        print(f"Female group - Shapiro-Wilk p-value: {shapiro_female.pvalue:.4f}")
    
    # Conclusion
    print(f"\nConclusion:")
    alpha = 0.05
    if p_value < alpha:
        print(f"At α = {alpha}, we reject the null hypothesis (p-value = {p_value:.4f} < {alpha})")
        print(f"There is sufficient evidence to conclude that there is a significant difference in systolic blood pressure between males and females")
    else:
        print(f"At α = {alpha}, we fail to reject the null hypothesis (p-value = {p_value:.4f} ≥ {alpha})")
        print(f"There is insufficient evidence to conclude that there is a significant difference in systolic blood pressure between males and females")
    
    # Interpretation of effect size
    print(f"\nEffect Size Interpretation:")
    if abs(cohens_d) < 0.2:
        print("Small effect size")
    elif abs(cohens_d) < 0.5:
        print("Small to medium effect size")
    elif abs(cohens_d) < 0.8:
        print("Medium to large effect size")
    else:
        print("Large effect size")

elif 'exercise' in data.columns:
    # Alternative analysis: ANOVA for exercise groups
    print("\nAlternative Analysis: ANOVA for Exercise Groups")
    
    clean_data = data[['systolic', 'exercise']].dropna()
    
    # Group data by exercise level
    groups = [group['systolic'].values for name, group in clean_data.groupby('exercise')]
    group_names = [str(name) for name, group in clean_data.groupby('exercise')]
    
    # Perform one-way ANOVA
    f_stat, p_value = stats.f_oneway(*groups)
    
    print(f"One-way ANOVA results:")
    print(f"F-statistic: {f_stat:.4f}")
    print(f"p-value: {p_value:.4f}")
    
    # Group statistics
    print(f"\nGroup Statistics:")
    for i, (name, group) in enumerate(clean_data.groupby('exercise')):
        print(f"Exercise level {name} - n: {len(group)}, mean: {np.mean(group['systolic']):.4f}, std: {np.std(group['systolic'], ddof=1):.4f}")
    
    # Create visualization for ANOVA
    plt.figure(figsize=(12, 8))
    
    plt.subplot(2, 2, 1)
    clean_data.boxplot(column='systolic', by='exercise', ax=plt.gca())
    plt.title('Systolic BP by Exercise Level')
    plt.suptitle('')  # Remove default title
    
    plt.subplot(2, 2, 2)
    for i, group in enumerate(groups):
        plt.hist(group, alpha=0.7, label=f'Exercise {group_names[i]}', bins=10, density=True)
    plt.xlabel('Systolic Blood Pressure')
    plt.ylabel('Density')
    plt.title('Distribution by Exercise Level')
    plt.legend()
    
    plt.tight_layout()
    plt.savefig('/Users/devvrathans/stats-assignment/assignment05/assignment05_problem02_anova_python_output.png', dpi=300, bbox_inches='tight')
    plt.show()

else:
    print("Required variables not available in the dataset.")
    print("Please verify the problem requirements and dataset.")

print("\n" + "="*50)
print("Problem 2 Analysis Complete")
print("="*50)
