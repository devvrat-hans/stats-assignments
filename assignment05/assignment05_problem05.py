# Assignment 5 - Problem 5
# Statistical Analysis Solution in Python

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import scipy.stats as stats
from scipy.stats import norm, t, chi2, f, bartlett, levene
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
    n = 120
    data = pd.DataFrame({
        'age': np.random.normal(40, 15, n),
        'weight': np.random.normal(150, 25, n),
        'systolic': np.random.normal(120, 20, n),
        'serum.chol': np.random.normal(200, 40, n),
        'exercise': np.random.choice([0, 1, 2, 3], n),
        'ed.level': np.random.choice([0, 1, 2, 3], n)
    })

# Problem 5: Analysis of Variance (ANOVA)
# Example: Comparing means across multiple groups

if 'systolic' in data.columns and 'exercise' in data.columns:
    # Clean data - remove missing values
    clean_data = data[['systolic', 'exercise']].dropna()
    
    print(f"\nOne-Way ANOVA Analysis")
    print(f"Dependent Variable: Systolic Blood Pressure")
    print(f"Independent Variable: Exercise Level")
    print(f"Sample size: {len(clean_data)}")
    
    # Group data by exercise level
    groups = [group['systolic'].values for name, group in clean_data.groupby('exercise')]
    group_names = [str(name) for name, group in clean_data.groupby('exercise')]
    
    print(f"\nNumber of groups: {len(groups)}")
    
    # Descriptive statistics by group
    print(f"\nDescriptive Statistics by Exercise Level:")
    group_stats = clean_data.groupby('exercise')['systolic'].agg([
        'count', 'mean', 'std', 'min', 'max'
    ]).round(4)
    print(group_stats)
    
    # Overall statistics
    overall_mean = clean_data['systolic'].mean()
    overall_std = clean_data['systolic'].std()
    total_n = len(clean_data)
    
    print(f"\nOverall Statistics:")
    print(f"Overall mean: {overall_mean:.4f}")
    print(f"Overall standard deviation: {overall_std:.4f}")
    print(f"Total sample size: {total_n}")
    
    # Perform one-way ANOVA
    f_statistic, p_value = stats.f_oneway(*groups)
    
    print(f"\nOne-Way ANOVA Results:")
    print(f"F-statistic: {f_statistic:.4f}")
    print(f"p-value: {p_value:.4f}")
    
    # Calculate degrees of freedom
    k = len(groups)  # number of groups
    n = total_n     # total sample size
    df_between = k - 1
    df_within = n - k
    df_total = n - 1
    
    print(f"Degrees of freedom between groups: {df_between}")
    print(f"Degrees of freedom within groups: {df_within}")
    print(f"Total degrees of freedom: {df_total}")
    
    # Calculate sum of squares manually for detailed output
    grand_mean = overall_mean
    
    # Sum of squares between groups (SSB)
    ssb = sum([len(group) * (np.mean(group) - grand_mean)**2 for group in groups])
    
    # Sum of squares within groups (SSW)
    ssw = sum([sum((group - np.mean(group))**2) for group in groups])
    
    # Total sum of squares (SST)
    sst = sum((clean_data['systolic'] - grand_mean)**2)
    
    # Mean squares
    msb = ssb / df_between
    msw = ssw / df_within
    
    print(f"\nANOVA Table:")
    print(f"Source\t\tSS\t\tdf\tMS\t\tF\t\tp-value")
    print(f"Between\t\t{ssb:.4f}\t\t{df_between}\t{msb:.4f}\t\t{f_statistic:.4f}\t\t{p_value:.4f}")
    print(f"Within\t\t{ssw:.4f}\t\t{df_within}\t{msw:.4f}")
    print(f"Total\t\t{sst:.4f}\t\t{df_total}")
    
    # Effect size (eta-squared)
    eta_squared = ssb / sst
    print(f"\nEffect Size (η²): {eta_squared:.4f}")
    
    # Test assumptions
    print(f"\nAssumption Testing:")
    
    # 1. Test for normality within each group
    print(f"\n1. Normality Tests (Shapiro-Wilk):")
    normality_violated = False
    for i, (name, group) in enumerate(zip(group_names, groups)):
        if len(group) <= 5000:  # Shapiro-Wilk limitation
            shapiro_stat, shapiro_p = stats.shapiro(group)
            print(f"Exercise level {name}: W = {shapiro_stat:.4f}, p-value = {shapiro_p:.4f}")
            if shapiro_p < 0.05:
                normality_violated = True
        else:
            print(f"Exercise level {name}: Sample too large for Shapiro-Wilk test")
    
    # 2. Test for homogeneity of variances
    print(f"\n2. Homogeneity of Variance Tests:")
    
    # Levene's test (more robust)
    levene_stat, levene_p = stats.levene(*groups)
    print(f"Levene's test: W = {levene_stat:.4f}, p-value = {levene_p:.4f}")
    
    # Bartlett's test (assumes normality)
    bartlett_stat, bartlett_p = stats.bartlett(*groups)
    print(f"Bartlett's test: χ² = {bartlett_stat:.4f}, p-value = {bartlett_p:.4f}")
    
    # Create comprehensive visualization
    plt.figure(figsize=(15, 12))
    
    # Box plot
    plt.subplot(2, 3, 1)
    clean_data.boxplot(column='systolic', by='exercise', ax=plt.gca())
    plt.title('Box Plot by Exercise Level')
    plt.suptitle('')  # Remove default title
    plt.xlabel('Exercise Level')
    plt.ylabel('Systolic Blood Pressure')
    plt.grid(True, alpha=0.3)
    
    # Violin plot
    plt.subplot(2, 3, 2)
    positions = range(len(groups))
    violin_parts = plt.violinplot(groups, positions=positions)
    plt.xticks(positions, group_names)
    plt.xlabel('Exercise Level')
    plt.ylabel('Systolic Blood Pressure')
    plt.title('Violin Plot by Exercise Level')
    plt.grid(True, alpha=0.3)
    
    # Strip plot with means
    plt.subplot(2, 3, 3)
    for i, (name, group) in enumerate(zip(group_names, groups)):
        y_values = group
        x_values = [i] * len(y_values)
        plt.scatter(x_values, y_values, alpha=0.6, s=20)
        
        # Add mean line
        group_mean = np.mean(group)
        plt.hlines(group_mean, i-0.2, i+0.2, colors='red', linestyle='-', linewidth=3)
    
    plt.xticks(range(len(group_names)), group_names)
    plt.xlabel('Exercise Level')
    plt.ylabel('Systolic Blood Pressure')
    plt.title('Strip Plot with Group Means')
    plt.grid(True, alpha=0.3)
    
    # Means plot with error bars
    plt.subplot(2, 3, 4)
    group_means = [np.mean(group) for group in groups]
    group_stds = [np.std(group) for group in groups]
    group_sems = [np.std(group)/np.sqrt(len(group)) for group in groups]
    
    plt.errorbar(range(len(group_names)), group_means, yerr=group_sems, 
                fmt='o-', capsize=5, capthick=2, markersize=8, linewidth=2)
    plt.xticks(range(len(group_names)), group_names)
    plt.xlabel('Exercise Level')
    plt.ylabel('Mean Systolic Blood Pressure')
    plt.title('Group Means with Standard Error')
    plt.grid(True, alpha=0.3)
    
    # Q-Q plots for normality check
    plt.subplot(2, 3, 5)
    # Combine all residuals for overall normality check
    all_residuals = []
    for i, group in enumerate(groups):
        residuals = group - np.mean(group)
        all_residuals.extend(residuals)
    
    stats.probplot(all_residuals, dist="norm", plot=plt)
    plt.title('Q-Q Plot of Residuals')
    plt.grid(True, alpha=0.3)
    
    # Residuals vs fitted values
    plt.subplot(2, 3, 6)
    fitted_values = []
    residuals_all = []
    
    for i, group in enumerate(groups):
        group_mean = np.mean(group)
        fitted_values.extend([group_mean] * len(group))
        residuals_all.extend(group - group_mean)
    
    plt.scatter(fitted_values, residuals_all, alpha=0.6)
    plt.axhline(y=0, color='red', linestyle='--')
    plt.xlabel('Fitted Values (Group Means)')
    plt.ylabel('Residuals')
    plt.title('Residuals vs Fitted Values')
    plt.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig('/Users/devvrathans/stats-assignment/assignment05/assignment05_problem05_python_output.png', dpi=300, bbox_inches='tight')
    plt.show()
    
    # Post-hoc tests if ANOVA is significant
    if p_value < 0.05:
        print(f"\nPost-hoc Analysis:")
        print(f"Since ANOVA is significant, performing pairwise comparisons...")
        
        # Tukey's HSD (using scipy.stats for pairwise t-tests with Bonferroni correction)
        from itertools import combinations
        
        print(f"\nPairwise t-tests with Bonferroni correction:")
        alpha = 0.05
        num_comparisons = len(list(combinations(range(len(groups)), 2)))
        bonferroni_alpha = alpha / num_comparisons
        
        for i, j in combinations(range(len(groups)), 2):
            group1, group2 = groups[i], groups[j]
            name1, name2 = group_names[i], group_names[j]
            
            t_stat, p_val = stats.ttest_ind(group1, group2)
            
            print(f"Exercise {name1} vs Exercise {name2}:")
            print(f"  t-statistic: {t_stat:.4f}")
            print(f"  p-value: {p_val:.4f}")
            print(f"  Bonferroni adjusted p-value: {p_val * num_comparisons:.4f}")
            
            if p_val < bonferroni_alpha:
                print(f"  Significant difference (p < {bonferroni_alpha:.4f})")
            else:
                print(f"  No significant difference (p ≥ {bonferroni_alpha:.4f})")
            print()
    
    # Conclusion
    print(f"\nConclusion:")
    alpha = 0.05
    if p_value < alpha:
        print(f"At α = {alpha}, we reject the null hypothesis (p-value = {p_value:.4f} < {alpha})")
        print(f"There is sufficient evidence to conclude that there are significant differences in systolic blood pressure among exercise levels")
    else:
        print(f"At α = {alpha}, we fail to reject the null hypothesis (p-value = {p_value:.4f} ≥ {alpha})")
        print(f"There is insufficient evidence to conclude that there are significant differences in systolic blood pressure among exercise levels")
    
    # Effect size interpretation
    print(f"\nEffect Size Interpretation (η²):")
    if eta_squared < 0.01:
        print("Small effect")
    elif eta_squared < 0.06:
        print("Medium effect")
    elif eta_squared < 0.14:
        print("Large effect")
    else:
        print("Very large effect")
    
    # Assumption summary
    print(f"\nAssumption Summary:")
    if normality_violated:
        print("- Normality assumption may be violated for some groups")
    else:
        print("- Normality assumption appears to be met")
    
    if levene_p < 0.05:
        print("- Homogeneity of variance assumption is violated (consider Welch's ANOVA)")
    else:
        print("- Homogeneity of variance assumption appears to be met")

elif 'serum.chol' in data.columns and 'ed.level' in data.columns:
    # Alternative analysis: Serum cholesterol by education level
    print("\nAlternative Analysis: Serum Cholesterol by Education Level")
    
    clean_data = data[['serum.chol', 'ed.level']].dropna()
    
    # Group data by education level
    groups = [group['serum.chol'].values for name, group in clean_data.groupby('ed.level')]
    group_names = [str(name) for name, group in clean_data.groupby('ed.level')]
    
    # Perform one-way ANOVA
    f_statistic, p_value = stats.f_oneway(*groups)
    
    print(f"One-way ANOVA results:")
    print(f"F-statistic: {f_statistic:.4f}")
    print(f"p-value: {p_value:.4f}")
    
    # Group statistics
    print(f"\nGroup Statistics:")
    group_stats = clean_data.groupby('ed.level')['serum.chol'].agg([
        'count', 'mean', 'std'
    ]).round(4)
    print(group_stats)

else:
    print("Required variables not available in the dataset.")
    print("Please verify the problem requirements and dataset.")

print("\n" + "="*50)
print("Problem 5 Analysis Complete")
print("="*50)
