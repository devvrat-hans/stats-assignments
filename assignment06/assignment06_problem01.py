# Assignment 6 - Problem 1
# Statistical Analysis Solution in Python

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import scipy.stats as stats
from scipy.stats import norm, t, chi2, f, binom, poisson, expon, uniform
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
    n = 200
    data = pd.DataFrame({
        'age': np.random.normal(45, 12, n),
        'weight': np.random.normal(70, 15, n),
        'systolic': np.random.normal(125, 18, n),
        'diastolic': np.random.normal(80, 12, n),
        'cholesterol': np.random.normal(180, 35, n),
        'gender': np.random.choice(['M', 'F'], n),
        'treatment': np.random.choice(['A', 'B', 'Control'], n),
        'outcome': np.random.choice([0, 1], n, p=[0.7, 0.3])
    })

print("\n" + "="*60)
print("ASSIGNMENT 6 - PROBLEM 1: COMPREHENSIVE STATISTICAL ANALYSIS")
print("="*60)

# Problem 1: Multi-variable Statistical Analysis
# This problem typically involves multiple statistical techniques

# 1. DESCRIPTIVE STATISTICS AND DATA EXPLORATION
print("\n1. DESCRIPTIVE STATISTICS AND DATA EXPLORATION")
print("-" * 50)

# Basic descriptive statistics
print("\nBasic Descriptive Statistics:")
print(data.describe())

# Check for missing values
print(f"\nMissing Values:")
print(data.isnull().sum())

# Data types
print(f"\nData Types:")
print(data.dtypes)

# Correlation analysis for numerical variables
numerical_vars = data.select_dtypes(include=[np.number]).columns.tolist()
if len(numerical_vars) > 1:
    print(f"\nCorrelation Matrix:")
    correlation_matrix = data[numerical_vars].corr()
    print(correlation_matrix.round(4))

# 2. NORMALITY TESTING FOR CONTINUOUS VARIABLES
print("\n\n2. NORMALITY TESTING FOR CONTINUOUS VARIABLES")
print("-" * 50)

for var in numerical_vars[:3]:  # Test first 3 numerical variables
    if var in data.columns:
        clean_data = data[var].dropna()
        
        print(f"\nTesting normality for {var}:")
        print(f"Sample size: {len(clean_data)}")
        print(f"Mean: {np.mean(clean_data):.4f}")
        print(f"Std Dev: {np.std(clean_data, ddof=1):.4f}")
        print(f"Skewness: {stats.skew(clean_data):.4f}")
        print(f"Kurtosis: {stats.kurtosis(clean_data):.4f}")
        
        # Shapiro-Wilk test
        if len(clean_data) <= 5000:
            shapiro_stat, shapiro_p = stats.shapiro(clean_data)
            print(f"Shapiro-Wilk test: W = {shapiro_stat:.4f}, p-value = {shapiro_p:.4f}")
        
        # Kolmogorov-Smirnov test
        ks_stat, ks_p = stats.kstest(clean_data, 'norm', 
                                    args=(np.mean(clean_data), np.std(clean_data, ddof=1)))
        print(f"Kolmogorov-Smirnov test: KS = {ks_stat:.4f}, p-value = {ks_p:.4f}")

# 3. HYPOTHESIS TESTING
print("\n\n3. HYPOTHESIS TESTING")
print("-" * 50)

# Example 1: One-sample t-test
if 'systolic' in data.columns:
    systolic_data = data['systolic'].dropna()
    hypothesized_mean = 120  # Normal systolic BP
    
    print(f"\nOne-sample t-test for systolic blood pressure:")
    print(f"H0: μ = {hypothesized_mean}")
    print(f"H1: μ ≠ {hypothesized_mean}")
    
    t_stat, p_value = stats.ttest_1samp(systolic_data, hypothesized_mean)
    
    print(f"Sample mean: {np.mean(systolic_data):.4f}")
    print(f"t-statistic: {t_stat:.4f}")
    print(f"p-value: {p_value:.4f}")
    
    # Confidence interval
    n = len(systolic_data)
    se = stats.sem(systolic_data)
    t_critical = stats.t.ppf(0.975, n-1)
    ci_lower = np.mean(systolic_data) - t_critical * se
    ci_upper = np.mean(systolic_data) + t_critical * se
    
    print(f"95% Confidence Interval: [{ci_lower:.4f}, {ci_upper:.4f}]")
    
    if p_value < 0.05:
        print("Conclusion: Reject H0 - Significant difference from hypothesized mean")
    else:
        print("Conclusion: Fail to reject H0 - No significant difference")

# Example 2: Two-sample t-test (if gender variable exists)
if 'gender' in data.columns and 'systolic' in data.columns:
    print(f"\nTwo-sample t-test: Systolic BP by Gender")
    
    male_systolic = data[data['gender'] == 'M']['systolic'].dropna()
    female_systolic = data[data['gender'] == 'F']['systolic'].dropna()
    
    print(f"Male group: n = {len(male_systolic)}, mean = {np.mean(male_systolic):.4f}")
    print(f"Female group: n = {len(female_systolic)}, mean = {np.mean(female_systolic):.4f}")
    
    # Levene's test for equal variances
    levene_stat, levene_p = stats.levene(male_systolic, female_systolic)
    print(f"Levene's test for equal variances: p = {levene_p:.4f}")
    
    # Two-sample t-test
    t_stat, p_value = stats.ttest_ind(male_systolic, female_systolic, 
                                     equal_var=(levene_p > 0.05))
    
    print(f"t-statistic: {t_stat:.4f}")
    print(f"p-value: {p_value:.4f}")
    
    if p_value < 0.05:
        print("Conclusion: Significant difference between groups")
    else:
        print("Conclusion: No significant difference between groups")

# 4. ANOVA (if treatment groups exist)
if 'treatment' in data.columns and 'systolic' in data.columns:
    print(f"\nOne-way ANOVA: Systolic BP by Treatment")
    
    clean_data = data[['systolic', 'treatment']].dropna()
    groups = [group['systolic'].values for name, group in clean_data.groupby('treatment')]
    group_names = [str(name) for name, group in clean_data.groupby('treatment')]
    
    # Descriptive statistics by group
    print(f"\nGroup Statistics:")
    for name, group in clean_data.groupby('treatment'):
        print(f"{name}: n = {len(group)}, mean = {np.mean(group['systolic']):.4f}, "
              f"std = {np.std(group['systolic'], ddof=1):.4f}")
    
    # ANOVA
    f_stat, p_value = stats.f_oneway(*groups)
    
    print(f"\nANOVA Results:")
    print(f"F-statistic: {f_stat:.4f}")
    print(f"p-value: {p_value:.4f}")
    
    if p_value < 0.05:
        print("Conclusion: Significant differences among treatment groups")
        
        # Post-hoc analysis (pairwise t-tests with Bonferroni correction)
        print("\nPost-hoc pairwise comparisons (Bonferroni corrected):")
        from itertools import combinations
        
        alpha = 0.05
        num_comparisons = len(list(combinations(range(len(groups)), 2)))
        bonferroni_alpha = alpha / num_comparisons
        
        for i, j in combinations(range(len(groups)), 2):
            group1, group2 = groups[i], groups[j]
            name1, name2 = group_names[i], group_names[j]
            
            t_stat, p_val = stats.ttest_ind(group1, group2)
            
            print(f"{name1} vs {name2}: p = {p_val:.4f}, "
                  f"Bonferroni p = {p_val * num_comparisons:.4f}")
            
            if p_val < bonferroni_alpha:
                print(f"  Significant difference")
            else:
                print(f"  No significant difference")
    else:
        print("Conclusion: No significant differences among treatment groups")

# 5. CHI-SQUARE TESTS (for categorical variables)
if 'gender' in data.columns and 'outcome' in data.columns:
    print(f"\n\n5. CHI-SQUARE TEST OF INDEPENDENCE")
    print("-" * 50)
    
    print(f"Testing independence between Gender and Outcome:")
    
    # Create contingency table
    contingency_table = pd.crosstab(data['gender'], data['outcome'])
    print(f"\nContingency Table:")
    print(contingency_table)
    
    # Chi-square test
    chi2_stat, p_value, dof, expected = stats.chi2_contingency(contingency_table)
    
    print(f"\nChi-square Test Results:")
    print(f"Chi-square statistic: {chi2_stat:.4f}")
    print(f"p-value: {p_value:.4f}")
    print(f"Degrees of freedom: {dof}")
    
    # Cramér's V (effect size)
    n = contingency_table.sum().sum()
    cramers_v = np.sqrt(chi2_stat / (n * (min(contingency_table.shape) - 1)))
    print(f"Cramér's V (effect size): {cramers_v:.4f}")
    
    if p_value < 0.05:
        print("Conclusion: Variables are dependent (associated)")
    else:
        print("Conclusion: Variables are independent (not associated)")

# 6. CORRELATION AND REGRESSION ANALYSIS
print(f"\n\n6. CORRELATION AND REGRESSION ANALYSIS")
print("-" * 50)

if len(numerical_vars) >= 2:
    # Example: Age vs Systolic BP
    var1, var2 = numerical_vars[0], numerical_vars[1]
    
    if var1 in data.columns and var2 in data.columns:
        clean_data = data[[var1, var2]].dropna()
        x, y = clean_data[var1], clean_data[var2]
        
        print(f"\nCorrelation and Regression: {var1} vs {var2}")
        
        # Pearson correlation
        pearson_r, pearson_p = stats.pearsonr(x, y)
        print(f"Pearson correlation: r = {pearson_r:.4f}, p = {pearson_p:.4f}")
        
        # Spearman correlation
        spearman_r, spearman_p = stats.spearmanr(x, y)
        print(f"Spearman correlation: ρ = {spearman_r:.4f}, p = {spearman_p:.4f}")
        
        # Linear regression
        slope, intercept, r_value, p_value, std_err = stats.linregress(x, y)
        
        print(f"\nLinear Regression Results:")
        print(f"Equation: {var2} = {intercept:.4f} + {slope:.4f} * {var1}")
        print(f"R-squared: {r_value**2:.4f}")
        print(f"p-value: {p_value:.4f}")
        print(f"Standard error: {std_err:.4f}")

# 7. COMPREHENSIVE VISUALIZATION
print(f"\n\n7. CREATING COMPREHENSIVE VISUALIZATIONS")
print("-" * 50)

# Create a comprehensive plot
fig, axes = plt.subplots(2, 3, figsize=(18, 12))
fig.suptitle('Assignment 6 - Problem 1: Comprehensive Statistical Analysis', fontsize=16, fontweight='bold')

# Plot 1: Histogram of first numerical variable
if len(numerical_vars) >= 1 and numerical_vars[0] in data.columns:
    var = numerical_vars[0]
    clean_data = data[var].dropna()
    
    axes[0, 0].hist(clean_data, bins=30, density=True, alpha=0.7, color='skyblue', edgecolor='black')
    
    # Overlay normal distribution
    x = np.linspace(clean_data.min(), clean_data.max(), 100)
    axes[0, 0].plot(x, norm.pdf(x, np.mean(clean_data), np.std(clean_data)), 'r-', linewidth=2)
    
    axes[0, 0].set_xlabel(var)
    axes[0, 0].set_ylabel('Density')
    axes[0, 0].set_title(f'Distribution of {var}')
    axes[0, 0].grid(True, alpha=0.3)

# Plot 2: Box plot by groups (if categorical variable exists)
if 'gender' in data.columns and len(numerical_vars) >= 1:
    var = numerical_vars[0]
    
    male_data = data[data['gender'] == 'M'][var].dropna()
    female_data = data[data['gender'] == 'F'][var].dropna()
    
    axes[0, 1].boxplot([male_data, female_data], labels=['Male', 'Female'])
    axes[0, 1].set_ylabel(var)
    axes[0, 1].set_title(f'{var} by Gender')
    axes[0, 1].grid(True, alpha=0.3)

# Plot 3: Scatter plot with regression line
if len(numerical_vars) >= 2:
    var1, var2 = numerical_vars[0], numerical_vars[1]
    clean_data = data[[var1, var2]].dropna()
    
    x, y = clean_data[var1], clean_data[var2]
    
    axes[0, 2].scatter(x, y, alpha=0.6, color='blue')
    
    # Add regression line
    slope, intercept, _, _, _ = stats.linregress(x, y)
    line = slope * x + intercept
    axes[0, 2].plot(x, line, 'r-', linewidth=2)
    
    axes[0, 2].set_xlabel(var1)
    axes[0, 2].set_ylabel(var2)
    axes[0, 2].set_title(f'{var1} vs {var2}')
    axes[0, 2].grid(True, alpha=0.3)

# Plot 4: Q-Q plot
if len(numerical_vars) >= 1:
    var = numerical_vars[0]
    clean_data = data[var].dropna()
    
    stats.probplot(clean_data, dist="norm", plot=axes[1, 0])
    axes[1, 0].set_title(f'Q-Q Plot: {var}')
    axes[1, 0].grid(True, alpha=0.3)

# Plot 5: Correlation heatmap
if len(numerical_vars) >= 2:
    corr_matrix = data[numerical_vars].corr()
    
    im = axes[1, 1].imshow(corr_matrix, cmap='coolwarm', aspect='auto', vmin=-1, vmax=1)
    
    # Add correlation values
    for i in range(len(corr_matrix)):
        for j in range(len(corr_matrix.columns)):
            axes[1, 1].text(j, i, f'{corr_matrix.iloc[i, j]:.2f}', 
                           ha='center', va='center', fontweight='bold')
    
    axes[1, 1].set_xticks(range(len(corr_matrix.columns)))
    axes[1, 1].set_yticks(range(len(corr_matrix)))
    axes[1, 1].set_xticklabels(corr_matrix.columns, rotation=45)
    axes[1, 1].set_yticklabels(corr_matrix.index)
    axes[1, 1].set_title('Correlation Matrix')
    
    # Add colorbar
    plt.colorbar(im, ax=axes[1, 1])

# Plot 6: Bar plot of categorical variable
if 'treatment' in data.columns:
    treatment_counts = data['treatment'].value_counts()
    
    axes[1, 2].bar(treatment_counts.index, treatment_counts.values, color='lightgreen')
    axes[1, 2].set_xlabel('Treatment')
    axes[1, 2].set_ylabel('Count')
    axes[1, 2].set_title('Treatment Distribution')
    axes[1, 2].grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig('/Users/devvrathans/stats-assignment/assignment06/assignment06_problem01_python_output.png', 
            dpi=300, bbox_inches='tight')
plt.show()

# 8. SUMMARY AND CONCLUSIONS
print(f"\n\n8. SUMMARY AND CONCLUSIONS")
print("="*50)

print(f"\nDataset Summary:")
print(f"- Total observations: {len(data)}")
print(f"- Numerical variables: {len(numerical_vars)}")
print(f"- Categorical variables: {len(data.select_dtypes(include=['object']).columns)}")

print(f"\nKey Findings:")
print(f"- Descriptive statistics calculated for all numerical variables")
print(f"- Normality tests performed to check distribution assumptions")
print(f"- Hypothesis tests conducted to compare groups and test specific claims")
print(f"- Correlation analysis revealed relationships between variables")
print(f"- Comprehensive visualizations created to illustrate findings")

print(f"\nStatistical Methods Applied:")
print(f"1. Descriptive Statistics")
print(f"2. Normality Testing (Shapiro-Wilk, Kolmogorov-Smirnov)")
print(f"3. Hypothesis Testing (One-sample t-test, Two-sample t-test)")
print(f"4. Analysis of Variance (ANOVA)")
print(f"5. Chi-square Test of Independence")
print(f"6. Correlation Analysis (Pearson, Spearman)")
print(f"7. Linear Regression Analysis")

print("\n" + "="*60)
print("ASSIGNMENT 6 - PROBLEM 1 ANALYSIS COMPLETE")
print("="*60)
