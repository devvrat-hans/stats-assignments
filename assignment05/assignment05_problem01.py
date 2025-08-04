# Assignment 5 - Problem 1
# Statistical Analysis Solution in Python

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import scipy.stats as stats
from scipy.stats import norm, t, chi2, f
import seaborn as sns

# Load the dataset
try:
    # Try to load the databank dataset
    data = pd.read_csv('/Users/devvrathans/stats-assignment/data/databank.txt')
    print("Dataset loaded successfully")
    print(f"Dataset shape: {data.shape}")
    print("\nFirst few rows:")
    print(data.head())
    print("\nDataset info:")
    print(data.info())
    print("\nDescriptive statistics:")
    print(data.describe())
    
except FileNotFoundError:
    print("Dataset file not found. Creating sample data for demonstration.")
    # Create sample data if file not found
    np.random.seed(42)
    n = 100
    data = pd.DataFrame({
        'age': np.random.normal(40, 15, n),
        'weight': np.random.normal(150, 25, n),
        'height': np.random.normal(68, 4, n),
        'systolic': np.random.normal(120, 20, n)
    })

# Problem 1: Statistical analysis
# This is a template that can be adapted based on the specific problem requirements

# Example: Hypothesis testing
# H0: μ = 120 (null hypothesis)
# H1: μ ≠ 120 (alternative hypothesis)

if 'systolic' in data.columns:
    sample_data = data['systolic'].dropna()
    
    # Calculate sample statistics
    sample_mean = np.mean(sample_data)
    sample_std = np.std(sample_data, ddof=1)
    sample_size = len(sample_data)
    
    print(f"\nSample Statistics:")
    print(f"Sample mean: {sample_mean:.4f}")
    print(f"Sample standard deviation: {sample_std:.4f}")
    print(f"Sample size: {sample_size}")
    
    # Perform one-sample t-test
    hypothesized_mean = 120
    t_statistic, p_value = stats.ttest_1samp(sample_data, hypothesized_mean)
    
    print(f"\nOne-sample t-test results:")
    print(f"t-statistic: {t_statistic:.4f}")
    print(f"p-value: {p_value:.4f}")
    
    # Calculate confidence interval
    confidence_level = 0.95
    alpha = 1 - confidence_level
    df = sample_size - 1
    t_critical = stats.t.ppf(1 - alpha/2, df)
    margin_error = t_critical * (sample_std / np.sqrt(sample_size))
    
    ci_lower = sample_mean - margin_error
    ci_upper = sample_mean + margin_error
    
    print(f"\n{confidence_level*100}% Confidence Interval:")
    print(f"[{ci_lower:.4f}, {ci_upper:.4f}]")
    
    # Create visualization
    plt.figure(figsize=(12, 8))
    
    # Histogram with normal curve overlay
    plt.subplot(2, 2, 1)
    plt.hist(sample_data, bins=20, density=True, alpha=0.7, color='skyblue', edgecolor='black')
    x = np.linspace(sample_data.min(), sample_data.max(), 100)
    plt.plot(x, norm.pdf(x, sample_mean, sample_std), 'r-', linewidth=2, label='Normal fit')
    plt.axvline(sample_mean, color='red', linestyle='--', label=f'Sample mean: {sample_mean:.2f}')
    plt.axvline(hypothesized_mean, color='green', linestyle='--', label=f'Hypothesized mean: {hypothesized_mean}')
    plt.xlabel('Systolic Blood Pressure')
    plt.ylabel('Density')
    plt.title('Distribution of Systolic Blood Pressure')
    plt.legend()
    plt.grid(True, alpha=0.3)
    
    # Q-Q plot for normality check
    plt.subplot(2, 2, 2)
    stats.probplot(sample_data, dist="norm", plot=plt)
    plt.title('Q-Q Plot for Normality Check')
    plt.grid(True, alpha=0.3)
    
    # Box plot
    plt.subplot(2, 2, 3)
    plt.boxplot(sample_data)
    plt.ylabel('Systolic Blood Pressure')
    plt.title('Box Plot')
    plt.grid(True, alpha=0.3)
    
    # Confidence interval visualization
    plt.subplot(2, 2, 4)
    plt.errorbar(1, sample_mean, yerr=margin_error, fmt='o', capsize=10, capthick=2, markersize=8)
    plt.axhline(hypothesized_mean, color='red', linestyle='--', label=f'Hypothesized mean: {hypothesized_mean}')
    plt.xlim(0.5, 1.5)
    plt.ylabel('Systolic Blood Pressure')
    plt.title(f'{confidence_level*100}% Confidence Interval')
    plt.legend()
    plt.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig('/Users/devvrathans/stats-assignment/assignment05/assignment05_problem01_python_output.png', dpi=300, bbox_inches='tight')
    plt.show()
    
    # Conclusion
    print(f"\nConclusion:")
    if p_value < 0.05:
        print(f"At α = 0.05, we reject the null hypothesis (p-value = {p_value:.4f} < 0.05)")
        print(f"There is sufficient evidence to conclude that the population mean is significantly different from {hypothesized_mean}")
    else:
        print(f"At α = 0.05, we fail to reject the null hypothesis (p-value = {p_value:.4f} ≥ 0.05)")
        print(f"There is insufficient evidence to conclude that the population mean is significantly different from {hypothesized_mean}")

else:
    print("Systolic blood pressure data not available in the dataset.")
    print("Please verify the problem requirements and dataset.")

print("\n" + "="*50)
print("Problem 1 Analysis Complete")
print("="*50)
