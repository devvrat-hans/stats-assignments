# Assignment 5 - Problem 6
# Statistical Analysis Solution in Python

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import scipy.stats as stats
from scipy.stats import norm, t, chi2, f, binom, poisson
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
        'success': np.random.binomial(1, 0.3, n),  # Binary outcome
        'counts': np.random.poisson(5, n)  # Count data
    })

# Problem 6: Probability Distributions and Goodness of Fit
# Example: Testing if data follows a specific distribution

# Case 1: Testing if a continuous variable follows normal distribution
if 'systolic' in data.columns:
    clean_data = data['systolic'].dropna()
    
    print(f"\nGoodness of Fit Test: Normal Distribution")
    print(f"Variable: Systolic Blood Pressure")
    print(f"Sample size: {len(clean_data)}")
    
    # Sample statistics
    sample_mean = np.mean(clean_data)
    sample_std = np.std(clean_data, ddof=1)
    
    print(f"\nSample Statistics:")
    print(f"Mean: {sample_mean:.4f}")
    print(f"Standard deviation: {sample_std:.4f}")
    print(f"Skewness: {stats.skew(clean_data):.4f}")
    print(f"Kurtosis: {stats.kurtosis(clean_data):.4f}")
    
    # Normality tests
    print(f"\nNormality Tests:")
    
    # Shapiro-Wilk test
    if len(clean_data) <= 5000:
        shapiro_stat, shapiro_p = stats.shapiro(clean_data)
        print(f"Shapiro-Wilk test: W = {shapiro_stat:.4f}, p-value = {shapiro_p:.4f}")
    
    # Anderson-Darling test
    ad_stat, ad_critical, ad_significance = stats.anderson(clean_data, dist='norm')
    print(f"Anderson-Darling test: AD = {ad_stat:.4f}")
    print("Critical values and significance levels:")
    for i, (cv, sl) in enumerate(zip(ad_critical, ad_significance)):
        print(f"  {sl}%: {cv:.4f}")
    
    # Kolmogorov-Smirnov test
    ks_stat, ks_p = stats.kstest(clean_data, 'norm', args=(sample_mean, sample_std))
    print(f"Kolmogorov-Smirnov test: KS = {ks_stat:.4f}, p-value = {ks_p:.4f}")
    
    # D'Agostino and Pearson's test
    dagostino_stat, dagostino_p = stats.normaltest(clean_data)
    print(f"D'Agostino-Pearson test: χ² = {dagostino_stat:.4f}, p-value = {dagostino_p:.4f}")
    
    # Chi-square goodness of fit test (binning the data)
    print(f"\nChi-square Goodness of Fit Test:")
    
    # Create bins
    num_bins = min(10, int(np.sqrt(len(clean_data))))
    observed_freq, bin_edges = np.histogram(clean_data, bins=num_bins)
    
    # Calculate expected frequencies under normal distribution
    bin_centers = (bin_edges[:-1] + bin_edges[1:]) / 2
    bin_width = bin_edges[1] - bin_edges[0]
    
    expected_freq = []
    for i in range(len(bin_edges) - 1):
        prob = norm.cdf(bin_edges[i+1], sample_mean, sample_std) - \
               norm.cdf(bin_edges[i], sample_mean, sample_std)
        expected_freq.append(prob * len(clean_data))
    
    expected_freq = np.array(expected_freq)
    
    # Combine bins with expected frequency < 5
    while np.any(expected_freq < 5) and len(expected_freq) > 2:
        min_idx = np.argmin(expected_freq)
        if min_idx == 0:
            # Combine with next bin
            observed_freq[1] += observed_freq[0]
            expected_freq[1] += expected_freq[0]
            observed_freq = observed_freq[1:]
            expected_freq = expected_freq[1:]
            bin_edges = bin_edges[1:]
        elif min_idx == len(expected_freq) - 1:
            # Combine with previous bin
            observed_freq[-2] += observed_freq[-1]
            expected_freq[-2] += expected_freq[-1]
            observed_freq = observed_freq[:-1]
            expected_freq = expected_freq[:-1]
            bin_edges = bin_edges[:-1]
        else:
            # Combine with adjacent bin with smaller frequency
            if expected_freq[min_idx-1] < expected_freq[min_idx+1]:
                # Combine with previous
                observed_freq[min_idx-1] += observed_freq[min_idx]
                expected_freq[min_idx-1] += expected_freq[min_idx]
                observed_freq = np.delete(observed_freq, min_idx)
                expected_freq = np.delete(expected_freq, min_idx)
                bin_edges = np.delete(bin_edges, min_idx)
            else:
                # Combine with next
                observed_freq[min_idx+1] += observed_freq[min_idx]
                expected_freq[min_idx+1] += expected_freq[min_idx]
                observed_freq = np.delete(observed_freq, min_idx)
                expected_freq = np.delete(expected_freq, min_idx)
                bin_edges = np.delete(bin_edges, min_idx+1)
    
    # Perform chi-square test
    chi2_stat, chi2_p = stats.chisquare(observed_freq, expected_freq, ddof=2)  # ddof=2 for mean and std
    
    print(f"Number of bins after combining: {len(observed_freq)}")
    print(f"Chi-square statistic: {chi2_stat:.4f}")
    print(f"p-value: {chi2_p:.4f}")
    print(f"Degrees of freedom: {len(observed_freq) - 1 - 2}")
    
    # Create comprehensive visualization
    plt.figure(figsize=(15, 10))
    
    # Histogram with normal overlay
    plt.subplot(2, 3, 1)
    plt.hist(clean_data, bins=30, density=True, alpha=0.7, color='skyblue', edgecolor='black')
    x = np.linspace(clean_data.min(), clean_data.max(), 100)
    plt.plot(x, norm.pdf(x, sample_mean, sample_std), 'r-', linewidth=2, label='Normal fit')
    plt.xlabel('Systolic Blood Pressure')
    plt.ylabel('Density')
    plt.title('Histogram with Normal Distribution Overlay')
    plt.legend()
    plt.grid(True, alpha=0.3)
    
    # Q-Q plot
    plt.subplot(2, 3, 2)
    stats.probplot(clean_data, dist="norm", plot=plt)
    plt.title('Q-Q Plot (Normal)')
    plt.grid(True, alpha=0.3)
    
    # P-P plot
    plt.subplot(2, 3, 3)
    sorted_data = np.sort(clean_data)
    empirical_cdf = np.arange(1, len(sorted_data) + 1) / len(sorted_data)
    theoretical_cdf = norm.cdf(sorted_data, sample_mean, sample_std)
    
    plt.plot(theoretical_cdf, empirical_cdf, 'o', alpha=0.6)
    plt.plot([0, 1], [0, 1], 'r-', linewidth=2)
    plt.xlabel('Theoretical CDF')
    plt.ylabel('Empirical CDF')
    plt.title('P-P Plot (Normal)')
    plt.grid(True, alpha=0.3)
    
    # Chi-square goodness of fit visualization
    plt.subplot(2, 3, 4)
    x_pos = np.arange(len(observed_freq))
    width = 0.35
    
    plt.bar(x_pos - width/2, observed_freq, width, label='Observed', alpha=0.7)
    plt.bar(x_pos + width/2, expected_freq, width, label='Expected', alpha=0.7)
    plt.xlabel('Bin')
    plt.ylabel('Frequency')
    plt.title('Chi-square Goodness of Fit')
    plt.legend()
    plt.grid(True, alpha=0.3)
    
    # Box plot
    plt.subplot(2, 3, 5)
    plt.boxplot(clean_data)
    plt.ylabel('Systolic Blood Pressure')
    plt.title('Box Plot')
    plt.grid(True, alpha=0.3)
    
    # Empirical vs theoretical quantiles
    plt.subplot(2, 3, 6)
    empirical_quantiles = np.percentile(clean_data, np.linspace(0, 100, 101))
    theoretical_quantiles = norm.ppf(np.linspace(0.01, 0.99, 99), sample_mean, sample_std)
    
    plt.plot(theoretical_quantiles, empirical_quantiles[1:-1], 'o', alpha=0.6)
    min_val = min(np.min(theoretical_quantiles), np.min(empirical_quantiles[1:-1]))
    max_val = max(np.max(theoretical_quantiles), np.max(empirical_quantiles[1:-1]))
    plt.plot([min_val, max_val], [min_val, max_val], 'r-', linewidth=2)
    plt.xlabel('Theoretical Quantiles')
    plt.ylabel('Empirical Quantiles')
    plt.title('Quantile-Quantile Comparison')
    plt.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig('/Users/devvrathans/stats-assignment/assignment05/assignment05_problem06_python_output.png', dpi=300, bbox_inches='tight')
    plt.show()
    
    # Summary of normality tests
    print(f"\nSummary of Normality Tests:")
    alpha = 0.05
    tests_failed = 0
    
    if len(clean_data) <= 5000 and shapiro_p < alpha:
        print(f"- Shapiro-Wilk test: FAILED (p = {shapiro_p:.4f} < {alpha})")
        tests_failed += 1
    elif len(clean_data) <= 5000:
        print(f"- Shapiro-Wilk test: PASSED (p = {shapiro_p:.4f} ≥ {alpha})")
    
    if ks_p < alpha:
        print(f"- Kolmogorov-Smirnov test: FAILED (p = {ks_p:.4f} < {alpha})")
        tests_failed += 1
    else:
        print(f"- Kolmogorov-Smirnov test: PASSED (p = {ks_p:.4f} ≥ {alpha})")
    
    if dagostino_p < alpha:
        print(f"- D'Agostino-Pearson test: FAILED (p = {dagostino_p:.4f} < {alpha})")
        tests_failed += 1
    else:
        print(f"- D'Agostino-Pearson test: PASSED (p = {dagostino_p:.4f} ≥ {alpha})")
    
    if chi2_p < alpha:
        print(f"- Chi-square goodness of fit: FAILED (p = {chi2_p:.4f} < {alpha})")
        tests_failed += 1
    else:
        print(f"- Chi-square goodness of fit: PASSED (p = {chi2_p:.4f} ≥ {alpha})")

# Case 2: Testing discrete distributions
if 'counts' in data.columns:
    print(f"\n" + "="*50)
    print(f"Testing Poisson Distribution for Count Data")
    print(f"="*50)
    
    count_data = data['counts'].dropna()
    
    print(f"Sample size: {len(count_data)}")
    print(f"Sample mean: {np.mean(count_data):.4f}")
    print(f"Sample variance: {np.var(count_data, ddof=1):.4f}")
    print(f"Variance-to-mean ratio: {np.var(count_data, ddof=1)/np.mean(count_data):.4f}")
    
    # For Poisson distribution, variance should equal mean
    sample_mean = np.mean(count_data)
    
    # Chi-square goodness of fit for Poisson
    observed_counts = np.bincount(count_data.astype(int))
    max_val = len(observed_counts) - 1
    
    # Calculate expected counts under Poisson distribution
    expected_counts = []
    for k in range(max_val + 1):
        expected_counts.append(len(count_data) * poisson.pmf(k, sample_mean))
    
    # Combine bins with expected count < 5
    while len(expected_counts) > 1 and expected_counts[-1] < 5:
        if len(expected_counts) > 1:
            expected_counts[-2] += expected_counts[-1]
            observed_counts[-2] += observed_counts[-1]
            expected_counts = expected_counts[:-1]
            observed_counts = observed_counts[:-1]
    
    # Perform chi-square test
    chi2_stat_poisson, chi2_p_poisson = stats.chisquare(observed_counts, expected_counts, ddof=1)
    
    print(f"\nPoisson Goodness of Fit Test:")
    print(f"Chi-square statistic: {chi2_stat_poisson:.4f}")
    print(f"p-value: {chi2_p_poisson:.4f}")
    print(f"Degrees of freedom: {len(observed_counts) - 1 - 1}")

# Conclusion
print(f"\n" + "="*50)
print(f"CONCLUSIONS")
print(f"="*50)

if 'systolic' in data.columns:
    print(f"\nNormal Distribution Test for Systolic BP:")
    if tests_failed == 0:
        print("The data appears to follow a normal distribution (all tests passed)")
    elif tests_failed <= 2:
        print("The data shows some deviation from normality (some tests failed)")
    else:
        print("The data significantly deviates from normal distribution (most tests failed)")

if 'counts' in data.columns:
    print(f"\nPoisson Distribution Test for Count Data:")
    if chi2_p_poisson >= 0.05:
        print("The count data appears to follow a Poisson distribution")
    else:
        print("The count data does not follow a Poisson distribution")

print("\n" + "="*50)
print("Problem 6 Analysis Complete")
print("="*50)
