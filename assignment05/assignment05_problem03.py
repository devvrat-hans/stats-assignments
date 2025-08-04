# Assignment 5 - Problem 3
# Statistical Analysis Solution in Python

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import scipy.stats as stats
from scipy.stats import norm, t, chi2, f, pearsonr, spearmanr, linregress
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
        'height': np.random.normal(68, 4, n),
        'systolic': np.random.normal(120, 20, n),
        'serum.chol': np.random.normal(200, 40, n)
    })

# Problem 3: Correlation and Regression Analysis
# Example: Analyzing relationship between variables

if 'systolic' in data.columns and 'age' in data.columns:
    # Clean data - remove missing values
    clean_data = data[['systolic', 'age']].dropna()
    x = clean_data['age']
    y = clean_data['systolic']
    
    print(f"\nCorrelation and Regression Analysis")
    print(f"Variables: Age (X) vs Systolic Blood Pressure (Y)")
    print(f"Sample size: {len(clean_data)}")
    
    # Calculate correlation coefficients
    pearson_corr, pearson_p = pearsonr(x, y)
    spearman_corr, spearman_p = spearmanr(x, y)
    
    print(f"\nCorrelation Analysis:")
    print(f"Pearson correlation coefficient: {pearson_corr:.4f} (p-value: {pearson_p:.4f})")
    print(f"Spearman correlation coefficient: {spearman_corr:.4f} (p-value: {spearman_p:.4f})")
    
    # Linear regression using scipy
    slope, intercept, r_value, p_value_reg, std_err = linregress(x, y)
    
    y_pred = slope * x + intercept
    r_squared = r_value**2
    
    print(f"\nLinear Regression Analysis:")
    print(f"Regression equation: Y = {intercept:.4f} + {slope:.4f}X")
    print(f"R-squared: {r_squared:.4f}")
    print(f"Slope: {slope:.4f}")
    print(f"Intercept: {intercept:.4f}")
    
    # Calculate confidence intervals for slope
    n = len(x)
    residuals = y - y_pred
    mse = np.sum(residuals**2) / (n - 2)
    se_slope = np.sqrt(mse / np.sum((x - np.mean(x))**2))
    
    t_critical = stats.t.ppf(0.975, n - 2)  # 95% confidence interval
    slope_ci_lower = slope - t_critical * se_slope
    slope_ci_upper = slope + t_critical * se_slope
    
    print(f"95% CI for slope: [{slope_ci_lower:.4f}, {slope_ci_upper:.4f}]")
    
    # Test significance of slope
    t_stat_slope = slope / se_slope
    p_value_slope = 2 * (1 - stats.t.cdf(abs(t_stat_slope), n - 2))
    
    print(f"t-statistic for slope: {t_stat_slope:.4f}")
    print(f"p-value for slope: {p_value_slope:.4f}")
    
    # Create comprehensive visualization
    plt.figure(figsize=(15, 12))
    
    # Scatter plot with regression line
    plt.subplot(2, 3, 1)
    plt.scatter(x, y, alpha=0.6, color='blue')
    plt.plot(x, y_pred, color='red', linewidth=2, label=f'y = {intercept:.2f} + {slope:.2f}x')
    plt.xlabel('Age')
    plt.ylabel('Systolic Blood Pressure')
    plt.title(f'Scatter Plot with Regression Line\nR² = {r_squared:.4f}')
    plt.legend()
    plt.grid(True, alpha=0.3)
    
    # Residual plot
    plt.subplot(2, 3, 2)
    plt.scatter(y_pred, residuals, alpha=0.6)
    plt.axhline(y=0, color='red', linestyle='--')
    plt.xlabel('Fitted Values')
    plt.ylabel('Residuals')
    plt.title('Residual Plot')
    plt.grid(True, alpha=0.3)
    
    # Q-Q plot of residuals
    plt.subplot(2, 3, 3)
    stats.probplot(residuals, dist="norm", plot=plt)
    plt.title('Q-Q Plot of Residuals')
    plt.grid(True, alpha=0.3)
    
    # Histogram of residuals
    plt.subplot(2, 3, 4)
    plt.hist(residuals, bins=20, density=True, alpha=0.7, color='skyblue', edgecolor='black')
    x_norm = np.linspace(residuals.min(), residuals.max(), 100)
    plt.plot(x_norm, norm.pdf(x_norm, np.mean(residuals), np.std(residuals)), 'r-', linewidth=2)
    plt.xlabel('Residuals')
    plt.ylabel('Density')
    plt.title('Distribution of Residuals')
    plt.grid(True, alpha=0.3)
    
    # Cook's distance
    plt.subplot(2, 3, 5)
    leverage = 1/n + (x - np.mean(x))**2 / np.sum((x - np.mean(x))**2)
    standardized_residuals = residuals / np.sqrt(mse * (1 - leverage))
    cooks_d = standardized_residuals**2 * leverage / (2 * (1 - leverage))
    
    plt.scatter(range(len(cooks_d)), cooks_d, alpha=0.6)
    plt.axhline(y=4/n, color='red', linestyle='--', label=f'Threshold: 4/n = {4/n:.4f}')
    plt.xlabel('Observation')
    plt.ylabel("Cook's Distance")
    plt.title("Cook's Distance Plot")
    plt.legend()
    plt.grid(True, alpha=0.3)
    
    # Scale-Location plot
    plt.subplot(2, 3, 6)
    sqrt_std_residuals = np.sqrt(np.abs(standardized_residuals))
    plt.scatter(y_pred, sqrt_std_residuals, alpha=0.6)
    plt.xlabel('Fitted Values')
    plt.ylabel('√|Standardized Residuals|')
    plt.title('Scale-Location Plot')
    plt.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig('/Users/devvrathans/stats-assignment/assignment05/assignment05_problem03_python_output.png', dpi=300, bbox_inches='tight')
    plt.show()
    
    # Additional diagnostic tests
    print(f"\nDiagnostic Tests:")
    
    # Durbin-Watson test for autocorrelation
    from scipy import stats as scipy_stats
    diff_residuals = np.diff(residuals)
    dw_stat = np.sum(diff_residuals**2) / np.sum(residuals**2)
    print(f"Durbin-Watson statistic: {dw_stat:.4f}")
    
    # Shapiro-Wilk test for normality of residuals
    if len(residuals) <= 5000:
        shapiro_stat, shapiro_p = stats.shapiro(residuals)
        print(f"Shapiro-Wilk test for residuals normality: statistic = {shapiro_stat:.4f}, p-value = {shapiro_p:.4f}")
    
    # Predictions and confidence intervals
    print(f"\nSample Predictions:")
    sample_ages = [25, 40, 60]
    for age in sample_ages:
        pred_y = intercept + slope * age
        
        # Prediction interval
        x_mean = np.mean(x)
        s_xx = np.sum((x - x_mean)**2)
        se_pred = np.sqrt(mse * (1 + 1/n + (age - x_mean)**2/s_xx))
        
        pred_lower = pred_y - t_critical * se_pred
        pred_upper = pred_y + t_critical * se_pred
        
        print(f"Age {age}: Predicted systolic BP = {pred_y:.2f}, 95% PI: [{pred_lower:.2f}, {pred_upper:.2f}]")
    
    # Conclusion
    print(f"\nConclusion:")
    alpha = 0.05
    if pearson_p < alpha:
        print(f"At α = {alpha}, there is significant correlation between age and systolic blood pressure (p-value = {pearson_p:.4f})")
        if pearson_corr > 0:
            print("The correlation is positive - as age increases, systolic blood pressure tends to increase")
        else:
            print("The correlation is negative - as age increases, systolic blood pressure tends to decrease")
    else:
        print(f"At α = {alpha}, there is no significant correlation between age and systolic blood pressure (p-value = {pearson_p:.4f})")
    
    # Strength of correlation
    print(f"\nCorrelation Strength:")
    abs_corr = abs(pearson_corr)
    if abs_corr < 0.3:
        print("Weak correlation")
    elif abs_corr < 0.7:
        print("Moderate correlation")
    else:
        print("Strong correlation")

elif 'weight' in data.columns and 'height' in data.columns:
    # Alternative analysis: Weight vs Height
    print("\nAlternative Analysis: Weight vs Height Relationship")
    
    clean_data = data[['weight', 'height']].dropna()
    x = clean_data['height']
    y = clean_data['weight']
    
    # Similar analysis as above but for weight vs height
    pearson_corr, pearson_p = pearsonr(x, y)
    print(f"Pearson correlation (height vs weight): {pearson_corr:.4f} (p-value: {pearson_p:.4f})")
    
    # Simple linear regression using scipy
    slope, intercept, r_value, p_value_reg, std_err = linregress(x, y)
    
    y_pred = slope * x + intercept
    r_squared = r_value**2
    
    print(f"R-squared: {r_squared:.4f}")
    print(f"Regression equation: Weight = {intercept:.4f} + {slope:.4f} × Height")

else:
    print("Required variables not available in the dataset.")
    print("Please verify the problem requirements and dataset.")

print("\n" + "="*50)
print("Problem 3 Analysis Complete")
print("="*50)
