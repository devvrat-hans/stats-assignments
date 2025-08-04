# Assignment 5 - Problem 4
# Statistical Analysis Solution in Python

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import scipy.stats as stats
from scipy.stats import norm, t, chi2, f, chi2_contingency
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
        'gender': np.random.choice(['M', 'F'], n),
        'smoking.status': np.random.choice([0, 1, 2], n),
        'exercise': np.random.choice([0, 1, 2, 3], n),
        'marital.status': np.random.choice(['S', 'M', 'D', 'W'], n),
        'ed.level': np.random.choice([0, 1, 2, 3], n)
    })

# Problem 4: Chi-square tests and categorical data analysis
# Example: Testing independence between categorical variables

if 'gender' in data.columns and 'smoking.status' in data.columns:
    # Clean data - remove missing values
    clean_data = data[['gender', 'smoking.status']].dropna()
    
    print(f"\nChi-square Test of Independence")
    print(f"Variables: Gender vs Smoking Status")
    print(f"Sample size: {len(clean_data)}")
    
    # Create contingency table
    contingency_table = pd.crosstab(clean_data['gender'], clean_data['smoking.status'])
    print(f"\nContingency Table:")
    print(contingency_table)
    
    # Add row and column totals
    contingency_with_totals = pd.crosstab(clean_data['gender'], clean_data['smoking.status'], margins=True)
    print(f"\nContingency Table with Totals:")
    print(contingency_with_totals)
    
    # Perform chi-square test
    chi2_stat, p_value, dof, expected = chi2_contingency(contingency_table)
    
    print(f"\nChi-square Test Results:")
    print(f"Chi-square statistic: {chi2_stat:.4f}")
    print(f"p-value: {p_value:.4f}")
    print(f"Degrees of freedom: {dof}")
    
    print(f"\nExpected Frequencies:")
    expected_df = pd.DataFrame(expected, 
                              index=contingency_table.index, 
                              columns=contingency_table.columns)
    print(expected_df)
    
    # Check assumptions
    print(f"\nAssumption Check:")
    min_expected = np.min(expected)
    cells_less_than_5 = np.sum(expected < 5)
    total_cells = expected.size
    percentage_less_than_5 = (cells_less_than_5 / total_cells) * 100
    
    print(f"Minimum expected frequency: {min_expected:.4f}")
    print(f"Cells with expected frequency < 5: {cells_less_than_5} out of {total_cells} ({percentage_less_than_5:.1f}%)")
    
    if min_expected >= 5 and percentage_less_than_5 <= 20:
        print("Chi-square test assumptions are satisfied")
    else:
        print("Warning: Chi-square test assumptions may be violated")
    
    # Calculate effect size (Cramér's V)
    n = np.sum(contingency_table.values)
    cramers_v = np.sqrt(chi2_stat / (n * (min(contingency_table.shape) - 1)))
    print(f"\nEffect size (Cramér's V): {cramers_v:.4f}")
    
    # Create comprehensive visualization
    plt.figure(figsize=(15, 10))
    
    # Stacked bar chart
    plt.subplot(2, 3, 1)
    contingency_table.plot(kind='bar', stacked=True, ax=plt.gca())
    plt.title('Stacked Bar Chart: Gender vs Smoking Status')
    plt.xlabel('Gender')
    plt.ylabel('Count')
    plt.legend(title='Smoking Status')
    plt.xticks(rotation=0)
    plt.grid(True, alpha=0.3)
    
    # Grouped bar chart
    plt.subplot(2, 3, 2)
    contingency_table.plot(kind='bar', ax=plt.gca())
    plt.title('Grouped Bar Chart: Gender vs Smoking Status')
    plt.xlabel('Gender')
    plt.ylabel('Count')
    plt.legend(title='Smoking Status')
    plt.xticks(rotation=0)
    plt.grid(True, alpha=0.3)
    
    # Mosaic plot (approximation using rectangle areas)
    plt.subplot(2, 3, 3)
    # Calculate proportions
    row_totals = contingency_table.sum(axis=1)
    col_totals = contingency_table.sum(axis=0)
    total = contingency_table.sum().sum()
    
    # Create a simple mosaic-like visualization
    x_positions = np.cumsum([0] + list(row_totals[:-1])) / total
    widths = row_totals / total
    
    colors = plt.cm.Set3(np.linspace(0, 1, len(col_totals)))
    
    for i, gender in enumerate(contingency_table.index):
        bottom = 0
        for j, smoking in enumerate(contingency_table.columns):
            height = contingency_table.loc[gender, smoking] / row_totals[gender]
            plt.bar(x_positions[i], height, width=widths[i], 
                   bottom=bottom, color=colors[j], 
                   edgecolor='black', linewidth=0.5)
            bottom += height
    
    plt.title('Mosaic Plot Approximation')
    plt.xlabel('Gender (proportional width)')
    plt.ylabel('Proportion within Gender')
    
    # Heatmap of observed frequencies
    plt.subplot(2, 3, 4)
    sns.heatmap(contingency_table, annot=True, fmt='d', cmap='Blues')
    plt.title('Heatmap: Observed Frequencies')
    
    # Heatmap of expected frequencies
    plt.subplot(2, 3, 5)
    sns.heatmap(expected_df, annot=True, fmt='.1f', cmap='Oranges')
    plt.title('Heatmap: Expected Frequencies')
    
    # Residuals heatmap
    plt.subplot(2, 3, 6)
    residuals = (contingency_table - expected_df) / np.sqrt(expected_df)
    sns.heatmap(residuals, annot=True, fmt='.2f', cmap='RdBu_r', center=0)
    plt.title('Standardized Residuals')
    
    plt.tight_layout()
    plt.savefig('/Users/devvrathans/stats-assignment/assignment05/assignment05_problem04_python_output.png', dpi=300, bbox_inches='tight')
    plt.show()
    
    # Additional analysis - Column percentages
    print(f"\nColumn Percentages (within smoking status):")
    col_percentages = contingency_table.div(contingency_table.sum(axis=0), axis=1) * 100
    print(col_percentages.round(2))
    
    # Row percentages
    print(f"\nRow Percentages (within gender):")
    row_percentages = contingency_table.div(contingency_table.sum(axis=1), axis=0) * 100
    print(row_percentages.round(2))
    
    # Post-hoc analysis if significant
    if p_value < 0.05:
        print(f"\nPost-hoc Analysis (Standardized Residuals):")
        print("Values > |2| indicate significant deviations from independence")
        print(residuals.round(3))
        
        # Find significant cells
        significant_cells = np.where(np.abs(residuals) > 2)
        if len(significant_cells[0]) > 0:
            print(f"\nSignificant deviations (|residual| > 2):")
            for i, j in zip(significant_cells[0], significant_cells[1]):
                gender = contingency_table.index[i]
                smoking = contingency_table.columns[j]
                residual = residuals.iloc[i, j]
                print(f"Gender {gender}, Smoking {smoking}: residual = {residual:.3f}")
    
    # Conclusion
    print(f"\nConclusion:")
    alpha = 0.05
    if p_value < alpha:
        print(f"At α = {alpha}, we reject the null hypothesis (p-value = {p_value:.4f} < {alpha})")
        print(f"There is sufficient evidence to conclude that gender and smoking status are dependent/associated")
    else:
        print(f"At α = {alpha}, we fail to reject the null hypothesis (p-value = {p_value:.4f} ≥ {alpha})")
        print(f"There is insufficient evidence to conclude that gender and smoking status are dependent/associated")
    
    # Effect size interpretation
    print(f"\nEffect Size Interpretation (Cramér's V):")
    if cramers_v < 0.1:
        print("Negligible association")
    elif cramers_v < 0.3:
        print("Small association")
    elif cramers_v < 0.5:
        print("Medium association")
    else:
        print("Large association")

elif 'exercise' in data.columns and 'marital.status' in data.columns:
    # Alternative analysis: Exercise vs Marital Status
    print("\nAlternative Analysis: Exercise vs Marital Status")
    
    clean_data = data[['exercise', 'marital.status']].dropna()
    
    # Create contingency table
    contingency_table = pd.crosstab(clean_data['exercise'], clean_data['marital.status'])
    print(f"\nContingency Table:")
    print(contingency_table)
    
    # Perform chi-square test
    chi2_stat, p_value, dof, expected = chi2_contingency(contingency_table)
    
    print(f"\nChi-square Test Results:")
    print(f"Chi-square statistic: {chi2_stat:.4f}")
    print(f"p-value: {p_value:.4f}")
    print(f"Degrees of freedom: {dof}")
    
    # Effect size
    n = np.sum(contingency_table.values)
    cramers_v = np.sqrt(chi2_stat / (n * (min(contingency_table.shape) - 1)))
    print(f"Effect size (Cramér's V): {cramers_v:.4f}")

elif 'gender' in data.columns:
    # Goodness of fit test for single categorical variable
    print("\nGoodness of Fit Test: Gender Distribution")
    
    gender_counts = data['gender'].value_counts()
    print(f"\nObserved frequencies:")
    print(gender_counts)
    
    # Test against equal proportions
    expected_equal = [len(data) / len(gender_counts)] * len(gender_counts)
    
    chi2_stat, p_value = stats.chisquare(gender_counts.values, expected_equal)
    
    print(f"\nGoodness of Fit Test (against equal proportions):")
    print(f"Chi-square statistic: {chi2_stat:.4f}")
    print(f"p-value: {p_value:.4f}")
    print(f"Degrees of freedom: {len(gender_counts) - 1}")

else:
    print("Required categorical variables not available in the dataset.")
    print("Please verify the problem requirements and dataset.")

print("\n" + "="*50)
print("Problem 4 Analysis Complete")
print("="*50)
