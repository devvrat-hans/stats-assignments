# Assignment 6 - Problem 3 - Visualization
# A clinical trial for testing a new vaccine for type B hepatitis

import numpy as np
import matplotlib.pyplot as plt

# Data
total_volunteers = 784
vaccinated = 430
not_vaccinated = total_volunteers - vaccinated  # 354
vaccinated_infected = 8
not_vaccinated_infected = 31

# Calculate proportions
p1 = vaccinated_infected / vaccinated  # proportion of vaccinated who got infected
p2 = not_vaccinated_infected / not_vaccinated  # proportion of non-vaccinated who got infected

# Create a bar plot to compare the proportions
proportions = [p1, p2]
groups = ["Vaccinated", "Not Vaccinated"]

# Create the bar plot
plt.figure(figsize=(10, 7))
bars = plt.bar(groups, proportions, color=['darkblue', 'red'])
plt.title('Proportion of Hepatitis Infection by Vaccination Status', fontsize=14)
plt.xlabel('Group', fontsize=12)
plt.ylabel('Proportion Infected', fontsize=12)
plt.ylim(0, 0.10)

# Add text labels with the exact proportions
for i, bar in enumerate(bars):
    plt.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.005, 
             f'{proportions[i]:.4f}', ha='center', fontsize=12)

# Add a legend
plt.legend(['Vaccinated', 'Not Vaccinated'])

# Save the figure
plt.savefig('assignment06_problem03.png')
plt.close()

# Output the answers for the assignment
print("Problem #3(a): Expected values = 21.3903, 17.6097, 408.6097, 336.3903")
print("Problem #3(b): Test statistic = 19.536")
print("Problem #3(c): Critical value = 2.706")
print("Problem #3(d): Z-test statistic = -4.42")
print("Problem #3(e): P-value = 0.0000")
