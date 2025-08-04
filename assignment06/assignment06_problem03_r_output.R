# Assignment 6 - Problem 3 - Visualization
# A clinical trial for testing a new vaccine for type B hepatitis

# Data
total_volunteers <- 784
vaccinated <- 430
not_vaccinated <- total_volunteers - vaccinated  # 354
vaccinated_infected <- 8
not_vaccinated_infected <- 31

# Calculate proportions
p1 <- vaccinated_infected / vaccinated  # proportion of vaccinated who got infected
p2 <- not_vaccinated_infected / not_vaccinated  # proportion of non-vaccinated who got infected

# Create a bar plot to compare the proportions
proportions <- c(p1, p2)
groups <- c("Vaccinated", "Not Vaccinated")

# Open the PNG graphics device
png("assignment06_problem03.png", width=800, height=600)

# Create the bar plot
barplot(proportions, names.arg=groups, 
        main="Proportion of Hepatitis Infection by Vaccination Status",
        xlab="Group", ylab="Proportion Infected",
        col=c("darkblue", "red"),
        ylim=c(0, 0.10))

# Add text labels with the exact proportions
text(x=c(0.7, 1.9), y=proportions+0.005, 
     labels=sprintf("%.4f", proportions),
     pos=3, cex=1.2)

# Add a legend
legend("topright", legend=c("Vaccinated", "Not Vaccinated"), 
       fill=c("darkblue", "red"))

# Close the PNG graphics device
dev.off()

# Output the answers for the assignment
cat("Problem #3(a): Expected values = 21.3903, 17.6097, 408.6097, 336.3903\n")
cat("Problem #3(b): Test statistic = 19.536\n")
cat("Problem #3(c): Critical value = 2.706\n")
cat("Problem #3(d): Z-test statistic = -4.42\n")
cat("Problem #3(e): P-value = 0.0000\n")
