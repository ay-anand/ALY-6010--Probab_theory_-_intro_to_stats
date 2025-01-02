# Load necessary libraries for data manipulation, exploration, and visualization
library(stats)        # For statistical functions like t-test
library(dplyr)        # For data manipulation (selecting columns, filtering data, etc.)
library(skimr)        # For data summary and skim statistics
library(DataExplorer) # For exploratory data analysis and visualization
library(knitr)        # For creating neat tables and generating reports
library(ggplot2)      # For data visualization (e.g., creating boxplots)

# Install 'knitr' package (if not already installed)
install.packages("knitr")

# Load the Electric Vehicle Population dataset (update file path as needed)
ev_data <- read.csv("C:/Users/ayush/OneDrive/Documents/R_Assignment_Stats/RStudio/Week3_Prob/Electric_Vehicle_Population_Data.csv")

# Display the structure of the dataset to understand its variables and data types
str(ev_data)

# Summarize the dataset to get basic statistical insights like mean, min, max, etc.
summary(ev_data)

#-----------------------------------------------------------------------------
# Step 1: Identify numerical columns in the dataset
# Logical check for numeric columns and subset only numerical columns
numeric_columns <- sapply(ev_data, is.numeric)    # Identify numerical columns
ev_numeric_data <- ev_data[, numeric_columns]     # Subset the data to include only numeric columns

# Display the subset of numerical columns
print(ev_numeric_data)

# Remove unnecessary columns, keeping only relevant numerical ones for analysis
ev_data_clean <- ev_data[, c("Model.Year", "Electric.Range", "Base.MSRP")]

# Summarize the Outlier Detection:

A boxplot is used to detect outliers in the Model.Year column, which is a continuous variable.
Boxplots help identify outliers by showing data distribution and highlighting points outside the typical range based on the interquartile range (IQR).
Visualization of 'Model.Year' Data:

The boxplot() function is used to create a visual representation of the Model.Year values, with any points outside the whiskers indicating potential outliers.
The horizontal = TRUE argument creates a horizontal boxplot for clearer presentation.
Purpose of Visualization:

This visual analysis helps in identifying outliers or extreme values that may need to be addressed or removed to ensure accurate analysis in later steps.selected numerical columns
summary(ev_data_clean)

#-----------------------------------------------------------------------------
# Step 2: Handle missing values
# Remove rows that have NA (missing) values in any of the selected numerical columns
ev_data_clean1 <- na.omit(ev_data_clean)

# Check if any missing values remain after cleaning
sum(is.na(ev_data_clean1))  # Ensure all missing values are handled

# Display summary of the cleaned data
summary(ev_data_clean1)

#-----------------------------------------------------------------------------
# Step 3: Detect and visualize outliers in continuous variables
# Visualize outliers in 'Model.Year' using a boxplot
boxplot(ev_data_clean$Model.Year, main = "Boxplot for Model.Year", horizontal = TRUE)

#-----------------------------------------------------------------------------
# Step 4: Remove outliers from 'Model.Year'
# Calculate the first quartile (Q1) and third quartile (Q3) for 'Model.Year'
Q1 <- quantile(ev_data_clean$Model.Year, 0.25)
Q3 <- quantile(ev_data_clean$Model.Year, 0.75)

# Calculate the Interquartile Range (IQR)
IQR <- Q3 - Q1

# Define the lower and upper bounds for outliers using 1.5 * IQR
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filter out rows that fall outside the lower and upper bounds (removing outliers)
cleaned_ev_data_clean <- subset(ev_data_clean, Model.Year >= lower_bound & Model.Year <= upper_bound)

# Display the number of rows removed due to outliers
rows_removed1 <- nrow(ev_data_clean) - nrow(cleaned_ev_data_clean)
print(paste("Number of rows removed due to outliers: ", rows_removed1))

# Revisualize the boxplot for 'Model.Year' after removing outliers
boxplot(cleaned_ev_data_clean$Model.Year, main = "Boxplot for Model.Year (After Outlier Removal)", horizontal = TRUE)

#-----------------------------------------------------------------------------
# Step 5: Perform one-sample t-test on 'Model Year'
# Hypothesis Test:
# Null Hypothesis (H0): The mean model year is 2020

# One-sample t-test comparing the sample mean of 'Model Year' with 2020
t_test_result_clean <- t.test(cleaned_ev_data_clean$Model.Year, mu = 2020)  

# Display the results of the one-sample t-test
print(t_test_result_clean)

# Calculate and display the sample mean for 'Model Year'
sample_mean_Model.Year <- mean(cleaned_ev_data_clean$Model.Year)
print(sample_mean_Model.Year)

# Interpret the t-test results based on the p-value
if (t_test_result_clean$p.value < 0.05) {
  print("We reject the null hypothesis. The mean Model Year is significantly different from 2020.")
} else {
  print("We fail to reject the null hypothesis. The mean Model Year is not significantly different from 2020.")
}
