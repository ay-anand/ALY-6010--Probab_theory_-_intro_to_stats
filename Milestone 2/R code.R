
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# Read the dataset
data <- read.csv("C:/Users/ayush/OneDrive/Documents/R_Assignment_Stats/RStudio/Milestone_1/Border_Crossing_Entry_Data.csv")

# Check the structure of the data
str(data)

# Summary statistics of the entire dataset
descriptive_stats <- summary(data)
print(descriptive_stats)

# Checking for missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))
print(missing_values)

# Clean the data by removing extreme outliers using z-score
data_cleaned <- data %>%
  filter(Value >= 0) %>%  # Remove negative values
  filter(abs((Value - mean(Value)) / sd(Value)) < 3)  # Remove outliers

colSums(is.na(data))
plot_missing(data)

#--------------------Descriptive Analysis on whole data------------------------
# Obtain descriptive statistics of the cleaned dataframe
descriptive_stats_cleaned <- summary(data_cleaned)
print(descriptive_stats_cleaned)

#----------------------------------------------------------------
# removing columns which has null values > 42.5%
data <- data[, !(names(data) %in% c("Latitude", "Longitude", "Date", "Point"))]

#---------------------Visualization on whole data------------------------------

#Identifying Outliers in the columns Value
# Calculate Z-scores for numeric columns
z_scores <- scale(data_cleaned[sapply(data_cleaned, is.numeric)])

# Identify outliers based on Z-score
outliers_z <- which(abs(z_scores) > 3, arr.ind = TRUE)

# Print outliers
if (length(outliers_z) > 0) {
  print("Outliers detected using Z-score method:")
  print(data_cleaned[outliers_z[, 1], ])
} else {
  print("No outliers detected using Z-score method.")
}

#------------------------------------------------------------------
# Load necessary libraries
library(ggplot2)
library(scales)  # For comma formatting

# Calculate IQR to identify outliers
Q1 <- quantile(data_cleaned$Value, 0.25, na.rm = TRUE)
Q3 <- quantile(data_cleaned$Value, 0.75, na.rm = TRUE)
IQR <- IQR(data_cleaned$Value, na.rm = TRUE)

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Create a scatter plot to visualize outliers
ggplot(data_cleaned, aes(x = seq_along(Value), y = Value)) +
  geom_point(aes(color = Value < lower_bound | Value > upper_bound), size = 2) +
  geom_hline(yintercept = lower_bound, color = "red", linetype = "dashed") +
  geom_hline(yintercept = upper_bound, color = "red", linetype = "dashed") +
  labs(title = "Scatter Plot of Value with Outlier Detection",
       x = "Index",
       y = "Value") +
  scale_color_manual(values = c("black", "blue"), 
                     labels = c("Normal", "Outlier")) +
  scale_x_continuous(labels = comma_format()) +  # Format x-axis labels
  scale_y_continuous(labels = comma_format()) +  # Format y-axis labels
  theme_minimal() +
  theme(legend.title = element_blank())
#---------------------------------------------------------
#----------------------After handling Outlier----------------------
#Handling Outliers using IQR method:
#Calculate Q1 (25th percentile) and Q3 (75th percentile)
  Q1 <- quantile(data$Value, 0.25, na.rm = TRUE)
Q3 <- quantile(data$Value, 0.75, na.rm = TRUE)

# Calculate the IQR
IQR_value <- IQR(data$Value, na.rm = TRUE)

# Determine lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify outliers
outliers <- data$Value < lower_bound | data$Value > upper_bound

# Replacing outliers with NA
data$Value[outliers] <- NA
sum(is.na(data$Value))

# Now replacing those outliers with the median value
median_value <- median(data$Value, na.rm = TRUE)
data$Value[outliers] <- median_value

# Verify the changes by checking the number of NA values (if replaced with NA)
sum(is.na(data$Value))
#-----------------------------------------------------------

# Create a bar plot showing the average value for each border
ggplot(data_cleaned, aes(x = Border, y = Value)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue") +
  labs(title = "Average Value by Border", x = "Border", y = "Average Value") +
  theme_minimal()

#--------------------------------------------------------------

# Create a box plot to visualize the distribution of value across different measures
ggplot(data_cleaned, aes(x = Measure, y = Value)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Box Plot of Value by Measure", x = "Measure", y = "Value") +
  scale_y_continuous(labels = comma_format()) +  # Format y-axis labels to numeric
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#-----------------------------------------------------------

# Create a scatter plot showing how value varies by border and measure
ggplot(data_cleaned, aes(x = Border, y = Value, color = Measure)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.6) +
  labs(title = "Scatter Plot of Value by Border and Measure", x = "Border", y = "Value") +
  scale_y_continuous(labels = comma_format()) +  # Format y-axis labels as numbers
  theme_minimal()
#----------------------------------------------------------

# Load necessary library
library(ggplot2)
library(scales)  # For comma formatting

# Create a faceted plot to show the distribution of value for each measure
ggplot(data_cleaned, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  facet_wrap(~ Measure) +
  labs(title = "Distribution of Value by Measure", x = "Value", y = "Frequency") +
  scale_x_continuous(labels = comma_format()) +  # Format x-axis labels as numeric
  scale_y_continuous(labels = comma_format()) +  # Format y-axis labels as numeric
  theme_minimal()
#-----------------------------------------------------

# Create a violin plot to visualize the distribution density of value for each border
ggplot(data_cleaned, aes(x = Border, y = Value)) +
  geom_violin(fill = "lightyellow", alpha = 0.6) +
  labs(title = "Violin Plot of Value by Border", x = "Border", y = "Value") +
  scale_y_continuous(labels = comma_format()) +  # Format y-axis labels as numbers
  theme_minimal()
#--------------------For Subset Data---------------------------------

# Calculate descriptive statistics for each subset of data
descriptive_stats <- data_cleaned %>%
  group_by(Border, Measure) %>%  # Group the data by Border and Measure
  summarise(
    Count = n(),  # Count the number of observations in each group
    Mean = mean(Value, na.rm = TRUE),  # Calculate mean of Value
    Median = median(Value, na.rm = TRUE),  # Calculate median of Value
    Min = min(Value, na.rm = TRUE),  # Calculate minimum Value
    Max = max(Value, na.rm = TRUE),  # Calculate maximum Value
    StdDev = sd(Value, na.rm = TRUE)  # Calculate standard deviation of Value
  ) %>%
  ungroup()  # Remove grouping

# Display the descriptive statistics
print(descriptive_stats)

#Create Visualizations for Subset Data
# Create a bar plot of mean value for each Border and Measure
ggplot(descriptive_stats, aes(x = Border, y = Mean, fill = Measure)) +
  geom_bar(stat = "identity", position = "dodge") +  # Create bars for mean values, side-by-side for different measures
  labs(title = "Mean Value by Border and Measure", x = "Border", y = "Mean Value") +
  theme_minimal()  # Use a minimal theme for the plot
#--------------------------------------------------------
#Visualization of subset
# Create a bar plot of mean value for each Border and Measure
ggplot(descriptive_stats, aes(x = Border, y = Mean, fill = Measure)) +
  geom_bar(stat = "identity", position = "dodge") +  # Create bars for mean values, side-by-side for different measures
  labs(title = "Mean Value by Border and Measure", x = "Border", y = "Mean Value") +
  theme_minimal()  # Use a minimal theme for the plot
#-----------------------------------------------------------

# Create a box plot to visualize the distribution of value for each Border and Measure
ggplot(data_cleaned, aes(x = Border, y = Value, fill = Measure)) +
  geom_boxplot() +  # Create box plots to show value distributions
  labs(title = "Box Plot of Value by Border and Measure", x = "Border", y = "Value") +
  scale_y_continuous(labels = comma_format()) +  # Format y-axis labels as numerical values
  theme_minimal() +  # Use a minimal theme for the plot
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better readability

#--------------------------------------------Milestone 2----------------------------------------

# Filter the data for US-Mexico border
us_mexico_data <- data_cleaned %>%
  filter(Border == "US-Mexico Border")

# Create a density plot to show the hypothesized mean of 50,000
ggplot(us_mexico_data, aes(x = Value)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  geom_vline(xintercept = 50000, color = "red", linetype = "dashed", size = 1.2) +
  labs(title = "Density Plot of Border Crossings for US-Mexico Border",
       x = "Number of Crossings", y = "Density") +
  theme_minimal() +
  annotate("text", x = 50000, y = 0.00002, label = "H0 Mean (50,000)", color = "red")

#------------------------------One-Sample T-Test------------------------------------------

# Hypothesis testing steps
# 1. Define hypotheses:
#    H0: mu >= 50000 (Null Hypothesis)
#    H1: mu < 50000 (Alternative Hypothesis)

# Perform the one-sample t-test
t_test_result <- t.test(us_mexico_data$Value, mu = 50000, alternative = "less")

# Print the results of the t-test
print(t_test_result)

# 2. Determine significance level (alpha)
alpha <- 0.05

# 3. Extract test statistic and p-value
test_statistic <- t_test_result$statistic
one_sample_p_value <- t_test_result$p.value

# 4. Calculate the critical value for a one-tailed test
critical_value <- qt(alpha, df = t_test_result$parameter, lower.tail = FALSE)

# 5. Decision rule:
#    If the p-value < alpha, reject H0; otherwise, fail to reject H0.

# 6. Print the test statistic, p-value, critical value, and decision
cat("Test Statistic:", test_statistic, "\n")
cat("P-value:", one_sample_p_value, "\n")
cat("Critical Value:", critical_value, "\n")

if (one_sample_p_value < alpha) {
  cat("Reject the null hypothesis (H0).\n")
} else {
  cat("Fail to reject the null hypothesis (H0).\n")
}
#------------------------------Two-Sample T-Test Hypothesis---------------------------------------
# Filter the data for US-Mexico and US-Canada borders
us_mexico_data <- data_cleaned %>%
  filter(Border == "US-Mexico Border")

us_canada_data <- data_cleaned %>%
  filter(Border == "US-Canada Border")

# Display the first few rows of each data frame to verify
cat("First few rows of US-Canada Border data:\n")
head(us_canada_data)

cat("First few rows of US-Mexico Border data:\n")
head(us_mexico_data)

# Calculate mean for both borders
mean_values <- data.frame(
  Border = c("US-Mexico Border", "US-Canada Border"),
  Mean = c(mean(us_mexico_data$Value, na.rm = TRUE), 
           mean(us_canada_data$Value, na.rm = TRUE))
)
# Create a bar plot to visualize the average number of crossings
ggplot(mean_values, aes(x = Border, y = Mean, fill = Border)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = round(Mean, 0)), vjust = -0.5, size = 5) +  # Add mean labels above bars
  labs(title = "Average Number of Vehicle Crossings",
       x = "Border",
       y = "Average Crossings") +
  scale_y_continuous(labels = scales::comma_format()) +  # Format y-axis labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#-------------------------------------------------------------------------------------
# H0: mu_US-Mexico <= mu_US-Canada (The average number of vehicle crossings from US-Mexico is less than or equal to US-Canada)
# H1: mu_US-Mexico > mu_US-Canada (The average number of vehicle crossings from US-Mexico is greater than US-Canada)

# Perform the two-sample t-test
two_sample_t_test_result <- t.test(us_mexico_data$Value, us_canada_data$Value, 
                                   alternative = "greater")

# Print the results of the two-sample t-test
print(two_sample_t_test_result)

# Extract the test statistic and p-value
two_sample_test_statistic <- two_sample_t_test_result$statistic
two_sample_p_value <- two_sample_t_test_result$p.value

# Calculate the critical value for a one-tailed test
two_sample_critical_value <- qt(alpha, df = two_sample_t_test_result$parameter, lower.tail = FALSE)

# Decision rule: 
# If the p-value < alpha, reject H0; otherwise, fail to reject H0.

# Print the test statistic, p-value, critical value, and decision
cat("Two-Sample Test Statistic:", two_sample_test_statistic, "\n")
cat("Two-Sample P-value:", two_sample_p_value, "\n")
cat("Two-Sample Critical Value:", two_sample_critical_value, "\n")

if (two_sample_p_value < alpha) {
  cat("Reject the null hypothesis (H0).\n")
} else {
  cat("Fail to reject the null hypothesis (H0).\n")
}

