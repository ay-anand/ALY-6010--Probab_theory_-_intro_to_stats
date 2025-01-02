
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# Read the dataset
data <- read.csv("C:/Users/ayush/OneDrive/Documents/R_Assignment_Stats/RStudio/Week2_Prob/Border_Crossing_Entry_Data.csv")

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

Create Visualizations for Subset Data
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

