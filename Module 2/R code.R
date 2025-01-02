#Load necessary libraries
library(dplyr)

# Read the dataset
df1 <- read.csv("C:/Users/ayush/OneDrive/Documents/R_Assignment_Stats/RStudio/Module_2_R/Electric_Vehicle_Population_Size_History_By_County.csv")

# Select numerical columns for descriptive statistics
numerical_columns <- df1 %>% select(`Battery.Electric.Vehicles..BEVs.`, 
                                    `Plug.In.Hybrid.Electric.Vehicles..PHEVs.`, 
                                    `Electric.Vehicle..EV..Total`, 
                                    `Non.Electric.Vehicle.Total`, 
                                    `Total.Vehicles`, 
                                    `Percent.Electric.Vehicles`)

# Generate descriptive statistics for the entire sample
summary_stats <- numerical_columns %>% summarise_all(list(
  count = ~n(),
  mean = ~mean(., na.rm = TRUE),
  sd = ~sd(., na.rm = TRUE),
  min = ~min(., na.rm = TRUE),
  Q1 = ~quantile(., 0.25, na.rm = TRUE),
  median = ~median(., na.rm = TRUE),
  Q3 = ~quantile(., 0.75, na.rm = TRUE),
  max = ~max(., na.rm = TRUE)
))

print(summary_stats)


colSums(is.na(df1))
#------Above code will provide the descriptive statistics including mean, standard deviation, min, max, and quartiles for all numerical columns.--------------

#--------------------------------##---------------------------------------------
#Task 2: Descriptive Statistics by Group (State And Vehicle Primary Use)

# 2.1. Group by 'State' and calculate summary statistics for each state
grouped_stats <- df1 %>% group_by(State) %>% summarise(
  BEVs_mean = mean(`Battery.Electric.Vehicles..BEVs.`, na.rm = TRUE),
  BEVs_sd = sd(`Battery.Electric.Vehicles..BEVs.`, na.rm = TRUE),
  BEVs_min = min(`Battery.Electric.Vehicles..BEVs.`, na.rm = TRUE),
  BEVs_Q1 = quantile(`Battery.Electric.Vehicles..BEVs.`, 0.25, na.rm = TRUE),
  BEVs_median = median(`Battery.Electric.Vehicles..BEVs.`, na.rm = TRUE),
  BEVs_Q3 = quantile(`Battery.Electric.Vehicles..BEVs.`, 0.75, na.rm = TRUE),
  BEVs_max = max(`Battery.Electric.Vehicles..BEVs.`, na.rm = TRUE),
  
  PHEVs_mean = mean(`Plug.In.Hybrid.Electric.Vehicles..PHEVs.`, na.rm = TRUE),
  PHEVs_sd = sd(`Plug.In.Hybrid.Electric.Vehicles..PHEVs.`, na.rm = TRUE),
  EV_Total_mean = mean(`Electric.Vehicle..EV..Total`, na.rm = TRUE),
  Non_EVs_mean = mean(`Non.Electric.Vehicle.Total`, na.rm = TRUE),
  Total_Vehicles_mean = mean(`Total.Vehicles`, na.rm = TRUE),
  Percent_EV_mean = mean(`Percent.Electric.Vehicles`, na.rm = TRUE)
)
# Print the grouped summary statistics by state
print(grouped_stats)
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# 2.2. Group by 'Vehicle Primary Use' and calculate summary statistics for each group
grouped_stats_by_use <- df1 %>% group_by(Vehicle.Primary.Use) %>% summarise(
  BEVs_mean = mean(Battery.Electric.Vehicles..BEVs., na.rm = TRUE),
  BEVs_sd = sd(Battery.Electric.Vehicles..BEVs., na.rm = TRUE),
  BEVs_min = min(Battery.Electric.Vehicles..BEVs., na.rm = TRUE),
  BEVs_median = median(Battery.Electric.Vehicles..BEVs., na.rm = TRUE),
  BEVs_max = max(Battery.Electric.Vehicles..BEVs., na.rm = TRUE),
  
  
  PHEVs_mean = mean(Plug.In.Hybrid.Electric.Vehicles..PHEVs., na.rm = TRUE),
  PHEVs_sd = sd(Plug.In.Hybrid.Electric.Vehicles..PHEVs., na.rm = TRUE),
  PHEVs_min = min(Plug.In.Hybrid.Electric.Vehicles..PHEVs., na.rm = TRUE),
  PHEVs_median = median(Plug.In.Hybrid.Electric.Vehicles..PHEVs., na.rm = TRUE),
  PHEVs_max = max(Plug.In.Hybrid.Electric.Vehicles..PHEVs., na.rm = TRUE),
  
  EV_Total_mean = mean(Electric.Vehicle..EV..Total, na.rm = TRUE),
  EV_Total_sd = sd(Electric.Vehicle..EV..Total, na.rm = TRUE),
  EV_Total_min = min(Electric.Vehicle..EV..Total, na.rm = TRUE),
  EV_Total_median = median(Electric.Vehicle..EV..Total, na.rm = TRUE),
  EV_Total_max = max(Electric.Vehicle..EV..Total, na.rm = TRUE),
  
  Non_EVs_mean = mean(Non.Electric.Vehicle.Total, na.rm = TRUE),
  Total_Vehicles_mean = mean(Total.Vehicles, na.rm = TRUE),
  Percent_EV_mean = mean(Percent.Electric.Vehicles, na.rm = TRUE)
)

# Print the grouped statistics by Vehicle Primary Use
print(grouped_stats_by_use)
#846112122344324224244141448348843483483435145414138448454354414146641

#-------Above code groups the data by the State column and Vehicle Primary Use and calculates the descriptive statistics for each state.----------

#----------------------------##-------------------------------------------------
#Task 3: Interpretive Sentences

# Interpretation based on summary statistics

# Calculate statistics for BEVs
bevs_mean <- mean(df1$Battery.Electric.Vehicles..BEVs., na.rm = TRUE)
bevs_sd <- sd(df1$Battery.Electric.Vehicles..BEVs., na.rm = TRUE)
bevs_max <- max(df1$Battery.Electric.Vehicles..BEVs., na.rm = TRUE)
bevs_min <- min(df1$Battery.Electric.Vehicles..BEVs., na.rm = TRUE)
bevs_median <- median(df1$Battery.Electric.Vehicles..BEVs., na.rm = TRUE)

# Calculate statistics for Percent Electric Vehicles
percent_ev_mean <- mean(df1$Percent.Electric.Vehicles, na.rm = TRUE)
percent_ev_max <- max(df1$Percent.Electric.Vehicles, na.rm = TRUE)
percent_ev_median <- median(df1$Percent.Electric.Vehicles, na.rm = TRUE)

# Print interpretations
cat("The mean number of Battery Electric Vehicles (BEVs) across all counties is", bevs_mean, 
    "with a standard deviation of", bevs_sd, ".\n")

cat("The maximum number of BEVs in any county is", bevs_max, 
    "while the minimum is", bevs_min, ".\n")

cat("The median number of BEVs is", bevs_median, 
    "indicating that more than half of the counties have fewer than", bevs_median, "BEVs.\n")

cat("The average percentage of electric vehicles across all counties is", percent_ev_mean, 
    "with a maximum of", percent_ev_max, "% and a median of", percent_ev_median, "%.\n")



#00000000000000000000000000000000000

# Example interpretation for grouped statistics (state-level summary)
highest_bevs_state <- grouped_stats %>% filter(BEVs_mean == max(BEVs_mean)) %>% pull(State)
lowest_bevs_state <- grouped_stats %>% filter(BEVs_mean == min(BEVs_mean)) %>% pull(State)

cat("The state with the highest mean number of Battery Electric Vehicles (BEVs) is", highest_bevs_state, 
    "with an average of", max(grouped_stats$BEVs_mean), "BEVs per county.\n")
cat("The state with the lowest mean number of BEVs is", lowest_bevs_state, 
    "with an average of", min(grouped_stats$BEVs_mean), "BEVs per county.\n")

cat("On average, the state with the highest percentage of electric vehicles is Arkansas (AR) with", 
    max(grouped_stats$Percent_EV_mean), "% electric vehicles, while the state with the lowest is", 
    min(grouped_stats$Percent_EV_mean), "%.\n")

#-------------------Visualization----------------------------------

# Function to detect outliers using IQR method
detect_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)  # First quartile (25th percentile)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)  # Third quartile (75th percentile)
  IQR_value <- Q3 - Q1  # Interquartile range
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  return(which(column < lower_bound | column > upper_bound))
}

# Detecting outliers in the relevant numerical columns
outliers_bevs <- detect_outliers(df1$`Battery.Electric.Vehicles..BEVs.`)
outliers_phevs <- detect_outliers(df1$`Plug.In.Hybrid.Electric.Vehicles..PHEVs.`)
outliers_ev_total <- detect_outliers(df1$`Electric.Vehicle..EV..Total`)
outliers_non_ev_total <- detect_outliers(df1$`Non.Electric.Vehicle.Total`)
outliers_percent_ev <- detect_outliers(df1$`Percent.Electric.Vehicles`)

# Print outlier indices
cat("Outliers in BEVs:", outliers_bevs, "\n")
cat("Outliers in PHEVs:", outliers_phevs, "\n")
cat("Outliers in EV Total:", outliers_ev_total, "\n")
cat("Outliers in Non-EV Total:", outliers_non_ev_total, "\n")
cat("Outliers in Percent EV:", outliers_percent_ev, "\n")

# Treat outliers by capping them at the upper and lower bounds
treat_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  column[column < lower_bound] <- lower_bound
  column[column > upper_bound] <- upper_bound
  return(column)
}

# Treating outliers in relevant columns
df1$`Battery.Electric.Vehicles..BEVs.` <- treat_outliers(df1$`Battery.Electric.Vehicles..BEVs.`)
df1$`Plug.In.Hybrid.Electric.Vehicles..PHEVs.` <- treat_outliers(df1$`Plug.In.Hybrid.Electric.Vehicles..PHEVs.`)
df1$`Electric.Vehicle..EV..Total` <- treat_outliers(df1$`Electric.Vehicle..EV..Total`)
df1$`Non.Electric.Vehicle.Total` <- treat_outliers(df1$`Non.Electric.Vehicle.Total`)
df1$`Percent.Electric.Vehicles` <- treat_outliers(df1$`Percent.Electric.Vehicles`)

# Confirm outliers have been treated
summary(df1)
#----------------fixed outliers-----------------------------
# Load necessary libraries
library(dplyr)


# Function to detect outliers using IQR method (if not already handled)
detect_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  return(which(column < lower_bound | column > upper_bound))
}

# Treat outliers (already implemented)
treat_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  column[column < lower_bound] <- lower_bound
  column[column > upper_bound] <- upper_bound
  return(column)
}

# Treating outliers in relevant columns
df1$`Battery.Electric.Vehicles..BEVs.` <- treat_outliers(df1$`Battery.Electric.Vehicles..BEVs.`)
df1$`Plug.In.Hybrid.Electric.Vehicles..PHEVs.` <- treat_outliers(df1$`Plug.In.Hybrid.Electric.Vehicles..PHEVs.`)
df1$`Electric.Vehicle..EV..Total` <- treat_outliers(df1$`Electric.Vehicle..EV..Total`)
df1$`Non.Electric.Vehicle.Total` <- treat_outliers(df1$`Non.Electric.Vehicle.Total`)
df1$`Percent.Electric.Vehicles` <- treat_outliers(df1$`Percent.Electric.Vehicles`)

# Filter out NA and infinite values for plotting
df_clean <- df1 %>%
  filter(!is.na(`Electric.Vehicle..EV..Total`) & 
           !is.na(`Non.Electric.Vehicle.Total`) &
           is.finite(`Electric.Vehicle..EV..Total`) & 
           is.finite(`Non.Electric.Vehicle.Total`))

# Now plot after filtering out NA and infinite values
par(mfrow=c(1, 1), mar=c(4, 4, 2, 1))  # Reset plotting parameters

plot(df_clean$`Electric.Vehicle..EV..Total`, df_clean$`Non.Electric.Vehicle.Total`,
     main="Electric Vehicle Total vs Non-Electric Vehicle Total", 
     xlab="Electric Vehicle Total", 
     ylab="Non-Electric Vehicle Total", 
     col='blue', pch=19)

# Add a regression line
model <- lm(df_clean$`Non.Electric.Vehicle.Total` ~ df_clean$`Electric.Vehicle..EV..Total`)
abline(model, col='red')

# Display summary of the model
summary(model)

#-------------------------------------------

# Jitter Plot: Electric Vehicles by County
par(mfrow=c(1, 1), mar=c(4, 4, 2, 1))  # Reset plotting parameters
plot(jitter(as.numeric(factor(df_clean$County))), df_clean$`Electric.Vehicle..EV..Total`,
     main="Jitter Plot of Electric Vehicles by County", 
     xlab="Counties (jittered)", 
     ylab="Electric Vehicle Total", 
     col='green', pch=19)

# Boxplot: Percent Electric Vehicles by State
boxplot(df_clean$`Percent.Electric.Vehicles` ~ df_clean$State,
        main="Boxplot of Percent Electric Vehicles by State", 
        xlab="State", 
        ylab="Percent Electric Vehicles",
        col='purple', las=2, cex.axis=0.7)  # Reduced axis label size

# Boxplot: Total Vehicles by Vehicle Use
boxplot(df_clean$`Total.Vehicles` ~ df_clean$`Vehicle.Primary.Use`,
        main="Boxplot of Total Vehicles by Vehicle Use", 
        xlab="Vehicle Primary Use", 
        ylab="Total Vehicles",
        col='orange')
#--------------------------------------
# Scatter Plot: Percent Electric Vehicles vs Total Vehicles
plot(df_clean$`Total.Vehicles`, df_clean$`Percent.Electric.Vehicles`,
     main="Percent Electric Vehicles vs Total Vehicles", 
     xlab="Total Vehicles", 
     ylab="Percent Electric Vehicles", 
     col='blue', pch=19)

# Add a regression line
model1 <- lm(df_clean$`Percent.Electric.Vehicles` ~ df_clean$`Total.Vehicles`)
abline(model1, col='red')

# Description for the first plot
cat("1. Percent Electric Vehicles vs Total Vehicles:\n")
cat("   - Type: Scatter Plot\n")
cat("   - Description: This scatter plot shows the relationship between the total number of vehicles and the percentage of electric vehicles in each county.\n")
cat("   - Key Points:\n")
cat("     - A positive correlation may suggest that more total vehicles lead to a higher percentage of electric vehicles.\n")
cat("     - The red regression line provides a trend indicating the average relationship.\n")
cat("     - Points further from the line may indicate unique cases or outliers in EV adoption.\n\n")
#-----------------------------------------------------
# Boxplot: Total Vehicles by Vehicle Use
boxplot(df_clean$`Total.Vehicles` ~ df_clean$`Vehicle.Primary.Use`,
        main="Boxplot of Total Vehicles by Vehicle Use", 
        xlab="Vehicle Primary Use", 
        ylab="Total Vehicles",
        col='orange')

# Description for the third plot
cat("3. Boxplot of Total Vehicles by Vehicle Use:\n")
cat("   - Type: Boxplot\n")
cat("   - Description: This boxplot displays the distribution of total vehicles categorized by their primary use.\n")
cat("   - Key Points:\n")
cat("     - This helps to visualize the vehicle count in different usage categories (e.g., personal, commercial).\n")
cat("     - Variability in total vehicles can indicate demand trends in various sectors.\n")
cat("     - Allows stakeholders to identify areas for growth in EV adoption based on usage type.\n\n")

# Reset plot area
par(mfrow=c(1, 1), mar=c(5, 4, 4, 2))  # Reset to default layout

# Clean up workspace
rm(df1, model1)
#------------------------------------------------
# 2. Bar Plot: Total Vehicles Comparison
barplot(height = c(mean(df_clean$`Electric.Vehicle..EV..Total`, na.rm=TRUE), 
                   mean(df_clean$`Non.Electric.Vehicle.Total`, na.rm=TRUE), 
                   mean(df_clean$`Total.Vehicles`, na.rm=TRUE)),
        names.arg = c("Average EV Total", "Average Non-EV Total", "Average Total Vehicles"),
        main = "Bar Plot: Average Vehicles Comparison",
        ylab = "Average Count of Vehicles",
        col = c("green", "orange", "blue"))

# Description for the bar plot
cat("2. Bar Plot: Average Vehicles Comparison:\n")
cat("   - Type: Bar Plot\n")
cat("   - Description: This bar plot compares the average counts of electric vehicles, non-electric vehicles, and total vehicles across the dataset.\n")
cat("   - Key Points:\n")
cat("     - Provides a visual comparison of the average number of electric versus non-electric vehicles.\n")
cat("     - Highlights the overall vehicle count in the dataset, which can be useful for understanding EV adoption in context.\n")
cat("     - Can guide stakeholders on resource allocation based on vehicle distribution trends.\n\n")
#---------------------------------------------------
# 3. Boxplot: EV Total and Non-Electric Total by State
boxplot(df_clean$`Electric.Vehicle..EV..Total` ~ df_clean$State,
        main="Boxplot of EV Total by State", 
        xlab="State", 
        ylab="Electric Vehicle Total",
        col='lightblue', las=2, cex.axis=0.7)

# Description for the box plot
cat("3. Boxplot of EV Total by State:\n")
cat("   - Type: Boxplot\n")
cat("   - Description: This boxplot displays the distribution of electric vehicle totals across different states.\n")
cat("   - Key Points:\n")
cat("     - Illustrates the median and variability of electric vehicle totals in each state.\n")
cat("     - Outliers can be identified, which may indicate states with particularly high or low EV totals.\n")
cat("     - Useful for analyzing regional differences in electric vehicle adoption across states.\n\n")

# Reset plot area
par(mfrow=c(1, 1), mar=c(5, 4, 4, 2))  # Reset to default layout

# Clean up workspace
rm(df1, model_scatter)
#------------------------------------------------
# 1. Bar Plot: Count of Vehicle Primary Use by State
barplot(table(df_clean$`Vehicle.Primary.Use`), 
        main="Bar Plot of Vehicle Primary Use", 
        xlab="Vehicle Primary Use", 
        ylab="Count", 
        col='lightblue', 
        las=2, cex.axis=0.7)  # Reduced axis label size

# Description for the bar plot
cat("1. Bar Plot of Vehicle Primary Use:\n")
cat("   - Type: Bar Plot\n")
cat("   - Description: This bar plot illustrates the count of different vehicle primary uses in the dataset.\n")
cat("   - Key Points:\n")
cat("     - Each bar represents the number of vehicles categorized by their primary use (e.g., personal, commercial).\n")
cat("     - Provides a clear visual comparison of the distribution of vehicle usage types across the dataset.\n")
cat("     - Useful for understanding trends in vehicle usage that may influence EV adoption.\n\n")
#----------------------------------------------------

# 2. Pie Chart: Proportion of Vehicle Primary Use
vehicle_use_counts <- table(df_clean$`Vehicle.Primary.Use`)
pie(vehicle_use_counts,
    main="Pie Chart of Vehicle Primary Use",
    col=rainbow(length(vehicle_use_counts)),
    labels=paste(names(vehicle_use_counts), round(100 * vehicle_use_counts / sum(vehicle_use_counts), 1), "%"))

# Description for the pie chart
cat("2. Pie Chart of Vehicle Primary Use:\n")
cat("   - Type: Pie Chart\n")
cat("   - Description: This pie chart represents the proportion of different vehicle primary uses in the dataset.\n")
cat("   - Key Points:\n")
cat("     - Each slice indicates the percentage of vehicles that fall into a specific primary use category.\n")
cat("     - This visualization helps to quickly assess the dominance of certain vehicle usages (e.g., personal vs. commercial).\n")
cat("     - Provides insights into potential markets for electric vehicle deployment based on usage patterns.\n\n")

# Reset plot area
par(mfrow=c(1, 1), mar=c(5, 4, 4, 2))  # Reset to default layout

# Clean up workspace
rm(df1)
#-------------------------------------------------

# 2. Bar Plot: Average Percent Electric Vehicles by State
barplot(tapply(df_clean$`Percent.Electric.Vehicles`, df_clean$State, mean, na.rm=TRUE),
        main="Bar Plot of Average Percent Electric Vehicles by State", 
        xlab="State", 
        ylab="Average Percent Electric Vehicles", 
        col='lightblue', 
        las=2, 
        cex.axis=0.7)  # Reduced axis label size

# Description for the bar plot
cat("2. Bar Plot of Average Percent Electric Vehicles by State:\n")
cat("   - Type: Bar Plot\n")
cat("   - Description: This bar plot displays the average percent of electric vehicles for each state.\n")
cat("   - Key Points:\n")
cat("     - Each bar represents the average percentage of electric vehicles for a specific state.\n")
cat("     - Provides a clear comparison of electric vehicle adoption levels across different states.\n")
cat("     - Can highlight states that are leading in electric vehicle adoption or those that may need more incentives.\n\n")

# Reset plot area
par(mfrow=c(1, 1), mar=c(5, 4, 4, 2))  # Reset to default layout

# Clean up workspace
rm(df1)
#---------------------------------------------


