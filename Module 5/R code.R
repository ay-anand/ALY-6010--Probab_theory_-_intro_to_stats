# Install and load VIM package
install.packages("VIM")
install.packages("skimr")
install.packages("dplyr")

# Load necessary libraries
library(ggplot2)
library(GGally)
library(skimr)
library(VIM)
library(dplyr)
library(DataExplorer)


# Load the dataset
data <- read.csv("C:/Users/ayush/OneDrive/Documents/R_Assignment_Stats/RStudio/Module_5_R_practice/1_film-dataset_festival-program_wide.csv")

# Check the structure of the dataset
str(data)

# Get summary statistics
summary(data)

# Get a comprehensive overview
skim(data)

# Check for missing values in each column
colSums(is.na(data))

# Visualize missing data pattern
aggr(data, numbers = TRUE, sortVars = TRUE, cex.axis = 0.7)

plot_missing(data)

#removing columns which has null values > 70%
data_cleaned <- data[, !(names(data) %in% c("director.2", "director.3", "director.4", "director.5"))]
data_cleaned
# Check missing values again after cleaning
colSums(is.na(data_cleaned))


#Handling Missing values
data_cleaned <- na.omit(data)
plot_missing(data)

#Removing Unnecessary columns.
data <- data[, !(names(data) %in% c("prod.country.2.en", "regions.la", "regions.ocean", 
                                 "fest.show.delay.3year.and.more", "sample.year.first", "fest.sect.first"))]
plot_missing(data)

#------------Mean calculation---------------------------------------
# Filter the data for longer length movies
longer_movies <- data[data$length.min > 40, ]

# Calculate the mean length of longer movies
mean_longer_length <- mean(longer_movies$length.min)

print(mean_longer_length)
#------------------------actual mean------------------------------------
library(ggplot2)

# Create a data frame with the mean length
mean_df <- data.frame(
  Category = "Longer Movies",
  Mean_Length = mean_longer_length
)

# Create a histogram of film lengths
ggplot(longer_films, aes(x = longer_films$length.min)) +  # Replace 'Length' with the actual name of the column for film lengths in your dataset
  geom_histogram(binwidth = 30, fill = "skyblue", color = "black") +  # Adjust binwidth as necessary
  labs(title = "Distribution of Film Lengths",
       x = "Film Length (minutes)",
       y = "Count") +
  theme_minimal()


#-----------------hypothesis testing-------------------------------------------------------------
# Filter the data into longer and shorter films
longer_films <- data %>% filter(length.min > 90)
shorter_films <- data %>% filter(length.min <= 90)

t_test_result = t.test(longer_films$length.min, shorter_films$length.min, alternative = "greater")
print(t_test_result)

# Extract the p-value from the t-test result
p_value <- t_test_result$p.value

# Set a significance level
significance_level <- 0.05

# Decision-making using if-else statement
if (p_value < significance_level) {
  # Reject the null hypothesis
  print("The mean length of films longer than 90 minutes is significantly greater than films 90 minutes or shorter.")
} else {
  # Fail to reject the null hypothesis
  print("There is no significant evidence that the mean length of films longer than 90 minutes is greater than films 90 minutes or shorter.")
}
#------------------correlation--------------------
# Correlation Analysis
# Select relevant columns
cor_data <- data %>%
  select(prod.year, length.min, imdb.fest, retro.fest.sect, competition)

# Calculate correlation matrix
cor_matrix <- cor(cor_data, use = "complete.obs")
print(cor_matrix)

# Correlation Plot (limit to 5 variables for clarity)
corrplot(cor_matrix, method = "shade", 
         col = colorRampPalette(c("red", "yellow", "green"))(200),
         addCoef.col = "black", number.cex = 0.8)
#---------------------Regression----------------------------
# Fit a linear regression model with retrospective section participation as the outcome
# Load necessary package for exporting tables
library(stargazer)
# Model 1: Single predictor
reg_model_1 <- lm(length.min ~ prod.year, data = data_cleaned)
summary(reg_model_1)
stargazer(reg_model_1, type = "text", title = "Linear Regression Model 1")

# Model 2: Add an second predictor
reg_model_8 <- lm(length.min ~ prod.year + animt + doc + exp + fict + imdb.fest + retro.fest.sect + competition, data = data_cleaned)
summary(reg_model_8)
stargazer(reg_model_8, type = "text", title = "Linear Regression Model 2")

# Model 3: Add a third predictor
reg_model_10 <- lm(length.min ~ prod.year + animt + doc + exp + fict + imdb.fest + retro.fest.sect + competition , data = data_cleaned)
summary(reg_model_10)
stargazer(reg_model_10, type = "text", title = "Linear Regression Model 3")


