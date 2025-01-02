# Load necessary libraries
library(ggplot2)
library(car)
library(boot)  # For cross-validation

# Load dataset (adjust path if necessary)
car_data <- read.csv("C:/Users/ayush/OneDrive/Documents/R_Assignment_Stats/RStudio/Final_Project/CAR DETAILS FROM CAR DEKHO.csv")

# ---------------------- Step 1: Exploratory Data Analysis (EDA) ----------------------
# Display the summary statistics and structure of the dataset
summary(car_data)
str(car_data)

# Plot the distribution of selling price
ggplot(car_data, aes(x = selling_price)) +
  geom_histogram(binwidth = 50000, fill = "blue", color = "black") +
  labs(title = "Distribution of Selling Price", x = "Selling Price", y = "Count")

# Plot the distribution of km_driven
ggplot(car_data, aes(x = km_driven)) +
  geom_histogram(binwidth = 10000, fill = "green", color = "black") +
  labs(title = "Distribution of Kilometers Driven", x = "Kilometers Driven", y = "Count")

# ---------------------- Step 2: Handling Outliers using IQR Method ----------------------
remove_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25)
  Q3 <- quantile(data[[column]], 0.75)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  return(data[data[[column]] >= lower_bound & data[[column]] <= upper_bound, ])
}

# Remove outliers in 'selling_price' and 'km_driven'
car_data <- remove_outliers(car_data, "selling_price")
car_data <- remove_outliers(car_data, "km_driven")

# ---------------------- Step 3: Hypothesis Testing ----------------------

# Question 1: Does the year of manufacture have a significant positive impact on the car's selling price?
## Hypothesis:
# H0: Year of manufacture has no positive impact on selling price
# H1: Year of manufacture has a positive impact on selling price

# Perform linear regression for year vs selling price
model_year <- lm(selling_price ~ year, data = car_data)
summary(model_year)

# Calculate p-value for year coefficient using one-sided t-test
t_stat_year <- summary(model_year)$coefficients["year", "t value"]
df_year <- model_year$df.residual
p_value_year <- pt(t_stat_year, df = df_year, lower.tail = FALSE)

# Hypothesis testing result
if (p_value_year < 0.05) {
  cat("Reject H0: There is a significant positive impact of year on selling price.\n")
} else {
  cat("Fail to reject H0: No significant positive impact of year on selling price.\n")
}

# Plot regression line for year vs selling price
ggplot(car_data, aes(x = year, y = selling_price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Year vs Selling Price with Regression Line", x = "Year", y = "Selling Price")

# Question 2: Does the number of kilometers driven have a significant negative impact on the car's selling price?
## Hypothesis:
# H0: Kilometers driven has no negative impact on selling price
# H1: Kilometers driven has a negative impact on selling price

# Perform linear regression for km_driven vs selling price
model_km <- lm(selling_price ~ km_driven, data = car_data)
summary(model_km)

# Calculate p-value for km_driven coefficient using one-sided t-test
t_stat_km <- summary(model_km)$coefficients["km_driven", "t value"]
df_km <- model_km$df.residual
p_value_km <- pt(t_stat_km, df = df_km, lower.tail = TRUE)

# Hypothesis testing result
if (p_value_km < 0.05) {
  cat("Reject H0: There is a significant negative impact of kilometers driven on selling price.\n")
} else {
  cat("Fail to reject H0: No significant negative impact of kilometers driven on selling price.\n")
}

# Plot regression line for km_driven vs selling price
ggplot(car_data, aes(x = km_driven, y = selling_price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Kilometers Driven vs Selling Price with Regression Line", x = "Kilometers Driven", y = "Selling Price")

# Question 3: Does the fuel type (Diesel vs. Petrol) significantly affect car selling prices?
## Hypothesis:
# H0: Diesel cars do not have a higher selling price than Petrol cars
# H1: Diesel cars have a higher selling price than Petrol cars

# Subset data for Petrol and Diesel
petrol <- subset(car_data, fuel == "Petrol")$selling_price
diesel <- subset(car_data, fuel == "Diesel")$selling_price

# Perform one-sided t-test using Welch's t-test for unequal variances
fuel_t_test <- t.test(diesel, petrol, alternative = "greater", var.equal = FALSE)

# Hypothesis testing result
if (fuel_t_test$p.value < 0.05) {
  cat("Reject H0: Diesel cars have a significantly higher selling price than Petrol cars.\n")
} else {
  cat("Fail to reject H0: No significant difference in selling price between Diesel and Petrol cars.\n")
}

# ---------------------- Step 4: Linear Regression Analysis (Fuel Type) ----------------------
# Filter data for Diesel and Petrol fuel types
filtered_data <- subset(car_data, fuel %in% c("Diesel", "Petrol"))

# Convert fuel type to a factor with labels
filtered_data$fuel_binary <- factor(filtered_data$fuel, levels = c("Petrol", "Diesel"), labels = c(0, 1))

# Split dataset into training (80%) and testing (20%) sets
set.seed(123)  # For reproducibility
sample_index <- sample(seq_len(nrow(filtered_data)), size = 0.8 * nrow(filtered_data))
train_data <- filtered_data[sample_index, ]
test_data <- filtered_data[-sample_index, ]

# Train linear regression model
regression_model <- lm(selling_price ~ fuel_binary, data = train_data)
summary(regression_model)

# Plot regression line for fuel type vs selling price
# Plot boxplot for fuel type vs selling price
ggplot(filtered_data, aes(x = as.factor(fuel_binary), y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  labs(title = "Fuel Type vs Selling Price (Boxplot)", x = "Fuel Type (0 = Petrol, 1 = Diesel)", y = "Selling Price")

# Regression equation: selling_price = intercept + (coefficient * fuel_binary)
intercept <- coef(regression_model)[1]
coefficient <- coef(regression_model)[2]
cat("Regression Equation: selling_price =", intercept, "+ (", coefficient, "* fuel_binary)\n")

# Predict on test dataset
test_data$predicted_price <- predict(regression_model, newdata = test_data, na.action = na.exclude)

# Compare predicted vs actual values
result_comparison <- data.frame(Actual = test_data$selling_price, Predicted = test_data$predicted_price)
print(head(result_comparison))

# Calculate RMSE (Root Mean Squared Error) for model performance
rmse <- sqrt(mean((test_data$selling_price - test_data$predicted_price)^2, na.rm = TRUE))
cat("RMSE:", rmse, "\n")

# ---------------------- Step 5: Cross-Validation ----------------------
# Simplify the cross-validation by using a simpler model
simple_model <- glm(selling_price ~ fuel_binary, data = train_data)
cv_results <- cv.glm(train_data, simple_model, K = 10)
cat("Cross-Validation Error:", cv_results$delta[1], "\n")
