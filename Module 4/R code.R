# Step 1: Install and load MASS package
install.packages("MASS")  # You can skip this if the package is already installed
library(MASS)

# Step 2: Load and explore the 'cats' dataset
data(cats)
head(cats)  # View the first few rows of the dataset
str(cats)   # Check the structure of the dataset

# Step 3: Subset the data by gender
male_cats <- subset(cats, subset=(cats$Sex == "M"))
female_cats <- subset(cats, subset=(cats$Sex == "F"))

# Step 4: Perform two-sample t-test with unequal variance (Welch's t-test)
t_test_result <- t.test(male_cats$Bwt, female_cats$Bwt, var.equal = FALSE)

# Step 5: View the results of the t-test
t_test_result


#--------------------------Visualisation------------------------------------------------
# Load the necessary library
library(ggplot2)

# Boxplot: Male vs Female Bodyweights
ggplot(cats, aes(x=Sex, y=Bwt, fill=Sex)) +
  geom_boxplot() +
  labs(title="Bodyweight Comparison of Male and Female Cats",
       x="Sex", y="Bodyweight (kg)") +
  theme_minimal()

# Histogram: Distribution of Bodyweight for Male and Female Cats
ggplot(cats, aes(x=Bwt, fill=Sex)) +
  geom_histogram(position="dodge", binwidth=0.2, color="black") +
  labs(title="Distribution of Bodyweight by Sex",
       x="Bodyweight (kg)", y="Count") +
  theme_minimal() +
  scale_fill_manual(values=c("skyblue", "yellow"))

#-------------------------------Part_2--------------------------------------------

# Sleep quality scores before and after a relaxation technique
pre_meditation <- c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4)
post_meditation <- c(6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9)

# Perform a paired t-test
paired_t_test <- t.test(pre_meditation, post_meditation, paired = TRUE)

# Output the result
paired_t_test

# for level of significance = 0.05
if (paired_t_test$p.value < 0.05) {
  print("Reject the null hypothesis")
} else {
  print("Fail to reject the null hypothesis")
}

# for level of significance = 0.1
if (paired_t_test$p.value < 0.1) {
  print("Reject the null hypothesis")
} else {
  print("Fail to reject the null hypothesis")
}
#-----------------------------Visualisation-----------------------------------------------

# Visualize sleep quality before and after meditation
boxplot(pre_meditation, post_meditation, 
        names = c("Before Meditation", "After Meditation"), 
        col = c("lightblue", "lightgreen"),
        main = "Comparison of Sleep Quality Before and After Meditation",
        ylab = "Sleep Quality Score")


