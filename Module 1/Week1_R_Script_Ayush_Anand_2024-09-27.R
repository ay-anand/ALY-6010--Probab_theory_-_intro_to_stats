install.packages("stats")
install.packages("dplyr")
# Load necessary libraries

library(stats)
library(dplyr)
library(skimr)
library(DataExplorer)

data <- read.csv("C:/Users/ayush/OneDrive/Documents/R_Assignment_1_Stats/RStudio/data_1.csv")

head(data)

df1 <- data
# Check the structure of the data
str(data)
# Convert Disbursement Date and End Date to Date format
df1$Disbursement.Date <- as.Date(df1$Disbursement.Date, format="%m/%d/%Y")
df1$End.Date <- as.Date(data$End.Date, format="%m/%d/%Y")

# Verify that the conversion worked
str(df1$Disbursement.Date)
str(df1$End.Date)

# Replace NA in Business Sector with "Unknown"
df1$Business.Sector[is.na(df1$Business.Sector)] <- "Unknown"

# Verify that missing values have been replaced
summary(df1$Business.Sector)

# Check summary statistics for Amount (USD)
summary(df1$Amount..USD.)

# If scaling is required (i.e., dividing by 1000 or 1,000,000 for better interpretability):
df1$Amount.USD.K <- df1$Amount..USD. / 1000  # Amount in thousands
df1$Amount.USD.M <- df1$Amount..USD. / 1000000  # Amount in millions

# Check the summary of scaled data
summary(df1$Amount.USD.K)Z
summary(df1$Amount.USD.M)

# Remove outliers in Is Woman Owned?
df1$Is.Woman.Owned <- ifelse(df1$Is.Woman.Owned %in% c(0, 1), df1$Is.Woman.Owned, NA)

# Remove outliers in Is First Time Borrower?
df1$Is.First.Time.Borrower <- ifelse(df1$Is.First.Time.Borrower %in% c(0, 1), df1$Is.First.Time.Borrower, NA)

# Verify the cleaned data
table(df1$Is.Woman.Owned, useNA = "ifany")
table(df1$Is.First.Time.Borrower, useNA = "ifany")
#---------------------------------------------------------------------------------------------
#Step 1: Frequency Distribution of Woman-Owned Businesses and First-Time Borrowers
# Frequency distribution for Is Woman Owned?
table(df1$Is.Woman.Owned)

# Frequency distribution for Is First Time Borrower?
table(df1$Is.First.Time.Borrower)

#Step 2: Cross-Tabulation of Woman-Owned Businesses and First-Time Borrowers with Loan Amount and Business Sector

# Cross-tabulation between Business Sector and Is Woman Owned
woman_owned_vs_sector <- xtabs(~ Business.Sector + Is.Woman.Owned, data=df1)
print(woman_owned_vs_sector)

# Cross-tabulation between Business Sector and Is First Time Borrower
first_time_vs_sector <- xtabs(~ Business.Sector + Is.First.Time.Borrower, data=df1)
print(first_time_vs_sector)

# Cross-tabulation between Is Woman Owned and Loan Amount
woman_owned_vs_loan <- aggregate(Amount..USD. ~ Is.Woman.Owned, df1, mean)
print(woman_owned_vs_loan)

# Cross-tabulation between Is First Time Borrower and Loan Amount
first_time_vs_loan <- aggregate(Amount..USD. ~ Is.First.Time.Borrower, df1, mean)
print(first_time_vs_loan)

#--------------------------Reset to default plotting parameters-----------------------------
dev.off()
#-------------------------------------------------------------------------------------------

#Step 3: Analyze Trends Based on Disbursement and End Dates

# Analyzing trends in loan amounts by disbursement date
disbursement_trend <- aggregate(Amount..USD. ~ Disbursement.Date, df1, sum)

# Create a ggplot
ggplot(disbursement_trend, aes(x=Disbursement.Date, y=Amount..USD.)) +
  geom_line(color="blue", size=1) +  # Line plot
  geom_point(color="blue", size=2) +  # Add points for better visibility
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Set breaks on x-axis
  labs(x="Disbursement Date", y="Total Loan Amount (USD)", 
       title="Loan Amount Trends Over Time") +
  theme(axis.text.x = element_text(angle=45, hjust=1),  # Rotate x-axis labels
        panel.grid.major = element_line(color = "grey", size = 0.5),  # Grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        plot.title = element_text(hjust = 0.5))  # Center title


# Analyzing trends in loan amounts by end date
end_trend <- aggregate(Amount..USD. ~ End.Date, df1, sum)

# Create a ggplot for end date trends
ggplot(end_trend, aes(x=End.Date, y=Amount..USD.)) +
  geom_line(color="red", size=1) +  # Line plot
  geom_point(color="red", size=2) +  # Add points for better visibility
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Set breaks on x-axis
  labs(x="End Date", y="Total Loan Amount (USD)", 
       title="Loan End Amount Trends Over Time") +
  theme(axis.text.x = element_text(angle=45, hjust=1),  # Rotate x-axis labels
        panel.grid.major = element_line(color = "grey", size = 0.5),  # Grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        plot.title = element_text(hjust = 0.5))  # Center title


#Step 4: Visualizations
# Histogram for Loan Amount (USD)
hist(df1$Amount..USD., breaks=50, col="skyblue", border="black",
     main="Distribution of Loan Amounts (USD)", xlab="Loan Amount (USD)")
#-------------------------------------------------------------------------------
# Bar plot for distribution of Loans by Business Sector
# Count the number of loans per business sector
business_sector_counts <- as.data.frame(table(df1$Business.Sector))

# Rename the columns for clarity
colnames(business_sector_counts) <- c("Business.Sector", "Number.of.Loans")

# Order the data frame by Number of Loans in descending order
business_sector_counts <- business_sector_counts[order(-business_sector_counts$Number.of.Loans), ]

# Add a blank column for visualization purposes (if needed)
business_sector_counts <- rbind(business_sector_counts, data.frame(Business.Sector = "Blank", Number.of.Loans = NA))

# Create a horizontal bar plot
ggplot(business_sector_counts, aes(x=reorder(Business.Sector, -Number.of.Loans), y=Number.of.Loans)) +
  geom_bar(stat="identity", fill="lightgreen") +
  labs(title="Distribution of Loans by Business Sector",
       x="Business Sector",
       y="Number of Loans") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1, size=8),  # Rotate x-axis labels
        axis.text.y = element_text(size=8),  # Adjust y-axis text size
        plot.title = element_text(hjust = 0.5, size=10),  # Center title and adjust size
        panel.grid.major = element_line(color = "grey", size = 0.5),  # Grid lines
        panel.grid.minor = element_blank())  # Remove minor grid lines
#-------------------------------------------------------------------------------
# Bar chart comparing and Count the number of woman-owned vs non-woman-owned businesses
woman_owned_counts <- as.data.frame(table(df1$Is.Woman.Owned))
colnames(woman_owned_counts) <- c("Is_Woman_Owned", "Count")

# Create a ggplot bar chart
ggplot(woman_owned_counts, aes(x=Is_Woman_Owned, y=Count, fill=Is_Woman_Owned)) +
  geom_bar(stat="identity", position=position_dodge(), color="black") +  # Create bars
  scale_fill_manual(values=c("pink", "lightblue"), labels=c("Not Woman Owned", "Woman Owned")) +  # Custom colors
  labs(title="Comparison of Woman-Owned vs Non-Woman-Owned Businesses", 
       x="Ownership Status", y="Number of Businesses") +  # Labels
  theme_minimal() +  # Use a minimal theme
  theme(axis.text.x = element_text(size=12),  # X-axis text size
        axis.text.y = element_text(size=12),  # Y-axis text size
        axis.title.x = element_text(size=14),  # X-axis title size
        axis.title.y = element_text(size=14),  # Y-axis title size
        plot.title = element_text(size=16, hjust=0.5),  # Title size and alignment
        panel.grid.major = element_line(color = "grey", size = 0.5),  # Major grid lines
        panel.grid.minor = element_blank()) +  # Remove minor grid lines
  geom_hline(yintercept=0, color="black", size=0.8) +  # Add horizontal line at y=0
  geom_vline(xintercept=0, color="black", size=0.8)  # Add vertical line at x=0

#-------------------------------------------------------------------------------
# Bar chart comparing and count the number of first-time borrowers vs repeat borrowers
first_time_borrower_counts <- as.data.frame(table(df1$Is.First.Time.Borrower))
colnames(first_time_borrower_counts) <- c("Is_First_Time_Borrower", "Count")

# Create a ggplot bar chart
ggplot(first_time_borrower_counts, aes(x=Is_First_Time_Borrower, y=Count, fill=Is_First_Time_Borrower)) +
  geom_bar(stat="identity", position=position_dodge(), color="black") +  # Create bars
  scale_fill_manual(values=c("orange", "lightgreen"), labels=c("Not First-Time", "First-Time")) +  # Custom colors
  labs(title="Comparison of First-Time Borrowers vs Repeat Borrowers", 
       x="Borrower Status", y="Number of Businesses") +  # Labels
  theme_minimal() +  # Use a minimal theme
  theme(axis.text.x = element_text(size=12),  # X-axis text size
        axis.text.y = element_text(size=12),  # Y-axis text size
        axis.title.x = element_text(size=14),  # X-axis title size
        axis.title.y = element_text(size=14),  # Y-axis title size
        plot.title = element_text(size=16, hjust=0.5),  # Title size and alignment
        panel.grid.major = element_line(color = "grey", size = 0.5),  # Major grid lines
        panel.grid.minor = element_blank()) +  # Remove minor grid lines
  geom_hline(yintercept=0, color="black", size=0.8) +  # Add horizontal line at y=0
  geom_vline(xintercept=0, color="black", size=0.8)  # Add vertical line at x=0
#-------------------------------------------------------------------------------
# Calculate loan duration in days
df1$Loan.Duration <- as.numeric(df1$End.Date - df1$Disbursement.Date)

# Correlation between loan amount and borrower type (woman-owned and first-time borrower)
cor(df1$Amount..USD., as.numeric(df1$Is.Woman.Owned), use="complete.obs")
cor(df1$Amount..USD., as.numeric(df1$Is.First.Time.Borrower), use="complete.obs")

# Pie chart of loan amount for woman-owned businesses
# Frequency distribution for woman-owned businesses
woman_owned_counts <- table(df1$Is.Woman.Owned)

# Create labels with percentages
labels <- c("Not Woman-Owned", "Woman-Owned")
percentages <- round(100 * woman_owned_counts / sum(woman_owned_counts), 1)
labels <- paste(labels, percentages, "%", sep=" ")

# Create a pie chart
pie(woman_owned_counts, labels=labels, col=c("lightblue", "lightpink"),
    main="Percentage of Woman-Owned vs. Non-Woman-Owned Businesses")
#-------------------------------------------------------------------------------
# Pie chart of loan amount for first-time borrowers
# Frequency distribution for first-time borrowers
first_time_counts <- table(df1$Is.First.Time.Borrower)

# Create labels with percentages
labels <- c("Not First-Time Borrower", "First-Time Borrower")
percentages <- round(100 * first_time_counts / sum(first_time_counts), 1)
labels <- paste(labels, percentages, "%", sep=" ")

# Create a pie chart
pie(first_time_counts, labels=labels, col=c("lightgreen", "lightcoral"),
    main="Percentage of First-Time vs. Repeat Borrowers")
#-------------------------------------------------------------------------------
# Average loan amount by business sector and woman-owned status
loan_sector_woman_owned <- aggregate(Amount..USD. ~ Business.Sector + Is.Woman.Owned, df1, mean)


# Bar plot comparing average loan amount by sector for woman-owned vs non-woman-owned businesses
# Sort the data by Amount..USD. in descending order for plotting
loan_sector_woman_owned <- loan_sector_woman_owned %>%
  arrange(desc(Amount..USD.))

# Bar plot comparing average loan amount by sector for woman-owned vs non-woman-owned businesses
ggplot(loan_sector_woman_owned, aes(fill=as.factor(Is.Woman.Owned), y=Amount..USD., x=reorder(Business.Sector, -Amount..USD.))) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=c("pink", "lightblue")) +
  labs(title="Loan Amount by Business Sector (Woman-Owned vs Non-Woman-Owned)",
       x="Business Sector", y="Average Loan Amount (USD)", fill="Is Woman Owned?") +
  theme(axis.text.x = element_text(angle=45, hjust=1))