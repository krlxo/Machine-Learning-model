# setting up the directory
getwd()
setwd("/Users/neelima/Downloads/krishna reddy gbus/krishna reddy gbus")

#attaching the required libraries
library(readr)
library(tidyverse)
library(dplyr)

#loading the rds file
df<- readRDS("loan_data.rds")
class(df)

# writing it into csv file
write.csv(df,file="data.csv")
loan_data <- read.csv("data.csv")

df1 <- read_rds("/Users/neelima/Desktop/gbus_final/gbus_FinalProject/loan_data.rds")
df1

#summary for data analysis

term_summary <- df1 %>%
  group_by(term) %>%
  summarise(
    n_customers = n(),
    customers_default = sum(loan_default == 'yes'),
    default_percent = 100 * mean(loan_default == 'yes')
  )


# Load required library
library(ggplot2)

# Visualization: Bar chart for loan default rates by term
ggplot(data = term_summary, aes(x = term, y = default_percent, fill = term)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Loan Default Rate by Term',
       x = 'Term',
       y = 'Default Percentage') +
  theme_light()

#plotting the graphs:

# 1. does the credit lines impact default rates?


# Calculate default rates by total credit lines bracket
default_credit_lines <- df1 %>%
  mutate(credit_lines_bracket = cut(total_credit_lines, breaks = c(0, 10, 20, 30, Inf), labels = c('0-10', '11-20', '21-30', '31+'))) %>%
  group_by(credit_lines_bracket) %>%
  summarise(
    n_customers = n(),
    customers_default = sum(loan_default == 'yes'),
    default_percent = 100 * mean(loan_default == 'yes')
  )

# Visualization: Bar chart
ggplot(data = default_credit_lines, aes(x = credit_lines_bracket, y = default_percent)) +
  geom_bar(stat = 'identity', fill = '#006EA1', color = 'white') +
  labs(title = 'Loan Default Rate by Credit Lines',
       x = 'Credit Lines',
       y = 'Percentage') +
  theme_light()


#2. scatter plot for finding the relationship between missed payments from last 2 years and default rates 

# Calculate default rates by missed payment in the last 2 years
default_missed_payment <- df1 %>%
  group_by(missed_payment_2_yr) %>%
  summarise(
    n_customers = n(),
    customers_default = sum(loan_default == 'yes'),
    default_percent = 100 * mean(loan_default == 'yes')
  )

# Visualization: Scatter plot
ggplot(data = default_missed_payment, aes(x = as.factor(missed_payment_2_yr), y = default_percent, color = as.factor(missed_payment_2_yr))) +
  geom_point(size = 4) +
  labs(title = 'Loan Default Rate by Missed Payments in Last 2 Years',
       x = 'Missed Payments (Last 2 Years)',
       y = 'Default Percentage') +
  theme_light()


#3 . debt to income ratio vs default rates
# Assuming 'debt_to_income' and 'loan_default' are valid column names in your data

# Calculate default rates by debt-to-income ratio bracket
default_debt_to_income <- df1 %>%
  mutate(debt_to_income_bracket = cut(debt_to_income, breaks = c(0, 10, 20, 30, 40, Inf), labels = c('0-10', '11-20', '21-30', '31-40', '41+'))) %>%
  group_by(debt_to_income_bracket) %>%
  summarise(
    n_customers = n(),
    customers_default = sum(loan_default == 'yes'),
    default_percent = 100 * mean(loan_default == 'yes')
  )

# Visualization: Bar chart
ggplot(data = default_debt_to_income, aes(x = debt_to_income_bracket, y = default_percent)) +
  geom_bar(stat = 'identity', fill = '#006EA1', color = 'white') +
  labs(title = 'Loan Default Rate by Debt-to-Income Ratio Bracket',
       x = 'Debt-to-Income Ratio Bracket',
       y = 'Default Percentage') +
  theme_light()



# knn

# Load required libraries
library(class)

# Normalize predictors for KNN (optional, but often beneficial for KNN)
normalized_predictors <- scale(loan[, predictors])

# Train the KNN model
knn_model <- knn(train = normalized_predictors, test = normalized_predictors, cl = loan$loan_default, k = 5)

# Evaluate the KNN model
accuracy_knn <- sum(knn_model == loan$loan_default) / nrow(loan)
print(paste("KNN Accuracy: ", accuracy_knn))



# Load required libraries
library(class)
library(MASS)

# Define predictors and target variable
predictors <- c("loan_amount", "installment", "interest_rate", "annual_income", "debt_to_income", "total_credit_lines")
target <- "loan_default"

# Normalize predictors for KNN (optional, but often beneficial for KNN)
normalized_predictors <- scale(loan_data[, predictors])

# Train the KNN model
knn_model <- knn(train = normalized_predictors, test = normalized_predictors, cl = loan_data[[target]], k = 5)

# Evaluate the KNN model
accuracy_knn <- sum(knn_model == loan_data[[target]]) / nrow(loan_data)
print(paste("KNN Accuracy: ", accuracy_knn))

# Fit QDA model
qda_model <- qda(as.formula(paste(target, "~", paste(predictors, collapse = "+"))), data = loan_data)

# Make predictions using QDA model
qda_predictions <- predict(qda_model, loan_data)

# Print confusion matrix for QDA
confusion_matrix_qda <- table(qda_predictions$class, loan_data[[target]])
print("Confusion Matrix for QDA:")
print(confusion_matrix_qda)

# Fit LDA model
lda_model <- lda(as.formula(paste(target, "~", paste(predictors, collapse = "+"))), data = loan_data)

# Make predictions using LDA model
lda_predictions <- predict(lda_model, loan_data)

# Print confusion matrix for LDA
confusion_matrix_lda <- table(lda_predictions$class, loan_data[[target]])
print("Confusion Matrix for LDA:")
print(confusion_matrix_lda)




