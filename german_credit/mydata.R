library(readr)
data <- read_csv("C:\\Users\\User\\Documents\\GitHub\\Logistic_Regression\\MyData.csv")
plot(data$age, ylab = "Age")
index_highage <- which(data$age > 122)
new_data <- data[-index_highage, ]
plot(data$age, data$annual_inc, xlab = "Age", ylab = "Annual Income")
summary(data$int_rate)
na_index <- which(is.na(data$int_rate))
loan_data_delrow_na <- data[-na_index, ]
loan_data_delcol_na <- data
loan_data_delcol_na$int_rate <- NULL
# Compute the median of int_rate
median_ir <- median(data$int_rate, na.rm = T)

# Make copy of loan_data
loan_data_replace <- data

# Replace missing interest rates with median
loan_data_replace$int_rate[na_index] <- median_ir

# Check if the NAs are gone
summary(loan_data_replace$int_rate)
# Make the necessary replacements in the coarse classification example below 
data$ir_cat <- rep(NA, length(data$int_rate))

data$ir_cat[which(data$int_rate <= 8)] <- "0-8"
data$ir_cat[which(data$int_rate > 8 & data$int_rate <= 11)] <- "8-11"
data$ir_cat[which(data$int_rate > 11 & data$int_rate <= 13.5)] <- "11-13.5"
data$ir_cat[which(data$int_rate > 13.5)] <- "13.5+"
data$ir_cat[which(is.na(data$int_rate))] <- "Missing"

data$ir_cat <- as.factor(data$ir_cat)

# Look at your new variable using plot()
plot(data$ir_cat)
# Set seed of 567
set.seed(567)

# Store row numbers for training set: index_train
index_train <- sample(1:nrow(data), 2/3 * nrow(data))

# Create training set: training_set
training_set <- data[index_train, ]

# Create test set: test_set
test_set <- data[-index_train, ]

