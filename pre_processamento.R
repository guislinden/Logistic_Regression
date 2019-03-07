#Upload the data to Rstudio
library(readr)
loan_data <- read_csv("C:\\Users\\User\\Documents\\GitHub\\Logistic_Regression\\loan_data.csv")

  
  #INICIO DE PRE-PROCESSAMENTO
  # Save the outlier's index to index_highage
  index_highage <- which(loan_data$age > 122)
  # Create data set new_data with outlier deleted
  loan_data <- loan_data[-index_highage, ]
  # Get indices of missing interest rates: na_index
  na_index <- which(is.na(loan_data$int_rate))
  # Remove observations with missing interest rates: loan_data_delrow_na
  loan_data_delrow_na <- loan_data[-na_index, ]
  # Make copy of loan_data
  loan_data_delcol_na <- loan_data
  # Delete interest rate column from loan_data_delcol_na
  loan_data_delcol_na$int_rate <- NULL
  # Compute the median of int_rate
  median_ir <- median(loan_data$int_rate, na.rm = T)
  # Make copy of loan_data
  loan_data_replace <- loan_data
  # Replace missing interest rates with median
  loan_data_replace$int_rate[na_index] <- median_ir
  # Make the necessary replacements in the coarse classification example below 
  loan_data$ir_cat <- rep(NA, length(loan_data$int_rate))
  
  loan_data$ir_cat[which(loan_data$int_rate <= 8)] <- "0-8"
  loan_data$ir_cat[which(loan_data$int_rate > 8 & loan_data$int_rate <= 11)] <- "8-11"
  loan_data$ir_cat[which(loan_data$int_rate > 11 & loan_data$int_rate <= 13.5)] <- "11-13.5"
  loan_data$ir_cat[which(loan_data$int_rate > 13.5)] <- "13.5+"
  loan_data$ir_cat[which(is.na(loan_data$int_rate))] <- "Missing"
  
  loan_data$ir_cat <- as.factor(loan_data$ir_cat)
  loan_data$emp_cat <- rep(NA, length(loan_data$emp_length))
  
  loan_data$emp_cat[which(loan_data$emp_length <= 15)] <- "0-15"
  loan_data$emp_cat[which(loan_data$emp_length > 15 & loan_data$emp_length <= 30)] <- "15-30"
  loan_data$emp_cat[which(loan_data$emp_length > 30 & loan_data$emp_length <= 45)] <- "30-45"
  loan_data$emp_cat[which(loan_data$emp_length > 45)] <- "45+"
  loan_data$emp_cat[which(is.na(loan_data$emp_length))] <- "Missing"
  
  loan_data$emp_cat <- as.factor(loan_data$emp_cat)
  #DIVISÃO ENTRE DATA SET DE TREINO E DE TESTE
  # Set seed of 567
  set.seed(567)
  # Store row numbers for training set: index_train 
  index_train <- sample(1:nrow(loan_data), 2/3 * nrow(loan_data))
  # Create training set: training_set
  training_set <- loan_data[index_train, ]
  # Create test set: test_set
  test_set <- loan_data[-index_train, ]
  
#training_set[c("X1","int_rate","emp_length")] <- NULL


 #write.csv(training_set, file = "training_set.csv")
 #write.csv(test_set, file = "test_set.csv")
  
  #TERMINOU O PREPROCESSAMENTO 
  
