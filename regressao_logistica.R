
#### REGRESSAO LOGISTICA 
library(readr)
library(pROC)

test_set <- read_csv("test_set.csv")
training_set <- read_csv("training_set.csv")
attach(c(test_set, training_set))
test_set$X1 <- NULL
training_set$X1 <- NULL
# vamos construir um pequeno modelo para entendermos como funciona
log_model_cat <- glm(loan_status ~ ir_cat,
                     family = "binomial",
                     data = training_set
)


# Vamos ver os parametros do modelo 
log_model_cat

#QUANTO QUE A CHANCE DE DEFAULT AUMENTA SE COMPARARMOS 0-8 (variavel de referencia) COM 8-11
exp(log_model_cat$coefficients["ir_cat8-11"])

####Vamos construir um modelo maior com a seguinte linha de codigo
log_model_multi <- glm(loan_status ~ age + ir_cat + grade + loan_amnt + annual_inc, family = "binomial", data = training_set)


#### vamos ver os niveis de significancia dos parametros
summary(log_model_multi)

#### vamos usar a funcao predict para testar nosso modelo no test_set
predictions_all_small <- predict(log_model_multi, newdata = test_set, type = "response")

#### vamos verificar a variacao do nosso modelo de reg_logistica
range(predictions_all_small)

#### vamos finalmente construir um modelo com todos os parametros 
log_model_full <- glm(loan_status ~ ., family = "binomial", data = training_set)

#### Make PD-predictions for all test set elements using the the full logistic regression model
predictions_all_full <- predict(log_model_full, newdata = test_set, type = "response")

# vamos verificar novamente a variacao de probabilidade do nosso modelo
round(range(predictions_all_full),5)*100


### Vamos construir um vetor binario, transformando probabilidades acima de 15% 
### como 1 para default e probabilidades abaixo ou igual a 15% como nao default
pred_cutoff_15 <- ifelse(predictions_all_full > .15, 1,0)

# Construct a confusion matrix
table(test_set$loan_status, pred_cutoff_15)

# Classification accuracy=(TP+TN)(TP+FP+TN+FN)
# Sensitivity=TP(TP+FN)
# Specificity=TN(TN+FP)

# Load the pROC-package
#library(pROC)
# Construct the objects containing ROC-information
ROC_all_full <- roc(test_set$loan_status, predictions_all_full)

# Draw all ROCs on one plot
plot(ROC_all_full)

# Compute the AUCs
auc(ROC_all_full)

