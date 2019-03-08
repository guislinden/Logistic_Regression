
#### REGRESSAO LOGISTICA 
library(readr)
library(pROC)

test_set <- read_csv("test_set.csv")
training_set <- read_csv("training_set.csv")
attach(c(test_set, training_set))
test_set$X1 <- NULL
training_set$X1 <- NULL

# vamos construir um pequeno modelo para entendermos como funciona
modelo_pequeno <- glm(loan_status ~ ir_cat,
                     family = "binomial",
                     data = training_set
)


# Vamos ver os parametros do modelo 
modelo_pequeno

#QUANTO QUE A CHANCE DE DEFAULT AUMENTA SE COMPARARMOS 0-8 (variavel de referencia) COM 8-11
exp(modelo_pequeno$coefficients["ir_cat8-11"])

####Vamos construir um modelo maior com a seguinte linha de codigo
modelo_multiplo <- glm(loan_status ~ age + ir_cat + grade + 
                         loan_amnt + annual_inc, 
                       family = "binomial", data = training_set)


#### vamos ver os niveis de significancia dos parametros
summary(modelo_multiplo)

#### vamos usar a funcao predict para testar nosso modelo no test_set
previsao_modelo_multiplo <- predict(modelo_multiplo, newdata = test_set, type = "response")

#### vamos verificar a variacao do nosso modelo de reg_logistica
range(previsao_modelo_multiplo)

#### vamos finalmente construir um modelo com todos os parametros 
modelo_completo <- glm(loan_status ~ ., family = "binomial", data = training_set)

#### Realizaremos a previsao com o modelo completo
previsao_modelo_completo <- predict(modelo_completo, newdata = test_set, type = "response")

# Vamos verificar novamente a variacao de probabilidade do nosso modelo, mas agora o completo
round(range(previsao_modelo_completo),5)*100


### Vamos construir um vetor binario, transformando probabilidades acima de 15% 
### como 1 para default e probabilidades abaixo ou igual a 15% como nao default
pred_cutoff_15 <- ifelse(previsao_modelo_completo > .15, 1,0)

# Construiremos a matrix de confusao
table(test_set$loan_status, pred_cutoff_15)

# Precisão = (TP+TN)/(TP+FP+TN+FN)
# Sensibilidade = TP/(TP+FN)
# Especificidade = TN/(TN+FP)

# Aqui construiremos as informações para o gráfico ROC
ROC_modelo <- roc(test_set$loan_status, previsao_modelo_completo)

# Vamos fazer o nosso grafico ROC 
plot(ROC_modelo)

# Vamos medir a área debaixo da curva ROC

# A curva ROC é obtida pela representação da fração de Positivos Verdadeiros
# pelos Positivos Totais TP/(TP+FN) versus a
# fração de Positivos Falsos dos Negativos Totais TN/(TN+FP), 
# em várias configurações do limite

auc(ROC_modelo)


# novos_dados <- data.frame(loan_status = c(),
#                     loan_amnt = c(100,200),
#                     grade = c("A", "C") ,
#                     home_ownership = c("RENT", "RENT") ,
#                      annual_inc = c(1000000,5000),
#                      age = c(30,20),
#                     ir_cat = as.factor(c("8-11", "0-8")) ,
#                      emp_cat = as.factor(c("0-15","0-15")))
# 
# predict(modelo_completo, newdata = novos_dados, type = "response")

