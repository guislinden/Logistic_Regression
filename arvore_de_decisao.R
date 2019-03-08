#ARVORE DE DECISAO 

# Daremos restart nos datasets 
library(readr)
test_set <- read_csv("test_set.csv")
training_set <- read_csv("training_set.csv")
attach(c(test_set, training_set))
test_set$X1 <- NULL
training_set$X1 <- NULL

#instalar a biblioteca que contem o modelo de arvore de decisao
#install.packages("rpart")
library(rpart)
library(pROC)
library(rpart.plot)

#construiremos o modelo de arvore de decisao
arvore_completa <- rpart(loan_status ~ ., method = "class",              
                    data = training_set, parms = list(prior=c(0.7,0.3)),
                    control = rpart.control(cp = 0.001))

# plotaremos a arvore
plot(arvore_completa, uniform = T)

# adicionaremos o texto na arvore
text(arvore_completa)

## Vemos que a nossa arvore é absurda e necessita de ajustes 
## Vamos cortar a nossa árvore


# Plot the cross-validated error rate as a function of the complexity parameter
plotcp(arvore_completa)

# Vamos verificar para qual CP, a taxa de erro é  minimizada com a seguinte funcao. 
printcp(arvore_completa)

# Agora, selecionaremos o índice no qual o valor é o menor erro (existem outras maneiras de fazer isso)
indice <- which.min(arvore_completa$cptable[ , "xerror"])

# Criaremos o valor com o minimo de erro da arvore
minimo_arvore <- arvore_completa$cptable[indice, "CP"]

#  Usaremos a funcao prune para diminuir a complexidade da arvore
arvore_cortada <- prune(arvore_completa, cp = minimo_arvore)

# E agora vamos ver a nossa arvore usando prp, que faz uma arvore mais bonita

prp(arvore_cortada)
# E usar ela para prever o nosso test_set
previsao_arvore <- predict(arvore_cortada  , newdata = test_set,  type = "class")

# Vamos verificar a matrix de confusão

matrix_confusao <- table(test_set$loan_status, previsao_arvore)
matrix_confusao

# Medir a precisão (TP+TN)/(TP+FP+TN+FN)

precisao <- sum(diag(matrix_confusao)) / nrow(test_set)
precisao

# E agora vamos modificar o nosso modelo de previsão de modo que possamos escolher
# Quando queremos que o modelo considere quando default e quando não default

#Aqui, selecionaremos as probabilidades de default do nosso modelo
prob_default_cortada <- predict(arvore_cortada, newdata = test_set)[ ,2]

# Vamos dividir o nosso data set em 80% de um lado e 20% de outro
# E definir a porcentagem de default que corta a nossa serie de dados como o divisor de aguas

divisor_aguas <- quantile(prob_default_cortada, .8)  

# Vamos transformar o nosso data_set em 1 e 0  
previsor_binario_80 <- ifelse(prob_default_cortada > divisor_aguas, 1, 0)

# Obteremos o status real para os emprestimos que o modelo disse que foi 0
aceitos_previsor_80 <-  test_set$loan_status[previsor_binario_80 == 0]

# Aqui, compararemos quantos falsos positivos tem no total de positivos
# Temos a taxa de "Bad-Rate"

sum(aceitos_previsor_80) / length(aceitos_previsor_80)



## Vamos construir o ROC 

ROC_arvore_cortada <- roc(test_set$loan_status, prob_default_cortada)

# Vamos plotar o ROC plot e verificar a AUC

plot(ROC_arvore_cortada, col="blue")
auc(ROC_arvore_cortada)

# 
# rpart.plot(tree_min)

 # novos_dados <- data.frame(loan_status = c(0,1),     
 #                     loan_amnt = c(100,200),
 #                     grade = c("A", "C") ,
 #                     home_ownership = c("RENT", "RENT") ,
 #                      annual_inc = c(1000000,5000),
 #                      age = c(30,20),
 #                     ir_cat = as.factor(c("8-11", "0-8")) ,
 #                      emp_cat = as.factor(c("0-15","0-15")))

# resposta = predict(arvore_completa, newdata = novos_dados, type = "class")
# table(novos_dados$loan_status, resposta)

