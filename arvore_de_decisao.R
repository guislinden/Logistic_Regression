#ARVORE DE DECISAO 

#Precisamos dar restart nos datasets 
library(readr)
test_set <- read_csv("test_set.csv")
training_set <- read_csv("training_set.csv")
attach(c(test_set, training_set))
test_set$X1 <- NULL
training_set$X1 <- NULL

#instalar a biblioteca que contem o modelo de arvore de decisao
#install.packages("rpart")
library(rpart)

#construiremos o modelo de arvore de decisao
tree_prior <- rpart(loan_status ~ ., method = "class",              
                    data = training_set, parms = list(prior=c(0.7,0.3)),
                    control = rpart.control(cp = 0.001))

# plotaremos a arvore
plot(tree_prior, uniform = T)

# adicionaremos o texto na arvore
text(tree_prior)

## Vemos que a nossa arvore é absurda e necessita de ajustes 
## Vamos cortar a nossa árvore


# Plot the cross-validated error rate as a function of the complexity parameter
plotcp(tree_prior)

# Vamos verificar para qual CP, a taxa de erro é  minimizada com a seguinte funcao. 
printcp(tree_prior)

# Agora, selecionaremos o índice no qual o valor é o menor erro (existem outras maneiras de fazer isso)
index <- which.min(tree_prior$cptable[ , "xerror"])

# Criaremos o valor com o minimo de erro da arvore
tree_min <- tree_prior$cptable[index, "CP"]

#  Usaremos a funcao prune para diminuir a complexidade da arvore
ptree_prior <- prune(tree_prior, cp = tree_min)

# E agora vamos ver a nossa arvore usando prp, que faz uma arvore mais bonita

prp(ptree_prior)

# E usar ela para prever o nosso test_set
pred_prior <- predict(ptree_prior  , newdata = test_set,  type = "class")

# Vamos verificar a matrix de confusão

confmat_prior <- table(test_set$loan_status, pred_prior)
confmat_prior

# Medir a precisão (TP+TN)/(TP+FP+TN+FN)

acc_prior <- sum(diag(confmat_prior)) / nrow(test_set)
acc_prior

# E agora vamos modificar o nosso modelo de previsão de modo que possamos escolher
# Quando queremos que o modelo considere quando default e quando não default

#Aqui, selecionaremos as probabilidades de default do nosso modelo
prob_default_prior <- predict(ptree_prior, newdata = test_set)[ ,2]

# Vamos dividir o nosso data set em 80% de um lado e 20% de outro
# E definir a porcentagem de default que corta a nossa serie de dados como o divisor de aguas

cutoff_prior <- quantile(prob_default_prior, .8)  
mean(prob_default_prior)

# Vamos transformar o nosso data_set em 1 e 0  
bin_pred_prior_80 <- ifelse(prob_default_prior > cutoff_prior, 1, 0)

# Obtain the actual default status for the accepted loans
accepted_status_prior_80 <-  test_set$loan_status[bin_pred_prior_80 == 0]

# Obtain the bad rate for the accepted loans
sum(accepted_status_prior_80) / length(accepted_status_prior_80)

# vamos construir o ROC 

ROC_prior <- roc(test_set$loan_status, prob_default_prior)

# Vamos plotar o ROC plot e verificar a AUC

plot(ROC_prior, col="blue")
auc(ROC_prior)

# library(rpart.plot)
# rpart.plot(tree_min)

 # novos_dados <- data.frame(loan_status = c(0,1),     
 #                     loan_amnt = c(100,200),
 #                     grade = c("A", "C") ,
 #                     home_ownership = c("RENT", "RENT") ,
 #                      annual_inc = c(1000000,5000),
 #                      age = c(30,20),
 #                     ir_cat = as.factor(c("8-11", "0-8")) ,
 #                      emp_cat = as.factor(c("0-15","0-15")))

# resposta = predict(tree_prior, newdata = novos_dados, type = "class")
# table(novos_dados$loan_status, resposta)

