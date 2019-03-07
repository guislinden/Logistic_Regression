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

##aqui vemos que a nossa arvore eh absurda e necessita de ajustes 


# Plot the cross-validated error rate as a function of the complexity parameter
plotcp(tree_prior)

# Use printcp() to identify for which complexity parameter the cross-validated error rate is minimized.
printcp(tree_prior)

# Create an index for of the row with the minimum xerror
index <- which.min(tree_prior$cptable[ , "xerror"])

# Create tree_min
tree_min <- tree_prior$cptable[index, "CP"]

#  Prune the tree using tree_min
ptree_prior <- prune(tree_prior, cp = tree_min)

# Use prp() to plot the pruned tree
prp(ptree_prior)

pred_prior <- predict(ptree_prior  , newdata = test_set,  type = "class")

# construct confusion matrices using the predictions.

confmat_prior <- table(test_set$loan_status, pred_prior)
confmat_prior
# Compute the accuracies

acc_prior <- sum(diag(confmat_prior)) / nrow(test_set)
acc_prior

# Make predictions for the probability of default using the pruned tree and the test set.
prob_default_prior <- predict(ptree_prior, newdata = test_set)[ ,2]

# Obtain the cutoff for acceptance rate 80%
cutoff_prior <- quantile(prob_default_prior, 0.8)  

# Obtain the binary predictions.
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
