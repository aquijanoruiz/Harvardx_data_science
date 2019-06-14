
#-----------------------A way to download multiple packages---------------------
#Ipak function: install and load multiple R packages.
#Check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

model_packages <- c("class", "e1071", "MASS", "Rborist", "rpart", "randomForest", "Rborist", 
                    "gam", "caret", "naivebayes", "kknn", "klaR", "ranger", "wsrf", "RSNNS", 
                    "monmlp", "ada", "JOUSBoost", "gbm", "mboost", "import", "fastAdaboost")

ipak(model_packages)
install.packages(c("glm", "lda",  "naive_bayes",  "svmLinear", 
                   "gamboost",  "gamLoess", "qda", 
                   "knn", "kknn", "loclda", "gam",
                   "rf", "ranger",  "wsrf", "Rborist", 
                   "avNNet", "mlp", "monmlp",
                   "adaboost", "gbm",
                   "svmRadial", "svmRadialCost", "svmRadialSigma"))

#--------------------Q1---------------------
#Use the training set to build a model with several of the models available from the caret package. 
#We will test out all of the following models in this exercise

models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")

library(caret)
library(dslabs)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ #this takes the information form the vector models. We can call it function ("whatever name we like")
  print(model) #for the algorithm to work we need to print the information. 
  train(y ~ ., method = model, data = mnist_27$train)
})

names(fits) <- models

#--------------------Q2---------------------
#Now that you have all the trained models in a list, use sapply or map to create a matrix of predictions for the test 
#set. You should end up with a matrix with length(mnist_27$test$y) rows and length(models).

#What are the dimensions of the matrix of predictions?
fits[[23]] #we can get the info from each model using $
predict(fits[[23]],mnist_27$test) #we can predict the data of one model

pr <- sapply(fits, function(object){ #this takes the information form the vector models. We can call it function ("whatever name we like")
  print(object) #for the algorithm to work we need to print the information
  predict(object,mnist_27$test)
})

dim(pr)

#--------------------Q3---------------------
#Now compute accuracy for each model on the test set. Report the mean accuracy across all models.

confusionMatrix(factor(pr[,1]),mnist_27$test$y)$overall[["Accuracy"]] #this would be the confucion matrix of an individual model

library(tidyverse)

acc <- apply(pr, 2, function(prediction){ #we use the apply function because we need to work on a matrix
  print(prediction)
  confusionMatrix(factor(prediction),mnist_27$test$y)$overall[["Accuracy"]]
}) 

acc %>% mean()

#An easier way to do this

acc <- colMeans(pr == mnist_27$test$y)
acc
mean(acc)

#--------------------Q4---------------------
#Next, build an ensemble prediction by majority vote and compute the accuracy of the ensemble.
#What is the accuracy of the ensemble?

which.max(as.data.frame(t(pr))$V1)
library(mclust)

pr_data_frame <- as.data.frame(t(pr)) #this is the data we are going to use to apply the majority vote

pr_majority_vote <- apply(as.data.frame(t(pr)),2,function(prediction){
  print(prediction)
  majorityVote(prediction)$majority
})

ensemble <- mean(factor(pr_majority_vote) == mnist_27$test$y) #now we take the mean to get the accuracy

#This is a better way

votes <- rowMeans(pr == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

#--------------------Q5---------------------
#In Q3, we computed the accuracy of each method on the training set and noticed that the individual accuracies varied.

#How many of the individual methods do better than the ensemble?

acc #these are the accuracies obtained from different methods
acc > ensemble #there are the individual methods whose accuracy is higher than the ensemble

sum(acc > ensemble) #the total number

index <- acc > ensemble #we create an index to identify the models
models[index] #these are the models

mean(acc)

index <- acc > 0.845
models[index]

#--------------------Q6---------------------
#It is tempting to remove the methods that do not perform well and re-do the ensemble. The problem with this approach
#is that we are using the test data to make a decision. However, we could use the accuracy estimates obtained from 
#cross validation with the training data. Obtain these estimates and save them in an object. Report the mean accuracy 
#of the new estimates.
#-----------------------This was my try-------------------------
fits

pr_train <- sapply(fits, function(object){
  print(object)
  predict(object) #now we predict on the training data, not on the test data
})
  
acc_train <- apply(pr_train, 2, function(prediction){ #we use the apply function because we need to work on a matrix
  print(prediction)
  confusionMatrix(factor(prediction),mnist_27$train$y)$overall[["Accuracy"]]
}) 

library(tidyverse)
acc_train %>% mean()

votes <- rowMeans(pr_train == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
ensemble_train <- mean(y_hat == mnist_27$train$y)

#----------------------------This is what we had to do--------------------------
acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

min(fits$svmRadialSigma$results$Accuracy) #There are many accuracies in some models, we select the lowerst one
#--------------------Q7---------------------
#Now let's only consider the methods with an estimated accuracy of greater than or equal to 0.8 when constructing the ensemble.
#What is the accuracy of the ensemble now?

index_80 <- which(acc_hat >= 0.80)
new_mnodels <- models[index_80]

fits_new <- lapply(new_mnodels, function(model){ #this takes the information form the vector models. We can call it function ("whatever name we like")
  print(model) #for the algorithm to work we need to print the information. 
  train(y ~ ., method = model, data = mnist_27$train)
})

pr_new <- sapply(fits_new, function(object){ #this takes the information form the vector models. We can call it function ("whatever name we like")
  print(object) #for the algorithm to work we need to print the information
  predict(object,mnist_27$test)
})

votes <- rowMeans(pr_new == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

#This is a fastest way
ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)

rowMode