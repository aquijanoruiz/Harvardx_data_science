#----------------------contents-----------------------
#1. The caret package (introduction)
#2. The caret train function
#3. Crossvalitadion in the train furction (the default values)
#3.1 Crossvalitadion in the train function (the tuneGrid argument)
#4. Accessing the info obtained from the train function
#5. The confusion matrix
#6. Crossvalitadion in the train function (chaing the number of bootstraps)

#----------------------1. The caret package (introduction)------------------------
#Algorithms are distributed via different packages, developed by different authors, and often use different syntax. 
#The caret package tries to consolidate these differences and provide consistency. It currently includes 
#237 different methods. Keep in mind that caret does not include the needed packages and, to implement a package 
#through caret, you still need to install the library.

library(dslabs)
library(caret)

str(mnist_27)
head(mnist_27)

#----------------------2. The caret train function------------------------
#The caret train function lets us train different algorithms using similar syntax.


train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

#----------------------2. The caret predict function------------------------
#To make predictions, we can use the output of this function directly without needing to look at the specifics
#of predict.glm and predict.knn. Instead, we can learn how to obtain predictions from predict.train.

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

#This permits us to quickly compare the algorithms. For example, we can compare the accuracy like this:
confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

#----------------------3. Crossvalitadion in the train fucntion (the default values)------------------------

#When an algorithm includes a tuning parameter, train automatically uses cross validation to decide among
#a few default values. To find out what parameter or parameters are optimized, you can study the output of:
getModelInfo("knn")
modelLookup("knn") #to use a quick lookup

train_knn <- train(y ~ ., method = "knn", data = mnist_27$train) #these are the default values
ggplot(train_knn, highlight = TRUE) #you can quickly see the results of the cross validation using the ggplot function. 
#The argument highlight highlights the max.

#----------------------3.1 Crossvalitadion in the train function (the tuneGrid argument)------------------------
#By default, the cross validation is performed by taking 25 bootstrap samples comprised of 25% of the
#observations. For the kNN method, the default is to try k = 5, 7, 9.
set.seed(2008)
train_knn <- train(y ~ ., method = "knn",
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2))) #We change the defatult (k = 5, 7, 9), using the tuneGrid parameter.
#The grid of values must be supplied by a data frame with the parameter names as specified in the modelLookup output.

ggplot(train_knn, highlight = TRUE)

#NOTE: Note that when running this code, we are fitting 30 versions of kNN to 25 bootstrapped samples. Since we
#are fitting 30 × 25 = 750 kNN models, running this code will take several seconds. We set the seed because
#cross validation is a random procedure and we want to make sure the result here is reproducible.

#----------------------4. Accessing the info obtained from the train function------------------------
train_knn$bestTune #To access the parameter that maximized the accuracy

train_knn$finalModel #To access the best performing model

#----------------------5. The confusion matrix------------------------
#NOTE: The best accuracy obtained from the train function is obtained by doing the crossvalidation on the train set.
#Now we need to test the model on the test set. For this we use the confusionMatrix function.

confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

#The function predict will use this best performing model. Here is the accuracy of the best model when
#applied to the test set, which we have not used at all yet because the cross validation was done on the training set.

#----------------------6. Crossvalitadion in the train function (chaing the number of bootstraps)------------------------
#If we want to change how we perform cross validation, we can use the trainControl function. We can make
#use, for example, 10-fold cross validation. 

control <- trainControl(method = "cv", number = 10, p = .9) #This means we have 10 samples using 10% of the observations each.

train_knn_cv <- train(y ~ ., method = "knn",
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)

ggplot(train_knn_cv, highlight = TRUE)

#We can also see the standard deviation bars obtained from the cross validation samples

library(tidyverse)
train_knn$results %>%
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k,
                    ymin = Accuracy - AccuracySD,
                    ymax = Accuracy + AccuracySD))
