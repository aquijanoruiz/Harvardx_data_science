#---------------------contents-------------------------
#1. Definition of k-nearest neighbors (kNN)
#2. The knn3() function to compute the k-nearest neighbors (kNN)
#3. Comparing to logistic regression
#4.1 Definition of overtraining
#4.2 Overtraining when k=1
#5. Oversmoothing when k is very high
#6. Picking the right k


#----------------------Preparing the data------------------------
library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$test%>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

head(mnist_27$test)

#------------------------1. Definition of k-nearest neighbors (kNN)-------------------------
#With k-nearest neighbors (kNN) we estimate p(x1, x2) in a similar way to bin smoothing. However, kNN is easier 
#to adapt to multiple dimensions. First we define the distance between all observations based on the features. 
#Then, for any point (x1, x2) for which we want an estimate of p(x1, x2), we look for the k nearest points to (x1, x2) 
#and then take an average of the 0s and 1s associated with these points.

#As with bin smoothers, we can control the flexibility of our estimate, in this case through the k parameter: 
#larger ks result in smoother estimates, while smaller ks result in more flexible and more wiggly estimates.

#------------------------2. The knn3() function to compute the k-nearest neighbors (kNN)---------------------

library(caret)
knn_fit <- knn3(y ~ ., data = mnist_27$train) # ~. means we use all the variables in the data set (x_1, x_2)
#Additional, this formula uses the default k=5.

#Now that we computed the knn3 model in the train data, we test the model in the test data.
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class") #type=class?
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]


#------------------------3. Comparing to logistic regression---------------------

fit_lm <- mnist_27$train %>% mutate(y = ifelse(y == 7, 1, 0)) %>% 
  lm(y ~ x_1 + x_2, data = .) #we use the lm function to calculate the logistic regression.

p_hat_lm <- predict(fit_lm, mnist_27$test) #we calculate the probability of being a 7
y_hat_lm <- factor(ifelse(p_hat_lm > 0.5, 7, 2))
confusionMatrix(data = y_hat_lm, reference = mnist_27$test$y)$overall["Accuracy"]

#We see that if we use the knn model, we have an accuracy of 0.815 compared to 0.75 from the logistic regression.

#------------------------4.1 Definition of overtraining---------------------
#Over-training is the reason that we have higher accuracy in the train set compared to the test set.
y_hat_knn_train <- predict(knn_fit, mnist_27$train, type = "class")
accuracy_train <- confusionMatrix(data = y_hat_knn_train, reference = mnist_27$train$y)$overall["Accuracy"]

y_hat_knn_test <- predict(knn_fit, mnist_27$test, type = "class")
accuracy_test <- confusionMatrix(data = y_hat_knn_test, reference = mnist_27$test$y)$overall["Accuracy"]

#We can look at both accuracies
cbind(c("train","test"),c(accuracy_train,accuracy_test))

#------------------------4.2 Overtraining when k=1---------------------
#Over-training is at its worst when we set k = 1. With k = 1, the (x1, x2) are unique. We will
#obtain perfect accuracy in the training set because each point is used to predict itself.

#Let's calculate the accuracy in the train set. We can see the accuracy is 100%.
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$train$y)$overall[["Accuracy"]] 

#Let's calculate the accuracy in the test set. We can see the accuracy is 74%. The accuracy dropped a lot because of overtraining.
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall["Accuracy"]

#------------------------5. Oversmoothing when k is very high--------------------
#When we used k=1 we overtrained. However, even when we used k=5 we overtrained. This means we have to pick a higer k.
#Let's see what happens whem we use a high k. We also got a low accuracy. In this case, it is 79%.
#This is because we "over-smoothed".

knn_fit_401 <- knn3(y ~ ., data= mnist_27$train, k=401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type="class")
confusionMatrix(data=y_hat_knn_401, reference = mnist_27$test$y)$overall["Accuracy"]

#------------------------6. Picking the right k--------------------
#Let's create a sequence of ks.
ks <- seq(3, 251, 2)

library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  y_hat_train <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat_train, reference = mnist_27$train$y)
  train_accuracy <- cm_train$overall["Accuracy"]
  y_hat_test <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat_test, reference = mnist_27$test$y)
  test_accuracy <- cm_test$overall["Accuracy"]
  tibble(train = train_accuracy, test = test_accuracy)
}) %>% mutate(k=ks)

accuracy %>% gather(set,accuracy,-k) %>% 
  ggplot(aes(x=k,y=accuracy,colour=set)) +
           geom_line() #we can compare the different accuracies according to the different ks.

#We pick the k that estimates the highest accuracy.
ks[which.max(accuracy$test)] #this is the k that maximizes the accuracy
max(accuracy$test) #this is the highest accuracy

