#-------------------------contents------------------------
#1. Difference with regression trees
#2. Example of classification trees
#3. Random forest (using continuos data: the 2008poll example)

#---------------------1. Difference with regression trees--------------------
#The first difference is that we form predictions by calculating which class is the most common among 
#the training set observations within the partition, rather than taking the average in each partition.
#The second is that we can no longer use RSS to choose the partition. While we could use the naive 
#approach of looking for partitions that minimize training error, better performing approaches use 
#more sophisticated metrics. Two of the more popular ones are the Gini Index and Entropy.

#To learn mroe about the Gini Index and Entropy, check section 32.9.4 of the book.

#---------------------2. Example of classification trees--------------------
#We are going to use categorical variables. For this, we are going to use the dataset mnist_27$train, 
#to predict if a number is 2 or 7.

train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)), #The tuneGrid parameter lets us decide which values the main parameter will take
                     data = mnist_27$train)

plot(train_rpart)

#The accuracy achieved by this approach is better than what we got with regression, but is not as good as
#what we achieved with kernel methods:
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

#Note that with decision trees, the boundary can't be smoothed. Decision trees are rarely the best performing 
#method since it is not very flexible and is highly unstable to changes in training data.

#---------------------3. Random forest (using continuos data: the 2008poll example)--------------------
#The goal is to improve prediction performance and reduce instability by averaging multiple decision trees 
#(a forest of trees constructed with randomness).

#To understand the logic behind decision trees, watch:
#https://www.youtube.com/watch?v=J4Wdy0Wc_xQ

head(polls_2008) #we will continue using this data for this exercise

library(randomForest)
fit <- randomForest(margin~., data = polls_2008)

plot(fit) #if we apply the function plot to the resulting object, stored in fit, 
#we see the how the error rate of our algorithm changes as we add trees.

polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")

#---------------------3. Random forest (using categorical data: the mnist_27 example)--------------------
train_rf <- randomForest(y ~ ., data=mnist_27$train) #we can use this function, but it is not so smooth. 
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"] #we see the accuracy
#We can improve this first model using the Rborist algorithm

train_rf_2 <- train(y ~ .,
                    method = "Rborist", #Here we are going to use a different random forest algorithm, Rborist, that is a little bit faster.
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]
