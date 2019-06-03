#-------------------------contents------------------------
#1. Preparing the data
#2. Partitioning the data (creating the regression tree with the rpart function)
#2.1 Plotting the decision tree
#3. Complexity parameter (definition)
#3.1 Complexity parameter of 0 and minsplit = 2
#3.2 Pruning a tree
#3.3 How to choose the correct parameters?


#-------------------------1. Preparing the data---------------------
#To introduce regression trees, we will use the 2008 poll data used in previous sections to describe 
#the basic idea of how we build these algorithms.

data("polls_2008")
head(polls_2008)
qplot(day, margin, data = polls_2008)

#------------------------2. Partitioning the data (creating the regression tree with the rpart function)------------------
library(rpart) #we use this package
fit <- rpart(margin ~ ., data = polls_2008)

#------------------------2.1 Plotting the decision tree------------

plot(fit, margin = 0.1)
text(fit, cex = 0.75) #this is the size of the font

polls_2008 %>%
  mutate(y_hat = predict(fit)) %>% #we use the predict function to calculate y_hat
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red") #we use geom_step to illustrate the y_hat

#------------------------3. Complexity parameter (definition)----------------
#Every time we split and define two new partitions, our training set RSS decreases. This is because with 
#more partitions, our model has more flexibility to adapt to the training data. In fact, if you split until 
#every point is its own partition, then RSS goes all the way down to 0 since the average of one value is 
#that same value. To avoid this, the algorithm sets a minimum for how much the RSS must improve for another 
#partition to be added. This parameter is referred to as the complexity parameter (cp). The RSS must improve 
#by a factor of cp for the new partition to be added. Large values of cp will therefore force the algorithm 
#to stop earlier which result in less nodes.

#However, cp is not the only parameter used to decide if we should partition a current partition or not. 
#Another common parameter is the minimum number of observations required in a partition before partitioning
#it further. The argument used in the rpart function is minsplit and the default is 20. The rpart implementation
#of regression trees also permits users to determine a minimum number observations in each node.
#The argument is minbucket and defaults to round(minsplit/3).

#------------------------3.1 Complexity parameter of 0 and minsplit = 2----------------
#As expected, if we set cp = 0 and minsplit = 2, then our prediction is as flexible as possible and our
#predictor is our original data:

fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>%
  mutate(y_hat = predict(fit)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

#Intuitively we know that this is not a good approach as it will generally result in over-training.

#------------------------3.2 Pruning a tree---------------------
#Note that if we already have a tree and want to apply a higher cp value, we can use the prune function. We
#call this pruning a tree because we are snipping off partitions that do not meet a cp criterion. We previously
#created a tree that used a cp = 0 and saved it to fit.

pruned_fit <- prune(fit, cp = 0.01)

#------------------------3.3 How to choose the correct parameters?------------------
#These cp, minsplit and minbucket three parameters can be used to control the variability of the final predictors. 
#The larger these values are the more data is averaged to compute a predictor and thus reduce variability. 
#The drawback is that it restrict flexibility.

library(caret)
set.seed(1)
train_rpart <- train(margin ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), #The tuneGrid parameter lets us decide which values the main parameter will take
                     data = polls_2008) #this model uses different coplexities parameters an picks the one that minimizes RMSE

ggplot(train_rpart) #to plot the RMSE 

#To see the resulting tree that minimizes the mean squared error, we can access it through 
#the component finalmodel. To see the resulting tree, we access the finalModel and plot it:
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

#And because we only have one predictor, we can actually plot f(x):
polls_2008 %>%
  mutate(y_hat = predict(train_rpart)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")
