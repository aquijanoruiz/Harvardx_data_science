#Linear regression
rm(list=ls())
library(tidyverse)
library(caret)
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

#Q1 We will build 100 linear models using the data above and calculate the mean and standard deviation 
#of the combined models. First, set the seed to 1. Within a replicate loop, (1) partition the dataset 
#into test and training sets of equal size using dat$y to generate your indices, (2) train a linear model 
#predicting y from x, (3) generate predictions on the test set, and (4) calculate the RMSE of that model. 
#Then, report the mean and standard deviation of the RMSEs from all 100 models.

B<- 100
set.seed(1)
monte_carlo <- replicate(B, {
  test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  fit<- lm(data=train, y ~ x)
  y_hat <- predict(fit, test)
  sqrt(mean((y_hat - test$y)^2))
})

mean(monte_carlo)
sd(monte_carlo)

test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
test <- dat[test_index,]
train <- dat[-test_index,]
model_1 <- lm(train$y ~ train$x) #it's 
model_2 <- lm(data=train, y ~ x)

#Q2 Now we will repeat the exercise above but using larger datasets. Write a function that takes a size n, 
#then (1) builds a dataset using the code provided in Q1 but with n observations instead of 100 and without 
#the set.seed(1), (2) runs the replicate loop that you wrote to answer Q1, which builds 100 linear models 
#and returns a vector of RMSEs, and (3) calculates the mean and standard deviation. Set the seed to 1 and 
#then use sapply or map to apply this function to n <- c(100, 500, 1000, 5000, 10000).

n<- c(100,500,1000,5000,10000)

monte_carlo <- function(n) {
  replicate(n, {
    test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
    test <- dat[test_index,]
    train <- dat[-test_index,]
    fit<- lm(data=train, y ~ x)
    y_hat <- predict(fit, test)
    sqrt(mean((y_hat - test$y)^2))
  })
}

set.seed(1)
results <- sapply(n, monte_carlo)
#Subseting the data
mc_100<- results[[1]] #we use double brakets to subset the information inside a list
mc_500<- results[[2]]
mc_1000<- results[[3]]
mc_5000<- results[[4]]
mc_10000 <- results[[5]]

mean(mc_100)
sd(mc_100)

mean(mc_500)
sd(mc_500)

mean(mc_1000)
sd(mc_1000)

mean(mc_5000)
sd(mc_5000)

mean(mc_10000)
sd(mc_10000)


B<- 500
set.seed(1, sample.kind = "Rounding")
monte_carlo <- replicate(B, {
  test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  fit<- lm(data=train, y ~ x)
  y_hat <- predict(fit, test)
  sqrt(mean((y_hat - test$y)^2))
})

mean(monte_carlo)
sd(monte_carlo)


