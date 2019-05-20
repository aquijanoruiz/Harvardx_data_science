#Linear regression
rm(list=ls())
library(tidyverse)
library(caret)
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2) #correlation matrix
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

#Q1 We will build 100 linear models using the data above and calculate the mean and standard deviation 
#of the combined models. First, set the seed to 1. Within a replicate loop, (1) partition the dataset 
#into test and training sets of equal size using dat$y to generate your indices, (2) train a linear model 
#predicting y from x, (3) generate predictions on the test set, and (4) calculate the RMSE of that model. 
#Then, report the mean and standard deviation of the RMSEs from all 100 models.

B<- 100
set.seed(1)
res <- replicate(B, {
  test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  fit<- lm(data=train, y ~ x)
  y_hat <- predict(fit, test)
  sqrt(mean((y_hat - test$y)^2))
})

mean(res)
sd(res)

#Q2 Now we will repeat the exercise above but using larger datasets. Write a function that takes a size n, 
#then (1) builds a dataset using the code provided in Q1 but with n observations instead of 100 and without 
#the set.seed(1), (2) runs the replicate loop that you wrote to answer Q1, which builds 100 linear models 
#and returns a vector of RMSEs, and (3) calculates the mean and standard deviation. Set the seed to 1 and 
#then use sapply or map to apply this function to n <- c(100, 500, 1000, 5000, 10000).

set.seed(1)
n <- c(100, 500, 1000, 5000, 10000)
B <- 100
res <- sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(B, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat-test_set$y)^2))
  })
  c(avg = mean(rmse), sd = sd(rmse))
})

res

#Q4 Now repeat the exercise from Q1, this time making the correlation between x and y larger.

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

B<- 100
set.seed(1)
res <- replicate(B, {
  test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  fit<- lm(data=train, y ~ x)
  y_hat <- predict(fit, test)
  sqrt(mean((y_hat - test$y)^2))
})

mean(res)
sd(res)

#Q6: Note that y is correlated with both x_1 and x_2 but the two predictors are independent of each other, as seen by cor(dat).
#Set the seed to 1, then use the caret package to partition into a test and training set of equal size. Compare the RMSE when using
#just x_1, just x_2 and both x_1 and x_2. Train a linear model for each. Which of the three models performs the best (has the lowest RMSE)?

set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

B<- 100
set.seed(1)
test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
test <- dat[test_index,]
train <- dat[-test_index,]


fit_1<- lm(data=train, y ~ x_1)
fit_2<- lm(data=train, y ~ x_2)
fit_3<- lm(data=train, y ~ x_1 + x_2)
y_hat_1 <- predict(fit_1, test)
y_hat_2 <- predict(fit_2, test)
y_hat_3 <- predict(fit_3, test)

#x_1
sqrt(mean((y_hat_1 - test$y)^2))

#x_2
sqrt(mean((y_hat_2 - test$y)^2))

#x_3
sqrt(mean((y_hat_3 - test$y)^2))

#Q7: Report the lowest RMSE of the three models tested in Q6.
#x_3
sqrt(mean((y_hat_3 - test$y)^2))

#Q8: Repeat the exercise from q6 but now create an example in which x_1 and x_2 are highly correlated.
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

B<- 100
set.seed(1)
test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
test <- dat[test_index,]
train <- dat[-test_index,]


fit_1<- lm(data=train, y ~ x_1)
fit_2<- lm(data=train, y ~ x_2)
fit_3<- lm(data=train, y ~ x_1 + x_2)
y_hat_1 <- predict(fit_1, test)
y_hat_2 <- predict(fit_2, test)
y_hat_3 <- predict(fit_3, test)

#x_1
sqrt(mean((y_hat_1 - test$y)^2))

#x_2
sqrt(mean((y_hat_2 - test$y)^2))

#x_3
sqrt(mean((y_hat_3 - test$y)^2)) #Adding extra predictors can improve RMSE substantially, but not when 
#the added predictors are highly correlated with other predictors. 

#---------------------------logistic regression-------------------------
#Q1:

set.seed(2)
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p) #we use a binomial distribution to create a random list of 1s and 0s
  f_0 <- rnorm(n, mu_0, sigma_0) #we create a random sample with mean 0 and sigma 1
  f_1 <- rnorm(n, mu_1, sigma_1) #we create a random sample with mean 1 and sigman 1
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

#Note that we have defined a variable x that is predictive of a binary outcome y

dat$train %>% ggplot(aes(x, color = y)) + geom_density()

#Set the seed to 1, then use the make_data function defined above to generate 25 different datasets 
#with mu_1 <- seq(0, 3, len=25). Perform logistic regression on each of the 25 different datasets 
#(predict 1 if p>0.5) and plot accuracy (res in the figures) vs mu_1 (delta in the figures).”

#Which is the correct plot?

delta <- seq(0, 3, len=25)

calculate_accuracy <- function(delta) {
  dat <- make_data(mu_1=delta)
  glm <- dat$train %>% glm(y ~ x, data=., family = "binomial")
  p_hat_logit <- predict(glm, newdata = dat$test, type = "response")
  y_hat_logit <- ifelse(p_hat_logit > 0.5, 1, 0) %>% factor()
  y <- dat$test$y
  mean(y_hat_logit==y)
}

set.seed(1)
res<- sapply(delta,calculate_accuracy)

data.frame(delta=delta,res=res) %>% ggplot(aes(x=delta,y=res)) + geom_point()
