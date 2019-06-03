#--------------------------Q1-----------------------------
#Create a simple dataset where the outcome grows 0.75 units on average for every increase in a predictor.

library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1)
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

#Which code correctly uses rpart to fit a regression tree and saves the result to fit?
fit <- rpart(y~., data = dat)

#--------------------------Q2-----------------------------

plot(fit, margin = 0.1)
text(fit, cex = 0.75)

#--------------------------Q3-----------------------------

dat %>% 
  mutate(y_hat = predict(fit)) %>%
  ggplot(aes(x,y)) +
  geom_point() +
  geom_step(aes(x,y_hat),col=2) #col=2 means red

#--------------------------Q4-----------------------------
#Now run Random Forests instead of a regression tree using randomForest from the __randomForest__ package, 
#and remake the scatterplot with the prediction line.

library(randomForest)
fit <- randomForest(y ~., data = dat)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)

#--------------------------Q5-----------------------------
#Use the plot function to see if the Random Forest from Q4 has converged or if we need more trees.
plot(fit)

#--------------------------Q6-----------------------------
# seems that the default values for the Random Forest result in an estimate that is too flexible (unsmooth). 
#Re-run the Random Forest but this time with a node size of 50 and a maximum of 25 nodes. Remake the plot.

library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 25, maxnodes = 25)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)

