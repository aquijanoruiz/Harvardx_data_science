library(tidyverse)
library(dslabs)
data("mnist_27")
summary(mnist_27)

#X1: the proportion of dark pixels that are in the upper left quadrant
#X2: the proportion of dark pixels that are in the lower right quadrant

mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

#--------------------using regression-----------------
fit <- mnist_27$train %>%
  mutate(y = ifelse(y==7, 1, 0)) %>% #if y is 7, it takes the value of 1. It it is 2, it takes the value of 0. 
  lm(y ~ x_1 + x_2, data = .)

library(caret)
p_hat <- predict(fit, newdata = mnist_27$test) #we first calculate the probability of being 7, and we called it p_hat
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2)) #then according to that probability, we set an algorithm to take the value of 7 is p_hat is greater than 0.5
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall[["Accuracy"]]

#We have stored the true p(x1, x2) in the mnist_27 object and can plot the image using the ggplot2 function geom_raster()
mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) + geom_raster()

#We will choose better colors and use the stat_contour function to draw a curve that separates pairs (x1, x2)
#for which p(x1, x2) > 0.5 and pairs for which p(x1, x2) < 0.5.
mnist_27$true_p %>% ggplot(aes(x=x_1, y=x_2, z = p, fill = p)) + #we use z because we have three variables
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black")

#--------------------using logistic regression-----------------
fit_2 <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_2 <- predict(fit_2, newdata = mnist_27$test) 
y_hat_2 <- factor(ifelse(p_hat_2 > 0.5, 7, 2))
confusionMatrix(data = y_hat_2, reference = mnist_27$test$y)$overall[["Accuracy"]]

