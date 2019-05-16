rm(list=ls())
library(dslabs)
mnist<- read_mnist()
ncol(mnist$train$images)
str(mnist)

y <- mnist$train$labels

y[5] + y[6]
y[5] > y[6]
class(y)
class(y[5])
