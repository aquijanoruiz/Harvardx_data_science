#---------------------contents-------------------------
#1. Euclidian distance
#2. T[he dist() function to calculate distance
#3. Computing distance between all pairs

#--------------------Preparing the data-----------------
#We take a random sample 2s and 7s from the data
rm(list=ls())
library(tidyverse)
library(dslabs)
mnist <- read_mnist()
set.seed(1995)
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]


#-------------------1. Euclidian distance------------------
#Labels are stored in y and the predictors are stored in x. Rememeber that x is a matrix with 784 columns, each representing a pixel.

y[1:3] #we see the first three numbers (2,7,2)

#We can get the vector of the predictors.
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

sqrt(sum((x_1 - x_2)^2)) #this formula calucales the distance between the first 2 and the 7
sqrt(sum((x_1 - x_3)^2)) #the first 2 and the other 2
sqrt(sum((x_2 - x_3)^2)) #the 7 and the other 2

#We can do the same using matrix algebra
sqrt(crossprod(x_1 - x_2))

sqrt(crossprod(x_1 - x_3))

sqrt(crossprod(x_2 - x_3))

#-------------------2. the dist() function to calculate distance------------------
#We can also compute all the distances at once relatively quickly using the function dist, which computes
#the distance between each row and produces an object of class dist.
d <- dist(x)
class(d)

#To access the entries using row and column indices, we need to coerce it into a matrix.
as.matrix(d)[1:3,1:3]

#We can see an image of the distance.
image(as.matrix(d))

#If we order this distance by the labels, we can see that, in general, the twos are closer to each other and the
#sevens are closer to each other.
image(as.matrix(d)[order(y), order(y)])
#Those red squares demonstrate that digits that are the same are closer to each other. But another thing that 
#comes out of this plot is that there appears to be more uniformity in how the 7s are drawn since they appear to be closer.

#-------------------3. computing distance between all pairs------------------
#To compute the distance between all pairs of the 784 predictors, we can transpose the matrix first and then use dist.

d <- dist(t(x))
dim(as.matrix(d))


#An interesting thing to note here is that if we pick a predictor (a pixel), we can see which pixels are close.
#That is, the pair of pixels either have ink in the same images (small distance) or they don’t (large distance).
#The distance between, for example, and all other pixels is given by:
d_492 <- as.matrix(d)[492,]
image(1:28, 1:28, matrix(d_492, 28, 28))
