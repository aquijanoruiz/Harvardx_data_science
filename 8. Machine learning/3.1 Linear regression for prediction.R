#Examples of algorithms
library(HistData) #We upload the Galton data

set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>% #selects a random row from a table
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

#Building a machine learning algorithm that predicts the son???s height
#---------------ignoring the fathers' height-----------------
library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) #we create a partition

train_set <- galton_heights %>% slice(-test_index)
galton_heights [-test_index,] #same as doing this

test_set <- galton_heights %>% slice(test_index)

m <- mean(train_set$son) #this is our guess of the sons'

#Calculating the squared loss
mean((m - test_set$son)^2) #if our prediction was good, this should be close to zero

#---------------using a regression-----------------
#If the pair (X, Y ) follow a bivariate normal distribution, the conditional expectation 
#(what we want to estimate) is equivalent to the regression line.

fit <- lm(son ~ father, data = train_set)
fit$coef #we look at the coefficients to formulate the equation
# height_son =40.78 + 0.4126*height_father

#we can test it in the test data set
y_hat_1 <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat_1 - test_set$son)^2) #we recalculate the squared loss. This is better.

#----------------the preciction function---------------
#we can compute the first we computed for y_hat_1 using this formula
y_hat_2 <- predict(fit, test_set) #the firts argument is the linear model and the second is the data where we want to apply it

mean((y_hat_2 - test_set$son)^2)

#The predict is actually a special type of function in R (called a generic function) that calls other
#functions depending on what kind of object it receives. So if predict receives an object coming out of the
#lm function, it will call predict.lm. If it receives an object coming out of glm, it calls predict.glm.


