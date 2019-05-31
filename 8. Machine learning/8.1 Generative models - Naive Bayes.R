#---------------------contents------------------------
#1. Generative models
#2. Naive Bayes
#3. Calculating sensititvity and specificity
#4. Adjusting the prevalence for better sensitivity and specificity

#---------------------------------1. Generative models-------------------------
#Bayes??? theorem tells us that knowing the distribution of the predictors X may be useful. Methods
#that model the joint distribution of Y and X are referred to as generative models (we model how the entire
#data, X and Y , are generated).

#---------------------------------2. Naive Bayes-------------------------------
#the Naive Bayes approach is particularly appropriate because we know that the normal distribution is a good 
#approximation for the conditional distributions of height given sex for both classes Y = 1 (female) and Y = 0 (Male)

#2.1 We first create the training set and the test set.
library(tidyverse)
library(caret)
library(dslabs)
data("heights")
y <- heights$height
set.seed(1995)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

#                                 fX|Y=1 (x) * Pr(Y = 1)
#p(x) = Pr(Y = 1|X = x) = ----------------------------------------------------
#                           fX|Y=0 (x) * Pr(Y = 0) + fX|Y=1 (x) * Pr(Y = 1)

#2.2 Calculating the conditinal distributions f(X|Y=1) and f(X|Y=0)
#We can approximate the conditional distributions fX|Y =1 and fX|Y =0 by simply estimating averages and standard deviations from the data.

params <- train_set %>%
  group_by(sex) %>%
  summarize(avg = mean(height), sd = sd(height))
params #this are the conditional probabilities f(X|Y=1) = 65.1 and f(X|Y=0) = 69.2

#2.2 Calculating prevalence Pr(Y = 1)
#The prevalence, which we will denote with pi = Pr(Y = 1), can be estimated from the data.

pi <- train_set %>% summarize(pi=mean(sex=="Female")) %>% pull(pi)
pi

#2.3 Putting all together
x <- test_set$height
f0 <- dnorm(x, params$avg[2], params$sd[2]) #This is the distribution of the male
f1 <- dnorm(x, params$avg[1], params$sd[1]) #This is the distribution of the female

p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi)) #This is the Bayes' theorem formula

#----------------------3. Calculating sensititvity and specificity------------------------------
#In the previous example we computed the prevalence pi. This can be adjusted if we know the distributions of the height of males
#and females follow a normal distribution.

#1. We check the previous sensitivity with the previous prevalence
#The prevalence we used in exercise 1 is very low for female (21% of the total population).
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex))
#Our accuracy will be affected due to the low sensitivity. This means a lot of female are being predicted to be male.

specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))
#This is very high, as all the males are predicted as males.

#---------------4. Adjusting the prevalence for better sensitivity and specificity-------------------
#So to balance specificity and sensitivity, instead of changing the cutoff in the decision rule, 
#we could simply change pi to 0.5. We do this because we know that in the world there are the same number of 
#males and females.

p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased> 0.5, "Female", "Male")

sensitivity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))

#The new rule also gives us a very intuitive cutoff between 66-67, which is about the middle of the female and
#male average heights.

library(ggplot2)

data.frame(x=x, y=p_hat_bayes_unbiased) %>%
ggplot(aes(x, y)) +
  geom_line() +
  geom_hline(yintercept = 0.5, lty = 2) + #this creates a horizontal line
  geom_vline(xintercept = 67, lty = 2) #this creates a vertical line
