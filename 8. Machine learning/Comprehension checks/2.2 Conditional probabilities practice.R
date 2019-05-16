rm(list = ls())
library(dslabs)
library(tidyverse)
data("heights")

#-----------------------conditional probability------------------
#Q1: Compute the conditional probabilities for being male in the heights dataset. Round the heights 
#to the closest inch. Plot the estimated conditional probability P(x)=Pr(Male|height=x) for each x.
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

#Q2: In the plot we just made in Q1 we see high variability for low values of height. 
#This is because we have few data points. This time use the quantile (\ 0.1,0.2,\dots,0.9 \)and 
#the cut function to assure each group has the same number of points. Note that for any numeric vector x, 
#you can create groups based on quantiles like this: cut(x, quantile(x, seq(0, 1, 0.1)), include.lowest = TRUE).

ps <- seq(0, 1, 0.1)
breaks <- quantile(heights$height, ps)
heights %>% 
  mutate(g = cut(x=height, breaks=breaks, include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#------------the cut function--------------
#We use the cut function to create a factor, which is what R calls a categorical variable. 
#Then it classifies the categorical variable into larger bins, such as 50-63
#The function cut breaks up the factor into intervals intervals that we specify in the breaks argument. 
#The default labels use standard mathematical notation for open and closed intervals.

#------------creating a bivariate normal distribution---------------------

#Q3 You can generate data from a bivariate normal distrubution using the MASS package using the following code:

#First, we need to create the covariance matrix
Sigma <- 9*matrix(data=c(1,0.5,0.5,1), nrow=2, ncol=2)

library(MASS)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma)
dat <- as.data.frame(dat) %>% setNames(c("x", "y"))

#we can plot the conditional probability
ps <- seq(0, 1, 0.1)
dat %>% mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)


