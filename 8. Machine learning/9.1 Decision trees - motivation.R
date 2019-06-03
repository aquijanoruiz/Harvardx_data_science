#-------------------contents----------------------
#1. The curse of dimensionality
#2. CART motivation (fatty acids example)
#2.1 Using kNN neighbours (fatty acids example)
#2.2 Plotting the data (fatty acids example)


#-------------------------1. The curse of dimensionality---------------------
library(tidyverse)
p <- 1:100
qplot(p, .1^(1/p), ylim = c(0,1)) #If we want to include 10% of the data in a p dimensional space, 
#the size of the p dimensional shap is. This proportion gets close to 1 quickly, and if the proportion
#is 1 it means we include all the data and are no longer smoothing.

#-------------------------2. CART motivation (fatty acids example)---------------------
#We are going to try to predict the region using the 8 fatty accids as predictors
library(tidyverse)
library(dslabs)
data("olive")
olive %>% as_tibble()

summary(olive)
table(olive$region)

#-------------------------2.1 Using kNN neighbours (fatty acids example)---------------------
library(caret)
fit <- train(region ~ ., method = "knn",
             tuneGrid = data.frame(k = seq(1, 15, 2)),
             data = olive)

olive <- select(olive, -area) #We remove the area column because we won’t use it as a predictor.

#-------------------------2.2 Plotting the data (fatty acids example)---------------------
ggplot(fit)

library(ggplot2)
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free") +
  theme(axis.text.x = element_blank()) #eliminates the labels from the x axis

#This implies that we should be able to build an algorithm that predicts perfectly! We can see this clearly by
#plotting the values for eicosenoic and linoleic.

p <- olive %>%
  ggplot(aes(eicosenoic, linoleic, color = region)) +
  geom_point()

p + geom_vline(xintercept = 0.065, lty = 2) +
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2) #we use this code to add 
#the line segments

