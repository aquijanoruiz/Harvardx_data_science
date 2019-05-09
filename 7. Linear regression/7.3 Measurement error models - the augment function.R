#Imagine you are Galileo in the 16th century trying to describe the velocity of a falling object. 
#An assistant climbs the Tower of Pisa and drops a ball, while several other assistants record
#the position at different times. Let’s simulate some data using the equations we know today and adding
#some measurement error. The dslabs function rfalling_object generates these simulations:

rm(list=ls())
library(dslabs)
falling_object <- rfalling_object()

#The assistants hand the data to Galileo and this is what he sees:
falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")

#He deduces that the position should follow a parabola, which we can write like this:

#ƒ(x) = β_0 + β_1 * x_1 + β_2 * x_2

#The data does not fall exactly on a parabola. Galileo knows this is due to measurement error. His helpers
#make mistakes when measuring the distance. To account for this, he models the data with:

#Y = β_0 + β_1 * x_1 + β_2 * x_2 + ε

#The measurement error is assumed to be random, independent from each other, and having the same
#distribution for each i. We also assume that there is no bias, which means the expected value E[ε] = 0.

fit <- falling_object %>%
  mutate(time_sq = time^2) %>% #we square the time to fit the model #Y = β_0 + β_1 * x_1 + β_2 * x_2 + ε
  lm(observed_distance~time+time_sq, data=.) 
tidy(fit)

#--------------------------the augment() function----------------------------
#Let’s check if the estimated parabola fits the data. The broom function augment lets us do this easily:

augment(fit) %>% #augments data with information from a(n) lm object
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue") #is a generic function which extracts fitted values from objects returned by modeling functions

#this is the real formula:
#d = h_0 + v_0 * t − 0.5 * 9.8 * t^2
tidy(fit, conf.int = TRUE) #the values from the formula fall into the confidence intervals

