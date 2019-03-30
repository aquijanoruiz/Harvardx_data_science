#Pnorm 
#The cumulative distribution for the normal distribution is defined by a mathematical formula, 
#which in R canvbe obtained with the function pnorm.

#F(a)=pnorm(a,avg,s) 

#What is the probability that a randomly selected male student is taller than 70.5 inches?
library(dslabs)
library(dplyr)
data(heights)
attach(heights)
only_men <- filter(heights, sex=="Male")
height_men <- select(only_men, height)
x <-height_men$height
1 - pnorm(70.5,mean(x),sd(x))

table(x)
prop.table(table(x))
plot(table(x), xlab = "a = Height in inches", ylab ="Pr(X = a)")

#Proportion of students reporting the following intervals
#This is the real data, not the approximation
mean(x<=68.5)-mean(x<=67.5) #here mean calculates the proportion of people below a number
mean(x<=69.5)-mean(x<=68.5)
mean(x<=70.5)-mean(x<=69.5)

#This is the approximation using the normal distribution
pnorm(68.5,mean(x),sd(x)) - pnorm(67.5,mean(x),sd(x))
pnorm(69.5,mean(x),sd(x)) - pnorm(68.5,mean(x),sd(x))
pnorm(70.5,mean(x),sd(x)) - pnorm(69.5,mean(x),sd(x))

#The normal distribution is not useful for integrals that do not include an integer
#This situation is called discretization
mean(x<=70.9)-mean(x<=70.1) #Using the real data
pnorm(70.9,mean(x),sd(x)) - pnorm(70.1,mean(x),sd(x)) #Using the normal distribution

#Discretization.- Although the true height distribution is continuous, the reported heights
#tend to be more common at discrete values, in this case, due to rounding.

#Probability of someone being taller than 76 incles
avg <- mean(x)
s<- sd(x)
1 - pnorm(76,avg,s)

#In R you get the probability density function for the normal distribution
#using the function dnorm. #D stands for density.