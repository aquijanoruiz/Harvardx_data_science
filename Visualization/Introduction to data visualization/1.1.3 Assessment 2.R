#What proportion of the data is between 69 and 72 inches (taller than 69 but shorter or equal to 72)?
library(dslabs)
data(heights)
x <- heights$height[heights$sex == "Male"]
mean(x<=72)-mean(x<=69) #which can also be expressed like
mean(x>69 & x<=72)

#-------------2----------------
#Suppose you only have avg and stdev below, but no access to x, can you approximate the proportion 
#of the data that is between 69 and 72 inches?
avg <- mean(x)
stdev <- sd(x)
pnorm(72,avg,stdev)-pnorm(69,avg,stdev)

#-------------3----------------
exact <- mean(x > 79 & x <= 81)
approx <- pnorm(81,avg,stdev)-pnorm(79,avg,stdev)
exact/approx

#-------------4----------------
#Assume that the distribution of adult men in the world as normally distributed with an average 
#of 69 inches and a standard deviation of 3 inches. Someone asks you what percent of seven footers 
#are in the National Basketball Association (NBA). Can you provide an estimate? 
1-pnorm(84,69,3)

#-------------5----------------
#We know that there are about 1 billion men between the ages of 18 and 40 in the world, 
#the age range for the NBA. Can we use the normal distribution to estimate how many of 
#these 1 billion men are at least seven feet tall?
p<- pnorm(84,69,3)
p
floor(p*10^9)

#-------------6----------------
#How many seven footers are in the NBA? 
#There are about 10 National Basketball Association (NBA) players that are 7 feet tall or higher.

p<- 1-pnorm(84,69,3)
N<- floor(p*10^9)
10/N
#-------------7----------------
# Repeat the calculations performed in the previous question for Lebron James' height: 6 feet 8 inches. 
#There are about 150 players, instead of 10, that are at least that tall in the NBA.
p <- 1 - pnorm(80, 69, 3)
N <- round(p * 10^9)
150/N