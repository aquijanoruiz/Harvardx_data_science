#Quantile-Quantile plots
#To check is the the normal distribution is a good approximation of the distribution of a population

library(dslabs)
data(heights)
index<- heights$sex=="Male"
x<- heights$height[index]
mean(x<=69.5) #This means that if p is 0.05 the q (quantile) associated with that p is 69.5

#If the quantiles for the data match the quantiles for the normal distribution,
#then it must be because the data is approximated by a normal distribution.

#quantile.- to obtain the quantiles of the data
p<- seq(0.05,0.95,0.05)
observed_quantiles<- quantile(x,p) #The quantiles of our real data
observed_quantiles

theoretical_quantiles<- qnorm(p,mean(x),sd(x)) #To get the normal distributed quantiles

#To see if they match, we can plot against each other
plot(observed_quantiles,theoretical_quantiles)
abline(0,1) #This tells that the normal distribution is a good approximation

#USING STANDARIZED DATA
#We can standarize and then we don't need to define the mean and the sd in the qnorm function
z<- scale(x)
observed_quantiles_standarized<- quantile(z,p)
theoretical_quantiles_standarized<- qnorm(p)
plot(observed_quantiles_standarized,theoretical_quantiles_standarized)
abline(0,1)

#PERCENTILE
#The percentiles are the quantiles you obtain when you define p as 0.01, 0.02, up to 0.99, 
#1%, 2%, 3%, et cetera
#p=0.25 is the 25th percentile, gives you the number for with 25th of the data is below
#p=0.5 is the most famous percentile, also known as the medium. In a normal distribution the mean 
#and the medium are the same
#the quartiles are p=0.25, p=0.50, p=0.75

#BOXPLOTS
#We can see that the normal approximation does not apply in the murders data base
#Provide a five-number summary composed of the range along with the quartiles 
#(the 25th, 50th, and 75th percentiles). We can ignore outliers when computing the range 
#and instead plot these as independent points