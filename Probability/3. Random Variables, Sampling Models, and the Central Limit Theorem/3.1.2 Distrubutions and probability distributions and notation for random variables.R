#Distribution Vs Probability Distribution

#Distribution of a list of numbers
#F(a) What proportion of the list is less than ...?
#We need a list of numbers (like the height example), and when the distribution is approximately normal
#and we calculate the mean and the standard deviation
avg<- sqrt(sum(x)/length(x))
s<- sqrt(sum((x-avg)^2)/length(x))           

#Probability distribution
#F(x) What is the probability that x is equal or less to a?
#We do not need a list of numbers. This is a theoretical concept.

#When we run a Monte Carlo simulation and generate a very large list of outcomes of x.
#These outcomes are a list of numbers. The distribution of this list will be a very good 
#approximation of the probability distribution of x. The longer the list, the better the approximation.
#The average and standard deviation of this list will approximate the expected value
#and standard error of the random variable.

#--------------------------------

#x lower case is used for observed values (arbitrary value)
#X capital is used for random variable (random quantity we will see in the future)
#X <=x