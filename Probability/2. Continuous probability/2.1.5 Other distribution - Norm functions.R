#Norm functions

#pnorm give us the comulative distribution of the normal distribution
#dnorm give us the density of the normal distribution 
#rnorm generates Monte Carlo simulations of the normal distribution
#qnorm computes the quantiles of the normal distribution

x<- seq(-4,4, length.out = 100)
data.frame(x, f=dnorm(x)) %>% ggplot(aes(x,f)) + geom_line() #we can run this code to calculate the
#density function of the normal distribution