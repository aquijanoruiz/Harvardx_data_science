#WARNING: CHECK WITH THE BOOK AND THE VIDEO

#You can minimize your chances of losing money by increasing n, the number of loans, 
#and relying on the law of large numbers.

r <- (- loss_per_foreclosure*p/(1-p)) / 180000
r

r<- 0.05 #interest rate of 5%
p<- 0.04 #default rate is 4%
x<- r*180000
loss_per_foreclosure <-200000
loss_per_foreclosure*p + x*(1-p)

#As long as μ is positive, we can find an n that minimizes the probability of a loss. 
#This is a form of the law of large numbers: when n is large, our average earnings per loan 
#converges to the expected earning μ. With x fixed, now we can ask what n do we need for 
#the probability to be 0.01?

z <- qnorm(0.01)
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n

n*(loss_per_foreclosure*p + x * (1-p)) #What we are expecting to earn

#We can confirm this with a Monte Carlo Simulation
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n,
                   prob=c(1-p, p), replace = TRUE)
  sum(draws)
})
mean(profit)

#Note that in the case of averaging the same event over and over, an extreme example 
#of events that are not independent, we get a standard error that is sqrt(n) times bigger

p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n,
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit) #expected profit is still large

mean(profit<0) #the probability of the bank having negative earnings shoots up

mean(profit < -10000000) #probability of losing more than 10 million dollars

data.frame(profit_in_millions=profit/10^6) %>%
  ggplot(aes(profit_in_millions)) +
  geom_histogram(color="black", binwidth = 5)