#We want to bet with friends about two people having the same birthday in a group of people.
#When are the chances larger than 50%? Larger than 75%?

compute_prob <- function(n, B=10000){
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace=TRUE) #we can sample with replacement
    any(duplicated(bdays))
  })
mean(same_day)
}

compute_prob(10)
n<- 1:100
compute_prob(n) #It doesn't work like this because it does note expect a vector, it expects a scale, it expects and end

#--------------------
#sapply permits us to perform element-wise operationson any function
x<-1:10
sapply(x,sqrt)
#same as:
sqrt(x)

prob<- sapply(n,compute_prob)
prob
plot(n, prob)

#--------------------
#How to compute the exact probability
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365
  1 - prod(prob_unique)
}

eprob <- sapply(n, exact_prob)
plot(n, prob)
lines(n, eprob, col="red") #we can see that the montecarlo simulation approximates a lot to the excact probability

#--------------------
#How many Montecarlo simulations are enough?

B <- 10^seq(1, 5, len = 100)
compute_prob <- function(B, n=22){ #Here B (the number of experiments) is not defined, and the number of people is 25
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace=TRUE) #we can sample with replacement
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)
library(ggplot2)
qplot(log10(B), prob, geom = "line")
