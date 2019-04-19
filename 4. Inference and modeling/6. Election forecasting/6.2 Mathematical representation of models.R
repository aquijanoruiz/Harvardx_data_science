rm(list=ls())
#Xj = d + ej 
#This is the equation that explains the variability. We use the index j to represent the different polls and 
#we define ej to be a random variable that explains the poll-to-poll variability introduced by sampling error.


#If d is 2.1 and the sample size for these polls is 2,000, we can simulate J = 6 data points from this model like this:

set.seed(3)
J <- 6
N <- 2000
d <- .021 #the spread
p <- (d + 1)/2
X <- d + rnorm(J, 0, 2 * sqrt(p * (1 - p) / N)) #we generate random spreads according to the standar error given
X 

#Now suppose we have J = 6 data points from I = 5 different pollsters. We use Xij with i representing the pollster
#and j representing the j-th poll from that pollster.

#Xi,j = d + ei,j

I <- 5 #five different pollster (encuestadoras)
J <- 6 #six polls for each pollster (seis encuestas por cada encuestador)
N <- 2000
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2 * sqrt(p * (1 - p) / N)) #Xi,j = d + hi + ei,j
})
X
#We will use hi to represent the house effect of the i-th pollster. The model is now augmented to:
#Xi,j = d + hi + ei,j

I <- 5 
J <- 6
N <- 2000
d <- .021
p <- (d + 1) / 2
h <- rnorm(I, 0, 0.025) #we assume the average house effect (pollster effect) is zero with se 0.025
X <- sapply(1:I, function(i){ #I number of columns
  d + h[i] + rnorm(J, 0, 2 * sqrt(p * (1 - p) / N)) #J number of rows/ h[i] respresents the house effect
})
X

#Xij = d + b + hi + ei,j 
#b is the general bias affecting all polls. Here b is a random variable that accounts for the 
#election-to-election variability in history.

library(dplyr)
library(dslabs)
data(polls_us_election_2016)
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)

sd(one_poll_per_pollster$spread) #sigma b

mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + .025^2) #this sigma is different to the one we used in 6.1. sigma <- results$se 
#here we added the sigma of b (election to election variability). We calculated this as sd(one_poll_per_pollster$spread)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
1 - pnorm(0, posterior_mean, posterior_se)