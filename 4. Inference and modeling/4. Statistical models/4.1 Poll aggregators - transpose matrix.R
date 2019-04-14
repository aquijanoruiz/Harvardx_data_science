rm(list=ls())
library(dslabs)
library(tidyverse)

d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d + 1) / 2 #How to calculate H_hat using d_hat

#--------------------------Monte Carlo simulation-----------------------------
confidence_interval<- sapply(Ns, function(N){
  X<- sample(c(1,0), size=N, replace=TRUE, prob=c(p,1-p))
  X_hat<- mean(X)
  se_hat<- sqrt(X_hat*(1-X_hat)/N) #se of X_hat (not d_hat)
  2*c(X_hat,X_hat -qnorm(0.975)*se_hat, X_hat+qnorm(0.975)*se_hat)-1 #we get d_hat from X_hat
})

#--------------------------transpose matrix---------------------------------
confidence_interval
polls<- data.frame(poll=1:ncol(confidence_interval),
                   t(confidence_interval), #transposes the matrix
                   sample_size=Ns)
names(polls) <-c("poll","estimate","low","high","sample_size") #gives the names to the columns
polls #we can see in the columns low and high that all polls include d_hat=0, this means it is a tossup

#--------------------------weighted average-----------------------------------
#We can use mathematics to reconstruct what we would have obtained had we made one large poll with, 
#in this case, 11,269 people, participants.

sum(polls$sample_size)

d_hat<- polls %>%
  summarize(avg=sum(estimate*sample_size)/sum(sample_size)) %>% .$avg
d_hat #d_hat obtained from the weighted average

p_hat<- (1+d_hat)/2
moe<- 2*qnorm(0.975)*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size)) #margin of error of d_hat(not p_hat or H_hat)
moe #margin of error of the spread

round(d_hat*100,1)
round(moe*100,1) #Once we combine the 12 polls, we become quite certain that Obama
#will win the popular vote.
