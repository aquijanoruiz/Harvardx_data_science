rm(list=ls())

library(tidyverse)
library(dslabs)
take_poll(25)

#p is the proportion of blue beads and also the parameter (what we don't know).
#we will calculate an estimate, which is a summary of the observed data that we think is informative 
#about the parameter of interest.

#p is the proportion of blue beads
#1-p is the proportion of red beads
#p-(1-p) is the spread, also expressed as 2p-1

#-------------------------------------------------------------------
#Plug-in stimate.- we use the stimate of the parameter to calculate the standard error. 
#he CLT still works if we use an estimate of the standard error.

N<-25
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/N)
se

#We can compute the probability of the estimate to be 1% away from the parameter
#(x - u)/sd
#x - u = 0.01
#z= 0.01/se

pnorm(0.01/se) - pnorm(-0.01/se)

#------------------------------Margin of error-----------------------
2*se #Margin of error
pnorm(1.96) - pnorm(-1.96) #the probability of the standard normal distribution that
#has the expected value 0 and standard error one is within two values from 0

#------------------------------Monte Carlo simulation----------------
p<- 0.45
B<- 10000
N<- 1000
X_hat<- replicate(B,{
  X<- sample(c(1,0),size=N,replace=TRUE,prob = c(p,1-p))
  mean(X)
})
X_hat

#The theory tells us that X-bar has approximately normal distribution with expected value 0.45
#and a standard error of about 1.5%.
mean(X_hat)
sd(X_hat)

library(gridExtra)
p1<- data.frame(X_hat=X_hat) %>%
  ggplot(aes(X_hat)) +
  geom_histogram(binwidth = 0.005, color="black")
p1

#To compare our results with the theoretical normal
p2<- data.frame(X_hat=X_hat) %>%
  ggplot(aes(sample=X_hat)) +
  stat_qq(dparams = list(mean=mean(X_hat),sd=sd(X_hat)))+
  geom_abline()+
  ylab("X_hat")+
  xlab("Theoretical normal")
p2  

grid.arrange(p1,p2,nrow=1) #To put the two graphs one next to the other