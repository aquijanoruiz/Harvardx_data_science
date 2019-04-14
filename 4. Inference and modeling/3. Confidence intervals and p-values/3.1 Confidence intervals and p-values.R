rm(list=ls())
library(dslabs)
data(nhtemp) #This is a vector, not a data.frame
view(nhtemp)

#-----------------------Smoothed conditional means---------------------------
nhtemp <- data.frame(year= as.numeric(time(nhtemp)),temperature=as.numeric(nhtemp))

nhtemp %>%
  ggplot(aes(x=year,y=temperature)) +
  geom_point()+
  geom_smooth()+
  ggtitle("Average Yearly Temperature in New Haven")

#-----------------------Confidence intervals---------------------------------------------
N<- 1000
p<- 0.45
X<- sample(c(1,0),size=N, replace=TRUE, prob=c(p,1-p))
X_hat<- mean(X)
SE_hat<- sqrt(X_hat*(1-p)/N)
c(X_hat+2*SE_hat,X_hat-2*SE_hat) #We get a different value each time we run this code because it is due to random chance

qnorm(0.975) #this Z value gives us a 95% confidence interval
qnorm(0.995) #this Z value gives us a 99% confidence interval
pnorm(qnorm(0.995)) #we get the same 0.955

z<- qnorm(0.995)
z

pnorm(-qnorm(0.995))
pnorm(1-qnorm(0.995))

pnorm(z) - pnorm(-z)
1-(1-0.99)/2 #to know the quantile that gives us a 99% confidence interval 1-(1-p)/2

#-----------------------Monte Carlo simulation for confidence intervals-------------------
B<- 10000

inside<- replicate(B,{
  X<- sample(c(1,0),size=N, replace=TRUE, prob=c(p,1-p))
  X_hat<- mean(X)
  S_hat<- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat -2*SE_hat, X_hat +2*SE_hat) #if it shows TRUE, p is between the confidence intervals; if it is FALSE, it is not
})

between(1:12, 7, 9) #explample of what between does 

inside
mean(inside) #in 95% of the cases p will be between the confidence interval

#-----------------------The correct language---------------------------------------------

#It is important to remember that it is the intervals that are at random, not p.
#So the 95% relates to the probability that the random interval falls on top of p.
#Saying that p has a 95% chance of being between this and that is technically an incorrect statement
#because p is not random.


#--------------------------------------Power---------------------------------------------
N<- 25
X_hat<- 0.48

(2*X_hat-1) + c(-2,2)*2*sqrt(X_hat*(1-X_hat)/sqrt(N)) #pay atttention that the standard error of the 
#spread is 2*SE[X_hat]

#confidence interval of the spread
#This confidence interval is zero. An interval that does not include 0, an interval that
#makes a call of who's going to win. The fact that our interval includes 0, it does not
#mean that this election is close. It only means that we have a small sample size.
#In the context of polls, power can be thought of as the probability of detecting a spread 
#different from 0.

#--------------------------------------p values---------------------------------------------
#null hypothesis: The null hypothesis is the skeptic's hypothesis: the spread is 0.
#is p=0.48, the spread is 2*p-1= 0.04
#p value: how likely is it to see a value this large (0.04) when the null hypothesis is true?

N<- 100
z<- sqrt(N)*0.02/0.5
p_value<- 1-(pnorm(z) - pnorm(-z)) #In this case, there's actually a large chance of seeing 52 blue beads or more
#under the null hypothesis that there is the same amount of blue beads as red beads.
#In Spanish: Hay una probabilidad de 68% de ver 52 pelotitas o más incluso habiendo la misma cantidad de pelotitas con una muestra de 100

#If a 95% confidence interval of the spread does not include 0, we can do a little bit of math to see that this implies 
#that the p-value must be smaller than 1 minus 95%, or 0.05.

