#Interest rates explained
n<- 1000
loss_per_foreclosure <-200000
p<- 0.02
defaults<- sample(c(0,1),n,replace=TRUE,prob=c(1-p,p)) #this tells which people default
#and which don't, being 0 someone who didn't default, and 1 someone who defaulted
sum(defaults* loss_per_foreclosure)
n*p*loss_per_foreclosure
identical(n*p*loss_per_foreclosure,sum(defaults* loss_per_foreclosure))
#The answer is FALSE because defaults gives you a probability of defaulting.
#Every time you run the code you get a different answer. It's not going to happen for sure

#MONTE CARLO SIMULATION
B<- 10000
losses<- replicate(B,{
  defaults<- sample(c(0,1),n,replace=TRUE,prob=c(1-p,p))
  sum(defaults*loss_per_foreclosure)
})

data.frame(losses_in_millions=losses/10^6) %>% ggplot(aes(losses_in_millions)) +geom_histogram(binwidth = 0.6,col="black")

#NO NEED OF MONTE CARLO SIMULATION
#The CLT tells us that because our losses are a sum of independent draws,
#its distribution is approximately normal with expected

#Expected value
n*(p*loss_per_foreclosure+(1-p)*0)

#Standar error
abs(loss_per_foreclosure-(0))*sqrt(n)*sqrt(p*(1-p))

#-------------BREAK EVEN-------------
#Set an interest rate to guarantee that, on average, we break even
#Break even formula lp + x(1-p) =0
-loss_per_foreclosure*p/(1-p)
-loss_per_foreclosure*p/(1-p)/loss_per_foreclosure #This is an interest rate of about 2%

#So, let's they say that we want our chances of losing money to be one in 100.
#What does x have to be now?

#We want Pr(S<0) = 0.01
#We know S is approximately normal
#The expected value is given by [lp + x(1-p)]*n
#The standard error is |l-x| √np(1−p)
#Stadarize: z= (x - u)/ sd

qnorm(0.01) #z value of alpha 1% 
#Pr(Z<=z) = 0.01
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x #Check page 264 of the book
x/l #This is about 3%

#Expected profit per loan
loss_per_foreclosure*p + x*(1-p)

#Total expected profit
n*(loss_per_foreclosure*p + x*(1-p))

#Using a Monte Carlo
B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n,
                   prob=c(1-p, p), replace = TRUE)
  sum(draws)
})
mean(profit)
mean(profit<0)
