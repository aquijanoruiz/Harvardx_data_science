#-----------------------contents-------------------
#1. Bootstraping
#2. Constructing a confidence interval with the median

#----------------------preparing the data---------------------
library(tidyverse)

#This is the income of our population
n <- 10^6
income <- 10^(rnorm(n, 4.656786, 0.4394738))
qplot(log10(income), bins = 30, color = I("black"))

#We want to estimate the median
m <- median(income) 
m

#---------------------1. The distribution of the median------------------
#We take a sample of 250 and estimate the population median m with the sample median M.
set.seed(1)
N <- 250
X <- sample(income, N)
M <- median(X)
M

library(gridExtra)
B <- 10^4
Ms <- replicate(B, {
  X <- sample(income, N) #see that we sample without replacement. 
  M <- median(X)
})

p1 <- qplot(Ms, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(Ms)) + geom_abline() #We standarize the data using the sacale() function and compare it with the normal distribution using geom_abline()
grid.arrange(p1, p2, ncol = 2)

mean(Ms)
sd(Ms)
#From a Monte Carlo simulation, we see that the distribution of M is approximately normal with the following
#expected value and standard error:

#---------------------------------1. Bootstraping--------------------------------
#The bootstrap permits us to approximate a Monte Carlo simulation without access to the entire distribution.
#The general idea is relatively simple. We act as if the observed sample is the population. We then sample
#(with replacement) datasets, of the same sample size as the original dataset. Then we compute the summary
#statistic, in this case median, on this bootstrap sample.

library(tidyverse)
B <- 10^4
M_stars <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE) #We sample with replacement
  M_star <- median(X_star)
  })

tibble(monte_carlo = sort(Ms), bootstrap = sort(M_stars)) %>%
  qplot(monte_carlo, bootstrap, data = .) +
  geom_abline() #We see it is not perfect, but it provides a decent approximation

#--------------------------2. Constructing a confidence interval with the median---------------------------

quantile(Ms, c(0.05, 0.95)) #This was gotten useing the first Montecarlo. The distribution of the medians
quantile(M_stars, c(0.05, 0.95)) #This was gotten using bootstraping


median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1) #This was gotten using the CLT (NOT A VERY GOOD ESTIMATE)

#If we know the distribution is normal, we can use the bootstrap to estimate the mean.
mean(Ms) + 1.96 * sd(Ms) * c(-1,1)
mean(M_stars) + 1.96 * sd(M_stars) * c(-1, 1)

