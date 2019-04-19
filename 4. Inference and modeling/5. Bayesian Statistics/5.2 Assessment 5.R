rm(list=ls())
#----------------------------2-------------------------------
# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Calculate the probability of both sons dying of SIDS. Print this value to the console.
#Pr(A and B) = P(A dies) * (B dies|A dies)
Pr_1 * Pr_2

#----------------------------3-------------------------------
#Pr(mother is a murderer∣two children found dead with no evidence of harm)=

#Pr(two children found dead with no evidence of harm∣mother is a murderer)Pr(mother is a murderer)
#-------------------------------------------------------------------------------------------------
#                   Pr(two children found dead with no evidence of harm)

#----------------------------4-------------------------------
# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Define `Pr_B` as the probability of both sons dying of SIDS
Pr_B <- Pr_1*Pr_2

# Define Pr_A as the rate of mothers that are murderers
Pr_A <- 1/1000000

# Define Pr_BA as the probability that two children die without evidence of harm, 
#given that their mother is a murderer
Pr_BA <- 0.50

# Define Pr_AB as the probability that a mother is a murderer, given that her 
#two children died with no evidence of physical harm. Print this value to the console.
Pr_AB<- Pr_A * Pr_BA/ Pr_B
Pr_AB

#----------------------------6-------------------------------
# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Examine the `polls` object using the `head` function
head(polls)

# Create an object called `results` that has two columns containing the average spread (`avg`) 
#and the standard error (`se`). Print the results to the console.
results<- polls %>%
  summarise(avg=mean(spread),se=sd(spread)/sqrt(length(spread)))
results

#----------------------------8-------------------------------
#The CLT tells us that our estimate of the spread d̂  has a normal distribution with expected value
#d and standard deviation σ, which we calculated in a previous exercise.

#Use the formulas for the posterior distribution to calculate the expected value of the posterior distribution 
#if we set μ=0 and τ=0.01.

# Define `mu` and `tau`
mu <- 0
tau <- 0.01

# Define a variable called `sigma` that contains the standard error in the object `results
sigma<- results$se
sigma

# Define a variable called `Y` that contains the average in the object `results`
Y<- results$avg
Y

# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
B<- sigma^2/(sigma^2+tau^2)
B

# Calculate the expected value of the posterior distribution
exp_value<- B*mu+ (1-B)*Y

#----------------------------9-------------------------------
# Compute the standard error of the posterior distribution. Print this value to the console.
se<- sqrt(1/(1/sigma^2+1/tau^2))

#----------------------------10-------------------------------
# Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<- c(expedted_value - qnorm(0.975)*se,expedted_value + qnorm(0.975)*se)

#----------------------------11-------------------------------
# According to this analysis, what was the probability that Trump wins Florida?
# Using the `pnorm` function, calculate the probability that the actual spread was less than 0 (in Trump's favor). Print this value to the console.
pnorm(0,exp_value,se)
ci

#----------------------------12-------------------------------
# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)

# Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0
p_calc <- function(tau) {
  B<- sigma^2/(sigma^2+tau^2)
  exp_value<- B*mu+ (1-B)*Y
  se<- sqrt(1/(1/sigma^2+1/tau^2))
  p<- pnorm(0,exp_value,se)
  p
}


# Create a vector called `ps` by applying the function `p_calc` across values in `taus`

ps<- p_calc(taus)
ps
# Plot `taus` on the x-axis and `ps` on the y-axis

plot(x=taus,y=ps)
