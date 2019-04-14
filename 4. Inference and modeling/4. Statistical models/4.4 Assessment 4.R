#--------------------------------1---------------------------------
# Load the 'dslabs' package and data contained in 'heights'
rm(list=ls())
library(dslabs)
data(heights)

# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Calculate the population average. Print this value to the console.
mean(x)

# Calculate the population standard deviation. Print this value to the console.
sd(x)

#--------------------------------2---------------------------------
# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X<- sample(x,size=N, replace = TRUE)

# Calculate the sample average. Print this value to the console.
mean(X)

# Calculate the sample standard deviation. Print this value to the console.
sd(X)

#--------------------------------4---------------------------------
# Define `se` as the standard error of the estimate. Print this value to the console.
se<- sd(X)/sqrt(N)
se

# Construct a 95% confidence interval for the population average based on our sample. 
#Save the lower and then the upper confidence interval to a variable called `ci`.
ci<- c(mean(X) - qnorm(0.975)*se, mean(X) + qnorm(0.975)*se)

#--------------------------------5---------------------------------
# Define `B` as the number of times to run the model
B <- 10000

# Define an object `res` that contains a logical vector for simulated intervals that contain mu
mu <- mean(x)

res <- replicate(B, {
  X <- sample(x, N, replace=TRUE)
  interval <- mean(X) + c(-1,1)*qnorm(0.975)*sd(X)/sqrt(N)
  between(mu, interval[1], interval[2])
})
mean(res)

#--------------------------------6---------------------------------

# Load the libraries and data you need for the following exercises
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")

# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

# Make a boxplot with points of the spread for each pollster

polls %>%
  ggplot(aes(x=pollster,y=spread)) +
  geom_boxplot() +
  geom_point()

#--------------------------------13---------------------------------
# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread
sigma <- polls %>%
  group_by(pollster) %>%
  summarize(s=sd(spread)) %>%
  select(pollster,s)

# Print the contents of sigma to the console
sigma

#--------------------------------15---------------------------------
# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.
res<- polls %>%
  group_by(pollster) %>%
  summarize(avg=mean(spread),
            sd=sd(spread),
            N=n()) %>%
  select(pollster,avg,sd,N)

# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.
estimate<- res$avg[2]-res$avg[1]
estimate

# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(res$sd[1]^2/res$N[1]+res$sd[2]^2/res$N[2]) #this is the standard error of the difference between polls
se_hat

# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<- estimate +c(-qnorm(0.975),+qnorm(0.975))*se_hat

#--------------------------------16---------------------------------

# Calculate the p-value
#Use the pnorm function to calculate the probability that a random value is larger than the observed ratio 
#of the estimate to the standard error. Multiply the probability by 2, because this is the two-tailed test.

2*(1-pnorm(estimate,mean=0,sd=se_hat)) #we reject the null hypothesis

#--------------------------------17---------------------------------
# Execute the following lines of code to filter the polling data and calculate the spread
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()

# Create an object called `var` that contains columns for the pollster, mean spread, and standard deviation. Print the contents of this object to the console.
var<- polls %>%
  group_by(pollster) %>%
  summarize(avg=mean(spread),s=sd(spread))
var
