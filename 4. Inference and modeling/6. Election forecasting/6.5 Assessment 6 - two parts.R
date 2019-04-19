#-----------------------------1---------------------------------
rm(list=ls())
# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that has the columns indicated in the instructions

cis <- polls %>%
  mutate(X_hat = (spread+1)/2,
         se= 2*sqrt(X_hat*(1-X_hat)/samplesize),
         lower = spread - qnorm(0.975)*se,
         upper = spread + qnorm(0.975)*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

#-----------------------------2---------------------------------
# Add the actual results to the `cis` data set

add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
cis <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of confidence intervals that contain the actual value. 
# Print this object to the console.

cis<- cis %>%
  mutate(hit=ifelse(actual_spread>=lower & actual_spread<= upper,1,0))

p_hits <- cis %>%
  summarize(proportion_hits=mean(hit))
p_hits

#CAUTION: TO RUN CODES FOR EXERCISE 3 AND 4, YOU MUST RUN AGAIN THE CODES TO EXERCISE 2
#-----------------------------3---------------------------------

# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
cis <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has more than 5 polls.

p_hits <- cis %>%
  mutate(hit=ifelse(actual_spread>=lower & actual_spread<= upper,1,0)) %>%
  group_by(pollster) %>%
  filter(n()>=5) %>% #To select the pollsters with more than five polls
  summarize(proportion_hit= mean(hit),n=n(),grade=grade[1]) %>%
  arrange(desc(proportion_hit))

p_hits

#-----------------------------4---------------------------------

add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

head(ci_data)

p_hits <- ci_data %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% 
  group_by(state) %>%
  filter(n() >=  5) %>%
  summarize(proportion_hits = mean(hit), n = n()) %>%
  arrange(desc(proportion_hits))
p_hits

#-----------------------------5---------------------------------

# Make a barplot of the proportion of hits for each state

p_hits %>% ggplot(aes(x=state,y=proportion_hits)) +
  geom_bar(stat="identity")+ #indicates that the height of the bar should match the value
  coord_flip() #flips the axis

#-----------------------------6---------------------------------
errors <- cis %>%
  mutate(error=spread-actual_spread, hit=(sign(actual_spread) == sign(spread))) #sign tells if the signs of the two are the same. If the two are positive it means Clinto wins. 
tail(errors)  


#-----------------------------7---------------------------------
p_hits <- errors %>%
  group_by(state) %>%
  filter(n() >=  5) %>%
  summarize(proportion_hits = mean(hit), n = n())

p_hits %>%
  ggplot(aes(x=state,y=proportion_hits)) +
  geom_bar(stat="identity")+
  coord_flip()

#-----------------------------8---------------------------------
errors %>%
  ggplot(aes(error))+
  geom_histogram()

hist(errors$error) #this is taken as the right answer

median(errors$error)

#-----------------------------9---------------------------------

errors %>%
  filter(grade %in% c("A+","A","A-","B+")) %>%
  mutate(state=(reorder(x=state,X=error))) %>%
  ggplot(aes(x=state,y=error)) +
  geom_boxplot() +
  geom_point()

#-----------------------------10---------------------------------
errors %>%
  filter(grade %in% c("A+","A","A-","B+")) %>%
  group_by(state) %>%
  filter(n()>=5) %>%
  ungroup(state) %>%
  mutate(state=(reorder(x=state,X=error))) %>%
  ggplot(aes(x=state,y=error)) +
  geom_boxplot() +
  geom_point()

#-----------------------------1---------------------------------

# Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when 'df = 3'.
1-pt(2,3)+pt(-2,3)
pt(2,3,lower.tail = FALSE) + pt(-2,3) #we can also write it like this

#-----------------------------2---------------------------------
# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df<- seq(from=3,to=50,by=1)

# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom 
pt_func <- function(df){
  pt(2,df,lower.tail = FALSE) + pt(-2,df)
}

# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities
probs <- sapply(df,pt_func)

# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(x=df,y=probs)

#-----------------------------3---------------------------------

# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res<- replicate(B,{
  sample<- sample(x,size = N,replace = TRUE)
  interval<- c(mean(sample)+c(-1, 1)*qnorm(.975)*sd(sample)/sqrt(N))
  between(mu,interval[1],interval[2])
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)

#-----------------------------4---------------------------------
#N=15 is not that big. We know that heights are normally distributed, so the t-distribution should apply. 
#Repeat the previous Monte Carlo simulation using the t-distribution instead of using the normal 
#distribution to construct the confidence intervals.

# Generate a logical vector 'res' that contains the results of the simulations using the t-distribution
res<- replicate(B,{
  sample<- sample(x,size = N,replace = TRUE)
  interval<- c(mean(sample)+c(-1, 1)*qt(.975,N-1)*sd(sample)/sqrt(N))
  between(mu,interval[1],interval[2])
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. 
#Print this value to the console.
mean(res)
