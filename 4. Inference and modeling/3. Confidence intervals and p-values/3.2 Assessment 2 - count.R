#--------------------------1------------------------------
# Load the data
rm(list=ls())
library(dslabs)
data(polls_us_election_2016)

# Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States
polls<- polls_us_election_2016 %>% filter(enddate >="2016-10-31")  %>% filter(state=="U.S.")

# How many rows does `polls` contain? Print this value to the console.
nrow(polls)

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N<- polls_us_election_2016$samplesize[1]
N

# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. 
#Print this value to the console.
X_hat <- (polls_us_election_2016$rawpoll_clinton[1])/100
X_hat
# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat

# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<- c(qnorm(0.025,mean=X_hat,sd=se_hat),qnorm(0.975,mean=X_hat,sd=se_hat))
ci

#--------------------------2------------------------------
#Create a new object called pollster_results that contains the pollster's name, the end date of the poll, 
#the proportion of voters who declared a vote for Clinton, the standard error of this estimate, and the lower 
#and upper bounds of the confidence interval for the estimate.

# The `polls` object that filtered all the data by date and nation has already been loaded. Examine it using the `head` function.
head(polls)

# Create a new object called `pollster_results` that contains columns for pollster name, end date, X_hat, lower confidence interval,  
#se_hat, and upper confidence interval for each poll.

pollster_results <- polls %>%
  mutate(X_hat= rawpoll_clinton/100) %>%
  mutate(se_hat= sqrt(X_hat*(1-X_hat)/samplesize)) %>%
  mutate(lower= qnorm(0.025,mean=X_hat,sd=se_hat)) %>%
  mutate(upper= qnorm(0.975,mean=X_hat,sd=se_hat)) %>%
  select(pollster, enddate, X_hat, lower, se_hat, upper)

pollster_results2 <- polls %>% #we can also do it like this
  mutate(X_hat = rawpoll_clinton/100, 
         se_hat= sqrt(X_hat*(1-X_hat)/samplesize), 
         lower = X_hat - (qnorm(0.975))*se_hat, 
         upper = X_hat + (qnorm(0.975))*se_hat) %>%
  select(pollster, enddate, X_hat, se_hat, lower, upper)

#--------------------------3------------------------------

#The final tally for the popular vote was Clinton 48.2% and Trump 46.1%. Add a column called 
#hit to pollster_results that states if the confidence interval included the true proportion
#p=0.482 or not. What proportion of confidence intervals included p?

avg_hit<- pollster_results %>%
  mutate(hit= 0.482>=lower & 0.482<=upper) %>%
  summarise(average=mean(hit))

#--------------------------5------------------------------
# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. 
#The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") %>%
  mutate(d_hat= rawpoll_clinton-rawpoll_trump)


# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N<- polls$samplesize[1]
N

# For the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. 
#Print this value to the console.
d_hat<- polls$d_hat[1]/100
d_hat

# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat<- (d_hat+1)/2
X_hat

# Calculate the standard error of the spread and save it to a variable called `se_hat`. 
#Print this value to the console.
se_hat<- sqrt(X_hat*(1-X_hat)/N)
se_hat

# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. 
#Save the lower and then the upper confidence interval to a variable called `ci`.
ci<- -(1-2*c(qnorm(0.025,mean=X_hat,sd=se_hat),qnorm(0.975,mean=X_hat,sd=se_hat)))
ci

#-----------------------5 another way----------------------------
# Calculate the standard error of the spread and save it to a variable called `se_hat`. 
#Print this value to the console.
se_hat<- 2*sqrt(X_hat*(1-X_hat)/N)
se_hat

# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. 
#Save the lower and then the upper confidence interval to a variable called `ci`.
ci<- c(qnorm(0.025,mean=d_hat,sd=se_hat),qnorm(0.975,mean=d_hat,sd=se_hat))
ci

#-----------------------------6----------------------------------
# Create a new object called `pollster_results` that contains columns for pollster name, 
#end date, d_hat, lower confidence interval of d_hat, and upper confidence interval of d_hat for each poll.

polls <- polls %>%
  mutate(d_hat = d_hat/100)

pollster_results <- polls %>%
  mutate(X_hat = (d_hat+1)/2,
         se_hat = 2*sqrt(X_hat*(1-X_hat)/samplesize),
         lower = d_hat - qnorm(0.975)*se_hat,
         upper= d_hat + qnorm(0.975)*se_hat) %>%
  select(pollster,enddate,d_hat,lower,upper)

#-----------------------------7----------------------------------
# Add a logical variable called `hit` that indicates whether the actual value (0.021) exists within the 
#confidence interval of each poll. Summarize the average `hit` result to determine the proportion of 
#polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.

avg_hit<- pollster_results %>%
  mutate(hit= 0.021>=lower & 0.021<=upper) %>%
  summarise(average=mean(hit))

#-----------------------------8----------------------------------
# Add variable called `error` to the object `polls` that contains the difference between d_hat 
#and the actual difference on election day. Then make a plot of the error stratified by pollster.

error<- polls$d_hat-0.021
error

polls %>% mutate(error=error) %>%
  ggplot(aes(x=pollster, y = error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#-----------------------------9----------------------------------


error<- polls$d_hat-0.021
error

polls %>% mutate(error=error) %>%
  group_by(pollster) %>% 
  filter(n() >= 5) %>% #This counts the number of polls per pollster and selects the one with 5 or more polls
  ggplot(aes(x=pollster, y = error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
