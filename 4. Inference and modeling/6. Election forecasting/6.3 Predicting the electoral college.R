rm(list=ls())
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

results_us_election_2016 %>% top_n(5, electoral_votes) # We use top_n to rank by electoral vote. Here are the top 5
#states ranked by electoral votes in 2016.

#-----------------------------------------------aggregatind the polls results------------------------------
#We start by aggregating results from a poll taken during the last week before the election.
results_df <- polls_us_election_2016 %>%
  filter(state!="U.S." &
           !str_detect(state, "CD") & #removes polls that are not for entire states (it is explained later)
           enddate >="2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

results_df

results <- results_df %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

#-------------------------------------left joint----------------------------------------
#We now introduce the command left_join that will let us easily add the number of electoral votes for each
#state from the data set us_electoral_votes_2016.

results <- left_join(results, results_us_election_2016, by = "state") #joins the two data bases according to the variable state
results 

#Note that some states have no polls. This is because a winner is pretty much known.
results_us_election_2016 %>% filter(!state %in% results$state) #this shows the states that do not appear in the polls

#-----------------------correctin NA in sd of states with just one poll---------------------------------
results #note that some states have NA in sd, because there was just one poll.
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

#-----------------------------------------the prior-----------------------------------------------------
mu <- 0 #We could construct the priors for each state based on recent history. However, to keep it simple, 
#we assign a prior to each state that assumes we know nothing about what will happen.
tau <- 0.02 #Since from election year to election year the results from a specific state don’t change that much,
#we will assign a standard deviation of 2%

#-----------------------------------------Bayesian calculation-------------------------------------------
results_df2<- results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2 / (sd^2 + tau^2),
                   posterior_mean = B * mu + (1 - B) * avg,
                   posterior_se = sqrt(1/ (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))

#Note that estimates based on posteriors move the estimates towards 0, although the states with many polls are influenced less.

#-----------------------------------------Plot of the Bayesian calculation-----------------------------------
results %>% mutate(sigma = sd / sqrt(n),
                   B = sigma^2 / (sigma^2 + tau^2),
                   posterior_mean = B * mu + (1 - B) * avg,
                   posterior_se = sqrt(1/ (1/sigma^2 + 1/tau^2))) %>%
  ggplot(aes(avg, posterior_mean, size = n)) + geom_point() + #we assign the size to the number of n
  geom_abline(slope = 1, intercept = 0)
#---------------------------Monte Carlo simulation with Bayesian calculation without bias------------------------------

B <- 10000
mu <- 0
tau <- 0.02
clinton_EV <- replicate(B, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2 / (sigma^2 + tau^2),
                     posterior_mean = B * mu + (1 - B) * avg,
                     posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), #this is the simulation. We use Rnorm to simulate the spread
                                              posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result>0, electoral_votes, 0)) %>% #if the spread is positive, Clinton wins, if it is negative Trump wins
    summarize(clinton = sum(clinton)) %>%
    pull(clinton) + 7 #because we don't have information for Rhode Island and DC but we assume she will win there
})
mean(clinton_EV > 269) #This model gives Clinto 99% of winning

#---------------------------INSIDE THE MONTE CARLO SIMULATION (JUST FOR UNDERSTANDING)------------------------------

results_df2<- results %>% mutate(sigma = sd/sqrt(n),
                                 B = sigma^2 / (sd^2 + tau^2),
                                 posterior_mean = B * mu + (1 - B) * avg,
                                 posterior_se = sqrt(1/ (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))

length(results_df2$posterior_mean) #We create a simulation of each state. In total there are 47 states in this simulation (excluding Rhode Island and DC)
simulated_result<- rnorm(length(results_df2$posterior_mean), #this is the simulation. We use Rnorm to simulate the spread 47 times 
                         results_df2$posterior_mean, results_df2$posterior_se)
simulated_result

#---------------------------Monte Carlo simulation with Bayesian calculation with bias------------------------------
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/n + bias_sd^2), #we include bias when we calculate sigma this makes the probability of Clinton of winning the election to go down to 83%
                     B = sigma^2 / (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1/ (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean),
                                              posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result>0, electoral_votes, 0)) %>%
    summarize(clinton = sum(clinton) + 7) %>%
    pull(clinton)
})
mean(clinton_EV_2 > 269)
