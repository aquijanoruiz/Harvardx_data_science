rm(list=ls())
library(dslabs)
library(tidyverse)
names(polls_us_election_2016)

polls<- polls_us_election_2016 %>%
  filter(state=="U.S." & 
           enddate>= "2016-10-31" & #remember dates must be between ""
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) #we use | like an "and" we we use %in%

polls<- polls %>%
  mutate(spread=rawpoll_clinton/100-rawpoll_trump/100) #this is the spread of each polls

#For each pollster, let's collect their last-reported result before the election.

one_poll_per_pollster<- polls %>% group_by(pollster) %>%
  filter(enddate==max(enddate)) %>%
  ungroup() #undones what we do with group_by()

one_poll_per_pollster

#We can make a histogram
one_poll_per_pollster %>%
  ggplot(aes(x=spread)) +
  geom_histogram(binwidth = 0.01)

#We create a different model
one_poll_per_pollster

#----------------------------1-----------------------------
#We take the sd of the sample spread
sd(one_poll_per_pollster$spread)

#----------------------------2-----------------------------
#We use the central limit theorem to create a confidence interval

results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread),
            se = sd(spread) / sqrt(length(spread))) %>% #the se of the spread
  mutate(start = avg - 1.96 * se,
         end = avg + 1.96 * se)

round(results * 100, 1)
