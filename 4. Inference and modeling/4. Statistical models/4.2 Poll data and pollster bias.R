rm(list=ls())
library(dslabs)
library(tidyverse)
names(polls_us_election_2016)

#------------------------------ FiveThirtyEight poll--------------------------
#Filter the data to include national polls that happened during the week before the election.
#We also remove polls that FiveThirtyEight has determined not to be reliable, and they have graded 
#them with a B or less. Some polls have not been graded. And we're going to leave these in.

polls<- polls_us_election_2016 %>%
  filter(state=="U.S." & 
           enddate>= "2016-10-31" & #remember dates must be between ""
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) #we use | like an "and" we we use %in%

#Add a spread to the data.frame

polls<- polls %>%
  mutate(spread=rawpoll_clinton/100-rawpoll_trump/100) #this is the spread of each polls

#now we compute the stimated spread using a weighted average

d_hat<- polls %>%
  summarize(d_hat= sum(spread*samplesize)/sum(samplesize)) %>% .$d_hat
d_hat

#now we compute the estimated margin of error
p_hat <- (d_hat+1)/2 #we first compute p_hat estimated percentage of Clinton voters
moe<- qnorm(0.975)*2*sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
moe #this is the margin of error the spread stimate

#moe give us 0.66%. On election night, we find out that the actual percentage is 2.1%, which is outside of 
#the 95% confidence interval. So, what happened?

polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01) #The data does not appear to be normally distributed,
#and the standard error appears to be larger than 0.0066.

#Me can see how many polls each pollster conducted
polls %>% group_by(pollster) %>%
  summarize(n())

#Let's visualize the data for the pollsters that are regularly polling.
polls %>% 
  group_by(pollster) %>%
  filter(n() >=6) %>%
  ggplot(aes(x=pollster,y=spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle=90,hjust=1))

#Let's look at the theory
polls %>% 
  group_by(pollster) %>%
  filter(n() >=6) %>%
  summarize(se=2*sqrt(p_hat*(1-p_hat)/median(samplesize))) #we compute the standard error of the spread. We take the median samplesize to illustrate this table


