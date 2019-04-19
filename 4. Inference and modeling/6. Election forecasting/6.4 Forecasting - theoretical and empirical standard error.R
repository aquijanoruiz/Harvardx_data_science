#How informative are polls taken several weeks before the election?
rm(list=ls())
library(dplyr)
library(dslabs)

#In our example, to make sure that the variability we observe is not due to pollster effects, we're going 
#to stick to just one pollster. Using this code, we're going to look at Ipsos data.

one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

#Since there's no pollster effect, perhaps the theoretical standard error will match the data-derived standard deviation.
se <- one_pollster %>%
  summarize(empirical = sd(spread), #this is what you would do if you had all the data
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize))) #what you would do if you only had the mean and the sample size

#Furthermore, the distribution of the data does not look normal as the theory would predict, 
#as we can see in this figure.

#---------------------------------------histogram using qplot---------------------------------------------
qplot(spread, geom = "histogram", binwidth = 0.01, data = one_pollster, color = I("black"))

#---------------------------------------histogram using geom_smooth---------------------------------------------
polls_us_election_2016 %>%
  filter(state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n()>=10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) + #loess is the smoothing method, while span controls the amount of smoothing for the default loess smoother. 
  geom_point(aes(color=pollster), show.legend = FALSE, alpha=0.6) #alpha tells how transparent the dots are

polls_us_election_2016 %>%
  filter(state == "U.S." & enddate>="2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump","Clinton")))%>%
  group_by(pollster) %>%
  filter(n()>=10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha=0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30,50))