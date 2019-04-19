rm(list=ls())

#d ~ N(miu,tau^2) describes our best guess had we not seen any polling data
#X_hat|d <- N(d,sigma^2) describes randomness due to sampling and the pollster effect

library(dplyr)
library(dslabs)
data(polls_us_election_2016)

library(tidyverse)
library(dslabs)
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

mu <- 0 #is interpreted as a model that simply does not provideany information on who will win.
#ES: Mu es como información porcentaje incial que predice quién va a ganar antes de que hagas las encuestas; por ejemplo,
#la situación económica. En el caso del baseball, mu era 0.275 que era el "batting average" en la historia del baseball.
#Se la puede considerar como la constante en la regresión. En este caso, si mu es 0, la distribución prior no te dice nada de
#quién va a ganar. Pero la tau te dice que historicamente los datos muestran una diferencia de 0.035 en el voto popular.

tau <- 0.035 #we will use recent historical data that shows the winner of the popular vote has an average spread of about 3.5%.

sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se

#credible interval
posterior_mean + c(-1.96, 1.96)*posterior_se

#The posterior probability
1 - pnorm(0, posterior_mean, posterior_se)
