#---------------------1---------------------------
#To make the plot on the right in the exercise from the last set of assessments, we had to reorder the levels of the states' variables.
rm(list=ls())
library(dplyr)
library(ggplot2)
library(dslabs)

dat <- us_contagious_diseases %>%
  filter(year == 1967 & disease=="Measles" & !is.na(population)) %>% 
  mutate(rate = count / population * 10000 * 52 / weeks_reporting)

state <- dat$state 
rate <- dat$count/(dat$population/10000)*(52/dat$weeks_reporting)

#Redefine the state object so that the levels are re-ordered by rate.

state<- reorder(x=state,X=rate)
levels(print(state)) #reorders the states

#---------------------2---------------------------
#Reorder a bar chart

dat <- us_contagious_diseases %>% 
  filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>%
  mutate(state= reorder(x=state,X=rate))

dat %>% ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip()

#---------------------3---------------------------

murders %>% mutate(rate = total/population*100000) %>%
  group_by(region) %>%
  summarize(avg = mean(rate)) %>%
  mutate(region = factor(region)) %>%
  ggplot(aes(region, avg)) +
  geom_bar(stat="identity") +
  ylab("Murder Rate Average")

#---------------------4---------------------------

murders %>% mutate(rate = total/population*100000) %>%
  mutate(region=reorder(x=region,X=rate,FUN=median)) %>%
  ggplot(aes(x=region,y=rate)) +
  geom_point()+
  geom_boxplot()
