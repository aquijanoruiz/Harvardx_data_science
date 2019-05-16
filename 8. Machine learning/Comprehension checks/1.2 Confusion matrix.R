rm(list=ls())
library(dslabs)
library(dplyr)
library(lubridate)

data("reported_heights")
head(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, levels=c("Female", "Male")) #we want to predic the sex. If correctly predic a woman it is a TP, if we correctly predict a men it is a TN.
str(y)

x <- dat$type #we use the type of class to make our predictions

#---------------------------------1---------------------------------------------
#The type column of dat indicates whether students took classes in person ("inclass") or online ("online"). 
#What proportion of the inclass group is female? What proportion of the online group is female?

table(x,y) #we create a frequency table
dat_freq <- as.tibble(table(x,y)) %>% spread(y,n) 

dat_freq %>% 
  mutate(x=factor(x)) %>% 
  group_by(x) %>%
  summarize(Female= Female/(Female+Male))

dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female")) #another way to do it

#---------------------------------2---------------------------------------------
#this is an informed guess
y_hat <- ifelse(dat$type =="inclass", "Female","Male") %>%
  factor(levels=levels(y))
mean(y_hat==dat$sex)

#---------------------------------3---------------------------------------------
table(y, y_hat)

#---------------------------------4---------------------------------------------
library(caret)
sensitivity(data=y_hat, reference=y) #this is the percentage of women that was correctly predicted TP/(TP+FN)

#---------------------------------5---------------------------------------------
specificity(data=y_hat, reference=y) #this is the percentage of men we correctly predicted TN/(TN+FP)

#---------------------------------6---------------------------------------------
confusionMatrix(data = y_hat, reference = y)