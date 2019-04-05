#summarize
#The summarize function in dplyr provides a way to compute summary statistics 
#with intuitive and readable code
rm(list=ls())
library(tidyverse) #this package includes dplr
data(heights)

#average and sd
s<-heights %>% filter(sex=="Male") %>%
  summarize(average=mean(height),standard_deviation=sd(height)) #we can change the names
s
class(s) #s is a data frame
s$average
s$standard_deviation

#As with most other dplyr functions, summarize is aware of the variable names 
#and we can use them directly

heights %>% filter(sex=="Male") %>%
  summarize(median=median(height),
            maximum=max(height),
            minimun=min(height))

#But... here we get an error because summarize can only call functions that return a single value
heights %>% filter(sex=="Male") %>%
  summarize(median=median(height),
            maximum=max(height),
            minimun=min(height)，
            range=quantile(height,c(0,0.5,1)))