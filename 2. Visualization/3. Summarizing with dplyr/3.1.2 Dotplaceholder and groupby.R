rm(list=ls())
library(tidyverse) #this package includes dplr
data(murders)

DotPlaceholder #To access data that is stored in a data that is piped
us_murder_rate<- murders %>% 
  summarize(rate= sum(total)/sum(population)*10^5)
us_murder_rate #summarize always stores data in a data.frame 
us_murder_rate %>% .$rate 

#Or we can do this
us_murder_rate<- murders %>% 
  summarize(rate= sum(total)/sum(population)*10^5) %>% .$rate
us_murder_rate #Now this gives us the numer, not the table

#Split data into groups and then compute summaries for each group

heights %>% group_by(sex) #this is a group data frame
#You can think of it as many tables with the same columns but not necessarily the same rows

#For heights
heights %>% group_by(sex) %>%
  summarize(average=mean(height),standard_deviation=sd(height))

#For murders
data(murders)
murders$murder_rate <- murders$total/murders$population*10^5

murders %>% group_by(region) %>%
  summarize(median_rate =median(murder_rate))