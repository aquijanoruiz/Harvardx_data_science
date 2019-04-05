rm(list=ls())
library(tidyverse) 
data(murders)
data(murders)
murders$murder_rate <- murders$total/murders$population*10^5

#arrange
#Ascending
murders %>% arrange(population) %>% head() #order states by population size. Head shows the first 6

murders %>% arrange(murder_rate) %>% head() #order states by murder_rate
#Descending
murders %>% arrange(desc(murder_rate)) %>% head()

#Sorting more than one variable
murders %>% arrange(region, murder_rate) %>% head()

#top_n
murders %>% top_n(10, murder_rate) #top 10 accroding to murder rate. BUT THEY ARE NOT ORDERED

murders %>% arrange(desc(murder_rate)) %>% top_n(10)
#Now we don't have to tell it what column to use, because top_n(10) will show us the top 10 
#rows that have already been ordered

