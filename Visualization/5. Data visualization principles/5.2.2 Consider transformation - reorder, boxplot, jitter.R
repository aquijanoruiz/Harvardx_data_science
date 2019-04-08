rm(list = ls())
library(dslabs)
library(tidyverse) 
data(gapminder)

#-------------------------plotting population--------------------------
gapminder %>%
  filter(year==2015) %>%
  mutate(population=population/10^6) %>%
  mutate(continent = reorder(x=continent, X=population, FUN = median)) %>% #reorders them so that they do not appear in alphabetical order but the population median
  ggplot(aes(x=continent,y=population)) +
  geom_boxplot(coef=3)+
  geom_jitter(alpha=0.5, width = 0.1)+
  scale_y_continuous(trans = "log2",breaks =c(1,10,100,1000))+ #we assign the breaks
  xlab("Continent")+
  ylab("Population in Millions")