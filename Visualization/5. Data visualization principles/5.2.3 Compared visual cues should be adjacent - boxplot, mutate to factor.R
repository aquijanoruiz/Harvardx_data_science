rm(list = ls())
library(dslabs)
library(tidyverse) 
data(gapminder)

#-----------plotting and comparing income between two years--------------

gapminder %>%
  filter(year %in% c(1970,2010)) %>%
  mutate(income=gdp/population/365) %>%
  mutate(year=factor(year)) %>% #we need to convert year into a factor variable to be able to use as a category
  ggplot(aes(x=continent,y=income,fill=year)) + #fill groups the continents according to the year
  geom_boxplot(coef=1.5)+
  scale_y_continuous(trans = "log2")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Continent")+
  ylab("Income in dollars per day")