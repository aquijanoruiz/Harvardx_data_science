#Gapminder dataset
rm(list=ls())
library(dslabs)
data(gapminder)
head(gapminder)
gapminder %>% filter(year==2015 & country %in% c("Sri Lanka", "Turkey", )) %>% 
  select(country, infant_mortality)

#A scatterplot of life expectancy versus fertility rates.

library(tidyverse)
ds_theme_set()
gapminder %>% filter(year==1962) %>% 
  ggplot(aes(fertility,life_expectancy,color=continent)) +
  geom_point()

#same as
filter(gapminder, year==1962) %>% 
  ggplot(aes(x=fertility,y=life_expectancy,color=continent)) +
  xlab("Fertility")+
  ylab("Life expectancy")+
  ggtitle("Fertility Vs Life Expectancy")+
  scale_color_discrete(name = "Region")+
  geom_point()
