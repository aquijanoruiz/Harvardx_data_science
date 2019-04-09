rm(list=ls())
library(dslabs)
data(gapminder)
#Faceting.- to create side by side plots (stratify data). We use facet_gird
#The function lets you facet by up to two variables using columns to represent one variable and rows 
#to represent the other. The function expects the rows and column variables separated by a tilde

#------------------facet_grid------------------------
gapminder %>% filter(year %in% c(1962,2012)) %>% #the %in% we want to know whether or not each element of a first vector is in a second vector
  ggplot(aes(x=fertility,y=life_expectancy,col=continent))+
  geom_point()+
  facet_grid(continent~year) #The function lets you facet by up to two variables using columns to represent one variable and rows to represent 
#the other. The function expects the rows and column variables separated by a tilde

#Put all graphics in the same raw
gapminder %>% filter(year %in% c(1962,2012)) %>% #the %in% we want to know whether or not each element of a first vector is in a second vector
  ggplot(aes(x=fertility,y=life_expectancy,col=continent))+
  xlab("Fertility") + #adds the labels
  ylab("Life expectancy") +
  ggtitle("Fertility Vs Life Expectancy")+
  geom_point()+
  scale_color_discrete("Continent")+
  facet_grid(.~year) #the dot means we are not using a variable for the rows

#It shows that the majority of countries have moved from the developing world cluster to the Western world one

#------------------facet_warp------------------------
gapminder %>% filter(year %in% c(1962,1980,1990,2000,2012)) %>%
  ggplot(aes(x=fertility,y=life_expectancy,col=continent))+
  xlab("Fertility") + #adds the labels
  ylab("Life expectancy") +
  ggtitle("Fertility Vs Life Expectancy")+
  geom_point()+
  scale_color_discrete("Continent")+
  facet_wrap(~year)

#if I only want to compare Asia and Europe
gapminder %>% filter(year %in% c(1962,1980,1990,2000,2012) & continent %in% c("Asia","Europe")) %>%
  ggplot(aes(x=fertility,y=life_expectancy,col=continent))+
  xlab("Fertility") + #adds the labels
  ylab("Life expectancy") +
  ggtitle("Fertility Vs Life Expectancy")+
  geom_point()+
  scale_color_discrete("Continent")+
  facet_wrap(~year)