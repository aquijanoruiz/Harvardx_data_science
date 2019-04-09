rm(list=ls())
library(dslabs)
data(gapminder)

gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

gapminder %>% 
  filter(year==1970 & !is.na(gdp)) %>%
  ggplot(aes(x=dollars_per_day))+
  geom_histogram(binwidth = 1, color="black")

gapminder %>% 
  filter(year==1970) %>% filter(gdp, !is.na(gdp)) #the previous two lines can be written like this as well

#For the majority of countries, averages are below $10 a day. However, the majority of the x-axis 
#is dedicated to the 35 countries with averages above 10. We will see how many countries make on average about $1 a day

gapminder %>% 
  filter(year==1970 & !is.na(gdp)) %>%
  ggplot(aes(log2(x=dollars_per_day)))+
  geom_histogram(binwidth = 1, color="black")
#These bumps are sometimes referred to as modes. The distribution has two local modes

#------------------------------------------
#Why not to use the natural log
#It's not easy to compute e to the 2, e to the 3, et cetera. So we don't recommend using the natural log 
#for data exploration

log(25,base=2) #log(25,base=2)=4.643856
#2^4.643856=25

#------------------------------------------
#Same as writing this, but the x axis is not longer in the log scale. We see the original values in the lof scale
gapminder %>% 
  filter(year==1970 & !is.na(gdp)) %>%
  ggplot(aes(x=dollars_per_day))+
  geom_histogram(binwidth = 1, color="black")+
  scale_x_continuous(trans="log2")

  