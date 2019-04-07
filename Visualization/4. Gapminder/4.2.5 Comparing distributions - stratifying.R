rm(list=ls())
library(dslabs)
data(gapminder)

gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

west<- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zeland")
west

#---------------------comparing two regions-------------------------
gapminder %>%
  filter(year==1970 & !is.na(gdp)) %>%
  mutate(group=ifelse(region %in% west, "West", "Developing")) %>% #creates a new variable region to divide between the west and the developing world
  ggplot(aes(x=dollars_per_day)) +
  geom_histogram(bindwith=1,color="black")+
  scale_x_continuous(trans="log2")+
  facet_grid(.~group) #the . means we don't want rows

#---------------------comparing two regions and two years-------------------------

gapminder %>%
  filter(year %in% c(1970,2010) & !is.na(gdp)) %>%
  mutate(group=ifelse(region %in% west, "West", "Developing")) %>% #creates a new variable region to divide between the west and the developing world
  ggplot(aes(x=dollars_per_day)) +
  geom_histogram(bindwith=1,color="black")+
  scale_x_continuous(trans="log2")+
  facet_grid(year~group) #we want rows and columns
