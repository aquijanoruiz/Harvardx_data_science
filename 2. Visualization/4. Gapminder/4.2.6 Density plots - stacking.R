rm(list=ls())
library(dslabs)
library(tidyverse) 
data(gapminder)


#--------------------------preparation-----------------------------
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

west<- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zeland")
west

country_list_1 <- gapminder %>% 
  filter(year==1970 & !is.na(dollars_per_day)) %>% .$country

country_list_2 <- gapminder %>% 
  filter(year==2010 & !is.na(dollars_per_day)) %>% .$country

country_list<- intersect(country_list_1,country_list_2) #creates a matching
country_list

#-------------------------------------------------------

aes(x=dollars_per_day, y=..count..) #this puts count on the y axis (counts the number of countries in the developing and west categories)

gapminder %>%
  filter(year %in% c(1970,2010) & country %in% country_list) %>%
  mutate(group=ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(x=dollars_per_day, y=..count..,fill=group))+ #counts the number of countries in each category so that there doesn't seem to be the same amount of countries, but instead more developing countries than rich countries
  scale_x_continuous(trans="log2")+
  geom_density(alpa=0.2)+
  facet_grid(year~.)

#-------------------------changing bindwith------------------------------

gapminder %>%
  filter(year %in% c(1970,2010) & country %in% country_list) %>%
  mutate(group=ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(x=dollars_per_day, y=..count..,fill=group))+ #counts the number of countries in each category so that there doesn't seem to be the same amount of countries, but instead more developing countries than rich countries
  scale_x_continuous(trans="log2")+
  geom_density(alpa=0.2,bw=0.75)+ #changes the bindwith
  facet_grid(year~.)

#-------------------creating various regions-------------------------
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent %in% c("Africa") & region!="Nothern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))
    

#-------------------creating factor variable-------------------------
gapminder <- gapminder %>% 
  mutate(group = factor(group, levels= c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

#-------------------stacking-------------------------
gapminder %>%
  filter(year %in% c(1970,2010) & country %in% country_list) %>%
  ggplot(aes(x=dollars_per_day, y=..count..,fill=group))+ #counts the number of countries in each category so that there doesn't seem to be the same amount of countries, but instead more developing countries than rich countries
  scale_x_continuous(trans="log2")+
  geom_density(alpa=0.2,bw=0.75,position="stack")+ #the regions stack on each other
  facet_grid(year~.)

#-------------------weight the smooth densities-------------------------
gapminder %>%
  filter(year %in% c(1970,2010) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight=population/sum(population)*2) %>%
  ungroup() %>%
  ggplot(aes(x=dollars_per_day, fill=group, weight=weight))+ #counts the number of countries in each category so that there doesn't seem to be the same amount of countries, but instead more developing countries than rich countries
  scale_x_continuous(trans="log2")+
  geom_density(alpa=0.2,bw=0.75,position="stack")+ #the regions stack on each other
  facet_grid(year~.)
