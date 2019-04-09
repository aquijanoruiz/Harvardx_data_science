rm(list=ls())
library(dslabs)
data(gapminder)

gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

west<- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zeland")
west

#---------------------comparing two regions with histogram-------------------------
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

#-----------------include only the countries in we have data--------------------
#We make the plot again, but this time using only the subset of countries for which data is present in 1970 and 2010

country_list_1 <- gapminder %>% 
  filter(year==1970 & !is.na(dollars_per_day)) %>% .$country

country_list_2 <- gapminder %>% 
  filter(year==2010 & !is.na(dollars_per_day)) %>% .$country

country_list<- intersect(country_list_1,country_list_2) #creates a matching
country_list

gapminder %>%
  filter(year %in% c(1970,2010) & country %in% country_list) %>%
  mutate(group=ifelse(region %in% west, "West", "Developing")) %>% #creates a new variable region to divide between the west and the developing world
  ggplot(aes(x=dollars_per_day)) +
  geom_histogram(bindwith=1,color="black")+
  scale_x_continuous(trans="log2")+
  facet_grid(year~group) #we want rows and columns

#-------------------now box plot----------------------------------

gapminder %>%
  filter(year %in% c(1970,2010) & country %in% country_list) %>%
  mutate(region=reorder(region, dollars_per_day, FUN=median)) %>% #to reorder plots by their medium variable
  ggplot(aes(x=region,y=dollars_per_day,fill=continent))+ #we use fill so that each continent gets its color
  geom_boxplot()+ #we don't need to write anything because it has already been defined in ggplo()
  scale_y_continuous(trans = "log2") + #to change the scale to the log scale
  geom_point(show.legend = FALSE)+
  xlab("") + theme(axis.text.x=element_text(angle=90,hjust=1)) +#it can rotate the names
  facet_grid(year~.) #to include only the year(column)

#------------------puting the years next to each other--------------

gapminder %>%
  filter(year %in% c(1970,2010) & country %in% country_list) %>%
  mutate(region=reorder(region, dollars_per_day, FUN=median)) %>% #to reorder plots by their medium variable
  ggplot(aes(x=region,y=dollars_per_day,fill=factor(year)))+ #we use fill so that each year gets its color and be considered as factor (categorical variable) 
  geom_boxplot()+ #we don't need to write anything because it has already been defined in ggplo()
  scale_y_continuous(trans = "log2") + #to change the scale to the log scale
  geom_point(show.legend = FALSE)+
  xlab("") + theme(axis.text.x=element_text(angle=90,hjust=1)) #it can rotate the names