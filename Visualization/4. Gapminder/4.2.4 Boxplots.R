rm(list=ls())
library(dslabs)
data(gapminder)
length(levels(gapminder$region)) #to count the number of regions

gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

p<- gapminder %>% 
  filter(year==1970 & !is.na(gdp)) %>%
  ggplot(aes(x=region,y=dollars_per_day))
p + geom_boxplot() #there are many regions!

p + geom_boxplot() +
  theme(axis.text.x = element_text(angle=90,hjust=1)) #it can rotate the names

#--------------------------reorder function----------------------

p<- gapminder %>%
  filter(year== 1970 & !is.na(gdp)) %>%
  mutate(region=reorder(region, dollars_per_day, FUN=median)) %>% #to reorder plots by their medium variable
  ggplot(aes(x=region,y=dollars_per_day,fill=continent))+ #we use fill so that each continent gets its color
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90,hjust=1)) #it can rotate the names
p

p<- gapminder %>%
  filter(year== 1970 & !is.na(gdp)) %>%
  mutate(region=reorder(region, dollars_per_day, FUN=median)) %>% #to reorder plots by their medium variable
  ggplot(aes(x=region,y=dollars_per_day,fill=continent))+ #we use fill so that each continent gets its color
  geom_boxplot()+
  scale_y_continuous(trans = "log2") + #to change the scale to the log scale
  geom_point(show.legend = FALSE)+
  theme(axis.text.x=element_text(angle=90,hjust=1)) #it can rotate the names
p
