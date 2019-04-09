#Time series plots
rm(list=ls())
library(dslabs)
data(gapminder)



#------------------------------------------------
#Fertility rate in the USA over the years
gapminder %>% filter(country=="United States") %>% 
  ggplot(aes(x=year,y=fertility))+
  geom_line()

#------------------------------------------------
#Comparing fertility rates in South Korea and Germany
gapminder %>% filter(country %in% c("South Korea", "Germany")) %>% 
  ggplot(aes(x=year,y=fertility))+
  geom_line() #but this is not what we want

#What we want
gapminder %>% filter(country %in% c("South Korea", "Germany")) %>% 
  ggplot(aes(x=year,y=fertility,group=country))+ #we need to assign country to the group argument
  geom_line() #but this is not what we want

gapminder %>% filter(country %in% c("South Korea", "Germany")) %>% 
  ggplot(aes(x=year,y=fertility,col=country))+ #we can also use color to group the elements
  geom_line() +#but this is not what we want
  scale_color_discrete("Country")

#------------------------------------------------
#Labeling is usually preferred over legends
label <- data.frame(country=c("South Korea", "Germany"), x=c(1975,1965), y= c(60,72))
label #x and y tells where the labels should be located in the graphic (we pick this by eye)

gapminder %>% filter(country %in% c("South Korea", "Germany")) %>% 
  ggplot(aes(x=year,y=life_expectancy,col=country))+ #we can also use color to group the elements
  geom_line() +#but this is not what we want
  geom_text(data=label, aes(x,y,label=country),size=5)+ #this takes the information form the data.frame we created
  theme(legend.position = "none")

