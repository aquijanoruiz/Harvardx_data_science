rm(list=ls())
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)

#------------------------------1-------------------------------
#Using ggplot and the points layer, create a scatter plot of life expectancy versus fertility for 
#the African continent in 2012.

gapminder %>% filter(year==2012 & continent=="Africa") %>%
  ggplot(aes(x=fertility,y=life_expectancy)) +
  geom_point()

#------------------------------2-------------------------------
#Remake the plot from the previous exercises but this time use color to dinstinguish the different 
#regions of Africa to see if this explains the clusters.

gapminder %>% filter(year==2012 & continent=="Africa") %>%
  ggplot(aes(x=fertility,y=life_expectancy,color=region)) +
  geom_point()

#------------------------------3-------------------------------
#Create a table showing the country and region for the African countries (use select) that in 2012 
#had fertility rates of 3 or less and life expectancies of at least 70.

df<- gapminder %>%
  filter(year==2012 & continent=="Africa") %>%
  filter(fertility<=3,life_expectancy>=70) %>%
  select(country,region)
df

#------------------------------4-------------------------------
#The Vietnam War lasted from 1955 to 1975. Do the data support war having a negative effect on life expectancy?
#Use filter to create a table with data for the years from 1960 to 2010 in Vietnam and the United States.

unique(gapminder$year) #shows all the years

  tab<- gapminder %>%
    filter(year>=1960 & year<=2010) %>%
    filter(country %in% c("United States", "Vietnam"))

#------------------------------5-------------------------------
#Use geom_line to plot life expectancy vs year for Vietnam and the United States.

p<- tab %>%
  ggplot(aes(x=year,y=life_expectancy,color=country))+
  geom_line()
p

#------------------------------6-------------------------------
#Use a single line of code to create a time series plot from 1960 to 2010 of life expectancy vs year for Cambodia.

gapminder %>%
  filter(year>=1960 & year<=2010 & country=="Cambodia") %>%
  ggplot(aes(x=year,y=life_expectancy)) +
  geom_line()

#------------------------------7------------------------------- 
#Now we are going to calculate and plot dollars per day for African countries in 2010 using GDP data.
#In the first part of this analysis, we will create the dollars per day variable.

daydollars <- gapminder %>%
  filter(year==2010 & continent=="Africa") %>%
  mutate(dollars_per_day=gdp/population/365) %>%
  filter(!is.na(dollars_per_day))

#------------------------------8------------------------------- 
#Now we are going to calculate and plot dollars per day for African countries in 2010 using GDP data.

daydollars %>%
  ggplot(aes(dollars_per_day)) +
  geom_density()+
  scale_x_continuous(trans="log2")
 
#------------------------------9-------------------------------
#Create a smooth density plot of dollars per day for 1970 and 2010 using a log (base 2) scale for the x axis.

daydollars <- gapminder %>%
  filter(year %in% c(1970,2010) & continent=="Africa") %>%
  mutate(dollars_per_day=gdp/population/365) %>%
  filter(!is.na(dollars_per_day))

daydollars %>%
  ggplot(aes(x=dollars_per_day))+
  geom_density()+
  scale_x_continuous(trans="log2")+
  facet_grid(year~.)

#------------------------------10-------------------------------
#Now we are going to edit the code from Exercise 9 to show stacked histograms of each region in Africa.

daydollars %>%
  ggplot(aes(x=dollars_per_day,fill=region))+
  geom_density(bw=0.5,position="stack")+
  scale_x_continuous(trans="log2")+
  facet_grid(year~.)

#------------------------------11-------------------------------
#We are going to continue looking at patterns in the gapminder dataset by plotting infant mortality rates 
#versus dollars per day for African countries.

gapminder_Africa_2010 <- gapminder %>%
  filter(year==2010 & continent=="Africa") %>%
  mutate(dollars_per_day=gdp/population/365) %>%
  filter(!is.na(dollars_per_day))

gapminder_Africa_2010 %>% 
  ggplot(aes(x=dollars_per_day,y=infant_mortality,color=region))+
  geom_point()

#------------------------------12-------------------------------
#Transform the x axis to be in the log (base 2) scale.

gapminder_Africa_2010 %>% 
  ggplot(aes(x=dollars_per_day,y=infant_mortality,color=region))+
  geom_point()+
  scale_x_continuous(trans="log2")

#------------------------------13-------------------------------
#In this exercise, we will remake the plot from Exercise 12 with country names instead of 
#points so we can identify which countries are which.

gapminder_Africa_2010 %>% 
  ggplot(aes(x=dollars_per_day,y=infant_mortality,color=region,label=country))+
  geom_point()+
  scale_x_continuous(trans="log2")+
  geom_text()

#------------------------------14-------------------------------
#Now we are going to look at changes in the infant mortality and dollars per day patterns 
#African countries between 1970 and 2010.

gapminder_Africa_1970_2010 <- gapminder %>%
  filter(year %in% c(1970,2010) & continent=="Africa") %>%
  mutate(dollars_per_day=gdp/population/365) %>%
  filter(!is.na(dollars_per_day)) %>%
  filter(!is.na(infant_mortality))

gapminder_Africa_1970_2010 %>%
  ggplot(aes(x=dollars_per_day,y=infant_mortality,color=region,label=country))+
  geom_point()+
  scale_x_continuous(trans="log2")+
  geom_text()+
  facet_grid(year~.)
