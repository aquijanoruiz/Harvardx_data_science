rm(list=ls())
library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)

west <- c("Western Europe","Northern Europe","Southern Europe",
          "Northern America","Australia and New Zealand")
dat <- gapminder %>%
  filter(year%in% c(2010, 2015) & region %in% west &
           !is.na(life_expectancy) & population > 10^7) %>%
  mutate(location = ifelse(year == 2010, 1, 2), #1 and 2 represent the grid line where the label will be located
         location = ifelse(year == 2015 & country %in% c("United Kingdom","Portugal"), location+0.22, location), #so that they are separated
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year))

dat %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust),
            show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy")