library(dslabs)
library(dplyr)
data(heights)
attach(heights)
x<- heights %>% filter(sex=="Male") %>% .$height
n<- length(x)
avg<- mean(x)
s<- sd(x)
simulated_heights<- rnorm(n,avg,s) #generates normally distributed outcomes
library(ggplot2)
ds_theme_set()
data.frame(simulated_heights=simulated_heights) %>% ggplot(aes(simulated_heights)) + geom_histogram(color="black",binwidth = 2)
#generates a graph with the normally distributed outcomes

#If we pick 800 males at random, how rare is that the tallest person is a 7 footer?
B<- 10000
tallest<- replicate(B, {
  simulated_data<- rnorm(800,avg,s)
  max(simulated_data)
})
mean(tallest>=7*12)
