rm(list=ls())

#What is the maximum margin of error if we polled 100,000 people?
N<- 100000
p<- seq(from=0.35,to=0.65,length=100) #we see what happens in different scenarios

se_function <- function(x) {
  2*sqrt(x*(1-x)/N) #this is the formula for the margi of error (we can use 1.96 instead)
} #we first need to create the vector

SE<- sapply(p, se_function) #sapply needs the vector and the functinon
SE

data.frame(p=p,se=SE) %>%
  ggplot(aes(x=p,y=SE)) +
  geom_line() #we see from the graph that the maximum margin of error possible is 0.3% when p is 50%

#The larger standard errors occur when p=50%
