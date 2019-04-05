library(dslabs)
data(heights)
heights %>% filter(sex=="Male") #we use filter function to filter the males

#histogram

p<- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height)) 
#We are going to define a graph object p, that has the data piped into the ggplot function
#and defines the aesthetic mapping that tells us that heights is what we are going to make a histogram of

p + geom_histogram()
p + geom_histogram(binwidth = 1)
p + geom_histogram(binwidth = 1,fill="blue",col="black")+
  xlab("Male heights in inches")+
  ggtitle("Histogram")

#-----------------------------------------
#smooth density

p + geom_density()
p + geom_density(fill="blue")

#-----------------------------------------
#q-q plot
q<- heights %>% filter(sex=="Male") %>% ggplot(aes(sample=height)) 
q+geom_qq()
#By default, the Q-Q plot is compared to the normal distribution with average zero and standard deviation one.
#If we don't want to express this in standard units we can use the dparams argument

#dparams argument
params<- heights %>% filter(sex=="Male") %>%
  summarize(mean= mean(height),sd=sd(height)) #we use the dplyr functions

#We can add identity lines to see how well the normal approximation works
#And in this case, we simply add the layer geom_abline
q + geom_qq(dparams = params)
q + geom_qq(dparams = params) +
  geom_abline() 

heights %>% filter(sex=="Male") %>% 
  ggplot(aes(sample=scale(height)))+
  geom_qq()+
  geom_abline()

#-----------------------------------------
#To put two or more plots together we use the gridextra package
p<- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height)) 
p1<- p + geom_histogram(fill="blue",binwidth = 1, col="black")
p2<- p + geom_histogram(fill="blue",binwidth = 2, col="black")
p3<- p + geom_histogram(fill="blue",binwidth = 3, col="black")

library(gridExtra)
grid.arrange(p1,p2,p3,ncol=3)
