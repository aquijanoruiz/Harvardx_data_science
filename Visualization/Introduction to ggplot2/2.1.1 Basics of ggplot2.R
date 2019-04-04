library(tidyverse)
library(dslabs)
data(murders)

ggplot(data=murders) #disassociates the data set with the plotting object

#we can also pipe the data
murders %>% ggplot() #the same as ggplot(data=murders) #we have a a blank slate 
#since no geometry has been defined.

p<- ggplot(data=murders) #Here we're assigning the graph object to the object p
class(p) #we can see it is a ggplot object

print(p)
p #these two can help you see the plot

