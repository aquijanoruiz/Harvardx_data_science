#-----------------1---------------------
rm(list=ls())
library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
data(murders)
p <- ggplot(data=murders) #same as
p <- ggplot(murders) #same as
p <- murders %>% ggplot()

#-----------------2---------------------
class(p)

#Print the object p defined in exercise one
print(p)

#-----------------3---------------------
#Now we are going to review the use of pipes by seeing how they can be used with ggplot
data(heights)
# define ggplot object called p like in the previous exercise but using a pipe
p<- heights %>% ggplot()

#-----------------5---------------------
#To create a scatter plot, we add a layer with the function geom_point. 
#The aesthetic mappings require us to define the x-axis and y-axis variables respectively. 
#Fill out the sample code with the correct variable names to plot total murders versus population size

murders %>% ggplot(aes(x = population , y = total)) +
  geom_point()
#-----------------6---------------------
#Remake the plot but flip the axes so that total is on the x-axis and population is on the y-axis

murders %>% ggplot(aes(total, population)) +
  geom_point()

#-----------------8---------------------
#Rewrite the code from the previous exercise to add the state abbreviation as the label through aes
## edit the next line to add the label
murders %>% ggplot(aes(population, total,label=abb)) +
  geom_point() +
  geom_label()

#-----------------10---------------------
#Rewrite the code above to make the labels blue by adding an argument to geom_label
murders %>% ggplot(aes(population, total,label= abb)) +
  geom_label(color="blue")

#-----------------11---------------------
#We are now going to add color to represent the region
murders %>% ggplot(aes(population, total, label = abb, color=region)) +
  geom_label()

#seam as
murders %>% ggplot(aes(population, total, label = abb)) +
  geom_label(aes(color=region))

#-----------------13---------------------
#Now we are going to change the axes to log scales to account for the fact that the population distribution 
#is skewed. Let's start by defining an object p that holds the plot we have made up to now:

p <- murders %>% ggplot(aes(population, total, label = abb, color = region)) + geom_label()
p + scale_x_log10() + 
  scale_y_log10()

#-----------------14---------------------
# add a layer to add title to the next line
p + scale_x_log10() + 
  scale_y_log10() +
  ggtitle("Gun murder data")

#-----------------16---------------------
#We are now going to make a histogram of the heights so we will load the heights dataset
# define p here
p <- heights %>% ggplot(aes(x=height))

#-----------------17---------------------
#Now we are ready to add a layer to actually make the histogram
p <- heights %>% 
  ggplot(aes(height))
  
p + geom_histogram()
#-----------------18---------------------
#Use the binwidth argument to change the histogram made in the previous exercise to 
#use bins of size 1 inch
p <- heights %>% 
  ggplot(aes(height))
## add the geom_histogram layer but with the requested argument
p + geom_histogram(binwidth= 1)

#-----------------19---------------------
#Now instead of a histogram we are going to make a smooth density plot
heights %>% 
  ggplot(aes(height))+
  geom_density()

#-----------------20---------------------
#Now we are going to make density plots for males and females separately. 
#We can do this using the group argument within the aes mapping. 
heights %>% 
  ggplot(aes(height))+
  geom_density(aes(group=sex))

#same as
heights %>% 
  ggplot(aes(height,group=sex))+
  geom_density()

#-----------------21---------------------
# edit the next line to use color instead of group then add a density layer
heights %>% 
  ggplot(aes(height, group = sex, color=sex))+
  geom_density()

#-----------------22---------------------
#We can also assign groups using the fill argument. When using the geom_density geometry, 
#color creates a colored line for the smooth density plot while fill colors in the area under the curve
heights %>% 
  ggplot(aes(height, fill = sex)) + 
  geom_density(alpha=0.2) 
