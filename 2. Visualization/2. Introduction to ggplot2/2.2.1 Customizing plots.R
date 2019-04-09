?geom_point

library(dslabs)
data(murders)
murders %>% ggplot() +
  geom_point(aes(x=population/10^6,y=total))

#LAYERS
#In ggplot we can add layers to previously defined objects
p<- ggplot(data=murders)
p+geom_point(aes(population/10^6,total))

#Note also that the scales and labels are defined by default when adding this layer

#GEOM_LABEL and GEOM_TEXT
#The geom label and geom text functions permit us to add text to the plot

p+geom_point(aes(population/10^6,total))+
  geom_text(aes(population/10^6,total, label=abb))

#If we move the label outside of aes, we get an error.
p+geom_point(aes(population/10^6,total))+
  geom_text(aes(population/10^6,total)label=abb) #This is because abb is not found outside of aes

#TINKERING - SIZE
p+geom_point(aes(population/10^6,total),size=1)+
  geom_text(aes(population/10^6,total, label=abb))
#Note the size is not a mapping. It affects all the points the same. It was outside of aes

#TINKERING - NUDGE_X
p+geom_point(aes(population/10^6,total),size=1)+
  geom_text(aes(population/10^6,total, label=abb),nudge_x=1) #This moves the labes slightly to the right

#If we don't want to repeat population/10^6,total twice. We can use the global aesthetic mapping
args(ggplot)

#Defining a mapping in the ggplot function
p<- murders %>% ggplot(aes(population/10^6,total, label=abb)) #we create the layer and define the mapping
p + geom_point(size=1) +
  geom_text(nudge_x = 1.5)
#Note that we kept size and nudge x in the geom point and geom text functions respectively
#This is because those arguments are specific to those two geometries

#The local mappings override the global mappings
p<- murders %>% ggplot(aes(population/10^6,total, label=abb)) #This is the global aesthetic mapping
p+geom_point(size=3)+
  geom_text(aes(x=10,y=800,label="Hello There")) #This is the local aesthetic mapping
#the labels are no longer there. Only the label assigned by that new local aesthetic mappings