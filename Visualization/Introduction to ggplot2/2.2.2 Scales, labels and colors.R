#scale_x_continuous

library(tidyverse)
library(dslabs)
data(murders)

p<- murders %>% ggplot(aes(population/10^6,total, label=abb))
p+ geom_point(size=1) + #defines the size of the point
  geom_text(nudge_x = 0.05) + #moves the text slightly to the right
  scale_x_continuous(trans= "log10") + #uses the log10 transformation on the x and y axis 
  scale_y_continuous(trans= "log10")

#Note that because we're in the log scale now, the nudge must be made smaller

p+ geom_point(size=1) + #defines the size of the point
  geom_text(nudge_x = 0.05) + #moves the text slightly to the right
  scale_x_log10() + #same as we did before but more efficient
  scale_y_log10()

#Adding labels

p+ geom_point(size=1) + #defines the size of the point
  geom_text(nudge_x = 0.05) + #moves the text slightly to the right
  scale_x_log10() + #same as we did before but more efficient
  scale_y_log10() +
  xlab("Population in millions (log scale)") + #adds the labels
  ylab("Total of murders (log scale") +
  ggtitle("US Gun Murders in US2010") #adds the title name

#We can redefine p to be everything except the points layer

p<- murders %>% ggplot(aes(population/10^6,total, label=abb))+
  geom_text(nudge_x = 0.05) + #moves the text slightly to the right
  scale_x_log10() + #same as we did before but more efficient
  scale_y_log10() +
  xlab("Population in millions (log scale)") + #adds the labels
  ylab("Total of murders (log scale") +
  ggtitle("US Gun Murders in US2010") #adds the title name

p+ geom_point(size=1,color="blue")

#We want the colors to be associated with their geographical region
#To map each point to a color, we need to use aes, since this is a mapping

p+ geom_point(aes(col=region),size=1) #add different colors to different regions
#We a useful default behavior of ggplot. It has automatically added a legend that maps colors to region

#We want to add a line that represents the average murder rate for the entire country
#Compute the average rate for the entire country

r<- murders %>% 
  summarize(rate=sum(total)/sum(population)*10^6) %>% .$rate #to select data and carry out an operation

#To add a line: The default line for geom abline has slope 1 and intercept 0

p + geom_point(aes(col=region),size=1)+
  geom_abline(intercept = log10(r)) #we only need to design the intercept. The intercept here is log of r

#We have to change the line type from solid to dashed, change the color from black to grey, 
#and also, we need to draw the line before the points
#We redifine p
p<- p +
  geom_abline(intercept = log10(r),lty=2,color="darkgrey") + #lty equals 2 changes the line type
  geom_point(aes(col=region),size=1) 
print(p)

#We want region to be in capital letters
p<- p+
  scale_color_discrete(name="Region")
print(p)
