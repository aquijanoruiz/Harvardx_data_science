library(dslabs)
library(dplyr)
library(ggplot2)
data(heights)

heights %>%
  ggplot(aes(x=sex,height)) +
  geom_point()

#---------------------jitter graph-----------------------
heights %>%
  ggplot(aes(sex, height)) + 
  geom_jitter(width = 0.1, alpha = 0.2) #alpha mades the dots somewhat transparent

#---------------------histogram--------------------------
#Keep the axes the same when comparing data across plots.
heights %>%
  ggplot(aes(x=height,y=..density..)) + #so that instead of counting it shows the density
  geom_histogram(binwidth=1,color="black")+ #color puts the lines between each pair of bars
  facet_grid(sex~.)

#---------------------jitter graph--------------------------
heights %>%
  ggplot(aes(x=sex,y=height)) +
  geom_jitter(width = 0.1, alpha=0.2)+
  geom_boxplot(coef=3)+ #
  ylab("Height in inches")

#coef.- determines how far the plot ``whiskers'' extend out from the box. If coef is positive, 
#the whiskers extend to the most extreme data point which is no more than coef times the interquartile
#coef from the box. A value of zero causes the whiskers to extend to the data extremes.

