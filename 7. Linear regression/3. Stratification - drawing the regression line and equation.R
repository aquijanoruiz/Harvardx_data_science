library(HistData) #package with historical data with Galton's family data
library(tidyverse)

sum(galton_heights$father == 72) #we count how many 72 inches fathers there are

sum(galton_heights$father == 72.5) ##we count how many 72.5 inches fathers there are

#As there are few cases of fathers that are 72 inches tall, a practical way to improve these estimates of the 
#conditional expectations, is to define strata of with similar values of x. In our example, we can round father 
#heights to the nearest inch and assume that they are all 72 inches.

conditional_avg <- galton_heights %>% filter(round(father)==72) %>%
  summarize(avg=mean(son)) %>% pull(avg)
conditional_avg

#box plot
galton_heights %>% mutate(father_strata= factor(round(father))) %>%
  ggplot(aes(father_strata,son)) +
  geom_boxplot() +
  geom_point()

#taking the average and making a scattered plot
galton_heights %>% 
  mutate(father= factor(round(father))) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

#---------------------------drwawing the regression line STANDARIZING THE VARIABLES-----------------------
#Let's put a line with the slope equal to the correlation

r<- galton_heights %>% summarize(r= cor(father,son)) %>% pull(r) #we first calculate the correlation

#we need to use the geom_abline function to add the regression line
galton_heights %>% 
  mutate(father= round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  mutate(z_father=scale(father), z_son=scale(son_conditional_avg)) %>% #here we standarize the sample using the scale function
  ggplot(aes(z_father, z_son)) +
  geom_point() +
  geom_abline(intercept=0,slope=r)

#------------------drwawing the regression line WITHOUT STANDARIZING THE VARIABLES-----------------------

mu_x <- mean(galton_heights$father) #we are going to use the fathers' height to predict the sons' height
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r<- cor(galton_heights$father,galton_heights$son)

# y = b + m*x
# m = r * s_y/s_x
# b = y - m*x

m <- r * s_y / s_x #this is the slope
b <- mu_y - m*mu_x #this is the intercept
#so the regression equation is y= 35.71 + 0.50x

#Now we can draw the plot:
galton_heights %>%
  ggplot(aes(father,son)) +
  geom_point(alpha=0.5) + #remember that alpha makes the point a little transparent
  geom_abline(intercept = b, slope = m)

#------------------drwawing the regression line STANDARIZING THE VARIABLES-----------------------

galton_heights %>%
  ggplot(aes(scale(father), scale(son))) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r) #in the standarized version the intercept is 0 and the slope is rho (the correlation coefficient)

#When we standardize variables, both x and y will have a mean of zero and a standard deviation of one. 
#When you substitute this into the formula for the regression line, the terms cancel out until we have 
#the following equation: y[i] = rho*x[i]


galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>% #we standarized using the (x-mu)/sd
  filter(z_father %in% -2:2) %>% #we take out the very tall and the very short
  ggplot() +
  stat_qq(aes(sample = son)) +  #this is the quantile-quantile plot. We use sample to specify the info. 
  facet_wrap( ~ z_father) 

#-----------------------Predicting the father's height based on the son's height-----------------
#It is important to know that this is not determined by computing the inverse function of what we just saw.
#We need to compute the expected value of x given y. This gives us another regression function altogether.

m <- r * s_x / s_y
b <- mu_x - m*mu_y

#the regression equation here is y=34 + 0.5y
#So in summary, it's important to remember that the regression line comes from computing expectations, 
#and these give you two different lines, depending on if you compute the expectation of y given x or x given y.