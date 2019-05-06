library(HistData) #package with historical data with Galton's family data
library(tidyverse)
data("GaltonFamilies")
galton_heights <- GaltonFamilies %>% 
  filter(childNum == 1 &gender=="male") %>% 
  select(father, childHeight) %>%
  rename(son=childHeight)

galton_heights %>% 
  summarise(mean(father), sd(father), mean(son), sd(son))

galton_heights %>% ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) #the trend that the taller the father, the taller the son.

#Correlation

galton_heights %>% summarize(r=cor(father,son)) %>% pull(r) #we use pull to get the data as a vector

#In many cases, we do not observe data for the entire population of interest but rather for a random sample. As
#with the average and standard deviation, the sample correlation is the most commonly used estimate of the population correlation.

#let’s assume that the 179 pairs of fathers and sons is our entire population. A less fortunate geneticist 
#can only afford measurements from a random sample of 25 pairs. The sample correlation can be computed with:

#-------------------------------getting a sample using sample() and sample_n()-----------------------
R <- sample(galton_heights$father,size=25,replace=TRUE) #to get a sample from one variable
R <- sample_n(galton_heights,size=25,replace=TRUE) #to get a sample from the whole data frame
R %>% summarise(r=cor(father,son)) %>% pull(r) 

#R is a random variable. We can run a Monte Carlo simulation to see its distribution:

B<- 10000
N<- 25
R<- replicate(B, {
  sample_n(galton_heights,size=25,replace=TRUE) %>%
    summarize(r=cor(father,son)) %>%
    pull(r)
})

qplot(R,geom="histogram",binwidth=0.05,color=I("black")) 
mean(R) #the mean of the correlation is close to 0.5
sd(R) #the standard error is very high due to the range of values r can take

#In our example, N = 25 does not seem to be large enough to make the approximation a good one.
data.frame(R) %>%
  ggplot(aes(sample=R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))
