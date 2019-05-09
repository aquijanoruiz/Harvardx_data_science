#-----------------------------outliers------------------------------
#Suppose we take measurements from two independent outcomes, X and Y , and we standardize the measurements.
#However, imagine we make a mistake and forget to standardize entry 23.

set.seed(1985)
x <- rnorm(100,100,1) #100 values with mean 100 and standard deviation 1
y <- rnorm(100,84,1) #100 values with mean 84 and standard deviation 1

#We forgot to standarize the 23th sample.
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])
print(x) #If we print x, we can see that the 23th value has not been standarized

qplot(x,y, alpha = 0.5) #This is how the data looks

cor(x,y) #Not surprisingly, the correlation is very high:

#But this is driven by the one outlier. If we remove this outlier, the correlation is greatly reduced to almost 0.
cor(x[-23], y[-23])

#---------------------------how to deal with outliers----------------------

#There is an alternative to the sample correlation for estimating the population correlation that is robust
#to outliers. It is called Spearman correlation. The idea is simple: compute the correlation on the ranks of
#the values.

qplot(rank(x), rank(y)) #Here we are ordering the data and illustrating it according to their rank.
#The outlier is no longer associated with a very large value and the correlation comes way down.

cor(rank(x), rank(y)) 
cor(x, y, method = "spearman") #We can alscom compute it like this

#---------------------------reverse causality------------------------

library(HistData)
data("GaltonFamilies")
GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight) %>%
  do(tidy(lm(father ~ son, data = .))) #The model fits the data very well. If we look at the mathematical formulation 
#of the model above, it could easily be incorrectly interpreted so as to suggest that the son being tall caused 
#the father to be tall.



