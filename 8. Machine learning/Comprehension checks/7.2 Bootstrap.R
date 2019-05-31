#------------------------------------Q1---------------------------------
#The createResample function can be used to create bootstrap samples. For example, we can create 10 bootstrap 
#samples for the mnist_27 dataset.

library(dslabs)
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

#How many times do 3, 4, and 7 appear in the first resampled index?
sum(indexes$Resample01==3)
sum(indexes$Resample01==4)
sum(indexes$Resample01==7)

#------------------------------------Q2---------------------------------
#We see that some numbers appear more than once and others appear no times. This has to be this way for each 
#dataset to be independent. Repeat the exercise for all the resampled indexes.

#What is the total number of times that 3 appears in all of the resampled indexes?

x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)

#------------------------------------Q3---------------------------------
#Generate a random dataset using the following code. 
y <- rnorm(100, 0, 1)
qnorm(0.75) #this gives you the z value for the 75th quantile
quantile(y, 0.75)[[1]] #we can get the 75th quantile of y with this formula

quant <- quantile(y, 0.75)
view(as.data.frame(quant))


#Set the seed to 1 and perform a Monte Carlo simulation with 10,000 repetitions, generating the random 
#dataset and estimating the 75th quantile each time. What is the expected value and standard error 
#of the 75th quantile?

set.seed(1)
B<- 10000
q_75 <- replicate(B, {
  y<- rnorm(100, 0, 1)
  quant <- quantile(y, 0.75)[[1]]
}) 
mean(q_75)
sd(q_75)


#------------------------------------Q4---------------------------------
#In practice, we can't run a Monte Carlo simulation.
set.seed(1)
y <- rnorm(100, 0, 1)

#Set the seed to 1 again after generating y and use 10 bootstrap samples to estimate the expected value 
#and standard error of the 75th quantile.

set.seed(1)
indexes <- createResample(y, 10)

dat <-c(quantile(y[indexes$Resample01],0.75)[[1]],
  quantile(y[indexes$Resample02],0.75)[[1]],
  quantile(y[indexes$Resample03],0.75)[[1]],
  quantile(y[indexes$Resample04],0.75)[[1]],
  quantile(y[indexes$Resample05],0.75)[[1]],
  quantile(y[indexes$Resample06],0.75)[[1]],
  quantile(y[indexes$Resample07],0.75)[[1]],
  quantile(y[indexes$Resample08],0.75)[[1]],
  quantile(y[indexes$Resample09],0.75)[[1]],
  quantile(y[indexes$Resample10],0.75)[[1]])

mean(dat)
sd(dat)

#como lo hace Rafa
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

#------------------------------------Q5---------------------------------
set.seed(1)
indexes <- createResample(y,10000)

q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star,0.75)
})
mean(q_75_star)
sd(q_75_star)