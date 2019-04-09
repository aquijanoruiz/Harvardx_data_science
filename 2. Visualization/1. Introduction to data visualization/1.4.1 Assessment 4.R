#-------------------1-----------------
#For this chapter, we will use height data collected by Francis Galton for his genetics studies. 
#Here we just use height of the children in the dataset

library(HistData)
data(Galton)
x <- Galton$child

#Compute the average and median of these data
mean(x)
median(x)

#-------------------2-----------------
#Now for the same data compute the standard deviation and the median absolute deviation (MAD)
sd(x)
mad(x)

#-------------------3-----------------
#Now suppose that suppose Galton made a mistake when entering the first value, 
#forgetting to use the decimal point. Report how many inches the average grow after this mistake
x_with_error <- x
x_with_error[1] <- x_with_error[1]*10

mean(x_with_error)-mean(x)

#-------------------4-----------------
#Now let's explore the effect this outlier has on the standard deviation
sd(x_with_error)-sd(x)

#-------------------5-----------------
#Now we are going to see how the median and MAD are much more resistant to outliers 
#For this reason we say that they are robust summaries.
median(x_with_error)-median(x)

#-------------------6-----------------
#Now let's see how the MAD is affected
mad(x_with_error)-mad(x)

#-------------------8-----------------
#To see how outliers can affect the average of a dataset, let's write a simple 
#function that takes the size of the outlier as input and returns the average

#Write a function called error_avg that takes a value k and returns the average 
#of the vector x after the first entry changed to k. Show the results for k=10000 and k=-10000

error_avg <- function(k){
  x <- Galton$child
  x[1]=k
  mean(x)
}
error_avg(10000)
error_avg(-10000)