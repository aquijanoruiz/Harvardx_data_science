#--------------------5---------------------
#Write a line of code that calculates the standard error se of a sample average when you 
#poll 25 people in the population. Generate a sequence of 100 proportions of Democrats p 
#that vary from 0 (no Democrats) to 1 (all Democrats).

# `N` represents the number of people polled
N <- 25

# Create a variable `p` that contains 100 proportions ranging from 0 to 1 using the `seq` function
p<- seq(from=0,to=1,length.out=100)

# Create a variable `se` that contains the standard error of each sample average
se<- (1-0)*sqrt(p*(1-p)/N)

# Plot `p` on the x-axis and `se` on the y-axis
plot(x=p,y=se)

data <- data.frame(p=p,se=se)
library(ggplot2)
library(dplyr)
data %>% ggplot(aes(x=p,y=se)) + geom_line()

#--------------------6---------------------
#Using the same code as in the previous exercise, create a for-loop that generates three plots 
#of p versus se when the sample sizes equal N=25, N=100, and N=1000.

# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)

#Write a for-loop that calculates the standard error `se` for every value of `p` for each of the 
#three samples sizes `N` in the vector `sample_sizes`. Plot the three graphs, using the `ylim` argument
#to standardize the y-axis across all three plots.

for(N in sample_sizes){
  se <- sqrt(p*(1-p)/N)
  plot(p, se, ylim = c(0,0.5/sqrt(25)))
}

#--------------------9---------------------
#So far we have said that the difference between the proportion of Democratic voters and Republican 
#voters is about 10% and that the standard error of this spread is about 0.2 when N=25. Select the 
#statement that explains why this sample size is sufficient or not.

# `N` represents the number of people polled
N <- 25

# `p` represents the proportion of Democratic voters
p <- 0.45

# Calculate the standard error of the spread. Print this value to the console.
2*sqrt(p*(1-p)/N)
