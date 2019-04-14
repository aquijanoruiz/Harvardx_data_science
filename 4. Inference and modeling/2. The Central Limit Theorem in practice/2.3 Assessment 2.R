#----------------------------1------------------------------
# Write a function called `take_sample` that takes `p` and `N` as arguements and returns the average value of a randomly sampled population.
take_sample <- function(p,N) {
  sample<- sample(c(1,0),size=N,replace=TRUE,prob =c(p,1-p))
  mean(sample)
}

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Call the `take_sample` function to determine the sample average of `N` randomly selected people from a population containing a proportion of Democrats equal to `p`. Print this value to the console.
take_sample(p=p,N=N)

#----------------------------2------------------------------
# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Create an objected called `errors` that replicates subtracting the result of the `take_sample` 
#function from `p` for `B` replications
errors<- replicate(B,{
  p-take_sample(p=p,N=N)
})

# Calculate the mean of the errors. Print this value to the console.
mean(errors)

#----------------------------3------------------------------
#Use the hist function to plot a histogram of the values contained in the vector errors. 
#Which statement best describes the distribution of the errors?

format(mean(errors),scientific = FALSE) #to show a number without scientific notation

data.frame(errors=errors) %>%
  ggplot(aes(x=errors)) +
  geom_histogram()

#The errors are symmetrically distributed around 0.

#----------------------------4------------------------------
#What is the average size of the error if we define the size by taking the absolute value ∣p−X∣ ?

# Calculate the mean of the absolute value of each simulated error. Print this value to the console.

mean(abs(errors))

#----------------------------5------------------------------
#For mathematical reasons related to the central limit theorem, we actually use the standard deviation 
#of errors rather than the average of the absolute values.

sqrt(mean((errors)^2)) #the standard deviation is the square root of the average of the difference squared

#----------------------------6------------------------------
# Calculate the standard error of the parameter
sqrt((p*(1-p))/N)

#----------------------------7------------------------------
#Calculate the standard error of the estimate:
# Define `X` as a random sample of `N` voters with a probability of picking a Democrat ('1') equal to `p`
X<- sample(c(1,0),size=N,replace = TRUE,prob=c(p,1-p))

# Define `X_bar` as the average sampled proportion
X_bar<- mean(X)
X_bar
# Calculate the standard error of the estimate. Print the result to the console.

#We can get the same result in two different ways
sqrt(sum((X_bar-X)^2)/N)/sqrt(N) #using the "long" formula: the sd (square root of the average of the difference squared)
#divided by the square root of N

sqrt((X_bar*(1-X_bar))/N) #or the simplified form

#----------------------------8------------------------------
#The standard error estimates obtained from the Monte Carlo simulation, the theoretical prediction, and the estimate of the theoretical 
#prediction are all very close, which tells us that the theory is working.

sqrt((p*(1-p))/N) #thoretical standard error
sqrt((X_bar*(1-X_bar))/N)  #estimate of the theoretical standard error

#Create a plot of the largest standard error for N ranging from 100 to 5,000. Based on this plot, 
#how large does the sample size have to be to have a standard error of about 1%?

N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
data.frame(N=N,SE=se) %>%
  ggplot(aes(x=N,y=SE))+
  geom_line() #according to the graph we need a size of 2500 to have a standard error of less than 1%

#----------------------------11------------------------------
#Make a qq-plot of the errors you generated previously to see if they follow a normal distribution.

qqnorm(errors)
qqline(errors)

#----------------------------12------------------------------
# Calculate the probability that the estimated proportion of Democrats in the population is greater than 0.5. 
#Print this value to the console.

1-pnorm(0.5,mean=0.45,sd=sqrt((p*(1-p))/N),100) #we use the standard error as the standard deviation

#----------------------------13------------------------------
# Define `N` as the number of people polled
N <-100

# Define `X_hat` as the sample average
X_hat <- 0.51

# Define `se_hat` as the standard error of the sample average
se_hat<- sqrt(X_hat*(1-X_hat)/N)
se_hat

# Calculate the probability that the error is 0.01 or larger
pnorm(0.01, 0, se_hat, lower.tail = FALSE) + pnorm(-0.01, 0, se_hat, lower.tail = TRUE) #the argument lower.tail makes it 1 -pnorm()
1 - (pnorm(0.01,0,se_hat) - pnorm(-0.01,0,se_hat))