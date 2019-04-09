library(dslabs)
data(murders)
#Which state has the most population?
murders$state[which.max(murders$population)] #we index by selecting the state with the max population
max(murders$population)

height<- c(69,62,66,70,70,73,67,73,67,70)
height*2.54 #to convert into centimeters

murder_rate<- murders$total/murders$population*100000
murders$state[order(murder_rate,decreasing=TRUE)] #to know the states in order of number of murders for every 100000 people

#City temperatures
# Assign city names to `city` 
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
# Store temperature values in `temp`
temp <- c(35, 88, 42, 84, 81, 30)
# Convert temperature into Celsius and overwrite the original values of 'temp' with these Celsius values
temp<- 5/9*(temp-32)
# Create a data frame `city_temps` 
city_temps<- data.frame(city,temp)
city_temps

#What is the sum of the following equation: 1 + 1/2^2 + 1/3^2 + ... + 1/100^2?
#Thanks to Euler we know it should be close to ??2/6.
# Define an object `x` with the numbers 1 through 100
x<- 1:100
# Compute the sum 
sum(1/(x^2))

#Compute the per 100,000 murder rate for each state and store it in the object murder_rate.
# Load the data
# Store the per 100,000 murder rate for each state in murder_rate
murder_rate<- (murders$total/murders$population*100000)
# Calculate the average murder rate in the US 
mean(murder_rate)