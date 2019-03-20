# the "function" function
# There is already a function that calculates the average of a vector. This is the mean function.
# mean(x)
avg <- function(x){ #it talkes x
  s <- sum(x) #computes the sum, assigns it to s
  n <- length(x) #computes the length, assigns it to n.
  s/n #this is what is returned
}

# Note that variables defined inside a function are not saved in the workspace.

x<- 1:100
avg(x)
identical(mean(x),avg(x))

# General form
#my_function <- function(x) {
#  operations that operate on x which
#  is defined by user of function 
#  value final is returned
#}

# Function with more variables
#my_function <- function(x,y,z) {
#  operations that operate on x,y,z which
#  is defined by user of function 
#  value final is returned
#}

# We design a function that can calculate the arithmetic or geometric average
avg <- function(x, arithmetic=TRUE){
  n <- length(x)
  ifelse(arithmetic,sum(x)/n,prod(x)^(1/n))
}

avg(x,arithmetic = TRUE) # to calculate the arithmetic mean
avg(x,arithmetic = FALSE) # to calculate the geometric mean

# Q4
# Define a function to determine the sum of integers from 1 to 5,000
# Create function called `sum_n`
sum_n <- function(n){
  x<- 1:n
  sum(x)
}
# Use the function to determine the sum of integers from 1 to 5000
sum_n(5000)

# Q5
# Create a function altman_plot that takes two arguments x and y 
# and plots y-x (on the y-axis) against x+y (on the x-axis).
# Create `altman_plot` 
altman_plot <- function(x,y){
  plot(x+y,y-x)
}
