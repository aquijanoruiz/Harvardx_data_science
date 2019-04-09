compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}

n <- 3
compute_s_n(n)

compute_s_n(100)

# Loops
# Note that we are performing exactly the same task over and over again,
# except we're changing n.

# General form
# for (i in range of values) {
#   operations that use i, 
#   which is changing across
#   the range of values
# }

for(i in 1:5){  #the sequence
  print(i) #the body
}

# Note that we like to use i in for loops. But it can be any variable. 
# We can use n, we can use a, anything we want.

i
# Also note that at the end of the loop, the value of i is the last value of the range.
# So if I type i after that for loop, I get back 5.

m <- 25
# Create an empty vector
s_n <- vector (length = m)
s_n

for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}

# So inside the loop, I'm calling the function compute_s_n(n).
# n is the value that's changing from 1 through 25.
# And as I evaluate that, I assign it to the vector in the nth entry.
# So we have defined the sum for values ranging from 1 through 25.

n <- 1:m
plot(n, s_n)
lines(n, n*(n+1)/2)
print(s_n)

# Other functions
# apply, sapply, tapply, mapply
# split, cut, quantile, reduce, identical, unique, and many others.

# Q7
# Write a function compute_s_n that with argument n and returns of 1 + 2^2 + ...+ n^2
compute_s_n <- function(n){
  x<- 1:n
  sum(x^2)
}

# Create a vector for storing results
s_n <- vector("numeric", 25)

# write a for-loop to store the results in s_n
n <- 25 # We can skip this step putting for(i in 1:25) below
for(i in 1:n){
  s_n[i] <- compute_s_n(i) 
}
s_n

#  Create the plot 
n <- 1:25 # We create anothe n to make the graphic
plot(n,s_n)

# Check that s_n is identical to the formula given in the instructions.
identical(s_n, n*(n+1)*(2*n+1)/6)
