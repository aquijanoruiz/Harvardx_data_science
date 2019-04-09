#Coercion

#The concept of coercion is a very important one. Watching the video, 
#we learned that when an entry does not match what an R function is expecting, 
#R tries to guess what we meant before throwing an error. This might get confusing at times.

x<- c(1,"canada",3)
x
class(x) #Even though one and 3 were originally numbers, R has converted them to character
#We say that R coerced the data into a character string

x<- 1:5
y<- as.character(x) #For example, you can turn numbers into characters with the as.character
function.
y
class(y)
as.numeric(y) #to turn them back into number variables

#NA Not available
#when R fails to coerce something, it tries to coerce but it can't, we will get NA
x<- c(1,"canada",3)
as.numeric(x) #r will be able to get them to 1 and the 3 to the numeric values 1, 3
#but it won't know what to do with b.

#is.na function
#The is.na returns a logical vector that tells us which entries are NA. 
library(dslabs)
data(na_example)
# Checking the structure 
str(na_example)
# Find out the mean of the entire dataset 
mean(na_example)
# Use is.na to create a logical index ind that tells which entries are NA
ind<- is.na(na_example)
# Determine how many NA ind has using the sum function
sum(ind)

#another example of the is.na function
# Create the ind vector
library(dslabs)
data(na_example)
ind <- is.na(na_example)
ind
# We saw that this gives an NA
mean(na_example)
# Compute the average, for entries of na_example that are not NA 
mean(na_example[!ind])
length(na_example[!ind]) #this computes the number of values that are not NA
