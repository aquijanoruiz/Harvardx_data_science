a<-2
class(a) #tells the type of an object
class(ls) 
library(dslabs)
data("murders")

#analyse the structure of a data frame
class(murders)
str(murders) #shows the structure of the data frame
names(murders) #shows the names of the columns
head(murders) #shows the first six lines of the data frame
murders$population #the $ is called the accessor and it's needed to access the variables
length(murders$population) #shows the number of vectors

#numeric vectors
class(murders$population) #this variable is numeric 

#character vectors
a<- 1
a
"a" #this is a character string (words)
class(murders$state) #this variable is a caracter, it isn't numeric

#logical vectors
z<- 3==2 
z #this is false because 3 is not equal to 2
class(z) #this is a logical vector

#factor vectors
class(murders$region) #this variable is a factor
#Factors are used for storing what is called "categorical data"
levels(murders$region) #shows the levels of the factor

#integer vectors
#we can create an integer by adding the letter L after a whole number. If you type
class(3L) #in the console, you see this is an integer and not a numeric. 
#For most practical purposes, integers and numerics are indistinguishable.
3L - 3 #the main difference is that integers occupy less space in the computer memory, 
#so for big computations using integers can have a substantial impact.
a <- seq(1, 10)
class(a) #this is an integer

#R returns a subset of the original data frame containing just this column
#This new object will be of class data.frame rather than a vector
#To access the column itself you need to use either the $ accessor or the double square brackets [[
a<-murders$abb
b<-murders[["abb"]]
identical(a,b)

#tables
x <- c("a", "a", "b", "b", "b", "c")
table(x) #takes a vector as input and returns the frequency of each unique element in the vector
table(murders$region) #shows the number of states per region
