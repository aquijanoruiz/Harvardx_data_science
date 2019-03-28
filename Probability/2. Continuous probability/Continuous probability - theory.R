library(dslabs)
data(heights)

#FREQUENCY TABLE FOR CATEGORICAL DARA
#prop.table function
#to show a frequency table of the proportions of male and female in each group
prop.table(table(heights$sex))


#CDF FOR NUMERICAL DATA
#Comulative distribution function (CDF)
#Statistics textbooks teach us that a more useful way
#to define a distribution for numerical data
#is to define a function that reports the proportion of the data
#below a value A for all possible values of A.

#F(a)< Pr(x<a) 
#We define a function f of a and make that equal to the proportion of values
#x less than or equal to a, which is represented with this Pr, meaning
#proportion or probability, and then in parentheses
#the event that we require, x less than a.

#ECDF Empirical CDFs.- obtained with data and not matematically