library(dslabs)
data(murders)

#sort function
sort(murders$total) #The function sort, sorts the vector in increasing order.
#So we can see the largest total number of gun murders by simply typing sort.

#order funtion
#It takes a vector and returns the indices that sort the vector parameter.
x<- c(31,4,15,62,65)
sort(x) #it puts them in order
index<- order(x) #gives us back the index that if used to index the vector, will sort it.
#This is the index that puts x in order.
x[index] #to index x by that index

murders$state[1:10] #we get the first ten elements in order as they appear in the rows of the data frame
murders$abb[1:10]

#we can now order the state names by their total murders
#by first obtaining the index that orders according to murder totals,
#and then indexing the state names or abbreviations using that index.
index<- order(murders$total) #this is the index that orders according to the murder totals
murders$abb[index] #here we index the state abbreviations with that index

#which.max function
max(murders$total)
i_max<- which.max(murders$total) #shows the index where the max value resides
i_max
murders$state[i_max]

#which.min function
min(murders$total)
i_min<- which.min(murders$total) #shows the index where the min value resides
i_min
murders$state[i_min]

#rank function
#it gives you a vector with the rank of the first entry, second entry, et cetera.
x<- c(31,4,15,62,65)
rank(x) #rank(x) gives you the ranks of x from lowest to highest
rank(-x) #rank(-x) gives you the ranks from highest to lowest

#SORT VS ORDER VS RANK
x<- c(31,4,15,62,65)
sort<- sort(x)
order<- order(x)
rank<- rank(x)
data.frame(original=x,sort=sort,order=order,rank=rank)

#data.frame function
#You can create a data frame using the data.frame function. Here is a quick example:
temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
city_temps <- data.frame(name = city, temperature = temp)
city_temps

#another example of data frame with sorting
library(dslabs)
data(murders)
# Define a variable states to be the state names from the murders data frame
states<- murders$state

# Define a variable ranks to determine the population size ranks 
ranks<- rank(murders$population)

# Define a variable ind to store the indexes needed to order the population values
ind<- order(murders$population)

# Create a data frame my_df with the state name and its rank and ordered from least populous to most 
my_df<- data.frame(state=states[ind],rank=ranks[ind])