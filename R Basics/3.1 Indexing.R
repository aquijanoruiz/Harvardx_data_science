library(dslabs)
data(murders)

murder_rate<- murders$total/murders$population*100000
#let's suppose you're moving from Italy where
#according to the ABC News report we showed earlier, the murder
#rate is only 0.71 per 100,000 people

index<- murder_rate<=0.71
index #The entries that are true are the cases for which the murder
#rate is smaller than or equal 0.71

murders$state[index] #telss the states whose murder rate is less or equal to 0,71
sum(index) #sums the cases that are true

#now we want the murder rate to be <= 1 and to be located in the west of the country
west<- murders$region=="West"
west
safe<- murder_rate<=1
safe #these are the two conditions

index<- safe & west #we define index as safe and west
murders$state[index] #these are the five states we can move to from Italy

#WHICH & MATCH & %in%

#which function
#gives us the entries of a logical vector that are true
x<- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
which(x)

#suppose we want to look up Massachusetts' murder rate
index <- which(murders$state=="Massachusetts")
index <- murders$state=="Massachusetts" #another way to write it
index #it's the only entry that is true
murder_rate[index] #to get the murder rate of Massachusetts


#match funtion
#looks for entries in a vector and returns the index needed to access them
index <- match(c("New York", "Florida", "Texas"),murders$state)
index
murders$state[index]
murder_rate[index]
data.frame(state=murders$state[index],rate=murder_rate[index])

#in%in
#If rather than an index, we want to know whether or not each element 
#of a first vector is in a second vector
x <- c("a","b","c","d","e")
y <- c("a","d","f")
y %in% x #this is because a, the letter a, is in the object x
#the letter d is in the object x but the letter f is not

c("Boston", "Dakota", "Washington") %in% murders$state
#you are not sure of Boston, Dakota, and Washington are states

# Store the 5 abbreviations in abbs. (remember that they are character vectors)
abbs <- c("MA", "ME", "MI", "MO", "MU") 

# Use the `which` command and `!` operator to find out which abbreviation are not actually
#part of the dataset and store in ind
ind<- which(!abbs %in% murders$abb)
# What are the entries of abbs that are not actual abbreviations
abbs[ind]