#Suppose you're in a classroom with 50 people. If we assume this is a randomly selected group,
#what is the chance that at least two people have the same birthday?

n<- 50
bdays <- sample (1:365, n, replace=TRUE) 

#To check if, in this particular set of 50 people we have at least two
#with the same birthday, we can use the function duplicated. For example:

duplicated(c(1,2,3,1,4,3,5)) #we get true's for the 1 and the 3 the second time they appear

any(duplicated(bdays)) #any: Given a set of logical vectors, is at least one of the values true?

#Monte Carlo
B <- 10000
results <- replicate(B,{
  n<- 50
  bdays <- sample (1:365, n, replace=TRUE) 
  any(duplicated(bdays))
})
mean(results)
