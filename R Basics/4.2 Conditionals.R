#CONDITIONALS
#if - else statemeent

#if(boolean conditon) {
#  expressions
#} else {
#  alternative expressions
#}

a<- 2
if(a!=0){ #! means the opposite, so != means not equal to
  print(1/a)
} else {
  print("No reciprocal for 0.")
}

a<- 0
if(a!=0){
  print(1/a)
} else {
  print("No reciprocal for 0.")
}

library(dslabs)
data("murders")
murder_rate <- murders$total/murders$population*100000

#to find if the state with the lowest murder rate has a rate lower than 0.5
ind <- which.min(murder_rate)
if(murder_rate[ind]<0.5){
  print(murders$state[ind])
} else {
  print("No state has murder rate that low")
}

#to find if the state with the lowest murder rate has a rate lower than 0.25
ind <- which.min(murder_rate)
if(murder_rate[ind]<0.25){
  print(murders$state[ind])
} else {
  print("No state has murder rate that low")
}

#ifelse function
a<- 0
ifelse(a>0, 1/a, NA)
#the first argument is the condition, the second is the result if the condition
#is true, and the third is the result if the condition is false

a<- c(0,1,2,-4,5)
result<-ifelse(a>0,1/a,NA)
data.frame(a=a,is_a_positive=a>0,answer1=1/a,answer2=NA,result=result)

# How to transfor NA data into 0
data("na_example")
na_example
is.na(na_example) # Tells if there are NAs in a vector
sum(is.na(na_example))
no_nas <- ifelse(is.na(na_example),0,na_example)
no_nas
sum(is.na(no_nas))

x <- c(2, 3, -999, 1, 4, 5, -999, 3, 2, 9)
ifelse(x == -999, NA, x)

#any function
z<- c(TRUE,TRUE,FALSE)
any(z) #gets true because at least one of them is true

z<- c(FALSE,FALSE,FALSE)
any(z) #gets fasle because none of them is true

#all fuction
z<- c(TRUE,TRUE,FALSE)
all(z) #gets false because not all of them are true

z<- c(TRUE,TRUE,TRUE)
all(z) #gets true because all of them are true

# Assignment
# Q1
x <- c(1,2,-3,4)
if(all(x>0)){
  print("All Positives")
} else{
  print("Not All Positives")
}

library(dslabs)
data(murders)
char_len <- nchar(murders$state)
char_len # The function nchar tells you how many characters long a character vector is.

# Some datasets use the number -999 to denote NA. A bad practice! 
# You can convert the -999 in a vector to NA using the following ifelse call:
x <- c(2, 3, -999, 1, 4, 5, -999, 3, 2, 9)
ifelse(x == -999, NA, x)

# Q3
# Assign the state abbreviation when the state name is longer than 8 characters 

new_names <- ifelse(nchar(murders$state)>8,murders$abb,murders$state)
new_names
