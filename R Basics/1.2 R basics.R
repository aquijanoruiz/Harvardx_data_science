a<- 1
b<- 1
c<- -1
a #to print the value of a
print(a) #another way to print the value of a
ls() #to see all the values saved in your workspace
(-b+sqrt(b^2-4*a*c))/(2*a) #quadratic formula
(-b-sqrt(b^2-4*a*c))/(2*a) #quadratic formula
log(8) #computes the log of 8
log(a) #computes the log of a
exp(1) #euler's number to the 1
log(exp(1)) #this is an inverse funtion
log
help("log") #shows you the help window and gives you information about the function
?log #same as the help funtion
args(log) #shows the arguments of the funtion. If the argument has a "=" 
#it means it is not necessary
log(8,base=2) #if no argument name is used, R assumes
#you're entering arguments in the order shown in the help file or by args
log(x=8,base=2)
log(8,2)
help("+")
?"+" #gives you information about the aritmetic operator
data() #shows you all the data sets included in R
CO2 #show one of the pre-built data objects
pi #other pre-built objects
Inf #other pre-built objects
solution_1<- (-b+sqrt(b^2-4*a*c))/(2*a) #we chan use _ instead of space to name a function
solution_2<- (-b-sqrt(b^2-4*a*c))/(2*a)
a<- 3
b<- 2
c<- -1 #we can change the name of the scripts

#Assessment
n<- 1000
n*(n+1)/2

n<-1000
x<- seq(1,n)
sum(x)
seq(1, 9, by = 2) #example

# Compute log to the base 10 (log10) of the sqrt of 100. Do not use variables.
log(sqrt(100),base=10)

#Which of the following will always return the numeric value stored in x?
x<- 10
log(exp(x))