codes<- c(380, 124, 818) #the concatenate funtion is used to create vectors
country<- c("italy","canada","egypt") #quotes are used to denote that the entries are characters rather than variables
#If quotes are not used, R looks for variables with those names, and in this case, will return an error

codes_2<- c(italy=380, canada=124, egypt=818) #we can use the names to connect the two. here we assign a name to each number
codes_2
class(codes_2)
codes_3<- c("italy"=380, "canada"=124, "egypt"=818) #there is no difference between this code and the previous one
#you can also add the quotes
identical(codes_2,codes_3)

names(codes)<- country #another way to put names to the caracters
codes
identical(codes,codes_2)
?names #use of the function names
names(x)
names(x) <- value #a character vector of up to the same length as x, or NULL
temp<- c(35,88,42,84,81,30) #another example
city<- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
names(temp)<- city

a<- seq(1,10) #another way to create vectors. these are sequence of numbers
b<- 1:10
identical(a,b)
c<- seq(1,10,2) #tells R to jump by 2. it can also be written like c<- seq(from=1,to=10,by=2) 
a <- seq(1, 10, length.out = 100) #the length.out argument divides the sequence in 100 characters
a

#[ ] are used to access elements of a vector
codes[2] #gets the second element of the vector which is canada
codes[c(1,3)] #we can get more than one entry by using a multi-entry vector as an index
codes[1:2] #we can get the first two elements using the sequence
codes["canada"] #to access the entry named canada
codes[c("egypt","italy")]