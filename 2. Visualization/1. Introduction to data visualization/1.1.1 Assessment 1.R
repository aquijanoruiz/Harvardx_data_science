#The type of data we are working with will often influence the data visualization technique we use. 
#We will be working with two types of variables: categorical and numeric. Each can be divided into 
#two other groups: categorical can be ordinal or not, whereas numerical variables can be discrete or continuous.
library(dslabs)
data(heights)
names(heights)
head(heights)

#Keep in mind that discrete numeric data can be considered ordinal. Although this is technically true, 
#we usually reserve the term ordinal data for variables belonging to a small number of different groups, 
#with each group having many members.

#The height variable could be ordinal if, for example, we report a small number of values such as short, 
#medium, and tall. Let's explore how many unique values are used by the heights varialbe. 
#For this we can use the unique fuction:
x <- c(3, 3, 3, 3, 4, 4, 2)
unique(x)

x <- heights$height
length(unique(x))

#Frequency table using table function
#Use the table function to compute the frequencies of each unique height value. Because we are using the 
#resulting frequency table in a later exercise we want you to save the results into an object and call it tab.
tab<- table(x)
sum(tab==1) #to count the number of unique values
