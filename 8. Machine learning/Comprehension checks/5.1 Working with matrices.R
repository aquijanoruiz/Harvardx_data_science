#-------------------------Q1----------------------
#Which line of code correctly creates a 100 by 10 matrix of randomly generated normal numbers and assigns it to x?
x <- matrix(rnorm(100*10), 100, 10)

#-------------------------Q2----------------------
#Write the line of code that would give you the specified information about the matrix x that you generated in q1.
dim(x)

nrow(x)

ncol(x)

#-------------------------Q3----------------------
#Which of the following lines of code would add the scalar 1 to row 1, the scalar 2 to row 2, and so on, for the matrix x?

x <- x + seq(nrow(x))

x <- matrix(rnorm(100*10), 100, 10)
1:nrow(x) #this is a vector whose values start in zero and end in 100 (the number of columns)
x <- sweep(x, 1, 1:nrow(x),"+")

#-------------------------Q4----------------------
#Which of the following lines of code would add the scalar 1 to column 1, the scalar 2 to column 2, and so on, for the matrix x?

x <- matrix(rnorm(100*10), 100, 10)
x <- sweep(x, 2, 1:ncol(x), FUN = "+")

x <- matrix(rnorm(100*10), 100, 10)

#-------------------------Q5----------------------

#Which code correctly computes the average of each row of x?
rowMeans(x)
colMeans(x)

#-------------------------Q6----------------------
#For each digit in the mnist training data, compute the proportion of pixels that are in the grey area, 
#defined as values between 50 and 205. (To visualize this, you can make a boxplot by digit class.)

library(dslabs)
mnist <- read_mnist()

x <- mnist$train$images
dim(x) #There are 6000 rows and 784 column. Each rows represents a different number. The day for each number
#is stored in the 784 columns.

index <- x >=50 & x<= 205
mean(index)

#An easier example:
mat <- matrix(1:15,3,5)

index_1<- mat>=4 & mat<= 9 
index_1 #we can print the index to see the TRUEs and FALSEs
mat[index_1] <- 20  #we converted to 20 all those values between 4 and 9


index_2<- mat==20 #we will convert the rest of the values into 21
mat[!index_2] <- 21

mat 
