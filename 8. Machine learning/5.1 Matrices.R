#Matrix algebra
#----------------------preparation of the data--------------
rm(list=ls())
library(tidyverse)
library(dslabs)
mnist <- read_mnist() #it talkes a little to load because the data set is big

class(mnist$train$images) #these are the predictors. They are saved in a matrix, rather than a data frame

#We take the first 1000 predictors x and labels y.
x <- mnist$train$images[1:1000,] 
y <- mnist$train$labels[1:1000]

class(x)

#---------------------contents-------------------------
#1. Types of objects in matrix algebra
#2. The dimention of a matrix
#3. Creating a matrix
#4. Transposing a matrix
#5. The image function
#6. Rows and columns summaries
#7. The apply() function
#8. filtering columns based on summaries
#9. Indexing with matrices
#10. Binarizing the data
#11. Vectorization for matrices/ sweep() function
#12. Matrix algebra operations

#--------------1. types of objects in matrix algebra---------------
#scalar: A scalar is just one number (e.g. a = 1)

#vector: Vectors include several scalar entries.
length(x[,1]) #this vector contains 1000 rows and 1 column

#Matrix: A series of vectors of the same size joined together as columns.
x_1 <- 1:5 #x_1 and x_2 are vectors of 5
x_2 <- 6:10
cbind(x_1, x_2)

#--------------2. the dimention of a matrix---------------
dim(as.matrix(x_1)) #We see than this matrix has 5 rows and 1 column (this is just a vector)
dim(x) #We see that the matrix has 1000 rows and 784 columns

#--------------3. creating a matrix--------------
my_vector <- 1:15 #first, we create a vector with the data that will be in the matrix
mat <- matrix(data=my_vector, nrow=5, ncol=3) #we use the data, nrow, and ncol arguments to create the matrix
mat #we see that the data is filled in by colunns 

mat_t <- matrix(data=my_vector, nrow=3, ncol=5, byrow = TRUE) #we can use the byrow argument to fill in the data by rows.

#WARNING: The matrix function recycles values in the vector without warning. If the product of columns
#and rows does not match the length of the vector...
matrix(my_vector, 5, 5) #we see that data is recycled in the 4th and 5th columns

#--------------4. transposing a matrix--------------
mat #the dimensions are 5x3
t(mat) #now the dimensions are 3x5
identical(t(mat), mat_t)

#--------------5. the image() function--------------
x[3,] #this contains the data for the 3rd entry

#Now we create a matrix with the data
grid <- matrix(x[3,], 28, 28) #why 28? because the vector x[3,] contains 784 values, to make a matrix with the 
#shape of a square. we must take the square root of 784=28. and assing each side of the square a length of 28.

image(x=1:28, y=1:28, z=grid) #x and y are the locations of grid lines at which the values in z are measured

image(1:28, 1:28, grid[, 28:1]) #to flip it back. Now we can see it is a 4

#--------------6. rows and columns summaries--------------
sums <- rowSums(x) 
avg <- rowMeans(x)

#to plot the data, we have to create a data frame with the data from the matrix
data_frame(labels = as.factor(y), row_averages = avg) %>%
  qplot(labels, row_averages, data = ., geom = "boxplot")

#--------------7. the apply() function--------------
#The apply function lets you apply any function, not just sum or mean, to a matrix. The first argument is 
#the matrix, the second is the dimension, 1 for rows, 2 for columns, and the third is the function.

avgs<- apply(X=x, MARGIN=1, FUN=mean) #we calculate the mean of the rows (because MARGIN=1)
sds<- apply(X=x, MARGIN=2, FUN=sd) #we calculate the mean of the columns (because MARGIN=2)

#--------------8. introduction to filtering a matrix--------------
library(matrixStats)
sds <- colSds(x) #this function comes in the matrixStats package. This function calculates the sd of the columns in the matrix
#this is the same thing we did with the formula apply(X=x, MARGIN=2, FUN=sd), but with another formula

qplot(sds, bins = "30", color = I("black")) #this creates a plot with 30 bins (this DOES NOT mean that the binwidth is 30)

qplot(sds, binwidth = 3, color = I("black")) #this creates a plot with a binwidth of 3


image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1]) #this is the variance plotted by location
[, 28:1] #we use this to flip it back

#--------------8.1 filtering columns based on summaries--------------
x[ ,c(351,352)] #here we filter columns
x[c(2,3),] #here we filter rows

dim(x)

index<- colSds(x) > 60
new_x <- x[ ,index] #we filter the data using an index

dim(new_x) #only the columns for which the standard deviation is above 60 are kept

#WARNING: if you select one column or one row, the result is no longer a matrix but a vector.
class(x[,1])
dim(x[1,])

class(x[ , 1, drop=FALSE]) #we can fix this using the argument drop=FALSE
dim(x[, 1, drop=FALSE])
#--------------9. Indexing with matrices-----------------
#Transforming a matrix into a vector:
mat <- matrix(1:15, 5, 3)
as.vector(mat)

#Example 1
mat[mat < 3] <- 0
mat

#Example 2
mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0 #we can also use logical operations
mat

#To see a histogram of all our predictor data, we can use:
qplot(as.vector(x), bins = 30, color = I("black"))

#If we think that values below, say, 50 are smudges, we can quickly make them zero using:
new_x <- x #we copy x into a new matrix
new_x[new_x < 50] <- 0 #we transform the values into 0

#--------------10. Binarizing the data-----------------

#The histogram above seems to suggest that this data is mostly binary. A pixel either has ink or does not.
#We can binarize the data using just matrix operations.

bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1
bin_x #now the bin_x matrix only has values of 0 and 1

grid_bin_x <- matrix(bin_x[3,], 28, 28) #we create a matrix of the 3rd entry of the binarized matrix
image(1:28,1:28,z=grid_bin_x[, 28:1])

#we can compare it with the original entry
grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid[, 28:1])

#-----------------11. Vectorization for matrices/ sweep() function-----------------
(x - rowMeans(x)) / rowSds(x) #we can scale each row of a matrix like this

#If you want to scale each column, we convert the columns to rows using the transpose t, proceed as above, 
#and then transpose back.
t(t(x) - colMeans(x)) #we transpose twice

#We can also use a function called sweep that works similarly to apply. It takes each entry of a vector and
#subtracts it from the corresponding row or column.
X_mean_0 <- sweep(x, 2, colMeans(x))

#The function sweep actually has another argument that lets you define the arithmetic operation. So to divide
#by the standard deviation, we do the following:
x_standardized <- sweep(x_mean_0, 2, colSds(x), FUN = "/")

#-----------------12. Matrix algebra operations-----------------

#1. Matrix multiplication is done with %*%. For example, the cross product is:
t(x) %*% x

#2. We can compute the cross product directly with the function:
crossprod(x)

#3. To compute the inverse of a function, we use solve. Here it is applied to the cross product:
solve(crossprod(x))

#4. The QR decomposition is readily available by using the qr function:
qr(x)