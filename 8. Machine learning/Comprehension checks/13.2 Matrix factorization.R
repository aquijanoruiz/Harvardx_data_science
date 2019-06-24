##Comprehension Check: Matrix Factorization

rm(list=ls())
#Simulating the data

set.seed(1987)
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, mu=rep(0, 3), Sigma) #we use the mvrnorm function produce samples of grades using a multivariate normal distribution
m <- m[order(rowMeans(m), decreasing = TRUE),] #we take the means of each row (average score of each student) and order them in descending order

set.seed(1987)
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3) 
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

#--------------------Q1-----------------------
#You can visualize the 24 test scores for the 100 students by plotting an image.

my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

#The students that test well are at the top of the image and there seem to be three groupings by subject.

#--------------------Q2-----------------------
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
  
#--------------------Q3-----------------------
s <- svd(y)
s

dim(y)
##These are the tree matrixes we got from the decomposition

dim(s$u) #this is an orthogonal matrix
dim(diag(s$d)) #this is a diagonal matrix. All the values are non-negative and in descending order
dim(t(s$v)) #this is an orthogonal matrix

#Orthogonality
t(s$u) %*% s$u #this equals the identity matrix

t(s$v) %*% s$v

#You can check that the SVD works by typing:
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd)) 

#Compute the sum of squares of the columns of Y and store them in ss_y. Then compute the sum of squares of columns of the 
#transformed YV and store them in ss_yv. Confirm that sum(ss_y) is equal to sum(ss_yv).

#https://www.youtube.com/watch?v=P5mlg91as1c watch this video for an introduction to svd

#What is the value of sum(ss_y) (and also the value of sum(ss_yv))?

##Sum of squares of Y

ss_y <- sapply(1:ncol(y), function(i){
  means <- colMeans(y)
  y <- cbind(y[,i] - means[i])
  print(y^2)
})

colSums(ss_y)

sum(ss_y)

##Sum of squares of YV

yv <- y %*% s$v

ss_yv <- sapply(1:ncol(yv), function(i){
  means <- colMeans(yv)
  yv <- cbind(yv[,i] - means[i])
  print(yv^2)
})

sum(ss_yv)

#--------------------Q4-----------------------
#We see that the total sum of squares is preserved. This is because  𝑉  is orthogonal. Now to start
#understanding how  𝑌𝑉  is useful, plot ss_y against the column number and then do the same for ss_yv.
library(tidyverse)
as.data.frame(ss_y) %>% gather(column, row) %>% 
  ggplot(aes(x=column,y=row)) +
  geom_point()

as.data.frame(ss_yv) %>% gather(column, row) %>% 
  ggplot(aes(x=column,y=row)) +
  geom_point()

#--------------------Q5-----------------------

ud <- s$u %*% diag(s$d)

ss_ud <- sapply(1:ncol(ud), function(i){
  means <- colMeans(ud)
  ud <- cbind(ud[,i] - means[i])
  print(ud^2)
})

colSums(ss_ud) #we know that the sum of squares of the columns of UD are the diagonal entries of D squared
diag(s$d)^2

#What else is equal to YV?
plot(sqrt(colSums(ss_yv)), s$d)
abline(0,1)

#--------------------Q6-----------------------
#So from the above we know that the sum of squares of the columns of Y (the total sum of squares) adds up 
#to the sum of s$d^2 and that the transformation YV gives us columns with sums of squares equal to s$d^2. 
#Now compute the percent of the total variability that is explained by just the first three columns of YV.

sum(ss_yv)
sum(ss_y)
#The two above should be equal to the one below

sum(s$d^2)

yv

#--------------------Q7-----------------------
#Use the sweep function to compute UD without constructing diag(s$d) or using matrix multiplication.

identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))

#--------------------Q8-----------------------
#Compute the average score for each student, plot it against U1 d1,1 and describe what you find.

rowMeans(y) #this is the average score of each student
ud[,1] #is almost identical to the average score for each student except for the sign.

qplot(x=rowMeans(y),y=-ud[,1]) #
plot(-s$u[,1]*s$d[1], rowMeans(y))

#--------------------Q9-----------------------

y %*% s$v[,1]
image(s$v)
my_image(s$v) #The first column is very close to being a constant, which implies that the first column of YV 
#is the sum of the rows of Y multiplied by some constant, and is thus proportional to an average.

##Comprehension Check: Clustering

#--------------------Q1-----------------------
#Load the tissue_gene_expression dataset. Remove the row means and compute the distance between
#each observation. Store the result in d.

library(dslabs)
data("tissue_gene_expression")
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))

#--------------------Q2-----------------------

#Make a hierarchical clustering plot and add the tissue types as labels.

h <- hclust(d)
plot(h)

#--------------------Q3-----------------------
#Run a k-means clustering on the data with K=7. Make a table comparing the identified clusters 
#to the actual tissue types. Run the algorithm several times to see how the answer changes.

k <- kmeans(tissue_gene_expression$x, centers = 7)
groups <- k$cluster
table(groups, tissue_gene_expression$y)


#--------------------Q4-----------------------
#Select the 50 most variable genes. Make sure the observations show up in the columns, that the predictor are 
#centered, and add a color bar to show the different tissue types. Hint: use the ColSideColors argument to 
#assign colors. Also, use col = RColorBrewer::brewer.pal(11, "RdBu") for a better use of colors.

library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(n=7, name="Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)

#brewer.pal function: wrapper of brewer.pal function from RColorBrewer.
#n: Number of different colors in the palette, minimum 3, maximum depending on palette.
#name: A palette name from the lists below.