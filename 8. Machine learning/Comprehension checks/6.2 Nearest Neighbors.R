#----------------------------Q1--------------------------------
#Previously, we used logistic regression to predict sex based on height. Now we are going to use knn to do 
#the same. Set the seed to 1, then use the caret package to partition the dslabs "heights" data into a 
#training and test set of equal size. Use the sapply function to perform knn with k values of seq(1, 101, 3) 
#and calculate F_1 scores.

library(caret)
library(dslabs)
data("heights")
head(heights)

y <- heights$sex
x <- heights$height
set.seed(1)
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)

test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

ks<- seq(1, 101, 3)

library(purrr)

F_1 <- map_df(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class")
  F_1 <- F_meas(data = y_hat, reference = factor(test_set$sex))
  tibble(F_1 = F_1)
}) %>% mutate(k=ks)

F_1 %>% ggplot(aes(x=k, y=F_1)) + geom_line()

ks[which.max(F_1$F_1)]
max(F_1$F_1)

#----------------------------Q2--------------------------------
#Next we will use the same gene expression example used in the Comprehension Check: Distance exercises. 
data("tissue_gene_expression")
head(tissue_gene_expression)
#Split the data into training and test sets, and report the accuracy you obtain.
#Try it for k = 1, 3, 5, 7, 9, 11. Set the seed to 1.

#We can see the class of the data
class(tissue_gene_expression$x)
class(tissue_gene_expression$y)

#We separate the data to create the partition
x<- tissue_gene_expression$x
y<- tissue_gene_expression$y

set.seed(1)
test_index<- createDataPartition(y, times=1, p=0.5, list=FALSE)


test_set_x <- x[test_index,]
train_set_x <- x[-test_index,]

test_set_y <- y[test_index]
train_set_y <- y[-test_index]

#We group the data that we partitioned
train_set <- list(y=train_set_y,x=train_set_x)
test_set <- list(y=test_set_y,x=test_set_x)

#Now we can get the accuracy for each k.
ks<- seq(1,11, by=2)

accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ x, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class")
  accuracy <- mean(test_set$y==y_hat)
  tibble(accuracy=accuracy)
}) %>% mutate(k=ks)

accuracy

#The answer offered by edx.
set.seed(1)
library(caret)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
train_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[train_index,], y[train_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[-train_index,]),
                   type = "class")
  mean(y_hat == y[-train_index])
})