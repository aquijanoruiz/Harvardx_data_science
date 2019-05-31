#-----------------------prepare the data--------------------
#Create a dataset of samples from just cerebellum and hippocampus, two parts of the brain, 
#and a predictor matrix with 10 randomly selected columns using the following code:

library(dslabs)
library(caret)
data("tissue_gene_expression")

set.seed(1993) # use this line of code if you are using R 3.5 or earlier
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))

y <- droplevels(tissue_gene_expression$y[ind])
#if you compare the formula above with simply tipying:
tissue_gene_expression$y[ind] #the data looks the same, BUT if we look at the levels, we see that the unused 
#levels have been dropped (colon, endometrium, kidney, liver, placenta) by using the function dropleves

x <- tissue_gene_expression$x[ind, ]
dim(x) #there are 500 columns (variables)
x <- x[, sample(ncol(x), 10)] #we want to select a random sample of 10 of the 500 variables. For creating this sample, we used the set.seed function.

#-----------------------Q1--------------------------
#Use the train function to estimate the accuracy of LDA. For this question, use the entire tissue_gene_expression 
#dataset: do not split it into training and test sets (understand this can lead to overfitting).

#1. First, we need to transform the matrix into a data frame. The data argument in the function train, needs
#to be a data frame, with the y and xs and not a matrix.
as.data.frame(x)
dat <- cbind(y,as.data.frame(x))
row.names(dat) <- NULL #with this code we delete the row names left from the matrix
dat

#2. Now we can run the train function and get the accuracy.
train_lda<- train(y~., method="lda", data = dat)
train_lda$results

#-----------------------Q2--------------------------
#In this case, LDA fits two 10-dimensional normal distributions. Look at the fitted model by looking at 
#the finalModel component of the result of train. Notice there is a component called means that includes 
#the estimated means of both distributions. Plot the mean vectors against each other and determine which 
#predictors (genes) appear to be driving the algorithm.

means_lda <- train_lda$finalModel$means #we have to transform this matrix into a data.frame to plot it
means_lda <- t(means_lda) #first we transpose it (in order to plot the data)

means_lda <- as.data.frame(means_lda)
predictors <- row.names(means_lda)
means_lda <- cbind(predictors, means_lda)
row.names(means_lda) <- NULL

means_lda #this is the data we have to plot
means_lda %>% ggplot(aes(x=cerebellum,y=hippocampus)) +
  geom_point() +
  geom_text(aes(label=predictors)) +
  geom_abline()

#-----------------------Q3--------------------------
#Create a dataset of samples from just cerebellum and hippocampus, two parts of the brain, and a predictor 
#matrix with 10 randomly selected columns using the following code:

set.seed(1993) # use this line of code if you are using R 3.5 or earlier
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

#Use the train function to estimate the accuracy of QDA. For this question, use the entire tissue_gene_expression 
#dataset: do not split it into training and test sets (understand this can lead to overfitting).

#1. First, we need to transform the matrix into a data frame. The data argument in the function train, needs
#to be a data frame, with the y and xs and not a matrix.
as.data.frame(x)
dat <- cbind(y,as.data.frame(x))
row.names(dat) <- NULL #with this code we delete the row names left from the matrix
dat

#2. Now we can run the train function and get the accuracy.
train_qda<- train(y~., method="qda", data = dat)
train_lda$results

#-----------------------Q4--------------------------
#Which TWO genes drive the algorithm when using QDA instead of LDA?

means_qda <- train_qda$finalModel$means #we have to transform this matrix into a data.frame to plot it
means_qda <- t(means_qda) #first we transpose it (in order to plot the data)

means_qda <- as.data.frame(means_qda)
predictors <- row.names(means_qda)
means_qda <- cbind(predictors, means_qda)
row.names(means_qda) <- NULL

means_qda #this is the data we have to plot
means_qda %>% ggplot(aes(x=cerebellum,y=hippocampus)) +
  geom_point() +
  geom_text(aes(label=predictors)) +
  geom_abline()

#-----------------------Q5---------------------------
#One thing we saw in the previous plots is that the values of the predictors correlate in both groups: 
#some predictors are low in both groups and others high in both groups. The mean value of each predictor 
#found in colMeans(x) is not informative or useful for prediction and often for purposes of interpretation, 
#it is useful to center or scale each column. This can be achieved with the preProcess argument in train.

x #Recall the values of the predictors
colMeans(x)

#Re-run LDA with preProcess = "center". Note that accuracy does not change, but it is now easier to identify 
#the predictors that differ more between groups than based on the plot made in Q2.

train_lda <- train(y~., data = dat, method="lda", preProcess="center")
train_lda$results #Note that accuracy does not change

#Plot the means
means_lda <- train_lda$finalModel$means #we have to transform this matrix into a data.frame to plot it
means_lda <- t(means_lda) #first we transpose it (in order to plot the data)

means_lda <- as.data.frame(means_lda)
predictors <- row.names(means_lda)
means_lda <- cbind(predictors, means_lda)
row.names(means_lda) <- NULL

means_lda #this is the data we have to plot
means_lda %>% ggplot(aes(x=cerebellum,y=hippocampus)) +
  geom_point() +
  geom_text(aes(label=predictors)) +
  coord_flip()

#-----------------------Q6---------------------------
#Now we are going to increase the complexity of the challenge slightly. Repeat the LDA analysis from Q5 
#but using all tissue types. Use the following code to create your dataset:

set.seed(1993) # use this line of code if you are using R 3.5 or earlier
y_all <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

as.data.frame(x)
dat <- cbind(y_all,as.data.frame(x))
row.names(dat) <- NULL #with this code we delete the row names left from the matrix
dat

#2. Now we can run the train function and get the accuracy.
train_lda<- train(y_all~., method="lda", data = dat, preProcess="center")
train_lda$results
