#------------------------------------Q1----------------------------------
#Because x and y are completely independent, you should not be able to predict y using x with accuracy 
#greater than 0.5. Confirm this by running cross-validation using logistic regression to fit the model. 
#Because we have so many predictors, we selected a random sample x_subset.

library(caret)
library(tidyverse)
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p) #this has 1000 rows and 10000 columns
dim(x)

colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]
head(x_subset)

fit <- train(x_subset, y, method = "glm") #this is the cross validation
fit$results

#------------------------------------Q2----------------------------------
source("https://bioconductor.org/biocLite.R")
biocLite("genefilter")

library(genefilter)
tt <- colttests(x, y)

#Which code correctly creates a vector of the p-values called pvals?
pvals <- tt$p.value

#------------------------------------Q3----------------------------------
#Create an index ind with the column numbers of the predictors that were "statistically significantly" 
#associated with y. Use a p-value cutoff of 0.01 to define "statistically significantly."
#How many predictors survive this cutoff?

ind<- pvals <= 0.01
length(pvals)
length(pvals[ind])

#------------------------------------Q4----------------------------------
#Now re-run the cross-validation after redefinining x_subset to be the subset of x defined by the columns 
#showing "statistically significant" association with y. What is the accuracy now?

x_sta_sig <- x[,ind]
fit <- train(x_sta_sig, y, method = "glm")
fit$results

#------------------------------------Q5----------------------------------
#Re-run the cross-validation again, but this time using kNN. Try out the following grid k = seq(101, 301, 25) 
#of tuning parameters. Make a plot of the resulting accuracies.

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

#------------------------------------Q7----------------------------------
#Use the train function with kNN to select the best k for predicting tissue from gene expression on 
#the tissue_gene_expression dataset from dslabs. Try k = seq(1,7,2) for tuning parameters. 
#For this question, do not split the data into test and train sets (understand this can lead to overfitting, 
#but ignore this for now).

#What value of k results in the highest accuracy?

library(dslabs)
library(tidyverse)
data("tissue_gene_expression")  
head(tissue_gene_expression$x)

k <- seq(1,7,2)

y <- data.frame(y=tissue_gene_expression$y)
x <- as.data.frame(tissue_gene_expression$x)
dat <- cbind(y,x)


view(dat)

train_knn <- train(y ~., method = "knn",
                   data = dat,
                   tuneGrid = data.frame(k = seq(1,7,2)))
train_knn$results
train_knn$bestTune