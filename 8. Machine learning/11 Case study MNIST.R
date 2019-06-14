#-----------------contents------------------
#1. The MNIST digits data set
#2. Selecting a sample from the data set
#3. Preprocesing
#4. Removing features with zero variance
#5. k-Nearest neighbors
#5.1 Running the code with a subset
#5.2 Running the code with the enerire data set
#6. Random Forest
#6.1 Running the code with a subset
#6.2 Running the code with the enerire data set
#7. Variable iportance
#8. Ensembles

#----------------1. The MNIST digits data set---------------
library(tidyverse)
library(dslabs)
mnist <- read_mnist() #we need internet to load this data set

names(mnist) #The dataset includes two components, a training set and test set.

#The trining set has two components: the labels (y) and the images (x)

dim(mnist$train$images) #Each of these components includes a matrix with features in the columns:

class(mnist$train$labels)
table(mnist$train$labels) #we can create a frequency table to see how many of each number there are

#----------------2. Selecting a sample from the data set---------------
#Because we want this example to run on a small laptop and in less than one hour, we will consider a subset
#of the dataset. We will sample 10,000 random rows from the training set and 1,000 random rows from the test set.

set.seed(1990)
index <- sample(nrow(mnist$train$images), 10000) #10000 random rows from the train set
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

set.seed(1990)
index <- sample(nrow(mnist$test$images), 1000) #1000 random rows from the test set
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

#----------------3. Preprocesing---------------
#In machine learning, we often transform predictors before running the machine algorithm. We also remove
#predictors that are clearly not useful. We call these steps preprocessing.

#We can run the nearZero function from the caret package to see that several features do not vary much
#from observation to observation. We can see that there are a large number of features with 0 variability:

library(matrixStats)
sds <- colSds(x) #we can see the variability between predictors
qplot(sds, bins = 256) #Many predictors do not vary at all. This is expected because there are parts of the 
#image that rarely contain writing (dark pixels).

#----------------4. Removing features with zero variance---------------
#1. We identify the predictors that are not useful
library(caret)
nzv <- nearZeroVar(x) #The caret packages includes a function that recommends features to be removed due to near zero variance:
nzv #we can use it as and index to remove useless data

image(matrix(1:784 %in% nzv, 28, 28)) #we can see the columns recomended for removal

col_index <- setdiff(1:ncol(x), nzv) #these are the columns we are going to keep
length(col_index)#so we end up keeping this number of columns

?setdiff #this function is the opposite of the function intersect()

#2. Now we are ready to fit some models. Before we start, we need to add column names to the feature matrices
#as these are required by caret

#When we use this command x <- mnist$train$images[index,], the column names are dropped. We need to name them again.
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- 1:ncol(mnist$train$images)

colnames(mnist$train$images)

#----------------5. k-Nearest neighbors---------------
#Let???s start with kNN. The first step is to optimize for k. Keep in mind that when we run the algorithm, we
#will have to compute a distance between each observation in the test set and each observation in the training
#set. These are a lot of computations. We will therefore use k-fold cross validation to improve speed.

library(caret)
control <- trainControl(method = "cv", number = 10, p = .9) #this is the number of bootstraps 10, each with 10% of the data
train_knn <- train(x[,col_index], y,
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
train_knn

#----------------5.1 Running the code with a subset---------------
#In general, it is a good idea to try a test run with a subset of the data to get an idea of timing before we
#start running code that might take hours to complete. 
n <- 1000
b <- 2
index <- sample(nrow(x), n) #we take a sample of 1000 from the 10000
control <- trainControl(method = "cv", number = b, p = .9) #now we only have two bootstraps with 10% of the data
train_knn <- train(x[index, col_index], y[index], #index (the row index) / col_index (the column index with the predictors that are useful)
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
train_knn #we see from this model that the k that maximizes accuracy is k=3.
#Now we can run the complete model using k=3 as the best tuning parameter.

#----------------5.2 Running the code with the enerire data set---------------
fit_knn <- knn3(x[, col_index], y, k = 3)

y_hat_knn <- predict(fit_knn, x_test[, col_index], type="class") 
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"] #The accuracy is almost 0.95!

cm$byClass[,1:2] #From the specificity and sensitivity, we also see that 8s are the
#hardest to detect and the most commonly incorrectly predicted digit is 7.

#----------------6. Random Forest---------------
#With random forest, computation time is a challenge. For each forest, we need to build hundreds of trees. 
#We also have several parameters we can tune. We use the random forest implementation in the Rborist package, 
#which is faster than the one in therandomForest package.

#----------------6.1 Running the code with a subset---------------
library(Rborist)

control <- trainControl(method="cv", number = 5, p = 0.8) #this is the bootstrap parameter. Because with random forest 
#the fitting is the slowest part of the procedurer, we will use only 5-fold cross validation.

modelLookup("Rborist") 
grid <- expand.grid(minNode = c(1) , predFixed = c(10, 15, 35)) #this are the tuning parameters

train_rf <- train(x[, col_index],
                  y,
                  method = "Rborist",
                  nTree = 50,
                  trControl = control,
                  tuneGrid = grid,
                  nSamp = 5000) #to compute on a smaller dataset, we will take a random sample of the observations 
                  #when constructing each tree. We can change this number with the nSamp argument.


ggplot(train_rf)
train_rf$bestTune #we can see that the parameter predFixed 10 is the one that maximizes the accuracy. 
#Now that we have optimized our tree, we are ready to fit our final model.

#----------------6.2 Running the code with the enerire data set---------------

fit_rf <- Rborist(x[, col_index], y,
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode, #this is the minNode = 1
                  predFixed = train_rf$bestTune$predFixed) #this is the predFixed = 10 (we know these are the values that maximize the model from the previos algorithm)

y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"] #this is the accuracy using the entire data set

#----------------7. Variable iportance---------------
#Unfortunately, the Rborist implementation of random forest does not yet support importance calculations,
#so we demonstrate with a quick fit using the randomForest package.

library(randomForest)
rf <- randomForest(x, y, ntree = 50)

imp <- importance(rf) #the importance function computes the importance of each feature.

image(matrix(imp, 28, 28)) #we illustrate the importance of each of the 784 pixels (predictors): 28 x 28

#----------------8. Ensembles---------------
#In machine learning, one can usually greatly improve the final results by combining the results of different algorithms.
p_rf <- predict(fit_rf, x_test[,col_index])$census
p_rf<- p_rf / rowSums(p_rf)
p_knn <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2 #we take the average of the predictions usinght the two models
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)$overall["Accuracy"]