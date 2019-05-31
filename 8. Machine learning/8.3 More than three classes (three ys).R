#-------------------contents------------------
#1. Preparing the data
#2. How it would look like using the QDA model
#3. How it would look like using the LDA model
#4. How it would look like using the knn model

#--------------------------1. Preparing the data------------------------
if(!exists("mnist")) mnist <- read_mnist()

set.seed(3456)
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000) #we index a sample with 1, 2, 7

y <- mnist$train$labels[index_127]
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list = FALSE)

# get the quadrants
# temporary object to help figure out the quadrants
row_column <- expand.grid(row=1:28, col=1:28)
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)

# binarize the values. Above 200 is ink, below is no ink
x <- x > 200

# cbind proportion of pixels in upper right quadrant and
## proportion of pixels in lower right quadrant
x <- cbind(rowSums(x[ ,upper_left_ind])/rowSums(x), #This is x_1
           rowSums(x[ ,lower_right_ind])/rowSums(x)) #This is x_2

train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1],
                        x_2 = x[index_train,2])
test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1],
                       x_2 = x[-index_train,2])

#------------------2. How it would look like using the QDA model------------------------
#1. We plot the data.

train_set %>%
  ggplot(aes(x_1, x_2, color=y)) +
  geom_point()

#2. We use the caret package to train the QDA model.
train_qda <- train(y ~ ., method = "qda", data = train_set)

#Now we estimate three conditional probabilities (although they have to add to 1).
predict(train_qda, test_set, type = "prob") %>% head()
#For the first class we get 22% prob of being 1, 66% of being 2, and 0.12 of being 7.

#3. We see the confusion matrix and the accuaracy
confusionMatrix(predict(train_qda, test_set), test_set$y)$table
confusionMatrix(predict(train_qda, test_set), test_set$y)$overall["Accuracy"]

#------------------3. How it would look like using the LDA model------------------------
train_lda <- train(y ~ ., method = "lda", data = train_set)
confusionMatrix(predict(train_lda, test_set), test_set$y)$overall["Accuracy"]

#Note that one of the limitations of LDA here is due to the lack of fit of the normal assumption, 
#in particular for class 1.

train_set %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color=y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm")

#------------------4. How it would look like using the knn model------------------------
train_knn <- train(y ~ ., method = "knn", tuneGrid = data.frame(k = seq(15, 51, 2)),
                   data = train_set)
confusionMatrix(predict(train_knn, test_set), test_set$y)$overall["Accuracy"] #we do much better than with the other three