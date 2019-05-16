library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
head(iris)
y <- iris$Species

#------------------------------Q2--------------------------------
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) #this randomizes the data and divides it into half
test <- iris[test_index,]
train <- iris[-test_index,]

#to know the min value for each feature
train[which.min(train$Sepal.Length),] %>% .[1,1]
train[which.min(train$Sepal.Width),] %>% .[1,2]
train[which.min(train$Petal.Length),] %>% .[1,3]
train[which.min(train$Petal.Width),] %>% .[1,4]

#this is a better summary
summary_train <- train %>% gather(key=feature,value=value,-Species) %>% 
  group_by(feature) %>%
  summarize(min=min(value),
            max=max(value),
            range=max-min,
            length_range=range/0.1,
            n=n())

#creating some functions for gathering the min and maximum values
from<- function(x,y) {
  x %>% filter(feature==y) %>% select(min) %>% pull()
}
to<- function(x,y) {
  x %>% filter(feature==y) %>% select(max) %>% pull()
}
from(summary_train,"Petal.Length")

#this can be gotten easily using the range function
range(train %>% select(x))

#testing each feature - Petal.Length
Petal.Length.seq <- seq(from=from(summary_train, "Petal.Length"), to=to(summary_train, "Petal.Length"), by=0.1)

Petal.Width.seq <- seq(from=from(summary_train, "Petal.Width"), to=to(summary_train, "Petal.Width"), by=0.1)

Sepal.Length.seq <- seq(from=from(summary_train, "Sepal.Length"), to=to(summary_train, "Sepal.Length"), by=0.1)

Sepal.Width.seq <- seq(from=from(summary_train, "Sepal.Width"), to=to(summary_train, "Sepal.Width"), by=0.1)

#-----------------------------------------------------------------
#First, we need to check the leves:
str(test$Species) #there are three levels: setosa, vesicolor, virginica

#Then, we need to understand the nature of this flowers. We can see that checking this data. 
#We can see that in general, the virginica flowers are bigger than the versicolor flowers. 
summary(iris %>% filter(Species=="versicolor"))
summary(iris %>% filter(Species=="virginica"))


#testing each feature ----------- Petal.Length
Petal.Length.cutoff <- map_df(Petal.Length.seq, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica","versicolor") %>%
    factor(levels = c("setosa","versicolor", "virginica"))
  list(method = "Petal.Length.cutoff",
       accuracy=mean(train$Species==y_hat))
}) %>% 
  mutate(cutoff=Petal.Length.seq)
summary(Petal.Length.cutoff)

#testing each feature ----------- Petal.Width
Petal.Width.cutoff <- map_df(Petal.Width.seq, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica","versicolor") %>%
    factor(levels = c("setosa","versicolor", "virginica"))
  list(method = "Petal.Width.cutoff",
       accuracy=mean(train$Species==y_hat))
}) %>% 
  mutate(cutoff=Petal.Width.seq)

#testing each feature ----------- Sepal.Length
Sepal.Length.cutoff <- map_df(Sepal.Length.seq, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica","versicolor") %>%
    factor(levels = c("setosa","versicolor", "virginica"))
  list(method = "Sepal.Lenght.cutoff",
       accuracy=mean(train$Species==y_hat))
}) %>% 
  mutate(cutoff=Sepal.Length.seq)

#testing each feature ----------- Sepal.Width
Sepal.Width.cutoff <- map_df(Sepal.Width.seq, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica","versicolor") %>%
    factor(levels = c("setosa","versicolor", "virginica"))
  list(method = "Sepal.Width.cutoff",
       accuracy=mean(train$Species==y_hat))
}) %>% 
  mutate(cutoff=Sepal.Width.seq)

#------------a summary of our findings in the train data ---------------
accuracy_train <- rbind(Petal.Length.cutoff,Petal.Width.cutoff,Sepal.Length.cutoff,Sepal.Width.cutoff) %>%
  group_by(method) %>%
  filter(accuracy==max(accuracy))

plot(iris,pch=21,bg=iris$Species)

#-------------a more efficient way to do it-------------
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],MARGIN=2,FUN=foo) #MARGIN is a variable defining how the function is applied:
#when MARGIN=1, it applies over rows, whereas with MARGIN=2, it works over columns. Note that when you use
#the construct MARGIN=c(1,2), it applies to both rows and columns
sapply(predictions,max)	

#------------Q3 accuracy in the test data---------------

accuracy <- function(test.data=x,feature=y,cutoff=z) {
  y_hat <- ifelse(x$y >z, "virginica","versicolor")
  mean(x$Species==y_hat)
}

accuracy(test.data = test,feature = "Petal.Length", cutoff = 4.7)
y_hat <- ifelse(test$Petal.Length >4.8, "virginica","versicolor")
mean(test$Species==y_hat)

#------------Q4 treating the test data like it was training data---------------
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5],MARGIN=2,FUN=foo) #MARGIN is a variable defining how the function is applied:
#when MARGIN=1, it applies over rows, whereas with MARGIN=2, it works over columns. Note that when you use
#the construct MARGIN=c(1,2), it applies to both rows and columns
sapply(predictions,max)	


#------------Q5 using two features---------------

y_hat <- ifelse(test$Petal.Length > 4.7 | test$Petal.Width > 1.5, 'virginica','versicolor')
mean(test$Species==y_hat)

