#-------------------------Q1------------------------
#In the exercise in Q6 from Comprehension Check: Trees and Random Forests, we saw that changing nodesize to 50 
#and setting maxnodes to 25 yielded smoother results. Let's use the train function to help us pick what the 
#values of nodesize and maxnodes should be.

#From the caret description of methods, we see that we can't tune the maxnodes parameter or the nodesize 
#argument with randomForests. So we will use the __Rborist__ package and tune the minNode argument. 
#Use the train function to try values minNode <- seq(25, 100, 25). Set the seed to 1.

library(Rborist)
library(caret)
library(dslabs)

#First, we create the data
n <- 1000
sigma <- 0.25
set.seed(1)
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

modelLookup("Rborist") #we need to specify two parameters: predFixed, and minNode

minNode <- seq(25, 100, 25)

set.seed(1)
train_Rborist <- train( y ~ .,
       method = "Rborist", 
       tuneGrid = data.frame(predFixed = 1,
                             minNode = seq(25, 100, 25)),
       data = dat)

ggplot(train_Rborist, highlight = TRUE) #we see that the lowest RMSE is 50

#-------------------------Q2------------------------

#Part of the code to make a scatterplot along with the prediction from the best fitted model is provided below.

library(caret)
dat %>% 
  mutate(y_hat = predict(train_Rborist)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2) #we use geom_step to illustrate the y_hat. The 2 represents the color red.

#-------------------------Q3------------------------
#The exercises from Q3 onward take you through an analysis using the tissue_gene_expression dataset.

library(dslabs)
str(tissue_gene_expression)

#Use the rpart function to fit a classification tree to the tissue_gene_expression dataset. 
#Use the train function to estimate the accuracy. Try out cp values of seq(0, 0.1, 0.01). 
#Plot the accuracies to report the results of the best model. Set the seed to 1991.

modelLookup("rpart") #this model only has one tuning parameter

#CAVEAT: Sadly the method we used before of coverting the matrix into a data frame does not work here.
#I am still trying to find out why.
#------------------below is how we used to work the data------------------------
#1. First, we need to transform the matrix into a data frame. The data argument in the function train, needs
#to be a data frame, with the y and xs and not a matrix.

y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- as.tibble(x)
view(x)
row.names(x) <- NULL
datos <- cbind(y,x)
class(datos)

#2. Now we can run the train function and get the accuracy.
library(rpart)
fit_rpart <- train(y ~ .,
                   method = "rpart",
                   tuneGrid = data.frame(cp= seq(0, 0.1, 0.01)),
                   data = dat)
#---------------------------------------------------------------------------------

library(caret)
library(dslabs)
set.seed(1991)
data("tissue_gene_expression")

fit_rpart <- with(tissue_gene_expression, #we use the with function, which is like the attach function, so that we do not need to use the "data" argument
            train(x, y, method = "rpart", #we do not need to use the tilde, but instead x,y
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

ggplot(fit_rpart)

#-------------------------Q4------------------------
#Study the confusion matrix for the best fitting classification tree from the exercise in Q3.
#What do you observe happening for the placenta samples?

confusionMatrix(predict(fit),y) #Placenta samples are being classified somewhat evenly across tissues.

#-------------------------Q5------------------------
table(y) #Note that there are only 6 placentas in the dataset.
#By default, rpart requires 20 observations before splitting a node. That means that it is difficult to have
#a node in which placentas are the majority. Rerun the analysis you did in the exercise in Q3, but this time, 
#allow rpart to split any node by using the argument control = rpart.control(minsplit = 0). 
#Look at the confusion matrix again to determine whether the accuracy increases. Again, set the seed to 1991.

set.seed(1991)
data("tissue_gene_expression")

fit_rpart_minsplit0 <- with(tissue_gene_expression, #we use the with function, which is like the attach function, so that we do not need to use the "data" argument
                  train(x, y, method = "rpart", #we do not need to use the tilde, but instead x,y
                        tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
                        control = rpart.control(minsplit = 0))) #we permit rpart to split any node.

ggplot(fit_rpart_minsplit0, highlight = TRUE)
fit_rpart_minsplit0$bestTune
fit_rpart_minsplit0$results #here we see all the accuracies

confusionMatrix(fit_rpart_minsplit0) #we can see the accuracy of the best tune

#-------------------------Q6------------------------
#Plot the tree from the best fitting model of the analysis you ran in Q5.

fit_rpart_minsplit0$finalModel #her we can find the info for the decision tree
plot(fit_rpart_minsplit0$finalModel, margin = 0.1)
text(fit_rpart_minsplit0$finalModel, cex = 0.75)

#Another wat to do this
fit_0 <- rpart(y ~ ., data = datos, control = rpart.control(cp = 0.01, minsplit = 0))
plot(fit_0, margin = 0.1)
text(fit_0, cex = 0.75)

#-------------------------Q7------------------------
#We can see that with just seven genes, we are able to predict the tissue type. Now let's see if we can 
#predict the tissue type with even fewer genes using a Random Forest. Use the train function and the rf method
#to train a Random Forest. Try out values of mtry ranging from seq(50, 200, 25) (you can also explore other 
#values on your own). What mtry value maximizes accuracy? To permit small nodesize to grow as we did with 
#the classification trees, use the following argument: nodesize = 1.

library(randomForest)

set.seed(1991)
fit_rf <- with(tissue_gene_expression, #we use the with function, which is like the attach function, so that we do not need to use the "data" argument
                            train(x, y, method = "rf", #this is the random forest method
                                  tuneGrid = data.frame(mtry = seq(50, 200, 25)), 
                                  nodesize = 1)) #we use a small node size 
fit_rf

#-------------------------Q8------------------------
#Use the function varImp on the output of train and save it to an object called imp.

imp <- varImp(fit_rf)
imp


#-------------------------Q9------------------------
#The rpart model we ran above produced a tree that used just seven predictors. Extracting the predictor names 
#is not straightforward, but can be done. 

data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance))

