#---------------------contents------------------
#1. The parameters of the loess function
#2. Running the loess function

#-------------------1. The parameters of the loess function------------------
#Previously, we used kNN model to fit the data. However, we do see that the boundary is somewhat wiggly.
#This is because kNN, like the basic bin smoother,does not use a kernel. To improve this we could try loess. 
#By reading through the available models we see that we can use the gamLoess method. In the caret manual we see 
#that we need to install the gam package.

install.packages("gam")

modelLookup("gamLoess") #Then we see that we have two parameters to optimize:
#1. The degree --- If it is 1, it means local weighted regression. If it is 2, it means a parabola.
#2. The span --- This can take a value from 0 to 1 (the closer to 1 the smoother it becomes)

#To understand what loess does: https://www.youtube.com/watch?v=Vf7oJ6z2LCc

#-------------------2. Running the loess function------------------
grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1) #we use the function expand.grid to assing the parameters

train_loess <- train(y ~ .,
                     method = "gamLoess",
                     tuneGrid=grid, #here we use the specified parameters
                     data = mnist_27$train)

ggplot(train_loess, highlight = TRUE) #we can see which span best fits the model

#For the confucion matrix we need two parameters: the y_hat and the y
confusionMatrix(data = predict(train_loess, mnist_27$test), #this is the y_hat. We calculate using the predict function
                reference = mnist_27$test$y)$overall["Accuracy"] #this is the y from the test data set

