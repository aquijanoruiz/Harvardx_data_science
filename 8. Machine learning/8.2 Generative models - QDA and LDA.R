#---------------------contents------------------------
#1. Definition of Quadratic Discriminant Analysis (QDA)
#2. Coputing Quadratic Discriminant Analysis (QDA)
#3. Definition of Linear discriminant analysis (LDA)
#4. Coputing Linear Discriminant Analysis (LDA)

#--------------------1. Definition of Quadratic Discriminant Analysis (QDA)-------------------------
#Quadratic Discriminant Analysis (QDA) is a version of Naive Bayes in which we assume that the distributions
#p X|Y =1(x) and p X|Y =0(x) are multivariate normal.

library(dslabs)
data("mnist_27")
head(mnist_27$train)

params_1 <- mnist_27$train %>%
  group_by(y) %>% #we sort by y (2, 7) to have a summary of the following variables for each number
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1= sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))

params_1

#Here we provide a visual way of showing the approach. We plot the data and use contour plots to give an
#idea of what the two estimated normal densities look like (we show the curve representing a region that
#includes 95% of the points).

mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color=y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm", lwd = 1.5) #computes normal confidence elipses. lwd is the width of the line

#--------------------2. Coputing Quadratic Discriminant Analysis (QDA)-------------------------
#We use the train function from the caret package to fit the model and obtain predictors.
library(caret)
train_qda <- train(y ~ ., method = "qda", data = mnist_27$train)

y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

#Although for the 2s it seems reasonable, for the 7s it does seem to be off. Notice the slight curvature 
#in the points for the 7s.

mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color=y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm") +
  facet_wrap(~y)

#--------------------3. Definition of Linear discriminant analysis (LDA)-------------------------
#A relatively simple solution to the problem of having too many parameters is to assume that the correlation
#structure is the same for all classes, which reduces the number of parameters we need to estimate.

#1. Look at the previous parameters we got to calculate the QDA
params_1

#2. Now we calculate new parameters considering the correlation is the same for all clases.

params_2 <- params_1 %>% mutate(sd_1 = mean(sd_1), sd_2=mean(sd_2), r=mean(r))
params_2 #we can see here that sd_1 (sd of x_1), sd_2 (sd of x_2) and the correlation is the same for both 2 and 7

#---------------------4. Coputing Linear Discriminant Analysis (LDA)---------------------------
train_lda <- train(y ~ .,
                   method = "lda",
                   data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]
