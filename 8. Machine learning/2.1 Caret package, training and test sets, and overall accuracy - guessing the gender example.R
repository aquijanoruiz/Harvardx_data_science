library(caret) #This is the package to do machine learning.
??createDataPartition #A standard way of generating the training and test sets is by randomly splitting the data into test and training.

#Arguments of the createDataPartition
#times: defines how many random samples of indexes to return.
#p: definea what proportion of the index represented
#list: is used to decide you want indexes to be returned as a list or not.

library(tidyverse)
library(dslabs)

data(heights)
y <- heights$sex
x <- heights$height

set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) #this randomizes the data and divides it into half (because we set p=0.5)

train_set <- heights [-test_index,]
test_set <- heights [test_index,]

#------------------------------overall accuracy---------------------------------
#The simplest way to evaluate the algorithm when the outcomes are categorical is simply by reporting the proportion of 
#cases that were correctly predicted in the test set.

#------------------------------a function to guess the outcome---------------------------------
y_hat <- sample(c("Male","Female"), length(test_index),replace=TRUE) #we create a list equal to the length of the train_set list with random genders
y_hat

#In machine learning applications, it is useful to use factors to represent the categorical outcomes.

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$sex)) #the levels are the order in which you want the factors to be
str(y_hat) #we can check this using the str function. https://www.youtube.com/watch?v=xkRBfy8_2MU

#--------------------------------guessing the outcome--------------------------
y_hat #This is our prediction
test_set$sex #This is the real data
mean(y_hat == test_set$sex) #we have predicted 49.71% of the data correctly. We are guessing!


#----------------------------another function to guess the outcome------------------------
#Can we do better? Exploratory data analysis suggests we can because, on average, males are slightly taller than females:
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

#Predict male if height is within two standard deviations from the average male.

y_hat <- ifelse(x>62, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))

mean(y == y_hat) #the accuracy goes up to almost 80%

#-------------------------------trying different cutoffs---------------------------
cutoff <- seq(61, 70) #we create a sequence

accuracy <- map_dbl(cutoff, function(x){ #this is just like the apply function, but this one is used in programming
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

#we can plot the different cutoffs:
data.frame(accuracy=accuracy,cutoff=cutoff) %>%
  ggplot(aes(x=cutoff,y=accuracy)) +
  geom_point() +
  geom_line()

max(accuracy) #this is the highest accuracy level
best_cutoff <- cutoff[which.max(accuracy)] #this is the best cutoff

#---------------------------using the best cutoff--------------------------
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))

y_hat <- factor(y_hat)

mean(y_hat == test_set$sex) #our model predicts the gender of 81% of the students

#-----------------------------overfitting--------------------------
#Evaluating an algorithm on the training set can lead to overfitting, which often results in dangerously over-optimistic
#assessments.By testing on a data that we did not train on, we know it is not due to overfitting.

#----------------------------the confusion matrix------------------
table(predicted = y_hat, actual = test_set$sex) #cross tabulation and table creation: here we can see how many we predicted correctly and how many we predicted incorrectly.

#We get that we get a very high accuracy for males, 93%, but a very low accuracy for females, 42%.
test_set %>%
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarize(accuracy = mean(y_hat == sex)) #we can see the percentage of right predictions.

#---------------------------prevalence-----------------------------
#These heights were collected from three data science courses, two of which had more males enrolled.
prev <- mean(y == "Male") #this is the "prevalence" how many males there are in the population

#So when computing overall accuracy, the high percentage of mistakes made for females is outweighted 
#by the gains in correct calls for men.

#If your training data is biased in some way, you are likely to develop an algorithm that are biased as well.
#The fact that we evaluated on a test set does not matter, because that test set was also derived from the original
#biased data set. This is one of the reasons we look at metrics other than overall accuracy when evaluating
#a machine learning algorithm.

#The caret function confusionMatrix computes all these metrics for us once we define what category ???positive??? is. 
#The function expects factors as input, and the first level is considered the positive outcome or Y = 1. 
#In our example, Female is the first level because it comes before Male alphabetically.
library(e1071)
confusionMatrix(data = y_hat, reference = test_set$sex)

#sensitivity: we predict it is a female when it is a female
#specificity: we predict it is not a female when it is not a female. In other words, we predict it is a male when it is a male
#prevalence: the porportion of female

#We can see that the high overall accuracy is possible despite relatively low sensitivity.

#---------------------F1 score------------------------
#Because specificity and sensitivity are rates, it is more appropriate to compute the harmonic average of specificity
#and sensitivity.

#             2 ?? precision * recall
# F1 score=   ------------------------
#               precision + recall

#We define beta to represent how much more important sensitivity is compared to specificity, and consider 
#a weighted harmonic average using  this formula. 

#                      1
#--------------------------------------------
#  ??^2            1     +      1           1
#--------- * ---------    ---------- * ---------
# 1 + ??^2     recall        1 + ??^2     precision

#The F_meas function in the caret package computes this summary with beta defaulting to 1.

cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex)) #we compare the predicted data (y_hat) with the real data (train_set$sex) and we let the function calculate the F1 score
})

F_1

data.frame(F_1=F_1,cutoff=cutoff) %>%
  ggplot(aes(x=cutoff,y=F_1)) +
  geom_point() +
  geom_line()

cutoff[which.max(F_1)] #This maximum is achieved when we use this cutoff.

#---------------------------the sensitivity ans specificity function---------------------
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))

#we can see all the functions use the same arguments
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)
confusionMatrix(data = y_hat, reference = test_set$sex)

#-----------------------------adjusting to the prevalence--------------------

#Note that guessing male with higher probability would give us higher accuracy due to the bias in the sample. In our 
#sample of students, the majority are male.

p <- 0.9
n <- length(test_index)

y_hat_0.9 <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>%
  factor(levels = levels(test_set$sex))
mean(y_hat_0.9 == test_set$sex) 

#-----------------------------ROC----------------------

#The ROC curve plots sensitivity (TPR) versus 1 - specificity or the false positive rate (FPR). Here is an
#ROC curve for guessing sex but using different probabilities of guessing male.

#--------------GUESSING APPROACH
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <-
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>%
    factor(levels = c("Female", "Male")) #the first level is considered the positive outcome or Y=1. 
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>% mutate(probs=probs)
guessing

guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")
#we want the TPR to be high and the FPR to be low. (sensitivity to be high, 1-specificity to be low)

#--------------HEIGHT-BASED APPROACH
#We can construct an ROC curve for the height-based approach:

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female", "Male")) #the first level is considered the positive outcome or Y=1. 
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

#the first level is considered the positive outcome or Y=1. So if a student is predicted as a female and she is a female
#it is a true positive

#--------------------plotting the two approaches
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")
#we want the TPR to be high and the FPR to be low. (sensitivity to be high, 1-specificity to be low)
#the height-based approach seems to work better.

#When making ROC curves, it is often nice to add the cutoff used to the points.
library(ggrepel) #we need this package to add the label to each point
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female", "Male")) #the first level is considered the positive outcome or Y=1. 
  list(method = "Height cutoff",
       cutoff = x,
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01) #to add the label for each point


#-----------------precision-recall plot
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index),
                  replace = TRUE, prob=c(p, 1-p)) %>%
    factor(levels = c("Female", "Male")) #the first level is considered the positive outcome or Y=1. 
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female", "Male")) #the first level is considered the positive outcome or Y=1. 
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
#Remember that in this model a true positive is a women that was predicted to be a woman. 
#So if precission is low it means the a lot of men are predicted to be women, and
#if recall is low, there were many women who were not predicted to be women.
