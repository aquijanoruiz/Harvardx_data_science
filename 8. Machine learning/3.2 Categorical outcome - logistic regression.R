#Regression for a Categorical Outcome
rm(list=ls())
library(dslabs)
data("heights")
library(caret)

y<- heights$height

set.seed(2)
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

#We define the outcome Y= 1 for females and Y=0 for males and X as the height. In this case, we are
#interested in the conditional probability of being female given the height.

#---------------------------------------------------------------
#What is the condition of probably of being female if you're 66 inches tall?
train_set %>% filter(round(height)==66) %>% 
  summarize(mean(sex=="Female")) #this is the amoun of female in the train set that are 66 inches tall

#Let's see for other heights
heights %>%
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()

#--------------------------creating a prediction to predict if you are female--------------------
#Let's convert the factors to 1s and 0s

train_set <- train_set %>%
  mutate(y=as.numeric(sex=="Female")) #we use the as.numeric function to transform the TRUEs into 1s
train_set

lm_fit <- train_set %>% lm(y~height, data=.)

#We predict female if the conditional probability is bigger than 50%.

p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor() #if y_hat is greater than 0.5 it will be preadicted as a female
confusionMatrix(y_hat, test_set$sex)
#We can see the accuracy and it is 0.7852

#The function Beta_0 + Beta_1 can take any value including negatives and values larger than 1. 
#In fact, in the previous example when we predicted the probability of being female, the probability (y_hat) 
#became negatinve at around 76 inches. The estimate that we obtained for our conditional probability
#using linear regression goes from negative 0.4 to 1.12.

heights %>%
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2]) #this function can illustrate the regression line

range(p_hat)
#-------------------------logistic regression----------------------------

#Logistic regression is an extension of linear regression that assures us the estimate of the 
#conditional probability is, in fact, between 0 and 1.

#In R, we can fit the logistic regression model with the function glm: generalized linear models. This function
#is more general than logistic regression so we need to specify the model we want through the family parameter:

glm_fit <- train_set %>%
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data=., family = "binomial") #we have to specify "binomial" in the family parameter to get a logictical regresssion

p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response") #When using predict with a glm object, 
#we have to specify that we want type="response" if we want the conditional probabilities, since the default 
#is to return the logistic transformed values.

y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex) #we get an accuracy of 0.7985

#-------------------------creating a graph of the logistic regression----------------------------

data.frame(x = seq(min(test_set$height), max(test_set$height))) %>%
  mutate(logistic = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x),
         regression = lm_fit$coef[1] + lm_fit$coef[2]*x) %>% #we use the coefficients to calulate y_hat (or the probability)
  gather(method, p_x, -x) %>% #we use the gather function to organize the data in a colum with the method applied (logistic or regression)
  ggplot(aes(x, p_x, color = method)) +
  geom_line() +
  geom_hline(yintercept = 0.5, lty = 5) #we create a line that intercepts 0.5

