library(HistData)
library(tidyverse)
data("GaltonFamilies")
set.seed(1983)
#----------------------------------preparing the data--------------------------------
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>% #sample n rows from the table. This randomly chooses one boy from each family.
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

#---------------------------------calculating the residual sum of squares--------------------
rss <- function(beta0, beta1){ 
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father) #this is the formula to calulate the residual (rss)
  return(sum(resid^2))
}

#So this is a three-dimensional plot with beta 1 and beta 2, and x and y and the RSS as a z.
#Here, we're just going to make a two-dimensional version by keeping beta 0 fixed at 25.
#So it will be a function of the RSS as a function of beta 1.

beta1 <- seq(0, 1, len=nrow(galton_heights)) #we create a list of possible beta1

results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25)) #we use the sapply formula to use the rss function. The specify the function's specific arguments after the comma.
results %>% ggplot(aes(beta1, rss)) + geom_line() +
  geom_line(aes(beta1, rss))

#We can see a clear minimum for beta 1 at around 0.65. So you could see how we would pick the least squares estimates.
#However, this minimum is for beta 1 when beta 0 is fixed at 25.

#---------------------------------the lm function---------------------------------
fit <- lm(son ~ father, data = galton_heights)
fit

#The object fit includes more information about the fit. We can use the function summary to extract more
#of this information:

summary(fit)

#Run a linear model in R predicting the number of runs per game based on the number of bases on balls and the number of home runs. 
#Remember to first limit your data to 1961-2001

library(Lahman)
Teams <- Teams %>%
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G)

lm<- summary(lm(R/G ~ BB + HR,data=Teams))
?Teams

#---------------------------------LSE are random variables---------------------------------

#We can run a Monte Carlo simulation in which we assume that the son and father height data that we have defines
#an entire population. And we're going to take random samples of size 50 and compute the regression slope coefficient for each one.

B<- 10000
N<- 50
lse <- replicate(B,{
  sample_n(galton_heights,N,replace=TRUE) %>%
    lm(son~father,data=.) %>% #this means we are using the data from the pipe
    .$coef
})
lse<- data.frame(beta_0=lse[1,],beta_1=lse[2,])

#we can plot their distribution
library(gridExtra)
library(dplyr)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black")
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black")
grid.arrange(p1, p2, ncol = 2)

#You can see that what we get from the Monte Carlo simulation is close to what we get from the regression
summary(fit) #we can see that here

#------------------------------------------------------------------------------------
#Here it is for one of our simulated data sets:
sample_n(galton_heights, N, replace = TRUE) %>%
  lm(son ~ father, data = .) %>%
  summary %>%
  .$coef

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1)) #You can see that the standard errors estimates reported 
#by the summary are close to the standard errors from the simulation.

#-----------------------Interpreting the t statistic-----------------------------------
#The t-statistic is not actually based on the central limit theorem, but rather on the assumption that
#the epsilons follow a normal distribution.

#NOTE:
#beta_0/sd(beta_0) and beta_1/sd(beta_1), follow a t-distribution with N − p degrees of freedom, with p the number
#of parameters in our model.

lse %>% summarize(cor(beta_0, beta_1)) #we can see that the correlation of the betas is -1

#-----------------------the ggplot geom_smoth function witht method=lm-----------------------------------
#If we assume the errors are normal, or have a large enough sample size, we can use theory to construct confidence intervals as well.

galton_heights %>% ggplot(aes(son, father)) +
  geom_point() +
  geom_smooth(method = "lm")

#we can get the same with this code
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father) #we combine columns from the two data sets

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + #to input the confidence intervals 
  geom_point(data = galton_heights, aes(x = father, y = son)) #we have to specify new aesthetics here

#-----------------------the predict() function-----------------------------------

fit <- galton_heights %>% lm(son ~ father, data = .)

galton_heights %>% 
  mutate(Y_hat=predict(fit)) %>% #the argument is the linear model
  ggplot(aes(father,Y_hat)) +
  geom_line()

#If we requested the standard errors and other information from which we can construct confidence 
#intervals, they can be obtained from the predict function.

Y_hat <- predict(fit, se.fit = TRUE)
Y_hat
names(Y_hat)

