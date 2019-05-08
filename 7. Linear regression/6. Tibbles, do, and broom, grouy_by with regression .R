library(tidyverse)
library(Lahman)
data("Teams")

#------------------------preparing the data-------------------------
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1),
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)


#--------------------manually getting the slope of the regression-------------------------
#We want to see the relation betwween the bases on balls (BB) and the runs (R). So y= runs and x=bases on balls

#Since we didn’t know the lm function, to compute the regression line in each strata, we used the formula
#directly like this:

dat %>%
  group_by(HR) %>% #here we group by home runs 
  summarize(slope = cor(BB,R)*sd(R)/sd(BB)) #we get the coefficient for each group


#--------------------trying to the the slope of the regressiokn with the lm function---------------
dat %>%
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef

#we don’t get the result we want. The lm function ignores the group_by. This is expected because lm is not
#part of the tidyverse and does not know how to handle the outcome of a grouped tibble.

#--------------------the do function---------------
#The do functions serves as a bridge between R functions, such as lm, and the tidyverse. 
#The do function understands grouped tibbles and always returns a data frame.

dat %>%
  group_by(HR) %>%
  do(fit = lm(R ~ BB, data = .)) #Notice that we did in fact fit a regression line to each strata.
#And that column will contain the result of the lm( ) call. Therefore, the return table has a column 
#with lm( ) objects in the cells, which is not very useful.

dat %>%
  group_by(HR) %>%
  do(lm(R ~ BB, data = .)) #If we do not name the column we get an error.

#For a useful data frame to be constructed, the output of the function must be a data frame too. We could
#build a function that returns only what we want in the form of a data frame:

get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2], #this is what we want to print in the data frame
             se = summary(fit)$coefficient[2,2])
}

dat %>%
  group_by(HR) %>%
  do(get_slope(.)) #the dot means that we want to use the data frame from the pipe. We do not need to name the column 
#since we are already getting a data frame.

#This is important to know since if we name the output, then we get something we do not want, 
#a column containing a data frames:

dat %>%
  group_by(HR) %>%
  do(slope = get_slope(.))

get_lse <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             slope = fit$coefficients, #here we indicate we want all the coefficientes (different to the last function in we only specified beta_1)
             se = summary(fit)$coefficient[,2]) #here we ask for the standard error for each coefficient
}

dat %>%
  group_by(HR) %>%
  do(get_lse(.)) #here we get all the information from the lm function

#--------------------another example with the do function------------------
#You want to take the tibble dat, which we’ve been using in this video, and run the linear model R ~ BB for 
#each strata of HR. Then you want to add three new columns to your grouped tibble: the coefficient, standard error, 
#and p-value for the BB term in the model.

get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

dat %>% 
  group_by(HR) %>% 
  do(get_slope(.))

#---------------------------the broom package--------------------------
#The broom package has three main functions, all of which extract information from the object returned by
#lm and return it in a tidyverse friendly data frame. These functions are tidy, glance and augment. 

#---------------------------the tidy() function-----------------------
#The tidy function returns estimates and related information as a data frame:
library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit) #now the information is better organized

tidy(fit, conf.int = TRUE)#we can add confidence intervals

#---------------------------combining the functions tidy() and do()-----------------------
#Because the outcome is a data frame, we can immediately use it with do() to string together the commands
#that produce the table we are after.

dat %>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) #here we take the columns that we are interested in

#---------------------------creating a plot with the confidence intervals-----------------------
dat %>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>% #we use the do function to hangle with the group tibble, and we use the tidy to get the confidence intervals
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) + #we spedify the data like we did in the geom_smooth()
  geom_errorbar() + #this produces the plot we want
  geom_point()

#---------------------------the glance() function-----------------------
glance(fit) #this gets us other data from the regression


