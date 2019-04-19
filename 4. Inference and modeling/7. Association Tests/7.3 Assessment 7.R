rm(list=ls())
library(dplyr)
library(dslabs)

polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

cis <- polls %>%
  mutate(X_hat = (spread+1)/2,
         se= 2*sqrt(X_hat*(1-X_hat)/samplesize),
         lower = spread - qnorm(0.975)*se,
         upper = spread + qnorm(0.975)*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
cis <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

cis<- cis %>%
  mutate(hit=ifelse(actual_spread>=lower & actual_spread<= upper,1,0))

errors <- cis %>%
  mutate(error=spread-actual_spread, hit=(sign(actual_spread) == sign(spread))) #sign tells if the signs of the two are the same. If the two are positive it means Clinto wins. 
tail(errors)  

#----------------------------------------1-----------------------------------------
# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-

totals <- errors %>%
  filter(grade %in% c("A-","C-")) %>%
  group_by(grade,hit) %>%
  summarize(n=n())

totals
# Print the proportion of hits for grade A- polls to the console
totals$n[4]/(totals$n[3]+totals$n[4])
# Print the proportion of hits for grade C- polls to the console
totals$n[2]/(totals$n[1]+totals$n[2])


#----------------------------------------2-----------------------------------------
# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.

totals <- tibble(hit= c("FALSE","TRUE"),
                 "C-" =c(totals$n[1],totals$n[2]),
                 "A-" =c(totals$n[3],totals$n[4])) #we have to transform the table into something like this to run the chi squared
totals
chisq_test <- totals %>% select(-hit) %>% chisq.test()


# Print the p-value of the chi-squared test to the console
chisq_test. %>% .$p.value

#----------------------------------------3-----------------------------------------

# Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
odds_C<- totals$`C-`[2]/totals$`C-`[1]
odds_C #por cada encuesta incorrecta, hay 6.22 encuestas correctas


# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
odds_A<- totals$`A-`[2]/totals$`A-`[1]
odds_A #por cada encuesta incorrecta, hay 4.08 encuestas correctas

# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_A/odds_C

#----------------------------------------4-----------------------------------------
#In this hypothetical scenario, we get that the p-value for the difference in prediction success 
#if 0.0015 and the odds ratio describing the effect size of the performance of grade A- over 
#grade B- polls is 1.07.

#The p-value is below 0.05, but the odds ratio is very close to 1. There is not a scientifically 
#significant difference in performance.

Based on what we learned in the last section, which statement reflects the best interpretation of this result?