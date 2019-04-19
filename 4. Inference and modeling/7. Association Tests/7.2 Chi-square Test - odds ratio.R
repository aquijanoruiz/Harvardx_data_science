rm(list=ls())
library(dplyr)
library(dslabs)

data("research_funding_rates")
head(research_funding_rates)

#getting the overall funding rate
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>% #summarizes for all the variables y by running the function fun(). In this case we are summing all the variables.
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

funding_rate <- totals %>%
  summarize(percent_total =
              (yes_men + yes_women)/
              (yes_men + no_men +yes_women + no_women)) %>%
  pull(percent_total)
funding_rate #this is the overall funding rate

#So now the question is will we see a difference between men and women as big as the one 
#we see if funding was assigned at random using this rate?

#To compute a chi-square we need to compare the observed two_by_two table with the expected two by two table
#-------------------------------observed two_by_two table----------------------------------
#This is the table that actually shows what actually happened
two_by_two <- tibble(awarded= c("no","yes"),#tibble is like a data.frame function
                     men=c(totals$no_men,totals$yes_men),
                     women=c(totals$no_women,totals$yes_women))
two_by_two
class(two_by_two)

#The general idea of a chi-squared test is to compare this 2 by 2 table (the observed 2 by 2 table), 
#to what you expect to see at the overall.

#-------------------------------the expected two by two table--------------------------------
tibble(awarded = c("no", "yes"),
           men = (totals$no_men + totals$yes_men) *
             c(1 - funding_rate, funding_rate),
           women = (totals$no_women + totals$yes_women)*
             c(1 - funding_rate, funding_rate)) #Under the null hypothesis, this observation is a random variable.

#The chi-squared test tells us how likely it is to see a deviation like this, or larger, by chance.
#This test uses an asymptotic result, similar to the central limit theorem, related to the sums 
#of independent binary outcomes in a context like this.

#-------------------------------------chi-square test----------------------------------------
two_by_two

chisq_test <- two_by_two %>% #We take the observed two_by_two table
  select(-awarded) %>% #we take out the column awarded, and we leave the men and women columns
  chisq.test()

chisq_test$p.value
#p-value = 0.05091 this means that the probability of seeing a deviation like the one we see
#or bigger under the null that funding is assigned at random is 0.051.

#-------------------------------------the odds ratio----------------------------------------

#The odds of getting funded if you are a man is defined:
#Pr(Y = 1 | X = 1)/Pr(Y = 0 | X = 1)
#X = 1 if you are a male and 0 otherwise, and Y = 1 if you are funded and 0 otherwise.

odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1] / sum(two_by_two$men))
odds_men #this means the percentage of men who got funded divided by the percentage of men who didn't get funded

odds_men2 <- two_by_two$men[2]/two_by_two$men[1]
odds_men2 #this is the same as the previous formula, but this is the number of men who got funded divided by the number of men who didn't get funded

odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))
odds_women

#The odds ratio is the ratio for these two odds: how many times larger are the odds for men than for women?
odds_men / odds_women

#---------------relationship between odds ratio and p-value of chi-square-----------------------

#Note that the relationship between odds ratios and p-values is not one to one. It depends on the 
#sample size. So a very small p-value does not necessarily mean a very large odds ratio.

#Look at what happens to the p-value if we multiply our 2 by 2 table by 10. We multiply each cell by 10.

two_by_two %>%
  select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test() %>%
  .$p.value #when the sample size is 10 times bigger the p value goes down to zero

#However, THE ODDS RATIO IS UNCHANGED