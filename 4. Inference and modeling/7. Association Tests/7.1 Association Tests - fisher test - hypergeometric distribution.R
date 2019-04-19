rm(list=ls())
library(dplyr)
library(dslabs)

data("research_funding_rates")
head(research_funding_rates)

totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>% #summarizes for all the variables y by running the function fun(). In this case we are summing all the variables.
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

totals %>% summarize(percent_men = yes_men/(yes_men+no_men),
                     percent_women = yes_women/(yes_women+no_women))#we see that a larger percentage of men receive the award


#-----------------------Hypergeometric distribution-------------------------
#The probability of picking 3 is
#(4)  (4)
#(3)  (1)
#--------
#   (8)
#   (4)

#(4) 4 poured before      (4) 4 poured after
#(3) 3 guessed correctly  (1) 4 total cups drank - 3 guessed correctly
#------------------------------------------------------------------------
#                   (8) 4 poured before + 4 poured after
#                   (4) 
#Thus, the chance of observing a 3 or something more extreme, under the null hypothes is something
#more extreme, under the null hypothesis, is 0.24.

#---------------------------Fhisher's exact test---------------------------
tab <- matrix(data=c(3,1,1,3),nrow=2,ncol=2)

rownames(tab)<-c("Poured Before","Poured After")
colnames(tab)<-c("Guessed before","Guessed after")

tab
class(tab)
#These are referred to as a two-by-two table. For each of the four combinations one can get with a pair of
#binary variables, they show the observed counts for each occurrence. 

fisher.test(tab, alternative="greater") #here we get 0.24


