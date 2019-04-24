rm(list=ls())
library(tidyverse)
library(dslabs)

#---------------------------------------bind_cols---------------------------------------------
#The dplyr function bind_cols binds two objects by making them columns in a tibble.
bind_cols(a = 1:3, b = 4:6) #we need to create a column name

data(murders)
data(polls_us_election_2016)
tab <- left_join(murders, results_us_election_2016, by = "state") %>% select(-others)

tab_1 <- tab[, 1:3] #we index by columns we use the empty [, because we do not want to index the rows
tab_2 <- tab[, 4:6]
tab_3 <- tab[, 7:8]
new_tab <- bind_cols(tab_1, tab_2, tab_3)
head(new_tab)
#---------------------------------------cbind------------------------------------------------
#It has the exact same functionality. An important difference is that cbind can create different
#types of objects, while bind_cols always produces a data frame.

#---------------------------------------cbind------------------------------------------------
#The bind_rows function is similar to bind_cols, but binds rows instead of columns:

tab_1 <- tab[1:2,]
tab_2 <- tab[3:4,]
bind_rows(tab_1, tab_2)