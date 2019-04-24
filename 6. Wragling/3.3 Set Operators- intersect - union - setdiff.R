rm(list=ls())
library(tidyverse)
library(dslabs)
data(murders)
data(polls_us_election_2016)
tab <- left_join(murders, results_us_election_2016, by = "state") %>% select(-others)

tab_1 <- slice(murders, 1:6) %>% select(state, population) 
tab_2 <- results_us_election_2016 %>% filter(state%in%c("Alabama", "Alaska", "Arizona",
                                                        "California", "Connecticut", "Delaware")) %>% select(state,electoral_votes)

#---------------------------------------R-base intersect------------------------------------------------
#Intercect
#You can take intersections of vectors of any type, such as numeric
intersect(1:10, 6:15)

intersect(c("a","b","c"), c("b","c","d"))

#---------------------------------------Dplyr intersect-------------------------------------------------
#The dplyr package includes an intersect function that can be applied to tables with the same column
#names. This function returns the rows in common between two tables. To make sure we use the dplyr
#version of intersect rather than the base package version, we can use dplyr::intersect like this:

tab_1 <- tab[1:5,]
tab_2 <- tab[3:7,]
dplyr::intersect(tab_1, tab_2)

#---------------------------------------R-base union------------------------------------------------
#Similarly union takes the union of vectors.

union(c("a","b","c"), c("b","c","d"))
union(1:10, 6:15)
#---------------------------------------Dplyr union----------------------------------------------

#The dplyr package includes a version of union that combines all the rows of two tables with the same
#column names.

tab_1 <- tab[1:5,]
tab_2 <- tab[3:7,]
dplyr::union(tab_1, tab_2)

#---------------------------------------setdiff--------------------------------------------------
#The set difference between a first and second argument can be obtained with setdiff. Unlike intersect
#and union, this function is not symmetric.

setdiff(1:10, 6:15)
setdiff(6:15, 1:10)

#dplyr has a version for data frames
tab_1 <- tab[1:5,]
tab_2 <- tab[3:7,]
dplyr::setdiff(tab_1, tab_2)

#---------------------------------------setequal--------------------------------------------------
#Tells us if two sets are the same, regardless of order. 

setequal(1:5, 1:6)
setequal(1:5, 5:1)

#The dplyr version provides a useful message letting us know how the sets are different
dplyr::setequal(tab_1, tab_2)


