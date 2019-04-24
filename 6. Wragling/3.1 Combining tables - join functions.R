rm(list=ls())
library(tidyverse)
library(dslabs)

#We load the 2 data sets we want to join
data(murders)
data(polls_us_election_2016)

#---------------------------------------Left_join---------------------------------------------
#We join them using left_join
tab <- left_join(murders, results_us_election_2016, by = "state") %>% select(-others)

library(ggrepel)
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

#---------------------------------------Left_join NAs------------------------------------------
tab_1 <- slice(murders, 1:6) %>% select(state, population) #slice filters the first 6 observations. This is a subsect of the first table.
tab_1
dim(tab_1) #tells you the dimension of the data.frame

tab_2 <- results_us_election_2016 %>% filter(state%in%c("Alabama", "Alaska", "Arizona",
                          "California", "Connecticut", "Delaware")) %>% select(state,electoral_votes)
tab_2
dim(tab_2)

left_join(tab_1,tab_2,by="state") #Note that NAs are added to two states in tab one that are not appearing in tab two.

#---------------------------------------Right_join---------------------------------------------
right_join(tab_1,tab_2,by="state") #If instead of a table like tab one we want one like tab two, we can use the right join.

#---------------------------------------inner_join---------------------------------------------
#If we want to keep only the rows that have information in both tables, we use inner_join.
inner_join(tab_1, tab_2, by = "state")


#---------------------------------------full_join---------------------------------------------

#If we want to keep all the rows and fill the missing parts with NAs, we can use full_join.
full_join(tab_1, tab_2, by = "state")

#---------------------------------------semi_join---------------------------------------------
#The semi_join function lets us keep the part of first table for which we have information in the second.

semi_join(tab_1, tab_2, by = "state")

#---------------------------------------anti_join---------------------------------------------
#It is the opposite of semi_join. It keeps the elements of the first table for which there
#is no information in the second
anti_join(tab_1, tab_2, by = "state")
