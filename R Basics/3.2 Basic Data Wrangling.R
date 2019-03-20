#dplyr pakage
install.packages("dplyr")
library(dplyr)
library(dslabs)
data(murders)

#mutate
#changes the data table by adding a new column or changes an existing one
murders<- mutate(murders,rate=total/population*100000)
head(murders)
#he mutate function knows to look for these variables in the murders data frame
#rather than in the workspace. This makes the call much cleaner, because we
#don't have to keep writing murders$ over and over again

#filter
#filters the data by subsetting rows
filter(murders,rate<=0.71) #we get the five states where the condition is true
#We can remove rows using the != operator. For example to remove Florida we would do this:
no_florida <- filter(murders, state != "Florida")

#select
#subsets the data by selecting specific columns we want to work with
new_table <- select(murders,state,region,rate) #the first argument, murders, is an object
#It's the data table. But state, region, and rate are variable names from that table
filter(new_table,rate<=0.71)

#%>% the pipe operator
#we can also perform a series of operations for example, select this and then filter that
#by sending the results of one function to another function
murders %>% select(state,region,rate) %>% filter(rate<=0.71)
#we start with a data table, murders. We pipe that data into the select function
#that performs an operation

#EXERCISES
# Redefine murders to include a column named rank
# with the ranks of rate from highest to lowest
rank<-rank(-rate)
index<-order(rank(-rate))
murders<-mutate(murders,rank)
murders

# Filter to show the top 5 states with the highest murder rates
murders <- mutate(murders, rate = total/population * 100000, rank = rank(-rate))
filter(murders,rank<=5)

# Use filter to create a new data frame no_south
no_south<- filter(murders,region!="South")
# Use nrow() to calculate the number of rows
nrow(no_south)
no_south

# Create a new data frame called murders_nw with only the states from the northeast and the west
murders_nw<- filter(murders,region %in% c("Northeast","West") )
# Number of states (rows) in this category 
nrow(murders_nw)
murders_nw

#Suppose you want to live in the Northeast or West and want the murder rate to be less than 1
# add the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))
# Create a table, call it my_states, that satisfies both the conditions 
my_states<- filter(murders, rate<=1 & region %in% c("Northeast","West") )
my_states
# Use select to show only the state name, the murder rate and the rank
select(my_states,state,rate,rank)
#this can also be done like this by usint the pipe %>%
murders %>% filter(region %in% c("Northeast", "West") & rate < 1) %>% select(state,rate,rank)

#mutate, filter and select
my_states<- murders %>% mutate(rate =  total / population * 100000, rank = rank(-rate)) %>% 
  filter(region %in% c("Northeast","West") & rate<1) %>% select(state,rate,rank)
my_states
