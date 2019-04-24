#--------------------------------strings------------------------------------------
s<- "10"" #We want to write ten inches, but this is not a character string, this is just an unclosed double quote"

s<- '10"' #CORRECT
s #if we just see it like this we don't see what the string actually looks like
cat(s) #we use the cat (Concatenate and Print) function to see what the string actually looks like

s<- "10'" #CORRECT we want to write 10 feet

#---------------------------\ backslash to scape the double quote-----------------
#backslash is used to scape the double quotes, it is used before the quote you want to cancell
s<- "5'10\""
cat(s)

s<- '5\'10"'
cat(s)

cat(" LeBron James is 6’8\" ") 

#-----------------------downloading data from wikipedia-------------------------------
#The tidyverse provides a web harvesting package called rvest. The first step using this package is to import
#the webpage into R.

rm(list=ls())
library(tidyverse)
library(rvest)
url <- "https://en.wikipedia.org/wiki/Gun_violence_in_the_United_States_by_state"
h <- read_html(url)

class(h) #Note that the entire Murders in the US Wikipedia webpage is now contained in h. 
#NOTE: YOU MUST USE EXPRESS VPN IN CHINA TO DOWNLOAD FROM WIKIPEDIA
html_text(h)

#to extract the tables from the website we use
tab <- h %>% html_nodes("table")
tab #you can see there are three tables. Look at the one you want

tab[[2]] #This is the table we want. But we need to convert this into a data frame

tab <- tab[[2]] %>% html_table
class(tab)
tab #now we have a data frame, but we still have to do much more to make this data base tity

#We need to change the names of the columns which are too long
murders_raw <- tab %>% setNames(c("state", "population", "total", "murder_rate")) 
murders_raw <- murders_raw %>% select(state)

#-------------------------------Preparing the data-------------------------------
#The data is not ready to be used yet

class(murders_raw$population) #it is a character when it should be numeric
class(murders_raw$total) #it is a character when it should be numeric. This is because of the commas.
murders_raw$population[1:3] #we can see the first three strings of this variable

as.numeric(murders_raw$population[1:3]) #we cannot use the usual coercion
#In general, string processing tasks can be divided into: detecting, locating, extracting,
#or replacing patterns in strings.

#-------------------------detecting the commas---------------------------------
library(stringr) #we need to use the string package to use its functions
str_detect(murders_raw$population, ",") #we can use str_detect to see if there are commas


commas <- function(x) any(str_detect(x, ",")) #we can create a function to do this task in all the columns
murders_raw %>% summarize_all(funs(commas))
commas(murders_raw$murder_rate)

#----------------replacing the commas with str_replace or parse_number------------------
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)
test_1 #We can then use mutate_all to apply this operation to each column, since it won’t affect the columns
#without commas.

#We can better use parse_number specifically to remove non-numeric characters before coercing:
test_2 <- parse_number(murders_raw$population) 
identical(test_1, test_2)

murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
head(murders_new)
