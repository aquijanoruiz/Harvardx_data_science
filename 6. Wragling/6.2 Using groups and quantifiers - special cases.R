#-----------------------------------------------Preparation-----------------------------
rm(list=ls())
library(dslabs)
library(tidyverse)
library(stringr)
data(reported_heights)
class(reported_heights$height)

x <- as.numeric(reported_heights$height) #many NAs were introduced
sum(is.na(x))

reported_heights %>%
  mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>% head(n=10) #we can see where the peeople made mistake imputing the information

not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x)) #hace el verctor numérico, pero hay unos NAs
  ind <- !is.na(inches) & #nos quedamos con los que NO son NAs (es decir, los correctos)
    ((inches >= smallest & inches <= tallest) | #eliminamos los que son menores a 50 y mayores a 84
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}
problems <- reported_heights %>%
  filter(not_inches_or_cm(height)) %>%
  pull(height)
length(problems) #we still have 200 errors

converted <- problems %>%
  str_replace("feet|foot|ft", "'") %>% # convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>% # remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") # change format

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$" #this is close to the pattern we want
index <- str_detect(converted, pattern)
mean(index)

#--------------------------------------special cases------------------------------

#1 Many students measuring exactly 5 or 6 feet did not enter any inches. For example, 6' - our pattern requires that inches be included.
#2 Some students measuring exactly 5 or 6 feet entered just that number.
#3 Some of the inches were entered with decimal points. For example 5'7.5''. Our pattern only looks for two digits.
#4 Some entires have spaces at the end, for example 5 ' 9.
#5 Some entries are in meters and some of these use European decimals: 1.6, 1,7.
#6 Two students added cm.
#7 One student spelled out the numbers: Five foot eight inches.

#-----------------------------case 1-------------------------------
#1 Many students measuring exactly 5 or 6 feet did not enter any inches. For example, 6' - our pattern requires that inches be included.
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")

#-----------------------------case 2 and 4-------------------------
#Some students measuring exactly 5 or 6 feet entered just that number.
str_replace(s, "^([56])'?$", "\\1'0") #to include 5'

#-----------------------------case 3-------------------------------
#3 Some of the inches were entered with decimal points. For example 5'7.5''. Our pattern only looks for two digits.

#We can adapt our pattern, currently ^[4-7]\\s*'\\s*\\d{1,2}$ to permit a decimal at the end
pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$" # the period needs to be escaped since it is a special character 
#(it means any character except a line break). So we write the period like this \\.

#-----------------------------case 5-------------------------------

yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") #We need to write the period like this \\.

#-----------------------trimming (taking out spaces)-----------------------
#This is a very common problem, so there is a function specially made to correct this problem.
#This is used to eliminate the spaces at the start and end of a string.
str_trim(" 5 ' 9 ")

#-----------------------upper to lower case-----------------------
#there is a function to do this
s <- c("Five feet eight inches")
str_to_lower(s)

#----------------function to convert words into numbers-----------------------
words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}


#----------------putting everything into a function-----------------------

convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}

converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]

#-------------------------exercise--------------------------------

s <- c("5'10", "6'1\"", "5'8inches", "5'7.5")
tab <- data.frame(x = s)

extract(data = tab, col = x, into = c("feet", "inches", "decimal"), 
        regex = "(\\d)'(\\d{1,2})(\\.\\d+)?") #this does not the same as

str_extract(s, pattern= "(\\d)'(\\d{1,2})(\\.\\d+)?")
str_match(s, pattern= "(\\d)'(\\d{1,2})(\\.\\d+)?")  #but somethimg more similar to what this function does      
        