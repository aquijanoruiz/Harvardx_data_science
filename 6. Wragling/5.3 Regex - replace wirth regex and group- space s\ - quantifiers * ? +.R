#---------------------------------------------preparation----------------------------

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

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x)) #to avoid the warning messages we know the as.numeric will give us.
  ind <- is.na(inches) | inches < smallest | inches > tallest #we use | to include many logical commands
  ind
}

problems <- reported_heights %>%
  filter(not_inches(height)) %>%
  .$height
length(problems)

problems
#---------------------------------------search and replace with regex----------------------------
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems,pattern)) #we just get 14 examples with this pattern

problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern) #we see that our pattern only matches two of the five selected samples

str_subset(problems, "inches") #to see which students wrote the word "incles" instead of using quotes
str_subset(problems, "''") #to see that some people used the single quotes twice to represent inches


pattern <- "^[4-7]'\\d{1,2}$" #we take out the inches symbol " from pattern <- "^[4-7]'\\d{1,2}\"$"
#so in this case we want 5'10 not, 5'10" as we want to delete all the "

#---------------------------------------str_replace--------------------------------------------
problems %>%
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with '
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>%
  sum()

#-----------------------------------\s dealing with spaces--------------------------------------------
#Another problem we have are spaces. For example, our pattern does not match 5' 4" because there is a
#space between ' and 4 which our pattern does not permit. Spaces are characters and R does not ignore:
identical("Hi", "Hi ") 

#\s represents white space.
#To find patterns like 5' 4, we can change our pattern to:

pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$" #REMEMEBER TO ADD \\s (two backslash)
str_subset(problems, pattern_2)

#-----------------------------------quantifiers * ? + -------------------------------------------

#Even if there are several spaces, like in this example 5' 4, we still want it to match. 
#There is a quantifier for exactly this purpose.

#* means zero or more instances of the previous character.
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B") #we want to detect all the 1s after the A. BUT IT ALSO ADDS AB WHICH DOESN'T HAVE 1
str_detect(no, "A1*B") #1* permits 1 but does not require it

#For none or once, we can use ?, and for one or more, we can use +.

data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"), #permits but does not require
           nore_or_once = str_detect(yes, "A1?B"), #For none or once, we can use ?
           once_or_more = str_detect(yes, "A1+B")) #and for one or more, we can use +

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>%
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with '
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>%
  sum

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>%
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with '
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_view(pattern)

#------------------------------------exercises--------------------------------------
animals <- c("moose", "monkey", "meerkat", "mountain lion")
pattern <- "mo*" #This regex pattern looks for an “m” followed by nothing or more “o” characters.
pattern <- "mo?" #This regex pattern looks for an “m” followed by zero or one “o” characters. 
#This is true for all strings in the animal vector. Even though “moose” has two “o”s after the “m”, 
#it still matches the pattern.
str_detect(animals, pattern)

#--------------------------------groups () and extracting strings---------------------------
library(stringr)
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <- "^([4-7]),(\\d*)$" #[4-7] and \\d* are two groups so we encapsulate them in parenthesis
#Note that we encapsulate the part of the pattern that matches the parts we want to keep, 
#the parts we want to extract.

yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_detect(s, pattern_without_groups)

str_detect(s, pattern_with_groups)

#if we define groups we can use str_match to extract the values these group define

str_match(s, pattern_with_groups) #the second and third columns contains feet and inches respectively. 
#The first column is the part of the string matching the pattern. If no match occurred, we see an NA.
str_extract(s, pattern_with_groups) 

#Now we can understand the difference between the functions str_extract and str_match: str_extract
#extracts only strings that match a pattern, not the values defined by groups.

#The difference between detecting and stracting is that detecting is a logical function that returns 
#TRUE or FALSE, while extracting gets you the strings

#--------------------------------search and replacing using groups---------------------------
#The regex special character for the i-th group is \\i. So \\1 is the value extracted from the first group,
#\\2 the value from the second and so on.

pattern_with_groups <- "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2") #To replace , for ' we put the ' after \\1' and nothing after \\2

pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

#Let’s break this one down:
#• ˆ = start of the string
#• [4-7] = one digit, either 4,5,6 or 7
#• \\s* = none or more white space
#• [,\\.\\s+] = feet symbol is either ,, . or at least one space.
#• \\s* = none or more white space
#• \\d* = none or more digits
#• $ = end of the string

str_subset(problems, pattern_with_groups) %>% head() 

str_subset(problems, pattern_with_groups) %>%
  str_replace(pattern_with_groups, "\\1'\\2") %>% head

str_extract(problems, pattern_with_groups) %>% head() #the difference between extract and subset is that 
#extract gives you NAs and subsects takes the NAs out

#----------------------------examples replacing with groups---------------------------
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

#You forgot to check for any spaces in your regex pattern. While the first two entries of “problems” 
#have commas and periods correctly replaced, the last three entries are not identified as part of 
#the pattern and are not replaced.


problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

#The new regex pattern now checks for one character, either a comma, period or space, between the first 
#digit and the last one or two digits, and replaces it with an apostrophe (‘). However, because your 
#last two problem strings have additional space between the digits, they are not corrected.

#------------------------------testing and improving------------------------------------

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

#an example

yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

converted <- s %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)