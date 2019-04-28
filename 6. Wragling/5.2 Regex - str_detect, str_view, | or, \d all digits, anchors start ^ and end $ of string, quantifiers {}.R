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

#These are the patterns we see
#A pattern of the form x'y or x' y'' or x'y" with x and y representing feet and inches respectively

#------------------------identify sth that follows a pattern with str_subset------------------------------
pattern_1 <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d'*\"*$"
str_subset(problems,pattern_1) %>% head(n=10) %>% cat #to check if they use the 5'10 pattern

pattern_2 <- "^[4,6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems,pattern_2) %>% head(n=10) %>% cat

ind<- which(between(suppressWarnings(as.numeric(problems))/2.54,54,81))
ind<- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat

#------------------------------------ | --------------------------------------------------------- 
#| means "or"
str_subset(reported_heights$height,"cm") #to show the ones that contain cm

yes <- c("180 cm", "70 inches") #we're asking which of the strings include the pattern "cm" or the pattern "inches."
no <- c("180", "70''")
s <- c(yes, no) #we create one vector of strings including both.

print(s)
str_detect(s,"cm") | str_detect(s,"inches") #we can see it dectects the words. We use | (or) to add another condition
str_detect(s,"cm|inches") #we can use this instead and be more efficient


#------------------------------------ \d --------------------------------------------------------- 
#\d means "any digit": 0, 1, 2, 3, 4, 5, 6, 7, 8, 9. The backlash is used to distinguish it from the character d. 
#In R, we have to escape the backslash \ so we actually have to use \\d to represent digits.

yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d" #I want to detect the one that have digits. SAME AS WRITING [1-9]
str_detect(s, pattern)

#------------------------------------ str_view---------------------------------------------------------
library(htmlwidgets) #we use this package to use the str_view functionality
str_view(s,pattern) #it shows us the first match for each string

str_view_all(s, pattern) #shows us all the matches, so 3'2 has two matches and 5'10 has three


#------------------------------------ [ ]---------------------------------------------------------
#Character classes are used to define a series of characters that can be matched. 
#We define character classes with square brackets
str_detect(s, "[56]") #we want the pattern to match only if we have a 5 or a 6, we use the regex [56]
str_view(s, "[56]")

#If we want to match values between 4 and 7. A common way to define character classes is with ranges.
#So, for example, [0-9] is equivalent to \\d.

yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")

#NOTE: In regex, everything is a character. There are no numbers. So 4 is the character 4, not the number 4.
#If we type 1 through 20, this does not mean 1, 2, 3, 4, 5, up to 20. It means that characters 1 through 2 
#and then the character 0.

#[] for letters: lowercase letters as [a-z], uppercase letters as [A-Z], and [a-zA-z] as both.

#------------------------------------ anchors * $---------------------------------------------------------
#The two most common anchors are ^ and $ which represent the beginning and end of a string respectively.
#So the pattern ˆ\\d$ is read as “start of the string, followed by one digit (\\d means one digit),
#followed by end of string”.

pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view_all(s, pattern)

#------------------------------------ quantifiers {}------------------------------------------------------
#To use quantifiers, we follow the pattern by curly brackets with the possible number of times 
#the previous entry repeats. So the pattern for one or two digits is ^\\d{1,2}$

pattern <- "^\\d{1,2}$" #it tells to include the strings with 1 and 2 digits
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)

#------------------------------------ complicated pattern------------------------------------------------------
pattern <- "^[4-7]'\\d{1,2}\"$"

#The pattern is now getting complex, but you can look at it carefully and break it down:
#• ˆ = start of the string
#• [4-7] = one digit, either 4,5,6 or 7
#• ' = feet symbol
#• \\d{1,2} = one or two digits
#• \" = inches symbol
#• $ = end of the string

yes <- c("5'7\"", "6'2\"", "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)

#------------------------------------ examples------------------------------------------------------

animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]" #this considers all the chatacter strings with lowercase letters
str_detect(animals, pattern) 

animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[A-Z]$" #regex pattern tells str_detect to look for an uppercase ([A-Z]) letter 
#at the end of the string ($): this is only true for the string “MONKEY”.
str_detect(animals, pattern)

animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]{4,5}" #this regex command tells R to look for either 4 or 5 lowercase letters 
#in a row anywhere in the string. This is true for the animals “puppy” and “Moose”.
str_detect(animals, pattern)





