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
pattern <- "\\d" #I want to detect the one that have digits
str_detect(s, pattern)

#------------------------------------ str_view---------------------------------------------------------
library(htmlwidgets) #we use this package to use the str_view functionality
str_view(s,pattern) #it shows us the first match for each string

str_view_all(s, pattern) #shows us all the matches, so 3'2 has two matches and 5'10 has three

