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
#----------------the separate and extract function-----------------------

s <- c("5'10", "6'1")
tab <- data.frame(x = s)

#We can separate the feet and inches either using the separate function or the extract function
#the separate function

tab %>% separate(col=x, into=c("feet", "incles"), sep="'")

#the extract function
tab %>% extract(col=x, into=c("feet", "inches"), regex = "(\\d)'(\\d{1,2})") #the regex in the pattern

#Extract is more flexible than separate. For example, in this case we cannot use the separate function well:

s <- c("5'10", "6'1\"","5'8inches")
tab <- data.frame(x = s)

tab %>% separate(x, c("feet","inches"), sep = "'", fill = "right") #"right": fills with missing values on the right

#byt if we use the extract function we can select exactly what we want
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})", remove = FALSE) #the remove argument
#allows you to keep or eliminate the original string

#----------------------------Putting all together----------------------------

pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% #the remove argument allows you to keep or eliminate the original string
  mutate_at(c("height", "feet", "inches"), as.numeric) %>% #this is like sapply but can be used in a vector if you want to mutate more than one column
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when( #allows you to vectorise multiple if_else() statements
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

#We can check all the entries we converted using the following code:

not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x)) #hace el verctor numérico, pero hay unos NAs
  ind <- !is.na(inches) & #nos quedamos con los que NO son NAs (es decir, los correctos)
    ((inches >= smallest & inches <= tallest) | #eliminamos los que son menores a 50 y mayores a 84
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}

new_heights %>%
  filter(not_inches_or_cm(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()

#Let's take a look at the shortest students in our dataset using the following code:
new_heights %>% arrange(height) %>% head(n=7) #organizamos de forma ascendiente

