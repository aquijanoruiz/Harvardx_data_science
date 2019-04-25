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

yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")

#-----------------------------case 2 and 4-------------------------

