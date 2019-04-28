#Suppose we did not have the function read_csv or read.csv available to us.
#We instead have to read a csv file using the base R function readLines like this:

filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head()

#---------------------string_split-------------------------------
#It sextract the values that are separated by comma for each string in the vector.

x <- str_split(lines, ",")
x %>% head(2)

col_names <- x[[1]]
x <- x[-1]

#-----------------convert list into dataframe (the long way)--------------------
#map.- Apply a function to each element of a vector
#We use the map functions in the purrr package. The map function applies the same function 
#to each element in a list.

library(purrr)
map(x, function(y) y[1]) %>% head(2)

#However, because this is such a common task, purrr provides a shortcut. If the second argument receives
#an integer instead of a function, it assumes we want that entry. So the code above can be written more
#efficiently like this:
map(x, 1)

#To force map to return a character vector instead of a list, we can use map_chr. 
#Similarly, map_int returns integers.

dat <- data_frame(map_chr(x, 1),
                  map_chr(x, 2),
                  map_chr(x, 3),
                  map_chr(x, 4),
                  map_chr(x, 5)) %>%
  mutate_all(parse_guess) %>% #what does parse_guess do?
  setNames(col_names)
class(dat)
dat %>% head


#A more efficiente code
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>%
  as_tibble()

#-----------------convert list into dataframe (the short way)--------------------

#if we know that the data we are extracting can be represented as a table, we can use the argument 
#simplify=TRUE and str_split returns a matrix instead of a list
x <- str_split(lines, ",", simplify = TRUE)
col_names <- x[1,] #we set the column names as the first row in the list
x <- x[-1,] #we separate the date from the column names
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess) %>%
  head(5)

#-----------------the unnest function--------------------
library(dplyr)
df <- tibble(
  x = 1:3,
  y = c("a", "d,e,f", "g,h")
)
df %>%
  transform(y = strsplit(y, ",")) %>%
  unnest(y) #splits the second column into different rows