rm(list=ls())
library(dslabs)
data("research_funding_rates")
research_funding_rates 

#--------------------------------download the data--------------------------

library("pdftools")
temp_file <- tempfile() #to give it a temporary name
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)
getwd()

#We keep the page we want using the following code:

raw_data_research_funding_rates <- txt[2]
data("raw_data_research_funding_rates")

#--------------------------------examining the data------------------------

raw_data_research_funding_rates %>% head #this is the data of all the second page
#Each line on the page, including the table rows, is separated by the symbol for newline: \n

tab <- str_split(raw_data_research_funding_rates, "\n") #we split the data with str_split
tab

tab <- tab[[1]] #we just want the first table
tab %>% head

#taking the column names. We see the column names are in the thrid and fourth line
the_names_1 <- tab[3]
the_names_2 <- tab[4]

#--------------------------------fixing the column names------------------------
#--------------------------------fixing the first column------------------------
the_names_1
#We want to remove the leading space and everything following the comma. We can use regex for the latter. 
#Then we can obtain the elements by splitting using the space. We want to split only when 
#there are 2 or more spaces to avoid splitting success rate. So we use the regex \\s{2,} as follows:

the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>% 
  str_split("\\s{2,}", simplify = TRUE) #so for two or more spaces we split the tables
the_names_1

#--------------------------------fixing the seconc column------------------------
the_names_2
the_names_2<- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE) #Here we want to trim the leading space and then split by space as we did for the first line
the_names_2

#----------------------------joining the columns------------------------------
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_") #this is used to join multiple strings
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>% #Changes all characters to lower case.
  str_replace_all("\\s", "_")
the_names

#---------------------------putting the data with the column names-------------------
tab #By examining the tab object, we notice that the information is in lines 6 through 14. 
#We can use str_split again to achieve our goal.

new_research_funding_rates <- tab[6:14] %>% #the information we want is in this columns 6:14
  str_trim %>% #to take out the spaces
  str_split("\\s{2,}", simplify = TRUE) %>% #we split the data if it has two or more spaces
  data.frame(stringsAsFactors = FALSE) %>% #you do not need to transform data into factors
  setNames(the_names) %>% #we set the names to the column names we just created
  mutate_at(-1, parse_number) #This drops any non-numeric characters before or after the first number. We want to apply this to all the data except for the first row
new_research_funding_rates %>% head() 



