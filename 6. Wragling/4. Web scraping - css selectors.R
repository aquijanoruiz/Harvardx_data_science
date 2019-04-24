#-----------------------rvest package-------------------------------
#The tidyverse provides a web harvesting package called rvest. The first step using this package is to import
#the webpage into R.


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
tab <- tab %>% setNames(c("state", "population", "total", "murder_rate"))
head(tab)

#------------------------------CREATING FUNCTION FOR DATA SCRAPING-------------------------------------

#For the guacamole recipe page we already have done this and determined that we need the following selectors:
#NOTE: To get the selectors, we need to use this program https://selectorgadget.com/
  
webpage <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- webpage %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- webpage %>% html_node(".o-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- webpage %>% html_nodes(".o-Ingredients__a-ListItemText") %>% html_text()

#You can see how complex the selectors are. In any case we are now ready to extract what we want 
#and create a list:
  
guacamole <- list(recipe, prep_time, ingredients)

#Since recipe pages from this website follow this general layout, we can use this code to create 
#a function that extracts this information:
  
 get_recipe <- function(url){
    h <- read_html(url)
    recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
    prep_time <- h %>% html_node(".o-RecipeInfo__a-Description--Total") %>% html_text()
    ingredients <- h %>% html_nodes(".o-Ingredients__a-ListItemText") %>% html_text()
    return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
  } #now we can use this function on any of the recipies of the website
 
get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")
get_recipe(webpage)
