path<- system.file("extdata",package = "dslabs")
path
list.files(path) #we can see what files are inside the folder
filename<- file.path(path,"fertility-two-countries-example.csv")
wide_data <- read_csv(filename) #After running that code, the object wide_data includes the same 
#information as the object tidy_data, except it is in a wide format.
head(wide_data)

select(wide_data, country, `1960`:`1967`) #selects the "variables"
#to use tidyverese we need to wrangle this format

#--------------------------GATHER-------------------------------
#It converts wide data into tidy data

#first argument (key): sets the name of the column that will hold the variable that are currently 
#kept in the y data column names. In our case we can set this name to "year".

#second argument (value): sets the column name for the column that will hold the values in the column cells.
#In this case, we'll call it fertility since that's the data that is in those cells. In our case, 
#it's fertility

#third argument: specifies the columns that will be gathered. The default is to gather all the 
#columns, so we have to specify which collumns we want.

#convert argument: We can use as numeric if we want, but the gathered function actually
#has an argument for that. It's the convert argument.

#FIRST WAY: TELL THE FUNCTION WHAT TO GATHER
new_tidy_data <- wide_data %>% gather(year, fertility, `1960`:`2015`)
new_tidy_data #We can see that the data have been converted to tidy format would columns year and fertility

#SECOND WAY: TELL THE FUNCTION WHAT NOT TO GATHER
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)
new_tidy_data 

class(new_tidy_data$year) #However, the year is a character.
#The gather function assumes that column names are characters,

new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)

#--------------------------SPREAD-------------------------------
#It is sometimes useful for data wrangling purposes to convert tidy data into wide data. 
#We often use this as an intermediate step in tidying up data. The spread function is basically
#the inverse of gather.

#first argument: is for the data, but since we are using the pipe, we don’t show it.

#second argument: tells spread which variable will be used as the column names.

#third argument: specifies which variable to use to fill out the cells.

new_wide_data <- new_tidy_data %>% spread(year, fertility)
new_wide_data
