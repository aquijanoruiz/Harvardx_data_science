path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")

raw_dat <- read_csv(filename)
raw_dat #this data is in y format.

dat<- raw_dat %>% gather (key, value, -country) #The result is not exactly what we refer to as tidy 
#since each observation is associated with two, not one, rows.
dat
dat$key[1:5] #Note that the entries in this column separate the yearfrom the variable name using an underscore.

#-------------------------------------SEPARATE----------------------------------
#FIRST ARGUMENT:the name of the column to be separated
#SECOND ARGUMENT: the names to be used for the new columns
#THIRD ARGUMENT: the character that separates the variables

dat %>% separate(key, c("year","variable_name"),"_") #because the underscore is the default separator,
#we can actually simply write code like this:

dat %>% separate(key, c("year", "variable_name")) #we see the life expectancy variable is truncated to just life.

#BETER,,,
dat %>% separate(key, c("year","variable_name","second_variable_name"),fill="right") #we tell it to fill 
#the column on the right.

#EVEN BETTER...
dat %>% separate(key,c("year","variable_name"),extra="merge") #we merge the last two variables when 
#there's an extra separation.

dat %>% separate(key,c("year","variable_name"),extra="merge") %>% spread(variable_name, value)

#-----------------------------SEPARATE AND UNITE FUNCTION TOGETHER----------------------------------
dat %>%
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertlity = fertility_NA)


