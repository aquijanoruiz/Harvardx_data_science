#-------------------------------------Paths and the working directory-----------------------------

getwd()
#Note that unless a full path is provided, they search for files in the working directory.
#For this reason, our recommended approach for beginners is that you create a directory for each analysis
#and keep the raw data files in that directory.
setwd() #this is used to change your working directory

system.file("extdata",package = "dslabs")
#Once you download and install that the DSLABS package, files will be in the external data, extdata, 
#directory that you can get by typing this command.

#WE DON'T RECOMMEND DOING THIS (BECAUSE DIFFERENT SYSTENS USE DIFFERENT PATHS)
path <- "/Library/Frameworks/R.framework/Versions/3.5/Resources/library/dslabs/extdata"

#INSTEAD WE RECOMMEND THIS
path <- system.file("extdata",package = "dslabs")

list.files(path) #opens the files in my working directory
#Remember we need to use quotation marks when providing a full path to the working directory.

filename <- "murders.csv"
fullpath <- file.path(path,filename) #Creates a path using the file name you provide
fullpath

file.copy(fullpath,getwd()) #it moves the file from "6. Wragling/data" to our working directory
file.exists(filename) #we can confirm that the file has been moved

#-----------------------------The readr and the readxl packages----------------------------------

#readr package
#readr is the tidyverse library that includes functions for reading data stored in text file spreadsheets into r.
#read_table, read_csv, read_csv2, read_tsv, and read_delim.

#readxl package
#The read_excel package provides functions to read in data in the Microsoft Excel format.
#read_excel, read_xls, read_xlsx

#excel_sheets
#Gives us the names of the sheets in an Excel file. These names can then be passed on to the sheet 
#argument in the three functions that we just described to read in Excel files.

#examples: The first sheet is named “2015”, the second is named “2016”, and the third is named “2017”
#we want the second sheet
times_2016 <- read_excel("times.xlsx", sheet = 2)
times_2016 <- read_excel("times.xlsx", sheet = "2016")
times_2016 <- read_xlsx("times.xlsx", sheet = 2)

#readl_lines shows us the first few lines of a file within r, and tells of if there is a header or not
library(tidyverse)
read_lines("murders.csv",n_max=3)

#------------------WAYS YOU CAN READ A CSV FILE-------------------
dat<- read_csv(filename) #we can use the file name because it is in our working directory
dat<- read_csv(file.path(getwd(),"murders.csv")) #we can use the full path
file.path(getwd(),"murders.csv")

class(dat) #note that the read_csv file creates a tibble table. If we use read.csv we create a data frame.
#read_csv and read.csv are not the same

#------------------Importing data using r-base functions-----------------
dat2<- read.csv(filename)    
class(dat2) #this is a data frame, not a tibble
class(dat2$region) #this is a factor, different to a character which is what we get with the read_csv function

dat3<- read.csv(filename, stringsAsFactors = FALSE) #so that we do not get dactors
class(dat3$region) 

#------------------Downloading files from the internet--------------------
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat4 <- read_csv(url) #if we just want to read the file
download.file(url, "murders.csv") #if we want to download it

#tempdir # creates a directory with a name that is very unlikely not to be unique.
#tempfile #  creates a character string, not a file, that is likely to be a unique file name.

tmp_filename <- tempfile()
download.file(url, tmp_filename) #we download the file and give it a temporary name

dat <- read_csv(tmp_filename) #we load the data
file.remove(tmp_filename) #the we remove the file
