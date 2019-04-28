#R fortmat of dates
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate) #we can see these are dates, not strings

as.numeric(polls_us_election_2016$startdate) %>% head #if we transform them into numbers, this is what we get
as.Date("1970-01-01") %>% as.numeric #So to see that the epoch is day 0 we can type

polls_us_election_2016 %>% filter(pollster == "Ipsos" & state =="U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line() #Plotting functions, such as those in ggplot, are aware of the date format.

library(lubridate)

set.seed(2002)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort #sort sorts the vector in alphabetic order
dates

#------------------------------------how to extract months, days and years--------------------
#The functions year, month and day extract those values:
data_frame(date = dates,
           month = month(dates),
           day = day(dates),
           year = year(dates))

month(dates, label = TRUE) #to extract the months labels

#---------------------------------parsers-------------------------------------
#They confert strings into dates.
#The function ymd assumes= the dates are in the format YYYY-MM-DD and tries to parse as best as possible.

x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
x
ymd(x)

#--------------------------the ISO 8601 format--------------------------------
#The preferred format is to show year (with all four digits), month (two digits) and then day,
#or what is called the ISO 8601.

x <- "09/01/02"
class(x) #now this is a character

#these are some of the functions in the lubridate package
ymd(x) #The ymd function assumes the first entry is the year, the second is the month, and the third is the day
ydm(x) #The mdy function assumes the first entry is the month, then the day, then the year

#other functions are
dmy(x)
dym(x)

#--------------------------now function to define time zone--------------------------------
#The lubridate package provides a slightly more advanced function, now, that permits you to
#define the time zone:

Sys.time() #this gives you the time now
now()
now("GMT")

OlsonNames() #to get all the time zones

#-----------------------------how to extract hours, minutes and seconds----------------------

now() %>% hour()
now() %>% minute()
now() %>% second()


#It also includes a function to parse strings into times:
x <- c("12:34:56")
hms(x)

#as well as parsers for time objects that include dates:
x <- "Nov/2/2012 12:34:56"
mdy_hms(x)
