rm(list = ls())
library(dslabs)
library(tidyverse)
library(purrr)
library(pdftools)
library(lubridate)

#----------------------preparing the data----------------------
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s) #removes spaces at the start and the end of a string
  header_index <- str_which(s, "2015")[1] #finds the first string that matches the pattern 2015
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE) #Split a string into a list with parts separated by pattern
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+") #counts number of times a pattern appears in a string
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>% #Split a string into a matrix with parts separated by pattern
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01")

#---------------------------Q1---------------------------------
#calculating a spam of 2 months
dat_span <- dat %>% group_by(year,month) %>% summarize(n=n()) %>% print() 
nrow(dat_span) #here we can see there are 41 months
span <- 2/41

span <- 60 / as.numeric(diff(range(dat$date))) #another way to do it

fit <- loess(formula = deaths ~ as.numeric(date), span = span, degree = 1, data = dat) #the loess formula needs the date to be a numeric string

fit$fitted #this is the fitted data we need to plot
dat %>% filter(is.na(deaths)) #ther is an NA we have to take out

dat %>% filter(!is.na(deaths)) %>% mutate(fit=fit$fitted) %>%
  ggplot(aes(x=date,y=deaths)) +
  geom_point() +
  geom_line(aes(x=date,y=fit), color= "red",  size = 2)

#---------------------------Q2---------------------------------

dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

#---------------------------Q3---------------------------------
#Suppose we want to predict 2s and 7s in the mnist_27 dataset with just the second covariate. Can we do this? 
#On first inspection it appears the data does not have much predictive power.

library(broom)
mnist_27$train %>% 
  glm(y ~ x_2, family = "binomial", data = .) %>%  #the family="binomial" is used because the Y is binary
  tidy() #the tidy formula provides more information 

#Plotting a scatterplot here is not useful since y is binary:
qplot(x_2, y, data = mnist_27$train)

#This is the data we are going to use for the model
data_mnist <- mnist_27$train %>% mutate(y_1= ifelse(y==2, 1, 0)) #2 is 1, and 7 is 0

#This is the model
fit_mnist <- loess(formula = y_1 ~ x_2, span = 0.1, degree = 1, data = data_mnist )

#Now we ploted the fitted data
data_mnist %>% mutate(y_hat = fit_mnist$fitted) %>% 
  ggplot(aes(x=x_2,y=y_hat)) +
  geom_line()
  
#---We can also do it like this---
mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")
