library(dslabs)
data("movielens")

#------------------Q1--------------------
#Compute the number of ratings for each movie and then plot it against the year the movie came out. 
#Use the square root transformation on the counts.

#What year has the highest median number of ratings?

library(tidyverse)
movielens %>% as_tibble()

dat_median <- movielens %>% group_by(movieId) %>%
  summarize(n=n(), year=first(year)) %>% 
  group_by(year) %>%
  summarize(median=median(n)) 

ind_max_median <- which.max(dat_median$median)
dat_median[ind_max_median,]

#Suggested procedure
movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#------------------Q2--------------------

#Among movies that came out in 1993 or later, what are the 25 movies with the most ratings per year, 
#and what is the average rating of each of the top 25 movies?

#This is the approach, I think, is correct:----------------
library(lubridate)
dat_avg <- movielens %>% as_tibble() %>% 
  mutate(date = as_datetime(timestamp)) %>%
  mutate(rated_year = year(date)) %>%
  group_by(movieId,rated_year) %>%
  summarize(title=first(title),
            year=first(year),
            avg_rating=mean(rating),
            n=n()) 

dat_avg #This is the data we will work with to ansers the questions

dat_avg_cal <- dat_avg %>% group_by(movieId) %>%
  summarise(title=first(title), avg_rating = mean(avg_rating), avg_n = mean(n), total_n = sum(n)) #We get some more information

dat_avg_cal %>% arrange(desc(total_n)) #Here it is

#What is the average rating for the movie The Shawshank Redemption?
dat_avg_cal %>% filter(movieId==318) #we see that the the average rating is 4.44

#What is the average number of ratings per year for the movie Forrest Gump?
dat_avg_cal %>% filter(movieId==356)

Forrest_Gump <- movielens %>% filter(movieId==356) %>%
  mutate(date = as_datetime(timestamp)) %>%
  mutate(rated_year = year(date)) %>%
  group_by(rated_year) %>%
  summarize(n=n())

Forrest_Gump %>%
  summarize(sum(n)/length(n))

#These are the correct answers according to the course staff:
movielens %>% filter(movieId==318) %>%
  summarize(mean_rating = mean(rating))

movielens %>% filter(movieId==356) %>%
  mutate(date = as_datetime(timestamp)) %>%
  mutate(rated_year = year(date)) %>%
  group_by(rated_year) %>%
  summarize(n=n()) %>% summarize(sum(n)/24) 

2017 - 1994 + 1 #24 is the what the staff suggest to divide by

movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate)) 

#------------------Q3--------------------
#From the table constructed in Q2, we can see that the most frequently rated movies tend to have above average
#ratings. This is not surprising: more people watch popular movies. To confirm this, stratify the post-1993 
#movies by ratings per year and compute their average ratings. Make a plot of average rating versus ratings per
#year and show an estimate of the trend.

table_q2 <- movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  arrange(desc(rate)) 

table_q2 %>% ggplot(aes(x=rate,y=rating)) + geom_point() + geom_smooth()

#------------------Q4--------------------
#Suppose you are doing a predictive analysis in which you need to fill in the missing ratings with some value.

#Given your observations in the exercise in Q3, which of the following strategies would be most appropriate?


#------------------Q5--------------------
#The movielens dataset also includes a time stamp. This variable represents the time and data in which the 
#rating was provided. The units are seconds since January 1, 1970. Create a new column date with the date.
movielens_with_date <- mutate(movielens, date = as_datetime(timestamp))

#------------------Q6--------------------
#Compute the average rating for each week and plot this average against day. 
#Hint: use the round_date function before you group_by.

#What type of trend do you observe?

#------------------
#We see what happens with weeks

movielens_with_date %>% as_tibble()
avg_week_rating <- movielens_with_date %>% mutate(date = round_date (date)) %>%
  mutate(week = week(date), day = day(date)) %>%
  group_by(week) %>%
  summarize(avg_week_rating = mean(rating))

avg_week_rating %>% ggplot(aes(x=week, y=avg_week_rating)) + geom_point() + geom_smooth()

#------------------
#We see what happens with days

avg_day_rating <- movielens_with_date %>% mutate(date = round_date (date)) %>%
  mutate(week = week(date), day = day(date)) %>%
  group_by(day) %>%
  summarize(avg_day_rating = mean(rating))

avg_day_rating %>% ggplot(aes(x=day, y=avg_day_rating)) + geom_point() + geom_smooth()

#------------------Q8--------------------
#The movielens data also has a genres column. This column includes every genre that applies to the movie. 
#Some movies fall under several genres. Define a category as whatever combination appears in this column. 
#Keep only categories with more than 1,000 ratings. Then compute the average and standard error for each 
#category. Plot these as error bar plots.

#Which genre has the lowest average rating?

movielens %>% as_tibble()

movielens %>% group_by(genres) %>% 
  mutate(n = n()) %>%
  filter(n > 1000) %>%
  summarize(se_rating = sd(rating),
            avg_rating = mean(rating)) %>%
  arrange(avg_rating)
