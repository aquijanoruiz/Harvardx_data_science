#--------------------------downloading the data-------------------------
rm(list=ls())
url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at,
                                      orders = "a b! d! H!:M!:S! z!* Y!",
                                      tz="EST"))
library(dslabs)
library(lubridate) #to be able to use the hour() function
library(scales) #to be able to use the percent_format() function
library(tidyverse)
data("trump_tweets")
head(trump_tweets)
names(trump_tweets) #to check the variables

?trump_tweets #provides details on what each variable represents.

trump_tweets %>% select(text) %>% head #The tweets are represented by the textvariable

trump_tweets %>% count(source) %>% arrange(desc(n)) #the source variable tells us the device that was used to compose and upload each tweet.
#the count variable tells you hom many of each source there are

#--------------------------------------extracting the info we want-----------------------------
trump_tweets %>% 
  extract(col=source, into="source", regex= "Twitter for (.*)") %>% #Extract one column into multiple columns
  count(source) #to extract the device used to tweet

campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &#these are the two sources we care about
           created_at >= ymd("2015-06-17") & #we only want the tweets during Trumps campaign
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>% #we do not want the retweets
  arrange(created_at) #we arrange according to the date

#--------------------------------------looking at the times of the tweets-----------------------------
#We can now use data visualization to explore the possibility that two different groups 
#were tweeting from these devices.


ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>% #with_tz provides the time zone you want
  count(source, hour) %>% #tells the source depending on the hour
  group_by(source) %>% #we group by device
  mutate(percent = n / sum(n)) %>% #we calculate the percentage for each hour
  ungroup %>% # we ungroup
  ggplot(aes(hour, percent, color = source)) + #we make a plot to see the percentage of tweets according to the hour they are tweeted
  geom_line() + 
  geom_point() +
  scale_y_continuous(labels = percent_format()) + #we need to install the scales package
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

#------------------------------text as data- the tidytext package-----------------------------
library(tidytext)

#------------------------------unnest_tokens function-----------------------------
#unnest_tokens: A token refers to the units that we are considering to be a data point. The most common token 
#will be words, but they can also be single characters, ngrams, sentences, lines or a pattern defined by a regex. 
example <- data_frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text) #to convert text into a tidy table


#Now let's look at a quick example with a tweet number 3008
i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

#CAUTION: A token in twitter is not the same as in regular english. For this reason instead of using 
#the default, words, we define a regex that captures twitter character. The pattern appears complex 
#but all we are defining is a patter that starts with @, # or neither and is followed by 
#any combination of letter or digits

pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

#------------------------------removing the pictures-----------------------------
#Removing the links to pictures:
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 

#------------------------------removing the top words-----------------------------
#And we can now answer questions such as "what are the most commonly used words?"
tweet_words %>% 
  count(word) %>%
  arrange(desc(n))

#It is not surprising that these are the top words. The top words are not informative. 
#The tidytext package has database of these commonly used words.

stop_words #these are the top words
filter(!word %in% stop_words$word) #we can use the function filter to take out the top words


tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word ) #now our code looks like this

#------------------------------removing years and quotes-----------------------------
#We want to remove these and we can find them using the regex ^\d+$. 
#Second, some of our tokens come from a quote and they start with '.
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>% #to eliminate the years
  mutate(word = str_replace(word, "^'", "")) #to eliminate the quotes

#------------------------------adding the odds ratio-----------------------------
android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>% #this creates two columns and in each column it fills the n
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / #these are the odds ratio
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

