#--------------------------downloading the data-------------------------

rm(list=ls())
url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at,
                                      orders = "a b! d! H!:M!:S! z!* Y!",
                                      tz="EST"))

library(dslabs) #the Trump Tweets database is saved here
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

library(lubridate) #to be able to use the hour() function
library(scales) #to be able to use the percent_format() function
library(tidyverse)
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
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>% #the command replaces strings of the form "https://t.co/any combination of letters and diggits" and strings "&amp;" by empty strings
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
  count(word, source) %>% #to count the number if tweets a word appears
  spread(source, n, fill = 0) %>% #this creates two columns and in each column it fills the n
  mutate(odds_ratio = (Android + 0.5) / (sum(Android) - Android + 0.5) / #Here we will have many proportions that are 0, so we use the 0.5 correction
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(odds_ratio))
android_iphone_or %>% arrange(odds_ratio)

#------------------------------explanation of the odds ratio---------------------

#            Android[i]
#------------------------------------
#            Android[-i]            <-- In the formula we see it as (sum(Android)-Android)
#------------------------------------
#            Iphone[i]
#------------------------------------
#            Iphone[-i]             <-- In the formula we see it as (sum(iPhone)-iPhone)
tweet_words[-1,]

#Given that several of these words are overall low frequency words we can impose a filter based on the total frequency.
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(odds_ratio))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(odds_ratio) #if we look at this table we see 0 tweets of #makeamericagreatagain from Android, so if we don't 
#sum 0.5 when calculating the odds, we would get an error message. That's why it is important to sum 0.5.
#to understand the numbers we can devide 1/0.00144 or 1/0.00718

#------------------------------sentiment analysis--------------------

#In sentiment analysis we assign a word to one or more "sentiment". Although this approach will miss 
#context dependent sentiments, such as sarcasm, when performed on large numbers of words, summaries 
#can provide insights.

#The first step in sentiment analysis is to assign a sentiment to each word. 
#The tidytext package includes several maps or lexicons in the object sentiments:

table(sentiments$lexicon)
sentiments #we can see the list of words in the table and the sentiment they belong to

#------------------------------get_sentiments function--------------------
get_sentiments("bing") #The bing lexicon divides words into positive and negative. 
#We can see this using the tidytext function get_sentiments

get_sentiments("afinn") #The AFINN lexicon assigns a score between -5 and 5, 
#with -5 the most negative and 5 the most positive.

get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment) #The loughran and nrc lexicons provide several different sentiments

#For our analysis, we are interested in exploring the different sentiments of each tweet 
#so we will use the nrc lexicon

nrc <- sentiments %>%
  filter(lexicon == "nrc") %>% #we only take the words that can be classified in by the nrc lexicon
  select(word, sentiment) #we only take the columns with the word and sentiments
nrc #this table contains all the workds that can be classified in by the nrc lexicon

tweet_words %>% inner_join(nrc, by = "word") %>% #we combine both tables and take the words that appear in both of them.
  select(source, word, sentiment) %>%
  sample_n(10)


#------------------------------performing quantitative analysis--------------------

sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none")) #we replace the NAs with "none"
sentiment_counts

tweet_words %>% group_by(source) %>% summarize(n=n()) #we can see how many words were tweeted accoring 
#to the device

#------------------------------computing the odds for each sentiment--------------------

#For each sentiment we can compute the odds of being in the device: proportion of words with 
#sentiment versus proportion of words without and then compute the odds ratio comparing the two devices.

#HERE WE COMPUTE THE ODDS RATIO
sentiment_counts %>% 
  mutate(Android=Android/(sum(Android)-Android),
         iPhone=iPhone/(sum(iPhone)-iPhone),
         odds_ratio= Android/iPhone) %>%
  arrange(desc(odds_ratio)) #We do see some differences: the largest three sentiments are disgust, anger, and negative

#HERE WE COMPUTE THE LOG OF THE ODDS RATIO
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

#----------------------table explaining all the odds, odds ratio, and log of the odds ratio-------
#TABLE MADE BY ME: this table summarizes all the info

sentiment_counts %>% 
  mutate(Android_odds=Android/(sum(Android)-Android),
         iPhone_odds=iPhone/(sum(iPhone)-iPhone),
         odds_ratio= Android_odds/iPhone_odds,
         log_odds_ratio= log(odds_ratio),
         se=sqrt(1/Android + 1/(sum(Android)-Android) + 1/iPhone + 1/(sum(iPhone)-iPhone)), #this is the formula for the se of the odds ration. Check section 16.10.5 of the data science book for more info
         conf.low = log_odds_ratio - qnorm(0.975)*se,
         conf.high = log_odds_ratio + qnorm(0.975)*se) %>%
  arrange(desc(log_odds_ratio))

#----------------------graph of the standard errors of the logs of odds ratio-------
log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>% #this does the same as arrange(desc(log_or))
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) + #this is the aesthetic necessary for geom_errorbar
  geom_errorbar() + #this is the geom that produces the plot we want
  geom_point(aes(sentiment, log_or)) + #this produces a point in the middle with the log of the oods ratio
  ylab("Log odds ratio for association between Android and sentiment") + #we give a name to the y axis
  coord_flip() #we flip the coordinates for better visialization

#We see that the disgust, anger, negative sadness and fear sentiments are associated with the Android 
#in a way that is hard to explain by chance alone. We see this because the confidence intervals are
#far away from zero,

android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>% #we need to transorm the sentiment variable from a character into a factor, otherwise we cannot categorize it
  #we use the levels argument to tell r to sort the levels according to the order of sentiments. To know more watch https://www.youtube.com/watch?v=xkRBfy8_2MU
  mutate(log_or = log(odds_ratio)) %>% #we take the log of the odds ratio 
  filter(Android + iPhone > 10 & abs(log_or)>1) %>% #we take only the relevant information. The words that are repeated more than 10 times and the log_or is greater than 1
  mutate(word = reorder(word, log_or)) %>% #we reorder the words according to log_or
  ggplot(aes(word, log_or, fill = log_or < 0)) + #the fill argument tells to change the color to the values less than 0
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + #this creates multi-panel plots, the default scale for the x axis is fixed, but we want a free scale
  geom_bar(stat="identity",show.legend = FALSE) + #By default, geom_bar uses stat="bin". This makes the height of each bar equal to the number of cases in each group, 
  #and it is incompatible with mapping values to the y aesthetic. If you want the heights of the bars to represent values in the data, use stat="identity" and map a value to the y aesthetic.
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #to justify
