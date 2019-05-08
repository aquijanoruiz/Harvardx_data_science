rm(list = ls())
library(tidyverse)
library(Lahman)

#---------------------------definition of sophomore slump------------------------
#A sophomore slump refers to an instance in which a second, or sophomore, effort fails to live up to the 
#standards of the first effort. It is commonly used to refer to the apathy of students (second year of high school, 
#college or university), the performance of athletes (second season of play), singers/bands (second album), 
#television shows (second seasons) and movies (sequels/prequels). Source: Wikipedia.

#---------------------------preparing the data-----------------------------------

??Fielding
head(Fielding)

??Master

#-----------------------------the slice() function--------------------------
Fielding %>% slice(1:10) #to choose the first 10 rows from the list
Fielding %>% slice(1) #to take the first column of the list
Fielding[1,] #this does the same as Fielding %>% slice(1)

#-------------table with the players's id, first and last name and position --------------
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>% #takes the first row from each group. We only need one of the positions. 
  ungroup %>%
  left_join(Master, by="playerID") %>% #we insert the information from the Master database to the Fielding database. 
  select(playerID, nameFirst, nameLast, POS)

playerInfo

#----------------table with the ROY (rookie of the year) award winner-------------------

??AwardsPlayers
head(AwardsPlayers)
??Batting

ROY_1 <- AwardsPlayers %>% 
  filter(awardID=="Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>% #we add the information from the playerInfo table to this table.
  rename(rookie_year=yearID) %>%
  right_join(Batting, by="playerID") %>%#we add all this information we have been arranging to the Batting data base. 
  #This will create a lot of NAs in awardID and rookie_year because there will be a lot of playes that have not won the ROY award.
  mutate(AVG=H/AB) %>% #we devide the hits by the at bats.
  filter(POS !="P") #we take out the pitchers

#-------keeping the only ones who played the rookie and sophomore seasons and remove the players that did not play the sophomore seasons--------

#We will keep only the rookie and sophomore seasons and remove players that did not play sophomore seasons.

head(ROY_1)
ROY_2 <- ROY_1 %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>% #we fiter those who played the rookie or the sophomore season
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>% #we onluy keep the ones with two years
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

head(ROY_2)
ROY <- ROY_2 %>% spread(rookie, AVG) %>% arrange(desc(rookie)) # we use the spread function to crerate an extra column
head(ROY)#this way we have only one row with one id and instead we have two separate columns with the different averages
#corresponding to the season in which they played

#--------------------analysing the data-------------------
mean(ROY$sophomore - ROY$rookie <= 0) #we calculate the percentage of players that performed better than the season in which 
#they won rookie of the year

#-------------------So is it “jitters” or “jinx”?-------------------
#To answer this question, let’s turn our attention to all players that played the 2013 and 2014 seasons and
#batted more than 130 times (minimum to win Rookie of the Year).

two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>% #we sum the "at bat"
  summarize(AVG = sum(H)/sum(AB)) %>% #we get average of hits the player did
  ungroup %>%
  spread(yearID, AVG) %>% #we use the spread function to create two functions with the years as names, each with the average of hits
  filter(!is.na(`2013`) & !is.na(`2014`)) %>% #the take out the NAs
  left_join(playerInfo, by="playerID") %>% #we imput the data from the playerInfo
  filter(POS!="P") %>% #we take out the pitchers
  select(-POS) %>% #we takeout the position (we don't care about this, only the hits)
  arrange(desc(`2013`)) %>% #we arrange in descending order
  select(nameFirst, nameLast, `2013`, `2014`) #we compare the difference between one year and the other

two_years
#The same pattern arises when we look at the top performers: batting averages go down for the most of the top performers.
#These are not rookies!

arrange(two_years, `2013`) #now we orden in ascending order
#The same pattern arises when we look at the top performers: batting averages go down for the most of the top performers.

#Their batting averages go up! Is this some sort of reverse sophomore slump? It is not. There is no such thing
#as the sophomore slump. This is all explained with a simple statistical fact: the correlation for performance
#in two separate years is high, but not perfect:

qplot(`2013`, `2014`, data = two_years)
summarize(two_years, cor(`2013`,`2014`)) #we can see that the correlation is not perfect
#Because the correlation is not perfect, regression tells us that, on average, expect high performers from 2013
#to do a bit worse in 2014. It’s not a jinx; it’s just due to chance.
