#-----------------------------Multivariate models (preparing the data)------------------------
rm(list = ls())
library(tidyverse)
library(Lahman)
data("Teams")

#Y_i = Beta_0 + Beta_1*X1 + Beta_2*X2 + Epsilon

fit <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  lm(R ~ BB + HR, data = .)

tidy(fit, conf.int=TRUE)

#-----------------------------the regression-------------------------
#Y_i = Beta_0 + Beta_1*X1 + Beta_2*X2 + Beta_3*X3 + Beta_4*X4 + Beta_5*X5 + Epsilon

fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB / G,
         singles = (H - X2B - X3B - HR) / G,
         doubles = X2B / G,
         triples = X3B / G,
         HR = HR / G,
         R = R / G) %>%
  lm(R ~ BB + singles + doubles + triples + HR, data = .)

coefs <- tidy(fit, conf.int = TRUE)

#-----------------------------the prediction-------------------------
Teams %>%
  filter(yearID %in% 2002) %>%
  mutate(BB = BB/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples =X3B/G,
         HR=HR/G,
         R=R/G) %>%
  mutate(R_hat = predict(fit, newdata = .)) %>% #R_hat is the prediction of the 2002 using data from 1962 to 2001 that is saved in the "fit" regression
  ggplot(aes(R_hat, R, label = teamID)) + #we compare R_hat (the predicted value) with R, and label the team ID
  geom_point() + 
  geom_text(nudge_x=0.1, cex = 2) + #nudge_2 =makes the lables not to overlap with the dots. cex= changes the size of the font
  geom_abline() #we see that our prediction is close to what we see in real data.

#------------------------------defining metric for one player----------------------------

#To define a metric for player A, we imagine a team made up of players just like player A
#and use our fitted regression model to predict how many runs this team would produce.
fit
#-2.769 + 0.371 × BB + 0.519 × singles + 0.771 × doubles + 1.24 × triples + 1.443 × HR

#-------------------predicting how many runs would score a player using regression----------------------------

#For players, a rate that takes into account opportunities is a per-plate-appearance rate.
#To make the per-game team rate comparable to the per-plate-appearance player rate, 
#we compute the average number of team plate appearances per game

data("Batting")
head(Batting)
?Batting #these are the batting statistics per player
pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>% #we group by team
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% # average number of team plate appearances per game
  pull(pa_per_game) %>%
  mean


pa_per_game #the number of plate appearances per team per game, averaged across all teams

#We compute the per-plate-appearance rates for players available in 2002 on data from 1999-2001. To avoid
#small sample artifacts, we filter players with few plate appearances.

players <- Batting %>% filter(yearID %in% 1999:2001) %>%
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>% #plate appearances= at bats + bases on balls
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G,
            triples = sum(X3B)/G,
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>% #we care about those who have more plate appearances
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .)) #we predict the number of runs they would score with thae data from the regression

#The player-specific predicted runs computed here can be interpreted as the number of runs we predict a
#team will score if all batters are exactly like that player.
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))

#-----------------------------adding salary to the position information---------------------
players <- Salaries %>% #We add the salaries information to the players table we had already created
  filter(yearID == 2002) %>% 
  select(playerID, salary) %>%
  right_join(players, by="playerID")

#-----------------------------adding the defensive positions---------------------

#The Lahman package table Appearances tells how many games each player played in each position, 
#so we can pick the position that was most played using which.max on each row. We use apply to do this.

#However, because some players are traded, they appear more than once on the table, so we first sum their 
#appearances across teams. Here, we pick the one position the player most played using the top_n function. 
#To make sure we only pick one position, in the case of ties, we pick the first row of the resulting data frame. 
#We also remove the OF position which stands for outfielder, a generalization of three positions left field (LF), 
#center field (CF), and right field (RF). We also remove pitchers since they don’t bat in the 
#league in which the A’s play.

position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")

head(Appearances)
tmp_tab <- Appearances %>%
  filter(yearID == 2002) %>%
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>% #this summarizes multiple columbns, and we sum the appearances in each position
  ungroup()

#creating an index
pos <- tmp_tab %>% #this is the index we will use later
  select(position_names) %>%
  apply(., MARGIN=1, FUN=which.max)  #we apply the which.max formula. MARGIN=1 indicates rows. This mean the output will be in rows.

#we are going to continue transforming this table
head(players) 
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>% #here we create a list of the players' names and the position they play
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>% #str_to_upper makes the text upper case, and str_remove removes the character strings we don't want
  filter(POS != "P") %>% #we remove the pitchers because they don't bat
  right_join(players, by="playerID") %>% #we want a table with the same rows as second table, in this case the players table we created in the section "dding salary to the position information"
  filter(!is.na(POS) & !is.na(salary)) #we do not want the NAs

#Finally, we add their first and last name:

head(Master) #we are going to use this data set
??master
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>% #we only want this columns
  mutate(debut = as.Date(debut)) %>% #we transform the debut date into a date string
  right_join(players, by="playerID") #we are going to combine this table according to the players id we have from the previous table

players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>%
  arrange(desc(R_hat)) %>% #to arrange in descending order
  top_n(10) #when want the top 10

#-----------------------------plotting the graphs (picking 9 players)---------------------
#this illustration compares the estimated R (runs) and the salaries in 2002
players %>% ggplot(aes(salary, R_hat, color = POS)) +
  geom_point() +
  scale_x_log10()

#We do see some low cost players with very high metrics. These will be great for our team. Unfortunately,
#many of these are likely young players that have not yet been able to negotiate a salary and are unavailable.

#Here we remake plot without players that debuted before 1998. 

library(lubridate)
players %>% filter(year(debut) < 1998) %>% #We use the lubridate function year, to transform the specific dates into years
  ggplot(aes(salary, R_hat, color = POS)) +
  geom_point() +
  scale_x_log10()

#------------------------------linear programming------------------------------
#Linear programming (LP, also called linear optimization) is a method to achieve the best outcome 
#(such as maximum profit or lowest cost) in a mathematical model whose requirements are represented 
#by linear relationships.

library(reshape2)
library(lpSolve) #this is the package we can use to solve linear programming problems
players <- players %>% filter(lubridate::year(debut) < 1998)

#we need three things: the constraint matrix, the constraint dir, and the constraint limit

#------------------------------the constraint matrix----------------------------
head(players) #we are going to create the constraint matrix from this data frame
constraint_matrix <- acast(data=players, POS ~ playerID, fun.aggregate = length) #this matrix shows what position each player plays
view(constraint_matrix)

npos <- nrow(constraint_matrix) #we count the number of positins there are
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
view(constraint_matrix) #this matrix now has an additional column with the salaries

#------------------------------the constraint dir---------------------------
constraint_dir <- c(rep("==", npos), "<=") #we create the signs for the constraint. This tells that the position must
#be equal to the position the player plays, and that it must be lower or equal to the buget.
constraint_dir

#-------------------------------the constraint limit--------------------------
constraint_limit <- c(rep(1, npos), 50*10^6) #we create a list of eight 1s and 50 million

#-------------------------------the lp() function-----------------------------
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE)
summary(lp_solution)

#----------------------------Finally choosing the 9 players-------------------
our_team <- players %>%
  filter(lp_solution$solution == 1) %>% #we need to match the players with the restrictions
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

#We see that most of these players have above average BB and HR rates, while the same is not true for singles:
my_scale <- function(x) (x - median(x))/mad(x) #mad means the median absolute deviation
#I created this table just to show how we are going to transform this scale

sample <- players %>% mutate(BB_scale=my_scale(players$BB)) %>%
  select(playerID, BB, BB_scale) #what we do here is standarizing but instead of running the (X - mean)/sd formula
#we use (X-median)/mad
head(sample)


players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>% #we filter by the list of players we have already chosen
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat, salary) %>%
  arrange(desc(R_hat))

players #if we look at this list, we can see that Todd scores 3.08 standard deviations above the average.
#we standarize the numebr of runs to make the comparison more informative and easier to understand.

#-------------------------question 3-----------------------------
#Imagine you have two teams. Team A is comprised of batters who, on average, get two bases on balls, 
#four singles, one double, and one home run. Team B is comprised of batters who, on average, get one base on balls, 
#six singles, two doubles, and one triple.

2.769 + 0.371*2 + 0.519*4 + 0.771*1 + 1.443 *1 #team A
2.769 + 0.371 *1 + 0.519 *6 + 0.771 *2 + 1.24 *1 #team B