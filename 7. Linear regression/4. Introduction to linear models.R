#----------------------------confunding-----------------------------
#Previously, we noted a strong relationship between Runs and BB. If we find the regression line for predicting
#runs from bases on balls, we a get slope of:


library(tidyverse)
library(Lahman)

data(Teams)
?Teams

#----------------------------regressiin slope for the bases on balls----------------------
bb_slope <- Teams %>%
  filter(yearID %in% 1961:2001 ) %>%
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% #we calculate bases on balls per game and runs per game
  lm(R_per_game ~ BB_per_game, data = .) %>%
  .$coef %>%
  .[2]

bb_slope #the slope is 0.74
#So does this mean that if we go and hire low salary players with many BB, and who therefore increase the
#number of walks per game by 2, our team will score 1.5 more runs per game? Association is not causation.

#----------------------------regressiin slope for the singles----------------------

singles_slope <- Teams %>%
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>% #to calculate singles per games: (H-HR-X2B-X3B)/G
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef %>%
  .[2]

singles_slope #we get 0.45, which is a lower value than what we obtain for BB.

#-----------------calculating the correlations betweeen BB, singles and home runs-----------------

Teams %>%
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))

#we get a high correlation between BB and HR. It turns out that pitchers, afraid of HRs, will sometimes 
#avoid throwing strikes to HR hitters.

#----------------------------------multivariate regression-----------------
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), #we round to one decimal (stratify home runs per game to the closest 10th)
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)

#--------------------------making a scatter plot for each strata--------------

dat %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata) #we make different graphs. Each graph represents a strata.

#Remember that the regression slope for predicting runs with BB was 0.7. Once we stratify by HR, these
#slopes are substantially reduced.

dat %>%
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))

#--------------------------stratifying for bases on balls--------------

#Our understanding of the application tells us that HR cause BB but not the other way around, we
#can still check if stratifying by BB makes the effect of BB go down.

dat <- Teams %>% filter(yearID %in% 1961:2001) %>% #we firts prepare the data we will need for the graph
  mutate(BB_strata = round(BB/G, 1), #we round to one decimal (stratify home runs per game to the closest 10th)
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9)

dat %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

dat %>%
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game)) #this are the slopes for each strata

#---------------------multivariate regression---------------------
#E[R|BB=x1,HR=x2] = B[0] + B[1]x1 + B[2]x2

#In this analysis, referred to as multivariate regression, you will often hear people say that the BB slope B[1]
#is adjusted for the HR effect. If the model is correct then confounding has been accounted for.
