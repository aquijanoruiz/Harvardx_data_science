library(Lahman)
library(tidyverse)
ds_theme_set()

#-------------------Relation between home runs and runs-------------------
Teams %>% filter(yearID %in% 1921:2001) %>%
  mutate(HR_per_game= HR/G, R_per_game=R/G) %>%
  ggplot(aes(HR_per_game,R_per_game))+
  geom_point(alpha=0.5) #The plot shows a strong association: teams with more HRs tend to score more runs.

#-------------------Relation between stolen bases and runs-------------------
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) +
  geom_point(alpha = 0.5) #Here the relationship is not as clear.

#-------------------Relation between base on balls and runs-------------------

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5) 

#Here again we see a clear association. But does this mean that increasing a team’s BBs causes an increase
#in runs? One of the most important lessons you learn in this book is that assocation is not causation.

?Teams
