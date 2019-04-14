rm(list=ls())
library(dslabs)
library(dplyr)
library(ggplot2)
data(us_contagious_diseases)
str(us_contagious_diseases) #shows the structure of the data base

dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease== "Measles") %>%
  mutate(rate=count/population*10000) %>%
  mutate(state=reorder(x=state,X=rate))

#-------------------line chart-------------------------------

dat %>% filter(state=="California") %>%
  ggplot(aes(x=year,y=rate)) +
  geom_line() +
  ylab("Cases per 10,000")+
  geom_vline(xintercept = 1963,col ="blue") #We add a vertical line as it was the year when the vaccine was introduced

#--------------------geom_tile-------------------------------
#When choosing colors to quantify a numeric variable, we choose between: sequential and diverging.
#Sequential palettes are suited for data that goes from high to low.

library(RColorBrewer)
display.brewer.all(type="seq")

#On the other hand, diverging colors are used to represent values that verge from a center.
#We put equal emphasis on both ends of the data range, higher than the center and lower than the center.

library(RColorBrewer)
display.brewer.all(type="div")

dat %>% ggplot(aes(year, state, fill=rate))+
  geom_tile(color="grey50")+
  scale_x_continuous(expand=c(0,0))+ #expand is used for the years to cover the whole graph from left to right
  scale_fill_gradientn(colors=brewer.pal(9,"Reds"),trans="sqrt") + #Square root transformation avoids having the really high counts dominate the plot
  geom_vline(xintercept = 1963,col ="blue") + #We add a vertical line as it was the year when the vaccine was introduced
  theme_minimal() +#takes out the border
  theme(panel.grid = element_blank())+
  ggtitle("Measles") +
  ylab("") +
  xlab("")

#--------------------frequency / summary table-------------------------------
avg <- us_contagious_diseases %>%
  filter(disease=="Measles") %>% group_by(year) %>% #group by year
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)
avg 
#--------------------different plot with geom_line-------------------------------
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(mapping= aes(year, rate, group = state), #we group by state, not by color
            color = "grey50", show.legend = FALSE, alpha = 0.2, size = 1) + #show.legend takes out the legend, alpha makes the line transpartent, size makes the lines thicker or less thick
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, color = "black")+ #creates an additional line
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) + #we assign a square root transformation and specific breaks (grid lines)
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label="US average"),
            color="black")+ #to create a text and locate it in the graph
  geom_vline(xintercept=1963, col = "blue")

