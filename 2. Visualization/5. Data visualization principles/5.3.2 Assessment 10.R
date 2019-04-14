rm(list=ls())
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)

#------------------------1-------------------------------
#Modify the tile plot to show the rate of smallpox cases instead of measles cases.
#Exclude years in which cases were reported in fewer than 10 weeks from the plot.

the_disease = "Smallpox"
dat <- us_contagious_diseases %>% 
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease) %>% 
  mutate(rate = count / population * 10000) %>% 
  mutate(state = reorder(state, rate)) %>%
  filter(!weeks_reporting<10)

dat %>% ggplot(aes(year, state, fill = rate)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
  theme_minimal() + 
  theme(panel.grid = element_blank()) + 
  ggtitle(the_disease) + 
  ylab("") + 
  xlab("")

summary(us_contagious_diseases)

#------------------------2-------------------------------

the_disease = "Smallpox"
dat <- us_contagious_diseases %>% 
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease) %>% 
  mutate(rate = count / population * 10000) %>% 
  mutate(state = reorder(state, rate)) %>%
  filter(!weeks_reporting<10)

avg <- us_contagious_diseases %>%
  filter(disease==the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat %>% ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
  geom_vline(xintercept=1963, col = "blue")

#------------------------3-------------------------------
#Use a different color for each disease.

us_contagious_diseases %>% filter(state=="California") %>% 
  group_by(year, disease) %>%
  filter(!weeks_reporting<10) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, color=disease)) + 
  geom_line()

#------------------------4-------------------------------
us_contagious_diseases %>%
  filter(!is.na(population)) %>%
  group_by(year,disease) %>% #selects the columns you want
  summarize(us_rate=sum(count,na.rm=TRUE)/sum(population,na.rm=TRUE)*10000) %>%
  ggplot(aes(x=year,y=us_rate,color=disease)) +
  geom_line()