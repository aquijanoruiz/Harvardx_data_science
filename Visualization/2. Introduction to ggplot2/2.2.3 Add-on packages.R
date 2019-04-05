#The style of a ggplot graph can be changed using the theme function
#Several themes are included as part of the ggplt2 package
#We can use other pakages with predesigned themes like ggthemes

library(ggthemes)
library(tidyverse)
library(dslabs)
data(murders)

r<- murders %>% 
  summarize(rate=sum(total)/sum(population)*10^6) %>% .$rate #to select data and carry out an operation

p<- murders %>% ggplot(aes(population/10^6,total, label=abb))+
  geom_text(nudge_x = 0.05) + #moves the text slightly to the right
  scale_x_log10() + #same as we did before but more efficient
  scale_y_log10() +
  xlab("Population in millions (log scale)") + #adds the labels
  ylab("Total of murders (log scale)") +
  ggtitle("US Gun Murders in US2010") +#adds the title name
  geom_abline(intercept = log10(r),lty=2,color="darkgrey") + #lty equals 2 changes the line type
  geom_point(aes(col=region),size=1) +  #add different colors to different regions
  scale_color_discrete(name="Region") #We want region to be in capital letters
print(p)

p + ds_theme_set()

#ggthemes
#Many other themes can be added using the package ggthemes. Among those are the theme_economist 
#that we use to make our original plot

library(ggthemes)
p + theme_economist()
p + theme_fivethirtyeight()

#ggrepel
#The add-on package ggrepel includes a geometry that adds labels ensuring that they don't 
#fall on top of each other

library(ggthemes)
library(ggrepel)
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)

murders %>% ggplot(aes(x=population/10^6, y=total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col=region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()

