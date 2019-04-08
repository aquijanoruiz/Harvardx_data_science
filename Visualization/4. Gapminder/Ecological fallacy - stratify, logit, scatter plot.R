#Ecological fallacy
rm(list=ls())
library(dslabs)
data(gapminder)

#------------------------stratify with case_when---------------------------
west<- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zeland")

gapminder <- gapminder %>%
  mutate(group = case_when(
    region %in% c("Western Europe", "Northern Europe","Southern Europe",
                  "Northern America", "Australia and New Zealand") ~ "West",
    region %in% "Northern Africa" ~ "Northern Africa",
    region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    region == "Southern Asia"~ "Southern Asia",
    region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    continent == "Africa" & region != "Northern Africa" ~ "Sub-Saharan Africa",
    region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

#------------------------use summarize to analyze information---------------------------

surv_income <- gapminder %>% 
  filter(year %in% 2010 & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income=sum(gdp)/sum(population)/365,
            infant_survival_rate = 1- sum(infant_mortality/1000*population)/sum(population))
surv_income  

#------------------------illustrate the information---------------------------

surv_income %>%
  ggplot(aes(x=income,y=infant_survival_rate,label=group,color=group)) +
  scale_x_continuous(trans = "log2", limit=c(.25,150))+
  scale_y_continuous(trans = "logit", limit=c(.875,.9981),breaks =c(.85,.90,.99,.995,.998))+ #breaks let us set the location of the axis
  geom_label(size=3,show.legend = FALSE)

#trans = "logit"
#f(p) = log p/(1-p) ---> numer of kids who survive divided by the number of kids that die
#When p is a proportion or probability, the quantity that is being logged, p divided by 1 minus p, 
#is called the odds. And the case p is the proportion of children that survive. The odds tells us 
#how many more children are expected to survive than to die.

#This scale is useful when we want to highlight differences that are near 0 or near 1.
#https://www.youtube.com/watch?v=ARfXDSkQf1Y at 3:11
