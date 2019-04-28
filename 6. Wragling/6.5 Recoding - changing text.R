#Here is an example that shows how to rename countries with long names:
library(dslabs)
data("gapminder")

gapminder %>%
    filter(region == "Caribbean") %>%
    ggplot(aes(year, life_expectancy, color = country)) +
    geom_line()

#we can see the countries with long names
gapminder %>%
  filter(region == "Caribbean") %>%
  filter(str_length(country) >= 12) %>% #this shows the countries whose names have more than 12 characters
  distinct(country)

#-----------------------------------recode function----------------------------
#we can use the recode function to change the names of these countries along with the function mutate

gapminder %>% filter (region=="Caribbean") %>%
  mutate(country=recode(country,
                        `Antigua and Barbuda` = "Barbuda",
                        `Dominican Republic` = "DR",
                        `St. Vincent and the Grenadines` = "St. Vincent",
                        `Trinidad and Tobago` = "Trinidad")) %>% 
  ggplot(aes(x=year,y=life_expectancy,color=country)) +
  geom_line()
