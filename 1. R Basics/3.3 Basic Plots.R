#SCATTER PLOTS
library(dslabs)
data("murders")

#PLOT
population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total
plot(population_in_millions,total_gun_murders) #to see the coorelation betwee the murder rate and the population

#HISTOGRAMS
murder_rate <- murders$total/murders$population*100000
murders <- mutate(murders,murder_rate)
hist(murder_rate) #to get an idea of the distribution of the murder rate among states

murders$state[which.max(murder_rate)] #to find out which is the state that has such high murder rate

#BOXPLOT
boxplot(murder_rate~region, data=murders)

# Exercises

# 1
# Many states have populations below 5 million and are bunched up in the plot. 
# We may gain further insights from making this plot in the log scale.
# Transform population using the log10 transformation and save to object log10_population
log10_population<- log(murders$population,base=10)
# Transform total gun murders using log10 transformation and save to object log10_total_gun_murders
log10_total_gun_murders<- log(total_gun_murders,base=10)

# 2
# Create a scatterplot with the log scale transformed population and murders 
plot(log10_population,log10_total_gun_murders)
# Store the population in millions and save to population_in_millions 
population_in_millions <- murders$population/10^6
# Create a histogram of this variable
hist(population_in_millions)

# 3
# Create a boxplot of state populations by region for the murders dataset
boxplot(population~region, data=murders)

