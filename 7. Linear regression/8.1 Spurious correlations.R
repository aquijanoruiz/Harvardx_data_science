#------------------------spurious correlations--------------------------
#We are going to learn how we can use a Monte Carlo simulation to create two random groups of data that have a high coorelation

N<- 25
g <- 1000000
rep(1:g, each = N) ##Replicate elements of vectors and lists: we repeat a number 25 times until and we repeat this process 1 million times.
#We get 1 million groups, each with 25 observations

sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N * g), y = rnorm(N * g)) #we get a table with 25 million columns and 25 million simulated z values
head(sim_data)

res <- sim_data %>%
  group_by(group) %>% #we group by the column "group" 
  summarize(r = cor(x, y)) %>% #we calculate the coorelation of the two groups of data x and y for each of the 1 million groups
  arrange(desc(r)) #we ourder them in descending order

head(res)

#----------------------making a plot of the group with the highest correlation-----------------------
sim_data %>% filter(group==res$group[which.max(res$r)]) #this is the group with the highest coorelation

#Now let's make a plot with this group
sim_data %>% filter(group==res$group[which.max(res$r)]) %>%
  ggplot(aes(x,y)) +
  geom_point() +
  geom_smooth(method="lm") #the linear model also helps confirm that these two variables are highly correlated.

res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")

#----------------------making a plot of the group with the highest correlation-----------------------
res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")
#we can see from this graph that the average correlation is 0, with a standard error of 0.204, the largerst coorelation 
#should be close to 1.

#---------------------------using a linear model-----------------------------
sim_data %>% filter(group==res$group[which.max(res$r)]) %>%
  do(tidy(lm(y~x, data=.))) #remmember that everytime we are grouping or filtering we need to use the do function when we use lm

#We see that the p value is very small. However, this coorelation is just random.