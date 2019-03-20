#Say you???ve drawn 5 balls from the a box that has 3 cyan balls, 5 magenta balls, and 7 yellow balls, 
#with replacement, and all have been yellow. What is the probability that the next one is yellow?

cyan <- 3
magenta <- 5
yellow <- 7

# Assign the variable 'p_yellow' as the probability that a yellow ball is drawn from the box.
p_yellow <- yellow/(cyan+magenta+yellow)
# Using the variable 'p_yellow', calculate the probability of drawing a yellow ball on the sixth draw. 
#Print this value to the console.
p_yellow

#--------------------
#If you roll a 6-sided die once, what is the probability of not seeing a 6? If you roll a 6-sided die six times, 
#what is the probability of not seeing a 6 on any roll?

# Assign the variable 'p_no6' as the probability of not seeing a 6 on a single roll.
p_no6 <- 5/6

# Calculate the probability of not seeing a 6 on six rolls using `p_no6`.
p_no6^6

#--------------------
#Two teams, say the Celtics and the Cavs, are playing a seven game series. The Cavs are a better team 
#and have a 60% chance of winning each game.What is the probability that the Celtics win at least one game? 
#Remember that the Celtics must win one of the first four games, or the series will be over!

# Assign the variable `p_cavs_win4` as the probability that the Cavs will win the first four games of the series.
p_cavs_win4 <- 0.6^4

# Using the variable `p_cavs_win4`, calculate the probability that the Celtics win at least one game in the first four games of the series.
1 - p_cavs_win4

#--------------------
#Create a Monte Carlo simulation to confirm your answer to the previous problem by estimating how frequently the Celtics 
#win at least 1 of 4 games. Use B <- 10000 simulations.

# This line of sample code simulates four independent random games where the Celtics either lose or win. Copy this sample code to use within the `replicate` function.
simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))

# The variable 'B' specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `celtic_wins` that replicates two steps for B iterations: (1) using the sample code to generate a four-game series `simulated_games`, 
#then (2) determining whether the simulated series contains at least one win for the Celtics.
B <- 10000
celtic_wins <- replicate(B,{
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(simulated_games=="win")
})
celtic_wins
mean(celtic_wins)
# Calculate the frequency out of B iterations that the Celtics won at least one game. Print your answer to the console.