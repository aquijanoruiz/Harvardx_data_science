# Two teams, say the Cavs and the Warriors, are playing a seven game championship series. 
#The first to win four games wins the series. The teams are equally good, so they each have a 50-50 chance of winning each game. 
#If the Cavs lose the first game, what is the probability that they win the series?

# Assign a variable 'n' as the number of remaining games.
n<- 6

# Assign a variable `outcomes` as a vector of possible game outcomes, where 0 indicates a loss and 1 indicates a win for the Cavs.
outcomes<- c(0,1)

# Assign a variable `l` to a list of all possible outcomes in all remaining games. Use the `rep` function on `list(outcomes)` to create list of length `n`.
l<- rep(list(outcomes),n)

# Create a data frame named 'possibilities' that contains all combinations of possible outcomes for the remaining games.
possibilities<- expand.grid(l) # The expand.grid function has functionality beyond what is shown in the video. When you run it on a list,
#it creates all possible combinations of elements in the list.

# Create a vector named 'results' that indicates whether each row in the data frame 'possibilities' contains enough wins for the Cavs to win the series.
rowSums(possibilities)>=4 #Identifies which combinations of game outcomes result in the Cavs winning the number of games necessary to win the series.
results<- mean(rowSums(possibilities)>=4)
results

#--------------------
# The Cavs and the Warriors - Monte Carlo
# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `results` that replicates for `B` iterations a simulated series and determines whether that series contains at least four wins for the Cavs.
results<- replicate(B,{
  results<- sample(c(0,1),6,replace=TRUE) #Picks six at random
  sum_results<- sum(results)
  results_higher_or_equal_4<- sum_results>=4 #Sees if the sum of the results is higher or equal to 4
})
mean(results)

#--------------------
# A and B play a series - part 1
# Two teams, A and B, are playing a seven series game series. Team A is better than team B and has a p>0.5 chance of winning each game.
# Use the function sapply to compute the probability, call it Pr of winning for p <- seq(0.5, 0.95, 0.025).
# Then plot the result plot(p, Pr)

# Let's assign the variable 'p' as the vector of probabilities that team A will win.
p <- seq(0.5, 0.95, 0.025) #Creates a sequence from 0.5 to 0.95 with intervals of 0.025

# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p)) #7 because there are seven games
    sum(b_win)>=4 #4 because they need to win 4
  })
  mean(result)
}

# Apply the 'prob_win' function across the vector of probabilities that team A will win to determine the probability that team B will win. Call this object 'Pr'.
Pr<- sapply(p,prob_win)

# Plot the probability 'p' on the x-axis and 'Pr' on the y-axis.
plot(p,Pr)
#--------------------

# Repeat the previous exercise, but now keep the probability that team A wins fixed at p <- 0.75 and compute the probability for different series lengths.
# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(N, p=0.75){ #N is defined as the number of matches
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2 #OJO: If the number of matches is 7, to win you must win (7+1)/2 games, which is 4 in this case
  })
  mean(result)
}

# Assign the variable 'N' as the vector of series lengths. Use only odd numbers ranging from 1 to 25 games.
# We use odd number like 'N' because the number of matches can't be an even number. For example, if it's 8, there can be a tie
N<- seq(1,25,2)

# Apply the 'prob_win' function across the vector of series lengths to determine the probability that team B will win. Call this object `Pr`.
Pr<- sapply(N,prob_win)

# Plot the number of games in the series 'N' on the x-axis and 'Pr' on the y-axis.
plot(N,Pr)
