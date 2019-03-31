#An American roulette wheel has 18 red, 18 black, and 2 green pockets. 
#Each red and black pocket is associated with a number from 1 to 36. 
#The two remaining green slots feature "0" and "00". Players place bets 
#on which pocket they think a ball will land in after the wheel is spun. 
#Players can bet on a specific number (0, 00, 1-36) or color (red, black, or green).

#What are the chances that the ball lands in a green pocket?

# The variables `green`, `black`, and `red` contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green<- green/(green + black + red)

# Print the variable `p_green` to the console
p_green

#-----------------2--------------------
#In American roulette, the payout for winning on green is $17. 
#This means that if you bet $1 and it lands on green, you get $17 as a prize.

#Create a model to predict your winnings from betting on green one time.
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Create a model to predict the random variable `X`, your winnings from betting on green. Sample one time.
X<-sample(c(17,-1),1,prob=c(p_green,p_not_green))

# Print the value of `X` to the console
X
#-----------------3--------------------
#Now, compute the expected value of X, the random variable you generated previously.
# Calculate the expected outcome if you win $17 if the ball lands on green and 
#you lose $1 if the ball doesn't land on green
17*p_green+(-1*p_not_green)

#-----------------4--------------------
#Now, compute the standard error of that random variable, which represents a single outcome
#after one spin of the r
oulette wheel
abs(17-(-1))*sqrt(p_green*p_not_green) #abs takes the abosolute value

#-----------------5--------------------
#Now create a random variable S that sums your winnings after betting on green 1,000 times.
# Define the number of bets using the variable 'n'
n<1000

# Create a vector called 'X' that contains the outcomes of 1000 samples
X<- sample(c(17,-1),n,replace=TRUE,prob=c(p_green,p_not_green))

# Assign the sum of all 1000 outcomes to the variable 'S'
S<-sum(X)

# Print the value of 'S' to the console
S
#-----------------6--------------------
#What is the expected value of S?
# Calculate the expected outcome of 1,000 spins if you win $17 when the ball lands on green 
#and you lose $1 when the ball doesn't land on green
n*(17*p_green+(-1)*p_not_green)
#-----------------7--------------------
#What is the standard error of S?
sqrt(n)*abs(17-(-1))*sqrt(p_green*p_not_green)
