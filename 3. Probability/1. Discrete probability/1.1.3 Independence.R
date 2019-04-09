bead <- rep( c("red","blue"), times=c(2,3)) #replicate events of vectors and lists
bead
x <- sample(bead, 5) #picks five one at random
x[2:5] #These events are not independent
x

#Conditional probability
#Pr(Card2 is a King|Card1 is a king)=3/51 "|" means this is conditional on

#Conditional probability in indipendent events
#Pr(A|B)=Pr(A) The probability of A given B is the same as the probability of A

#Multiplication rule
#Pr(A and B)=Pr(A)*Pr(A|B)
#The probability of A and B is equal to the probability of A multiplied by the probability of
#B given that A already happened

#Pr(A and B and C)=Pr(A)*Pr(A|B)*Pr(C|A and B)

#Sampling without replacement (without puting back)
#Two balls will be drawn at random from a box containing: 3 cyan balls, 5 magenta balls, and 7 yellow balls.
#You take the second draw without returning the first draw to the box. We call this sampling without replacement.
#What is the probability that the first draw is cyan and that the second draw is not cyan?
3/15*12/14

#Sampling with replacement (puting back)
#This time, after taking the first draw and recording the color, return it back to the box and shake the box. 
#We call this sampling with replacement. What is the probability that the first draw is cyan and that the second draw is not cyan?
3/15*12/15

#Homework
#Problem 1
cyan <- 3
magenta <- 5
yellow <- 7

# Assign a variable `p` as the probability of choosing a cyan ball from the box
p<- cyan/(cyan+magenta+yellow)

# Print the variable `p` to the console
p

#Problem 2
# 'p' is defined as the probability of choosing a cyan ball from a box containing: 3 cyan balls, 5 magenta balls, and 7 yellow balls.
# Using variable `p`, calculate the probability of choosing any ball that is not cyan from the box
1-p

#Problem 3
# The variable `p_1` is the probability of choosing a cyan ball from the box on the first draw.
p_1 <- cyan / (cyan + magenta + yellow)

# Assign a variable `p_2` as the probability of not choosing a cyan ball on the second draw without replacement.
p_2 <- (magenta+yellow)/(cyan+magenta+yellow-1)

# Calculate the probability that the first draw is cyan and the second draw is not cyan using `p_1` and `p_2`.
p_1*p_2

#Problem 4
# The variable 'p_1' is the probability of choosing a cyan ball from the box on the first draw.
p_1 <- cyan / (cyan + magenta + yellow)

# Assign a variable 'p_2' as the probability of not choosing a cyan ball on the second draw with replacement.
p_2 <- 1 - cyan / (cyan + magenta + yellow)

# Calculate the probability that the first draw is cyan and the second draw is not cyan using `p_1` and `p_2`.
p_1*p_2