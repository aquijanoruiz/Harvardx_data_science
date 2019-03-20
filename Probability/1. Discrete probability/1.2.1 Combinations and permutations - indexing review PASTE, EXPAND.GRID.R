#Constructing a deck of card using R
number <- "Three"
suit <- "Hearts"
paste(number, suit) #Can create strings by joining smaller strings

paste(letters[1:5],as.character(1:5))

expand.grid(pants= c("blue", "black"), shirt= c("white", "grey", "plaid")) #Give us all the combinations of two lists
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(numbers=numbers, suits=suits) #This creates a data frame with two columns 
deck
deck <- paste(deck$numbers, deck$suit) #This makes a string (not data frame with two columns)
deck

#Vector that contains the four ways we can get a King
kings <- paste("King", suits)
kings
mean(deck %in% kings) #Tells us what proportion of the deck is one of these cards

#The conditional probability of the second card being a king,given that the first was a king

#--------------------
#Permutations
library(gtools)
permutations(5,2) #From 1 to five, pick all two possible numbers at random
#Note that 1, 1; 2, 2; and 3, 3 do not appear, because once we pick a number,
#it can't appear again

#Create random phone numbers
all_phone_numbers <- permutations(10,7,v=0:9) #v=0:9 means we want the vector digits to go from 0 to 9 rather than from 1 to 10
n <- nrow(all_phone_numbers)
index <- sample(n,5) #it randomly chooses five numbers
all_phone_numbers[index,] #it shows us five random phone numbers
all_phone_numbers

#--------------------
#Permutations with card deck
hands <- permutations(52,2, v=deck) #v means the vector we want those 52 elements to be
hands #Shows all the possible obtions of two cards
first_card <- hands[,1] #This code grabs the first column
second_card <- hands[,2] #This code grabs the second column

#To check how many cases have a first card as a King
sum(first_card %in% kings)

#What fraction of these 204 have also a king in the second card
sum(first_card %in% kings & second_card %in% kings)/ sum(first_card %in% kings)
first_card %in% kings & second_card %in% kings #This tells how many permutations have a king in the first hand
#and also have a king in the second hand

#Same as if we compute the proportions instead of the total
mean(first_card %in% kings & second_card %in% kings)/ mean(first_card %in% kings)

#--------------------
#Combination (WHERE THE ORDER DOES NOT MATTER) 
permutations(5,2) #1 & 2, and 2 & 1 are different elements (ORDER DOES MATTER)
combinations(5,2) #1 & 2, and 2 & 1 are the same elements (ORDER DOES NOT MATTER)

#The probability of getting a natural 21 in blackjack, which is when you get an ace and a face card or a 10
#Order doesn't matter since if we get an A and a king, or king and an A, it's still a 21

aces <- paste ("Ace", suits) #We need a vector that includes all the Aces
facecard <- c("King", "Queen", "Jack", "Ten") #We need a vector that includes all the face cards and the 10
facecard <- expand.grid(number=facecard, suit=suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52,2, v=deck) #all the combinations of picking 2 cards out of 52

mean(hands[,1] %in% aces & hands[,2] %in% facecard)
#Which is the same as:
mean(hands[,1] %in% aces & hands[,2] %in% facecard |
       hands[,2] %in% aces & hands[,1] %in% facecard ) #probability of a natural 21

#When can use a Monte Carlo to draw cards over and over and see how many 21s we get
hand <- sample(deck,2)
hand #We check if this is an Ace and a facecard or a 10

#We repeat this over and over
B <- 10000
results <- replicate(B, {
  hand <- sample(deck,2)
  (hand[1] %in% aces & hand[2] %in% facecard)|
  (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)
