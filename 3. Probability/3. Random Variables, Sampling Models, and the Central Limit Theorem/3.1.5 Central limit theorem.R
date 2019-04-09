#Central Limit Theorem
#When the number of independent draws--also called sample size-- is large, 
#the probability distribution of the sum of these draws is approximately normal.
#average -> expected value
#standard deviation -> standard error

#Expected value
#E[x]=u 
#A random variable will vary around an expected value in a way that if you take the 
#average of many, many draws, the average of the draws will approximate the expected value.
#Getting closer and closer the more draws you take.

#In the casino example the expected value was
(20+-18)/38

#We can get the same using a monte carlo
B<- 10^6
X<- sample(c(-1,1),B,replace=TRUE,prob = c(9/19,10/19))
mean(X) #This value approximates to (20+-18)/38 = 0.05

#average of the binomial distribution
#ap +b(1-p) , which can be deducted by
#(n * a * p + n * b * (1-p))

#expected value of the sum of draws
#number of draws x average of the numbers in the urn
#So if 1,000 people play roulette, the Casino expects to win,
#on average, 1,000 times $0.05, which is $50.

#standard error
#The standard error, or SE[X] for short, gives us
#an idea of the size of the variation around the expected value
#in a binomial it is |b-a| sqrt(p(1-p))
#in the roulette example it's |1-(-1)|sqrt(10/19 *9/19)
(1-(-1))*sqrt(90)/19

#So when 1,000 people bet on red, the Casino is expected to win $50 with a standard error of $32.
n<- 1000
sqrt(n)*2*sqrt(90)/19

#Probability of losing money
mu<- n*(20-18)/38
se<- sqrt(n)*2*sqrt(90)/19
pnorm(0,mu,se)
