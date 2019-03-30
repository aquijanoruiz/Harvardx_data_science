#Random variable
beads <- rep(c("red","blue"),times=c(2,3))
X <- ifelse(sample(beads,1)=="blue",1,0) #generates a random variable

#Sampling model - Casino roulette example
#A roulette wheel has 18 red pockets, 18 black pockets, and 2 green ones
color <- rep(c("Black","Red","Green"),c(18,18,2)) #We create the roulette

#If red comes up, the gambler wins, and the casino loses $1, so we draw a negative 1
#Otherwise, the casino wins $1

#DEFINING COLOR
n<- 10000
X<- sample(ifelse(color=="Red",1,-1), n, replace=TRUE) #Generates 1000 random outcomes
X[1:10] #We can look at the first 10 outcomes

#WITHOUT DEFINING COLOR
X<- sample(c(-1,1),n,replace=TRUE, prob = c(9/19,10/19))
sum(X)

#The probability distribution of a random variable tells us the probability 
#of the observed value falling in any given interval

#--------------------------------
#Probability like that the income (S) is less than 0
#F(a) = Pr(S<=a)
B<- 10000
n<- 1000
S<- replicate(B,{
  X<- sample(c(-1,1),n,replace=TRUE, prob = c(9/19,10/19))
  sum(X)
})
mean(S<=0) #We se what the probability of losing money is
mean(S)
sd(S)

#Histogram
s <- seq(min(S),max(S),length=100) #creates a sequence from the lowest value to the largest value
normal_density <- data.frame(s=s,f=dnorm(s,mean(S),sd(S))) #dnorm calculates the pdf
data.frame(S=S) %>% ggplot(aes(S,..density..)) + 
  geom_histogram(color="black",binwith=40) +
  ylab("Probability") +
  geom_line(data=normal_density,mapping = aes(s,f),color="blue")
