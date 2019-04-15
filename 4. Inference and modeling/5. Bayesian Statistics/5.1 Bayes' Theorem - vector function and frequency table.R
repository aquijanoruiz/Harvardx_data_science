#Bayes' Theorem
#| means given
#Prob(+|D=1)=0.99 The probability of a positive test given that you have the disease, D equals 1, is 0.99.
#Prob(-|D=0)=0.99 The probability of a negative test given that you don't have the disease, D equals 0, is 0.99.
#Prob(D=1|+)=? The probability that you have the disease given that the test is positive

#The cystic fibrosis rate is 1 in 3,900, which implies that the probability that D equals 1 is 0.00025.
1/3900

#Pr(A|B) = Pr(A and B)/Pr(B) To the probability of them both happening divided by the probability of B happening.
#        = Pr(A) * Pr(B|A)/Pr(B) because Pr(A and B) = Pr(A) * Pr(B|A)

#This is going to be useful because sometimes we know the probability of A given B and not the probability of B given A,
#as is the case in the cystic fibrosis example.

#Prob(D=1|+) =            Pr(D=1) * Pr(+|D=1) 
#             -------------------------------------------
#                               Pr(+)

#Prob(D=1|+) =            Pr(D=1) * Pr(+|D=1) 
#             -------------------------------------------
#             Pr(+|D=1) * Pr(D=1) + Pr(+|D=0) * Pr(D=0)

#Prob(D=1|+) =            0.00025 * 0.99
#             -------------------------------------------
#                     0.99 * 0.0025 + 0.01 * (1-0.00025)


#Prob(D=1|+) =            0.00025 * 0.99
#             -------------------------------------------
#                     0.99 * 0.0025 + 0.01 * 0.99975

#0.02

#This says that despite the test having 99% accuracy, the probability of having the disease 
#given a positive test is only 2%.

#-------------------------------Monte Carlo simulation------------------------------
prev<- 1/3900 #prevalence of the disease
N<- 100000
outcome<- sample(c("Disease","Healthy"),N, replace=TRUE,prob = c(prev,1-prev))

N_D <- sum(outcome=="Disease")
N_D #Only very few people have the disease

N_H <- sum(outcome=="Healthy")
N_H #There are a lot of healthy people
#This makes the probability that we see some false positives quite high. There are so many people without the disease
#that are getting the test that, although it's rare, we were going to get a few people getting a positive test despite them
#being healthy.

N_H*0.01 #0.01 is the probability of having a positive test despite not having the disease
#These are the false positive

accuracy <- 0.99
test <- vector("character", N) #we create an empty vector

test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace = TRUE,
                                     prob = c(accuracy, 1 - accuracy)) #This fills the table

test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace = TRUE,
                                     prob = c(accuracy, 1 - accuracy)) #we use the same 0.99 because Prob(+|D=1) and Prob(-|D=0) are both 0.99

#For each of the diseased and healthy people, we're going to sample either a correct or incorrect test
#with the appropriate probabilities, a very high probability of the correct test.

test[outcome == "Disease"]
test[outcome == "Healthy"]
sum(test[outcome == "Healthy"]=="+") #This tells us the number of false positives

#---------------------Frequency table--------------------------
table(outcome,test)
table(outcome) 
test #here we see the table already filled
