#Q1: Using Bayes' theorem, calculate the probability that you have the disease if the test is positive:

#The test is positive 85% of the time when tested on a patient with the disease (high sensitivity): P(test+|disease) = 0.85
#The test is negative 90% of the time when tested on a healthy patient (high specificity): P(test-|healthy) = 0.90
#The disease is prevalent in about 2% of the community: P(disease) = 0.02

p_test_positive_disease <- 0.85
p_test_negative_healthy <- 0.90
prevalence <- 0.02

p_disease <- p_test_positive_disease*prevalence / 
  (p_test_positive_disease*prevalence + (1-p_test_negative_healthy) * (1-prevalence))

#--------------------------Q2-Q4-----------------------------
#We have a hypothetical population of 1 million individuals with the following conditional probabilities as described below:
#The test is positive 85% of the time when tested on a patient with the disease (high sensitivity): P(test+|disease) = 0.85
#The test is negative 90% of the time when tested on a healthy patient (high specificity): P(test-|healthy) = 0.90
#The disease is prevalent in about 2% of the community: P(disease) = 0.02

set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
#Q2: What is the probability that a test is positive?

# P(test+|disease) * P(disease) + P(test-|healthy) * P(healthy)
mean(test)

#Q3:What is the probability that an individual has the disease if the test is negative?

dat <- data.frame(disease=disease , test=test)
p_disease_test_negative <- ifelse(dat$test==0 & dat$disease==1, 1,0)
mean(p_disease_test_negative)

mean(disease[test==0]) #same as this

0.10*0.02/(0.10*0.02+0.90*0.98)

#Q3:What is the probability that you have the disease if the test is positive?
mean(disease[test==1])

#Q4:If the test is positive, what is the relative risk of having the disease?
test

#Q5: If the test is positive, what is the relative risk of having the disease?
#First calculate the probability of having the disease given a positive test, 
#then normalize it against the disease prevalence.

mean(disease[test==1])/mean(disease)



