#Let's simulate a dataset for 100 schools. First, let's simulate the number of students in each school, 
#using the following code:
rm(list = ls())
set.seed(1986)
n <- round(2^rnorm(1000, 8, 1)) #this is the number of students

#Now let's assign a true quality for each school that is completely independent from size. 
#This is the parameter we want to estimate in our analysis.

set.seed(1)
mu <- round(80 + 2*rt(1000, 5)) #rt generates random variables using the t distribution. 5 referes to the dfs
range(mu)
schools <- data.frame(id = paste("PS",1:100), #paste is used to concatenate strings
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

#We can see the top 10 schools using this code: 

schools %>% top_n(10, quality) %>% arrange(desc(quality))

#Now let's have the students in the school take a test. There is random variability in test taking, 
#so we will simulate the test scores as normally distributed with the average determined by the school 
#quality with a standard deviation of 30 percentage points.

set.seed(1)
scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})

class(scores) #we created a list with random data according to the number of students in each school

schools <- schools %>% mutate(score = sapply(scores, mean))

#-------------------------------Q1-----------------------------
#What are the top schools based on the average score? Show just the ID, size, and the average score.

#Report the ID of the top school and average score of the 10th school.

#What is the ID of the top school?

schools %>% top_n(10, score) %>% arrange(desc(score)) %>% knitr::kable()

#-------------------------------Q2-----------------------------

#What is the median school size overall?
schools %>% arrange(desc(score)) %>%
  summarize(median(size))

#What is the median school size of the of the top 10 schools based on the score?
schools %>% top_n(10, score) %>% arrange(desc(score)) %>%
  summarize(median(size))

#-------------------------------Q3-----------------------------
#According to this analysis, it appears that small schools produce better test scores than large schools. 
#Four out of the top 10 schools have 100 or fewer students. But how can this be? We constructed the 
#simulation so that quality and size were independent. Repeat the exercise for the worst 10 schools.

#What is the median school size of the bottom 10 schools based on the score?

schools %>% arrange(score) %>%
  slice(1:10) %>% summarize(median(size))

#-------------------------------Q4-----------------------------
#From this analysis, we see that the worst schools are also small. Plot the average score versus school 
#size to see what's going on. Highlight the top 10 schools based on the true quality. Use a log scale to transform for the size.


schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2) #to highlight points in the graph

#-------------------------------Q5-----------------------------
#Let's use regularization to pick the best schools.

overall <- mean(sapply(scores, mean))
alpha <- 25

schools$penalization <- sapply(1:nrow(schools), function(i){
  return(sum(scores[[i]]-overall)/(schools$size[i] + alpha))
})

schools$reg_score <- overall + schools$penalization

schools %>% top_n(10, reg_score) %>% arrange(desc(reg_score))
#---------------------This is how Rafa does it

alpha <- 25
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

#-------------------------------Q6-----------------------------
alphas <- 1:1000
rmse_no_reg <- sum((schools$quality - schools$score)^2)/100 #this is the rmse without the use of regularization

rmse_25 <- sum((schools$quality - schools$reg_score)^2)/100 #this is the rmse from Q5

rmses <- sapply(alphas, function(a){
  reg_score <- sapply(1:nrow(schools), function(i){
    return(overall + sum(scores[[i]]-overall)/(schools$size[i] + a))
  })
  return(mean((schools$quality - reg_score)^2))
})

qplot(alphas, rmses)
data.frame(alpha=alphas,rmse=rmses) %>% filter(rmse==min(rmse)) #128 is the alpha that gives the lowest rmse

min_alpha <- alphas[which.min(rmses)]
#-------------------------------Q7-----------------------------

alpha <- min_alpha

schools$penalization <- sapply(1:nrow(schools), function(i){
  return(sum(scores[[i]]-overall)/(schools$size[i] + alpha))
})

schools$reg_score <- overall + schools$penalization

schools %>% top_n(10, reg_score) %>% arrange(desc(reg_score))

#-------------------------------Q8-----------------------------

alphas <- 10:250

rmses_Q8 <- sapply(alphas, function(a){
  reg_score <- sapply(1:nrow(schools), function(i){
    return(sum(scores[[i]])/(schools$size[i] + a))
  })
  return(mean((schools$quality - reg_score)^2))
})

qplot(alphas, rmses_Q8)
data.frame(alpha=alphas,rmse=rmses_Q8) %>% filter(rmse==min(rmses_Q8)) #128 is the alpha that gives the lowest rmse

min_alpha_Q8 <- alphas[which.min(rmses_Q8)]

alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  mean((score_reg - schools$quality)^2)
})
plot(alphas, rmse)
alphas[which.min(rmse)] 