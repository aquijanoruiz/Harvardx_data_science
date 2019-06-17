#-----------------contents-------------------
#1. Preparing the data
#2. First model (only averages)
#3. Second model (averages + movie effects)
#4. Third model (averages + movie effects + user effects)
#5. Regularization (motivation) Mistakes made
#6. Penalized least squares
#6.1 Comparing b_i using penalized least squares with normal with normal least squares
#6.2 Calculating lse using penalized ls
#6.3 Choosing the penalty term (lamda)
#7. Calculating b_u (user effect) using penalized ls
#8. Matrix factorization
#8.1 Creating the matrix
#8.2 Analyzing the residuals
#8.3 Coorelation between resiguals
#8.4 Factorization (an example)
#8.5 Factorization (an more complicated example)
#8.6 Correlation (using the real data)
#9. Factorization/Descomposition (using the real data)
#9.1 ploging principal components (using the real data)

#-----------------1. Preparing the data---------------
##  Loading the needed packages
rm(list = ls())
library(dslabs)
library(tidyverse)
library(caret)

##  Partitioning the data

data("movielens")
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

##  Creating a formula to calculate the RMSE

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#-----------------2. First model (only averages)---------------

# Y_u,i = ?? + error_u,i

mu_hat <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu_hat)
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

#-----------------3. Second model (averages + movie effects)---------------

#Y_u,i = ?? + bi + Error_u,i
mu <- mean(train_set$rating) 

movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))

#-----------------4. Third model (averages + movie effects + user effects)---------------

#Y_u,i = ?? + bi + bu + Error_u,i

user_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))

#-----------------5. Regularization (motivation) Mistakes made---------------
test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>% #we substract the average and the movie effect to get the residual
  arrange(desc(abs(residual))) %>% 
  select(title,  residual) %>% slice(1:10) %>% knitr::kable()

#We create a database that connects movieId to movie title.

movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct() #this function removes the duplicated data from the table

##The 10 best films according to our estimate (this does not make much sense)
movie_avgs %>% count(movieId) %>%
  left_join(movie_avgs) %>% 
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i, n) %>%
  slice(1:10)

##The worst films according to our estimate (this does not make much sense)
movie_avgs %>% count(movieId) %>%
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>%
  select(title, b_i, n) %>%
  slice(1:10)

#The supposed ???best??? and ???worst??? movies were rated by very few users, in most cases just 1.
#Regularization permits us to penalize large estimates that are formed using small sample sizes. It has
#commonalities with the Bayesian approach that shrunk predictions described in Section 17.4.


#-----------------6. Caculating b_i using penalized least squares---------------
#To calcuculate the penalized b_i, we use the formula in chapter 34.9.2 Penalized Least Squares
lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) #This is the formula we will use to calculate b_i

movie_reg_avgs #these are the movies with their respective penalized b_i

#-----------------6.1 Comparing b_i using penalized least squares with normal with normal least squares---------------
#We can plot the two models
data_frame(original = movie_avgs$b_i,
           regularlized = movie_reg_avgs$b_i,
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) +
  geom_point(shape=1, alpha=0.5)

##The top 10 best movies
train_set %>%
  count(movieId) %>%
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i, n) %>%
  slice(1:10)

##The top 10 worst movies
train_set %>%
  count(movieId) %>%
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>%
  select(title, b_i, n) %>%
  slice(1:10)

#-----------------6.2 Calculating lse using penalized ls---------------

predicted_ratings <- test_set %>%
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

model_3_rmse <- RMSE(predicted_ratings, test_set$rating) #we use the rmse formula we created before
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",
                                     RMSE = model_3_rmse))

rmse_results 

#-----------------6.3 Choosing the penalty term (lamda)---------------
#Note that lamda is a tuning parameter. We can use cross-validation to choose it.

lambdas <- seq(0, 10, 0.25)

mu <- mean(train_set$rating)

just_the_sum <- train_set %>%
  group_by(movieId) %>%
  summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>%
    left_join(just_the_sum, by='movieId') %>%
    mutate(b_i = s/(n_i+l)) %>% #s is the resulta of sum(rating - mu) that we got from the previous calculation just_the_sum
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating)) #this is the function we want to return 
})

qplot(lambdas, rmses)
lambdas[which.min(rmses)]

#-----------------7. Calculating b_u (user effect) using penalized ls---------------

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>% #we join this data with the b_i data frame we created in then lines above
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred #we pull this data
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

#-----------------8. Matrix factorization---------------
#Matrix factorization is motivated by the fact that variation related to the fact that groups of movies 
#have similar rating patterns and groups of users have similar rating patterns as well. We will discover 
#these patterns by studying the residuals.

#For illustrative purposes, we will only consider a small subset of movies with many ratings and users
#that have rated many movies. We also keep Scent of a Woman (movieId == 3252) because we use it for
#a specific example.

#----------------8.1 Creating the matrix----------------
train_small <- movielens %>% 
  group_by(movieId) %>% #we choose the movies with more than 50 reviews
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>% #3252 is Scent of a Woman used in example
  group_by(userId) %>% #we choose users with more than 50 reviews
  filter(n() >= 50) %>% ungroup()

y <- train_small %>% 
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>% #we create a matrix
  as.matrix()

##We add rows and column names

rownames(y)<- y[,1] #we name the rows as the first column (userIds)
y <- y[,-1] #then we remove the first column (userId)

movie_titles <- movielens %>%
  select(movieId, title) %>%
  distinct() #eliminates repetitive rows

colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])

view(y)

##We can get the --residuals-- by removing the column and row effects

y <- sweep(y, 1, rowMeans(y, na.rm=TRUE)) #we substract the means of the rows (user means) from the rows 
y <- sweep(y, 2, colMeans(y, na.rm=TRUE)) #we substract the means of the columns (movie means) from the columns

#----------------8.2 Analyzing the residuals----------------
#If the model above explains all the signals, and the residuals are just noise, then the residuals 
#for different movies should be independent from each other. But they are not. Here is an example:

m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2) #Above 0 means someone liked the movie above the movie average
#and the user avergae, below 0 is the opposite. This means, people who liked the Godfather also liked the Godfather II.

m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)

m_4 <- "You've Got Mail" 
m_5 <- "Sleepless in Seattle" 
qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)

#----------------8.3 Coorelation between resiguals----------------
#By looking at the correlation between movies, we can see a pattern.

cor(y[, c(m_1, m_2, m_3, m_4, m_5)], use="pairwise.complete") %>% 
  knitr::kable() #"pairwise.complete.obs" then the correlation or covariance between each pair of variables 
#is computed using all complete pairs of observations on those variables

#----------------8.4 Factorization (an example)----------------

options(digits = 3) #we round to two decimal plates
Q <- matrix(c(1 , 1, 1, -1, -1), ncol=1)
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5)
Q

rep(c(2,0,-2), c(3,5,4)) #we are going to use the rep function to create some data
P <- matrix(rep(c(2,0,-2), c(3,5,4)), ncol=1)
rownames(P) <- 1:nrow(P)
P

set.seed(1)
P%*%t(Q) #we are going to multiply these two vectors, but first we are going to add some random noise
X <- jitter(P%*%t(Q)) #we use the jitter function to add some random noise
X %>% knitr::kable(align = "c") #align = "c" aligns the numbers in the center

cor(X)

##Yu,i = ?? + b_i + b_u + p_u *q_i + error_u,i ;where in our exampole X is p_u *q_i
#The equation above is what we want to get. This is the q:
t(Q) %>% knitr::kable(aling="c")

#And this is the p:
P

#Those that like gangster movies and dislike romance movies (coded as 2), those that like romance movies and
#dislike gangster movies (coded as -2), and those that don???t care (coded as 0).

#----------------8.5 Factorization (an more complicated example)----------------
set.seed(1)
options(digits = 2)
m_6 <- "Scent of a Woman"

Q <- cbind(c(1 , 1, 1, -1, -1, -1), 
           c(1 , 1, -1, -1, -1, 1)) 
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5, m_6)

P <- cbind(rep(c(2,0,-2), c(3,5,4)), 
           c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2

rownames(P) <- 1:nrow(X)

X <- jitter(P%*%t(Q), factor=1)
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(aling="c")

P #the first column: 1 if they like crime and dislike romance, 0 if they don't care, -1 if they dislike crime and like romance 
#the second column: 0.5 if they like Al Pacino, 0 if they don't care, and -0.5 if they don't like Al Pacino

t(Q) #the first row: 1 if it's crime, -1 if it's romance
#the second row: 1 if it has Al Pacino, -1 if it does not have Al Pacino

#----------------8.6 Correlation (using the real data)----------------
six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
tmp <- y[,six_movies]
cor(tmp, use="pairwise.complete")

#----------------9. Factorization/Descomposition (using the real data)----------------
y[is.na(y)] <- 0 #to compute the decomposition, we will make the residuals with NAs equal to 0:

pca <- prcomp(y)
class(pca) #this is an object of class prcomp

#The q vectors are called the principal components and they are stored in this matrix
pca$rotation
dim(pca$rotation)

#While the p, or the user effects, are here:
pca$x
dim(pca$x)

#The PCA function returns a component with the variability of each of the principal components, and we can plot it.
plot(pca$sdev)

#And see that just the first few already explain a large percent
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2)) #cumsum returns a vector whose elements are the cumulative sums, 
#products, minima or maxima of the elements of the argument.

#For example, with just 50 principal components we're already explaining about half the variability 
#out of a total of over 300 principal components.
plot(var_explained) 

#We also notice that the first two principal components are related to the structure in opinions about movies
library(ggrepel)
#The add-on package ggrepel includes a geometry that adds labels while ensuring that they
#don???t fall on top of each other. We simply change geom_text with geom_text_repel.

#----------------9.1 ploging principal components (using the real data)----------------
#To see that the principal components are actually capturing something important about the data, we can make 
#a plot of for example, the first two principal components, but now label the points with the movie
#that each one of those points is related to. Just by looking at the top three in each direction, 
#we see meaningful patterns.
pcs <- data.frame(pca$rotation, name = colnames(y))
view(pcs)
pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = filter(pcs, 
                                PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))

#Just by looking at the top 10 in each direction, we see a meaningful pattern. The first PC shows the difference
#between critically acclaimed movies on one side
pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)

#and Hollywood blockbusters on the other:
pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10)

#While the second PC seems to go from artsy, independent films:
pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)

#to nerd favorites:
pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)