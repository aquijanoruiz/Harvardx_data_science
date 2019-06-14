library(tidyverse)
library(dslabs)
data("movielens")

movielens %>% as_tibble()

movielens %>% summarise(n_users = n_distinct(userId),
                        n_movies = n_distinct(movieId)) #we can see how many unique userIds and movieIds there are

length(unique(c(1,1,2,1,2))) #n_distinct does the same as this command

#--------------------1. Analyzing some features of the data------------------
movielens %>% group_by(movieId) %>% 
  summarize(n = n()) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies") #The first thing we notice is that some movies get rated more than others.

movielens %>% group_by(userId) %>% 
  summarize(n = n()) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Users")#Our second observation is that some users are more active than others at rating movies.

#The sparse matrix--------------------

keep <- movielens %>% #we create an index of the 5 most rated movies
  dplyr::count(movieId) %>%
  top_n(5) %>%
  pull(movieId)

tab <- movielens %>% #we use the index (keep) we created to show the preference 8 users in a matrix 
  filter(userId %in% c(13:20)) %>% 
  filter(movieId %in% keep) %>% 
  select(userId, title, rating) %>% 
  spread(title, rating) 

#A graph of how the data looks like (sparse matrix)

users <- sample(unique(movielens$userId), 100) #we create a sample of 100 random people

movielens %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>% #this will later be used to indicated if someone rated a movie
  spread(movieId, rating) %>% #this will create a matrix, putting the movieIds as columns and filling the matrix with the 1s or NAs
  select(sample(ncol(.), 100)) %>% #we select a sample of 100 movies. Note we had alreay selected a sample of 100 users. This will create a squared matrix.
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

view(movielens %>% filter(userId %in% users) %>% 
       select(userId, movieId, rating) %>%
       mutate(rating = 1) %>%
       spread(movieId, rating)) #to get an idea of what we are doing

#--------------------2. Partitioning the data------------------
library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, list = FALSE) #we create a test data that is 20% of the data set. times = 1 means we only want one random index
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

#To make sure we don’t include users and movies in the test set that do not appear in the training set, we
#remove these entries using the semi_join function:

#ES: Aqui, lo que hacemos es quitar del test_set los usuarios que no aparecen en el train_set, ya que vamos a predecir
#la calificacion de los usuarios en el test_set utilizando la calificacion que ellos mismos pusiseron en el train_set.

test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") #check section 23.1.6 of the book

#--------------------3. The loss function------------------

#We can write a function that computes the RMSE for vectors of ratings and their corresponding predictors.

RMSE <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#--------------------4. A first model------------------
#We predict the same rating for all movies regardless of user.
mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

#Keep in mind that if you plug in any other number, you get a higher RMSE.
predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)

#We can create a results table with this naive approach.
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

#--------------------5. Adding movie effects------------------
#Yu,i = μ + bi + error u,i

# fit <- lm(rating ~ as.factor(userId), data = movielens) #we can try this, but it will run very slowly

#We know that the least square estimate bi is just the average of Yu,i − μ for each movie i. So we can compute
#them this way (we will drop the hat notation in the code to represent estimates going forward).

mu <- mean(train_set$rating) #this is the mean of all movies. From this value we are going to add(or substrac) the movie effect

movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

movie_avgs #you can see the movie effect for each movie
movie_avgs %>% qplot(b_i, geom = "histogram", bins=10, data=., color = I("black")) #we have to use I() to set the aesthetics manually in qplot()

#Predicting the values with this model
predicted_ratings <- mu + (test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i) #the parenthesis are not necessary for this algorithm, but make it easier to understand

model_1_rmse <- RMSE(test_set$rating,predicted_ratings)

rmse_results <- bind_rows(rmse_results, #we use bind_rows or rbind
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse )) 

rmse_results 

#--------------------6. Adding user effects------------------
#We can compute the average rating for user u for those that have rated over 100 movies.

train_set %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating)) %>% #b_u means bias from the user
  filter(n()>=100) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black")


#Yu,i = μ + bi + bu + error u,i

# lm(rating ~ as.factor(movieId) + as.factor(userId)) #we can try this, but it will run very slowly

#Instead, we will compute an approximation by computing μ and bi and estimatingˆbu as the average of yu,i − μ − bi.

#We had already computed mu and b_1. 
mu 
movie_avgs #the movie effects were stored here

user_avgs <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#Creting the predictor--------------------

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",
                                     RMSE = model_2_rmse))

rmse_results #we can see how we have improved our preciction