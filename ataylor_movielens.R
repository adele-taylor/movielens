### R script file for all analysis conducted per report.Rmd
### Created: AT 25 NOV 2020
### Last updated: 22 DEC 2020

### Code between rows of #'s provided by HarvardX PH125.9x Data Science: Capstone
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)
##########################################################

# Examine dataset
str(edx)

length(unique(edx$userId)) %>% print() #no. of users
length(unique(edx$movieId)) %>% print() #no. of movies

# graph number of reviews per user
median_user_reviews <- edx %>% group_by(userId) %>% summarise(n=n()) %>% pull() %>% median()
png("user_reviews.png")
user_reviews <- edx %>% group_by(userId) %>% summarise(n=n()) %>% ggplot(aes(n))+geom_histogram()+geom_vline(xintercept=median_user_reviews)
print(user_reviews)
dev.off()
browseURL("user_reviews.png")

# graph number of reviews per film
median_film_reviews <- edx %>% group_by(movieId) %>% summarise(n=n()) %>% pull() %>% median()
png("movie_reviews.png")
movie_reviews <- edx %>% group_by(userId) %>% summarise(n=n()) %>% ggplot(aes(n))+geom_histogram()+geom_vline(xintercept = median_film_reviews)
print(movie_reviews)
dev.off()
browseURL("movie_reviews.png")

# look at distribution of number of reviews vs average rating for films

png("number_v_average_rating.png") 
number_v_average_rating <- edx %>% group_by(movieId) %>% summarise(n=n(), average=mean(rating)) %>% mutate(number=ifelse(n<10, "<10 reviews", NA)) %>% ggplot(aes(n, average, col=number))+geom_point()
print(number_v_average_rating)
dev.off()
browseURL("number_v_average_rating.png") 



lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){

  mu <- mean(train_set$rating)

  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))

  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))

  predicted_ratings <-
    test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)

  return(RMSE(predicted_ratings, test_set$rating))
})