### R script file for all analysis conducted per report.Rmd
### Created: AT 25 NOV 2020
### Last updated: 22 DEC 2020

### Code between rows of #'s provided by HarvardX PH125.9x Data Science: Capstone
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
library(knitr)
library(kableExtra)
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
user_reviews <- edx %>% group_by(userId) %>% summarise(n=n()) %>% ggplot(aes(n))+geom_histogram()+geom_vline(aes(xintercept=median_user_reviews, colour="Median"))+ggtitle("Number of reviews per user")+xlab("")+ylab("")+scale_color_manual(name = "", values = c(Median = "red")) 
print(user_reviews)
dev.off()
browseURL("user_reviews.png")

# graph number of reviews per film
median_film_reviews <- edx %>% group_by(movieId) %>% summarise(n=n()) %>% pull() %>% median()
png("movie_reviews.png")
movie_reviews <- edx %>% group_by(movieId) %>% summarise(n=n()) %>% ggplot(aes(n))+geom_histogram()+geom_vline(aes(xintercept = median_film_reviews, colour="Median"))+ggtitle("Number of reviews per film")+xlab("")+ylab("")+scale_colour_manual(name="", values=c(Median="red"))
print(movie_reviews)
dev.off()
browseURL("movie_reviews.png")

# look at distribution of number of reviews vs average rating for films

png("number_v_average_rating.png") 
number_v_average_rating <- edx %>% group_by(movieId) %>% summarise(n=n(), average=mean(rating)) %>% mutate(number=ifelse(n<10, "<10 reviews", ">=10 reviews")) %>% ggplot(aes(n, average, col=number))+geom_point()+ggtitle("Average rating vs number of reviews per film")+scale_colour_manual(name="", breaks=c('<10 reviews'), values=scales::hue_pal()(2))
print(number_v_average_rating)
dev.off()
browseURL("number_v_average_rating.png") 

# investigate possible associations using subsets

set.seed(20, sample.kind="Rounding")
subset <- edx[sample(9000055, 10000),]

set.seed(10, sample.kind="Rounding")
subset2 <- edx %>% semi_join(subset, by="userId")
subset2 <- subset2[sample(nrow(subset2),10000),]


#going to look at four basic models to start with
models <- c("glm", "knn", "rf", "rpart")

RMSE <- function(real, predicted){
sqrt(mean((real-predicted)^2))
}

set.seed(3, sample.kind="Rounding")

#frst userId alone
userId_predictions <- sapply(models,function(model){
fit<- train(rating ~ userId, model=model, data=subset)
predictions <- predict(fit,subset2)
RMSE(predictions, subset2$rating)
})

#then movieId alone
set.seed(1, sample.kind="Rounding")
subset3 <- edx[sample(9000055, 10000),]

set.seed(8, sample.kind="Rounding")
subset4 <- edx %>% semi_join(subset3, by="movieId")
subset4 <- subset4[sample(nrow(subset4),10000),]

set.seed(4, sample.kind="Rounding")

movieId_predictions <- sapply(models,function(model){
  fit<- train(rating ~ userId, model=model, data=subset3)
  predictions <- predict(fit,subset4)
  RMSE(predictions, subset4$rating)
})


#finally both

set.seed(19, sample.kind="Rounding")
subset5 <- edx[sample(9000055, 10000),]

set.seed(80, sample.kind="Rounding")
subset6 <- edx %>% semi_join(subset5, by="movieId")
subset6 <- subset6[sample(nrow(subset6),10000),]

set.seed(1, sample.kind="Rounding")

user_and_movie_predictions <- sapply(models,function(model){
  fit<- train(rating ~ userId, model=model, data=subset5)
  predictions <- predict(fit,subset6)
  RMSE(predictions, subset6$rating)
})



predictions <- userId_predictions %>% rbind(movieId_predictions) %>% rbind(user_and_movie_predictions)
rownames(predictions) <- c("userId only", "movieId only", "both userId and movieId")

predictions %>% kable() %>% kable_styling()

test_index <- edx %>% createDataPartition(times = 1, p=0.1)
test_set <- edx[index]
train_set <- edx[-index] %>% semi_join(test_set, by="userId") %>% semi_join(test_set, by="movieId")

###tune a regularised linear model

lambdas <- seq(0, 10, 0.25)

regularised <- sapply(lambdas, function(l){

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

plot(lambdas,regularised)
l<- lambdas[which(regularised==min(regularised))]
min(regularised)