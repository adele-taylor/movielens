### R script file for all analysis conducted per report.Rmd
### Created: AT 25 NOV 2020
### Last updated: 25 NOV 2020

#Load libraries and training data

if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")

library(tidyverse)
library(caret)

load("edx.Rda")

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