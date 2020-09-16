library(tidyverse)
library(caret)

load(file="rda/movielens.rda")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind = "Rounding") 
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

rm(test_index, temp, movielens, removed)

#-------------------------------------------------------------------------------
# CREATE SMALLER DATASET FOR TESTING
#-------------------------------------------------------------------------------
set.seed(1, sample.kind = "Rounding")
dat_index <- sample(edx$userId, 10^5, replace = FALSE)
dat <- edx[dat_index,]

# Split the data into test and train set
test_index <- createDataPartition(dat$rating, times = 1, p = 0.1, list = FALSE)
test_set <- dat[test_index,]
train_set <- dat[-test_index,]

# To make sure test set contains all movieId, userId from train set
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#-------------------------------------------------------------------------------
# TESTING MODELS
#-------------------------------------------------------------------------------
rmse <- function(pred_rating, actual_rating){
  sqrt(mean((pred_rating - actual_rating)^2))
}

# 1. SIMPLE MODEL
mu <- mean(train_set$rating)
rmse(mu, test_set$rating)

# 2. ADD MOVIE EFFECTS 
movie_avg <- train_set %>% 
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu)) 

b_i <- test_set %>% 
  left_join(movie_avg, by = "movieId") %>%
  .$b_i

y_hat <- mu + b_i

rmse(y_hat, test_set$rating)  

# 3. ADD USER EFFECTS
user_avg <- train_set %>%
  group_by(userId) %>% 
  left_join(movie_avg, by = "movieId") %>%
  summarise(u_i = mean(rating - mu - b_i))

u_i <- test_set %>%
  left_join(user_avg, by = "userId") %>%
  .$u_i

y_hat <- mu + b_i + u_i

rmse(y_hat, test_set$rating)

# 4. ADD DAY EFFECT

