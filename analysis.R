library(tidyverse)
library(caret)
library(lubridate)

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
test_index <- createDataPartition(y = dat$rating, times = 1, p = 0.1, list = FALSE)
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
  summarise(b_u = mean(rating - mu - b_i))

b_u <- test_set %>%
  left_join(user_avg, by = "userId") %>%
  .$b_u

y_hat <- mu + b_i + b_u

rmse(y_hat, test_set$rating)

# 4. ADD WEEK EFFECT
train_set_week <- train_set %>%
  mutate(date = as_datetime(timestamp), 
         week = round_date(date, unit = "week"))
  
# Fit with a Loess regression
week <- train_set_week %>% .$week 
total_week <- as.numeric(diff(range(week))) / 7
span <- 150 / total_week

fit <- loess(rating ~ as.numeric(week), degree = 2, 
             span = span, data = train_set_week)

# Plot to see the week effect
train_set_week %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(week, rating)) + 
  geom_point() +
  geom_line(aes(week, smooth), col = "red", lwd=1)

# Define the week effect f_w
week_avg <- train_set_week %>% 
  group_by(week) %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  summarise(f_w = mean(rating - mu - b_i - b_u))

f_w <- test_set %>% 
  mutate(date = as_datetime(timestamp), week = round_date(date, unit = "week")) %>%
  left_join(week_avg, by = "week") %>%
  .$f_w

y_hat <- mu + b_i + b_u + f_w

# Replace 1 NA value with 0
y_hat <- replace_na(y_hat, 0)
rmse(y_hat, test_set$rating)

## why there's one NA on the joined test_set with week_avg??
test_set %>% 
  mutate(date = as_datetime(timestamp), week = round_date(date, unit = "week")) %>%
  left_join(week_avg, by = "week") %>% filter(is.na(f_w))

# train_set doesn't have record on week 1996-10-20, while the test_set does
train_set_week %>% mutate(month = month(week), year = year(week), day = day(week)) %>%
  filter(year==1996 & month==10 & movieId==47)

test_set %>% 
  mutate(date = as_datetime(timestamp), week = round_date(date, unit = "week"), 
         year = year(week), month = month(week), day = day(week)) %>%
  left_join(week_avg, by = "week") %>%
  filter(year==1996 & month==10 & movieId==47)

# ADD GENRE EFFECT
# Plot to see the genre effect
train_set %>% group_by(genres) %>%
  summarise(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n)) %>%
  filter(n > 1000) %>% 
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) +
  geom_point() +
  geom_errorbar() +
  theme(axis.text.x = element_text(angle=60, hjust=1))

# Define the genre effect
genre_avg <- train_set %>% group_by(genres) %>%
  left_join(user_avg, by = "userId") %>% 
  left_join(movie_avg, by = "movieId") %>%
  summarise(g_k = mean(rating - mu - b_i - b_u))

g_k <- test_set %>%
  left_join(genre_avg, by = "genres") %>%
  .$g_k

y_hat <- mu + b_i + b_u + g_k

rmse(y_hat, test_set$rating)
