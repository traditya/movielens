library(tidyverse)
library(caret)
library(lubridate)
load(file="rda/movielens.rda")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind = "Rounding") 
test_index <- createDataPartition(y = movielens$rating, 
                                  times = 1, p = 0.1, list = FALSE)
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
test_index <- createDataPartition(y = dat$rating, 
                                  times = 1, p = 0.1, list = FALSE)
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

#------------------------------

# 1. SIMPLE MODEL
mu <- mean(train_set$rating)

model_1_rmse <- rmse(mu, test_set$rating)
#------------------------------

# 2. ADD MOVIE EFFECTS 
movie_avg <- train_set %>% 
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu)) 

b_i <- test_set %>% 
  left_join(movie_avg, by = "movieId") %>%
  .$b_i

y_hat <- mu + b_i

model_2_rmse <- rmse(y_hat, test_set$rating)  
#------------------------------

# 3. ADD USER EFFECTS
user_avg <- train_set %>%
  group_by(userId) %>% 
  left_join(movie_avg, by = "movieId") %>%
  summarise(b_u = mean(rating - mu - b_i))

b_u <- test_set %>%
  left_join(user_avg, by = "userId") %>%
  .$b_u

y_hat <- mu + b_i + b_u

model_3_rmse <- rmse(y_hat, test_set$rating)
#------------------------------

## ADD DAY EFFECT
train_set <- train_set %>% 
  mutate(date = as_datetime(timestamp))

# See the effect of day on rating
train_set %>% 
  ggplot(aes(date,rating)) +
  geom_point() + geom_smooth()

# Apply bin smoothing, with normal kernel
span <- 1000

train_set_daycount <- train_set %>% 
  mutate(day = as_date(date), 
         daycount = as.numeric(max(day) - day)) %>%
  arrange(daycount)

fit <- with(train_set_daycount, 
            ksmooth(daycount, rating, kernel="normal", bandwidth=span))

train_set_daycount %>% mutate(smooth = fit$y) %>%
  ggplot(aes(daycount, rating)) + 
  geom_point() +
  geom_line(aes(daycount, smooth), color="red")

# Add day effect d_ui into the model
day_avg <- train_set_daycount %>% 
  group_by(daycount) %>% 
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  summarise(d_ui = mean(rating - mu - b_i - b_u))

test_set_daycount <- test_set %>% 
  mutate(date = as_datetime(timestamp), 
         day = as_date(date), 
         daycount = as.numeric(max(day) - day)) %>%
  arrange(daycount)

test_set_daycount %>% 
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  left_join(day_avg, by="daycount")
# Notice there are a lot of NAs for the day effect
#------------------------------

# 4. ADD WEEK EFFECT 
train_set_week %>% 
  mutate(week = round_date(date, unit="week"))

week_avg <- train_set_week %>% 
  group_by(week) %>% 
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  summarise(w_ui = mean(rating - mu - b_i - b_u))

w_ui <- test_set %>% 
  mutate(date = as_datetime(timestamp), 
         week = round_date(date, unit="week")) %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  left_join(week_avg, by="week") %>%
  .$w_ui
# Now there's only 1 NA when using week effect. Replace NA with mu
w_ui <- replace_na(w_ui, mu)

y_hat <- mu + b_i + b_u + w_ui

model_4_rmse <- rmse(test_set$rating, y_hat)
# Week effect did improve from previous model, but not significant
#------------------------------

# 5. ADD GENRE EFFECT
# Plot to see the genre effect
train_set %>% group_by(genres) %>%
  summarise(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n)) %>%
  filter(n > 1000) %>% 
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) +
  geom_point() +
  geom_errorbar() +
  theme(axis.text.x = element_text(angle=60, hjust=1))

## Still working on it

#------------------------------

# 6. ADD REGULARIZATION FOR MOVIE + USER 
lambda = 0.75

# Penalize movie effect
movie_avg_reg <- train_set %>%
  group_by(movieId) %>% 
  summarise(n=n(), b_i_reg = sum(rating - mu) / (lambda + n) )

# Penalize user effect
user_avg_reg <- train_set %>%
  group_by(userId) %>% 
  left_join(movie_avg, by="movieId") %>%
  summarise(n=n(), b_u_reg = sum(rating - mu - b_i) / (lambda + n) )

b_i_reg <- test_set %>% 
  left_join(movie_avg_reg, by="movieId") %>%
  .$b_i_reg

b_u_reg <- test_set %>% 
  left_join(user_avg_reg, by="userId") %>%
  .$b_u_reg

y_hat <- mu + b_i_reg + b_u_reg

model_6_rmse <- rmse(y_hat, test_set$rating)
#------------------------------

# 7. ADD REGULARIZATION FOR MOVIE + USER + WEEK
week_avg_reg <- train_set_week %>% 
  group_by(week) %>% 
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  summarise(n=n(), w_ui_reg = sum(rating - mu - b_i - b_u) / (lambda + n) )
  
w_ui_reg <- test_set %>% 
  mutate(date = as_datetime(timestamp), 
         week = round_date(date, unit="week")) %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  left_join(week_avg_reg, by="week") %>%
  .$w_ui_reg
# Now there's only 1 NA when using week effect. Replace NA with mu
w_ui_reg <- replace_na(w_ui_reg, mu)

y_hat <- mu + b_i + b_u + w_ui_reg

model_7_rmse <- rmse(test_set$rating, y_hat)  

#------------------------------
# CHOOSE BEST LAMBDA
#------------------------------
lambdas <- seq(0, 5, 0.25)

rmses <- sapply(lambdas, function(lambda){
  movie_avg_reg <- train_set %>%
    group_by(movieId) %>% 
    summarise(n=n(), b_i_reg = sum(rating - mu) / (lambda + n) )
  
  user_avg_reg <- train_set %>%
    group_by(userId) %>% 
    left_join(movie_avg, by="movieId") %>%
    summarise(n=n(), b_u_reg = sum(rating - mu - b_i) / (lambda + n) )
  
  b_i_reg <- test_set %>% 
    left_join(movie_avg_reg, by="movieId") %>%
    .$b_i_reg
  
  b_u_reg <- test_set %>% 
    left_join(user_avg_reg, by="userId") %>%
    .$b_u_reg
  
  y_hat <- mu + b_i_reg + b_u_reg
  
  return(rmse(y_hat, test_set$rating))
})
lambda <- lambdas[which.min(rmses)]
lambda

data.frame(lambdas, rmses) %>%
  ggplot(aes(lambdas, rmses)) + geom_point()

#-------------------------------------------------------------------------------
# COMPARE THE RESULTS
#-------------------------------------------------------------------------------
rmse_results <- data.frame(Method = c("Simple Model", 
                                      "Movie Effect", 
                                      "Movie + User Effect", 
                                      "Movie + User + Week Effect",
                                      "Regularized Movie + User Effect",
                                      "Regularized Movie + User + Week Effect"), 
                           RMSE = c(model_1_rmse, model_2_rmse, model_3_rmse,
                                    model_4_rmse, model_7_rmse, model_8_rmse))

rmse_results %>% knitr::kable()
#------------------------------



