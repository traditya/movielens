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

# SIMPLE MODEL
mu <- mean(train_set$rating)

model_1_rmse <- rmse(mu, test_set$rating)

# ADD MOVIE EFFECTS 
movie_avg <- train_set %>% 
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu)) 

b_i <- test_set %>% 
  left_join(movie_avg, by = "movieId") %>%
  .$b_i

y_hat <- mu + b_i

model_2_rmse <- rmse(y_hat, test_set$rating)  

# ADD USER EFFECTS
user_avg <- train_set %>%
  group_by(userId) %>% 
  left_join(movie_avg, by = "movieId") %>%
  summarise(b_u = mean(rating - mu - b_i))

b_u <- test_set %>%
  left_join(user_avg, by = "userId") %>%
  .$b_u

y_hat <- mu + b_i + b_u

model_3_rmse <- rmse(y_hat, test_set$rating)

# ADD DAY EFFECT
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

# ADD WEEK EFFECT 
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


# ADD GENRE EFFECT
# Plot to see the genre effect
train_set %>% group_by(genres) %>%
  summarise(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n)) %>%
  filter(n > 1000) %>% 
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) +
  geom_point() +
  geom_errorbar() +
  theme(axis.text.x = element_text(angle=60, hjust=1))


#-------------------------------------------------------------------------------
# COMPARE THE RESULTS
#-------------------------------------------------------------------------------
rmse_results <- data.frame(Method = c("Simple Model", "Movie Effect", 
                                      "Movie + User Effect", "Movie + User + Week Effect",
                                      "Movie + User + Genre Effect"), 
                           RMSE = c(model_1_rmse, model_2_rmse, model_3_rmse,
                                    model_4_rmse, model_5_rmse))

rmse_results %>% knitr::kable()


## Genre Effect

