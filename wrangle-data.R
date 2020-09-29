# Wrangle raw data

library(tidyverse)
library(data.table)

file <- "data/ml-10m.zip"

# Create ratings dataframe
ratings <- fread(text = gsub("::", "\t", readLines(unzip(file, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

# Create movies dataframe
movies <- str_split_fixed(readLines(unzip(file, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

# Join ratings and movies into movielens dataframe
movielens <- left_join(ratings, movies, by = "movieId")

save(movielens, file="rda/movielens.rda")


