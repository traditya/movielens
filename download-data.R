# Download raw data from grouplens.org and save it under /data folder

url <- "http://files.grouplens.org/datasets/movielens/ml-10m.zip"
dest_file <- "data/ml-10m.zip"
download.file(url, destfile = dest_file)

