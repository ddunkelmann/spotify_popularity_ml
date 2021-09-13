# Machine Learning Project from Sliced S1E8

# Libraries
library(tidyverse)
library(readr)
library(caret)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# 1. Load Data 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
train <- read_csv("data/train.csv")
artists <- read_csv("data/artists.csv")
test <- read_csv("data/test.csv")

colnames(train)
colnames(artists)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# 2. Edit Data: 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# 2.1. Edit Train Data
edit_train <- train

# Helper Function for getting a specific value from the string-arrays in the data
get_first_value_from_string <- function(str, returned_nr) {
  cleaned_string <- str %>% 
    str_replace_all('[\\[\\]"]', "") %>% 
    str_replace_all("'", "")
  splitted_string <- cleaned_string %>% 
    str_split(",") %>% 
    unlist()
  return(splitted_string[returned_nr])
}

# -- Split Artists Column and return the first given Artist ID
edit_train$id_artists <- sapply(train$id_artists, get_first_value_from_string, returned_nr = 1)




# 2.2 Edit Artists Data

# Get the first genre Value
edit_artists <- artists
edit_artists$genres <- sapply(artists$genres, get_first_value_from_string, returned_nr = 1)

# Edit categorical genres
count_genres <- edit_artists %>% 
  count(genres)
# => Too many genres available, too many have missing values. Drop feature

edit_artists <- edit_artists %>% 
  select(-genres) %>% 
  rename("artist_popularity" = "popularity")


# Combine all the calculated Features
X_train <- edit_train %>% 
  select(#id, name, 
         popularity,
         duration_ms, danceability, energy, key, 
         loudness, speechiness, acousticness, instrumentalness, liveness,
         valence, tempo, 
         release_year, #release_month, release_day, 
         id_artists)

# Join the Artist Features
X <- X_train %>% 
  left_join(edit_artists, by = c("id_artists" = "id")) %>% 
  select(-c(id_artists, name))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# 3. Visualize Data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

gg_dance <- ggplot(X_train, aes(y = popularity, x = danceability)) +
  geom_point(alpha = 0.1) + 
  geom_smooth()

gg_energy <- ggplot(X_train, aes(y = popularity, x = energy)) +
  geom_point(alpha = 0.1) + 
  geom_smooth()

gg_loud <- ggplot(X_train, aes(y = popularity, x = loudness)) +
  geom_point(alpha = 0.1) + 
  geom_smooth()




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# 3. Modelling
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

?createDataPartition
folds <- createFolds(1:nrow(X), k = 5)

for(f_index in folds){
  X_train <- X[f_index,]
  
  x_test <- X %>% 
    anti_join(X_train, by = colnames(X_train))
  
  print(paste("Got Split with", paste(head(f_index), collapse = "-")))
}

