# Installing and loading the packages
install.packages("tidyverse")
install.packages("lintr")
install.packages("corrplot")
install.packages("caTools")
install.packages("fastDummies")
install.packages("olsrr")
library(tidyverse)
library(lintr)
library(corrplot)
library(caTools)
library(fastDummies)
library(olsrr)

# 1. Loading and Inspecting the dataset
# Reading the data
Playlist = read.csv("playlist_2010to2022.csv")

# Inspecting the data
head(Playlist)

# Checking the structure of the data
str(Playlist)

# Checking the summary of the data
summary(Playlist)

# Checking the missing values
colSums(is.na(Playlist))

# Removing rows with missing values
Playlist <- Playlist[complete.cases(Playlist),]

# Checking the duplicates
Playlist[duplicated(Playlist),]

# 2. Data Cleaning and EDA
# Creating a separate genre DataFrame
Playlist <- Playlist %>% 
  mutate(artist_genres = strsplit(gsub("\\['|'\\]|'", "", artist_genres), ", ")) %>%
  unnest(artist_genres)
  
# Count the number of occurrences of each genre
genre_count <- Playlist %>%
  group_by(artist_genres) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice_head(n = 10)

# Top 10 genre counts and ordering them in descending order
ggplot(genre_count, aes(x = reorder(artist_genres, count), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "white") +
          coord_flip() + 
          labs(x = "Genre", y = "Count", 
          title = "Top 10 Genres") +
          theme_minimal()


# Exploring artist popularity over the years
Playlist %>%
  group_by(year) %>%
  summarise(avg_popularity = mean(artist_popularity)) %>%
  ggplot(aes(x = year, y = avg_popularity)) +
  geom_line() +
  labs(x = "Year", 
        y = "Average Popularity", 
        title = "Artist Popularity Over the Years") +
        theme_minimal()

# Exploring the popularity of songs over the years
Playlist %>%
  group_by(year) %>%
  summarise(avg_popularity = mean(track_popularity)) %>%
  ggplot(aes(x = year, y = avg_popularity)) +
  geom_line() +
  labs(x = "Year", 
        y = "Average Popularity", 
        title = "Song Popularity Over the Years") +
        theme_minimal()

# Duration of songs over the years
Playlist <- Playlist %>%
  mutate(duration_minutes = duration_ms / 60000)

Playlist %>%
  group_by(year) %>%
  summarise(avg_duration = mean(duration_minutes)) %>%
  ggplot(aes(x = year, y = avg_duration)) +
  geom_line() +
  labs(x = "Year", 
        y = "Average Duration", 
        title = "Song Duration Over the Years") +
        theme_minimal()

# Correlation Heatmap
corr_matrix <- cor(select_if(Playlist, is.numeric), use = "complete.obs")
print(corr_matrix)
corrplot::corrplot(corr_matrix)

# Track popularity vs Artist popularity
Playlist %>%
  ggplot(aes(x = artist_popularity, y = track_popularity)) +
  geom_point() +
  labs(x = "Artist Popularity", 
        y = "Track Popularity", 
        title = "Track Popularity vs Artist Popularity") +
    geom_smooth(method = "lm") +
    theme_minimal()

# Track popularity vs Danceability
Playlist %>%
  ggplot(aes(x = danceability, y = track_popularity)) +
  geom_point() +
  labs(x = "Danceability", 
        y = "Track Popularity", 
        title = "Track Popularity vs Danceability") +
    geom_smooth(method = "lm") +
    theme_minimal()

# Exploring the distribution of the features
Playlist %>%
  select_if(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram(fill = "skyblue", color = "white", alpha = 0.7) +
  facet_wrap(~key, scales = "free") +
  theme_minimal() +
    labs(title = "Distribution of Various Features", x = "Value", y = "Frequency")


# Hypothesis Formulation
# H1: Songs with higher danceability are more popular.
# H2: Over the years, the average duration of popular songs has decreased.
# H3: Artist popularity is a significant predictor of track popularity.

# One hot encoding the time signature column
Playlist <- dummy_cols(Playlist, select_columns = "time_signature")
head(Playlist)

# Scaling the features
feature_cols <- c("danceability", "energy", "key", "loudness", "mode", "speechiness", 
                  "acousticness", "instrumentalness", "liveness", "valence", 
                  "tempo", "duration_minutes", "time_signature_1", "time_signature_3", 
                  "time_signature_4", "time_signature_5")


Playlist_scaled <- Playlist %>%
    mutate(across(all_of(feature_cols), scale, .names = "{col}_scaled"))
  

# Scaling the target
Playlist_scaled$track_popularity_scaled <- scale(Playlist$track_popularity)

head(Playlist_scaled)

# Linear Regression Model
# Split the data into training and test sets
set.seed(123)  # Setting seed to reproduce the results
split <- sample.split(Playlist_scaled$track_popularity_scaled, SplitRatio = 0.7)

# Creating training and test sets
training_set <- subset(Playlist_scaled, split == TRUE)
test_set <- subset(Playlist_scaled, split == FALSE)

# Building the Linear Regression model using the training set
lm_model <- lm(track_popularity_scaled ~ danceability_scaled + energy_scaled + key_scaled + 
                loudness_scaled + mode_scaled + speechiness_scaled + 
                acousticness_scaled + instrumentalness_scaled + liveness_scaled + 
                valence_scaled + tempo_scaled + duration_minutes_scaled + 
                time_signature_1_scaled + time_signature_3_scaled + 
                time_signature_4_scaled + time_signature_5_scaled, 
              data = training_set)

# Summary of the model to view the coefficients and other details
summary(lm_model)

# Improving the model
# Checking for Multicollinearity using VIF
corrplot(cor(select_if(Playlist_scaled, is.numeric), use = "complete.obs"))

# 1. Selecting the significant variables using correlation matrix
