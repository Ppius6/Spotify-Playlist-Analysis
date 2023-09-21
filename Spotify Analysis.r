# Installing and loading the packages
install.packages("tidyverse")
install.packages("lintr")
install.packages("corrplot")
install.packages("caTools")
install.packages("olsrr")
install.packages("car")
install.packages("boot")
install.packages("MASS")
install.packages("caret")
install.packages("stats")
install.packages("tseries")
install.packages("forecast")
install.packages("rmarkdown")
library(tidyverse)
library(lintr)
library(corrplot)
library(caTools)
library(olsrr)
library(car)
library(boot)
library(MASS)
library(caret)
library(stats)
library(tseries)
library(forecast)
library(rmarkdown)

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
Playlist_genre <- Playlist %>% 
  mutate(artist_genres = strsplit(gsub("\\['|'\\]|'", "", artist_genres), ", ")) %>%
  unnest(artist_genres)
  
# Count the number of occurrences of each genre
genre_count <- Playlist_genre %>%
  group_by(artist_genres) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice_head(n = 20)

print(genre_count)

# Top 20 genre counts and ordering them in descending order
ggplot(genre_count, aes(x = reorder(artist_genres, count), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "white") +
          coord_flip() + 
          labs(x = "Genre", y = "Count", 
          title = "Top 20 Genres") +
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

Playlist %>%
  group_by(year) %>%
  summarise(avg_duration = mean(duration_ms / 60000)) %>%
  ggplot(aes(x = year, y = avg_duration)) +
  geom_line() +
  labs(x = "Year", 
        y = "Average Duration", 
        title = "Song Duration Over the Years") +
  theme_minimal()

# Correlation Heatmap
corr_matrix <- cor(select_if(Playlist, is.numeric), use = "complete.obs")

# Print the entire correlation matrix
print(corr_matrix)

# Print the correlation between each variable and track popularity
track_popularity_corr <- corr_matrix[,'track_popularity']
print(track_popularity_corr)

# Plot the correlation matrix
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

# Checking the distribution and normality of the target variable
ggplot(Playlist, aes(x = track_popularity)) +
  geom_histogram(fill = "skyblue", color = "white", alpha = 0.7, binwidth = 2, aes(y = after_stat(density))) +
  geom_line(stat = "density", color = "red") +
  theme_minimal() +
  labs(title = "Distribution of Track Popularity", x = "Track Popularity", y = "Density")

# Further exploring the outliers
mean_popularity <- mean(Playlist$track_popularity)
std_popularity <- sd(Playlist$track_popularity)

ggplot(Playlist, aes(x = track_popularity)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "black") +
  geom_vline(aes(xintercept = mean_popularity), color = "red", linetype = "dashed", size = 1, show.legend = TRUE) +
  geom_vline(aes(xintercept = mean_popularity + 2*std_popularity), color = "green", linetype = "dashed", size = 1, show.legend = TRUE) +
  geom_vline(aes(xintercept = mean_popularity - 2*std_popularity), color = "green", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_popularity + 3*std_popularity), color = "black", linetype = "dashed", size = 1, show.legend = TRUE) +
  geom_vline(aes(xintercept = mean_popularity - 3*std_popularity), color = "black", linetype = "dashed", size = 1) +
  labs(x = 'Track Popularity', y = 'Density', title = 'Distribution of Track Popularity') +
  theme_minimal() +
  theme(legend.position = "top")

# Shapiro-Wilk Test
shapiro.test(Playlist$track_popularity)

# The p-value is less than 0.05, indicating that the target variable is not normally distributed. The null hypothesis in the Shapiro-Wilk test is that the data is normally distributed.
# W = 0.83107: This is the test statistic value. A value close to 1 would indicate a distribution similar to the normal distribution. In this case, 0.83107 is somewhat deviated from 1, indicating a departure from normality.


# Removing the outliers
lower_limit <- mean_popularity - 3*std_popularity
upper_limit <- mean_popularity + 3*std_popularity
print(lower_limit)
print(upper_limit)

Playlist <- Playlist %>%
  filter(track_popularity > lower_limit & track_popularity < upper_limit)

# Checking the distribution and normality of the target variable
ggplot(Playlist, aes(x = track_popularity)) +
  geom_histogram(fill = "skyblue", color = "white", alpha = 0.7, binwidth = 2, aes(y = after_stat(density))) +
  geom_line(stat = "density", color = "red") +
  theme_minimal() +
  labs(title = "Distribution of Track Popularity", x = "Track Popularity", y = "Density")

# Shapiro-Wilk Test - After removing the outliers
shapiro.test(Playlist$track_popularity)

# W = 0.98923: The Shapiro-Wilk statistic is closer to 1 compared to your earlier test, which indicates that the distribution of track_popularity is closer to a normal distribution, but still not perfectly normal.
# p-value = 5.576e-12: Although the W value has increased, the p-value is still significantly less than common alpha levels (like 0.05 or 0.01), indicating strong evidence against the null hypothesis of normality. You would reject the null hypothesis that the data comes from a normal distribution.

# Convert year and genre to factors
Playlist$year <- as.factor(Playlist$year)

# Creating a new column with the number of genres listed for each track
Playlist$num_genres <- sapply(strsplit(Playlist$artist_genres, ", "), length)

# Creating dummy variables for each top genre
Playlist$pop_genre <- grepl("pop", Playlist$artist_genres, ignore.case = TRUE)
Playlist$dance_pop_genre <- grepl("dance pop", Playlist$artist_genres, ignore.case = TRUE)
Playlist$rap_genre <- grepl("rap", Playlist$artist_genres, ignore.case = TRUE)
Playlist$pop_rap_genre <- grepl("pop rap", Playlist$artist_genres, ignore.case = TRUE)
Playlist$hip_hop_genre <- grepl("hip hop", Playlist$artist_genres, ignore.case = TRUE)
Playlist$rnb_genre <- grepl("r&b", Playlist$artist_genres, ignore.case = TRUE)
Playlist$urban_contemporary_genre <- grepl("urban contemporary", Playlist$artist_genres, ignore.case = TRUE)
Playlist$trap_genre <- grepl("trap", Playlist$artist_genres, ignore.case = TRUE)
Playlist$southern_hip_hop_genre <- grepl("southern hip hop", Playlist$artist_genres, ignore.case = TRUE)
Playlist$modern_rock_genre <- grepl("modern rock", Playlist$artist_genres, ignore.case = TRUE)

# Approach A: Build the linear regression model
lm_model <- lm(track_popularity ~ year + artist_popularity + danceability + energy + acousticness + duration_ms + pop_genre + dance_pop_genre + rap_genre + pop_rap_genre + hip_hop_genre + rnb_genre + urban_contemporary_genre + trap_genre + southern_hip_hop_genre + modern_rock_genre, data = Playlist)

summary(lm_model)

# Diagnostic plots to check assumptions
par(mfrow = c(2,2))
plot(lm_model)

# Detecting outliers using Cook's distance
cooksd <- cooks.distance(lm_model)
plot(cooksd, pch = "*", cex = 2, main = "Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm = TRUE), col = "red")  # add cutoff line

# Print the influential observations
influential_obs <- which(cooksd > 4*mean(cooksd, na.rm = TRUE))
print(influential_obs)

# Remove the influential observations
Playlist <- Playlist[-influential_obs, ]

# Approach B: Re-build the linear regression model
new_model <- lm(track_popularity ~ year + artist_popularity + danceability + energy + acousticness + duration_ms + pop_genre + dance_pop_genre + rap_genre + pop_rap_genre + hip_hop_genre + rnb_genre + urban_contemporary_genre + trap_genre + southern_hip_hop_genre + modern_rock_genre, data = Playlist)
summary(new_model)

# Identify observations with large residuals
# large_residuals <- which(residuals(lm_model) < -2 | residuals(lm_model) > 2)

# Filter out these observations
# Playlist_filtered <- Playlist[-large_residuals, ]

# Convert categorical variables to numeric using one-hot encoding
categorical_vars <- c("year", "pop_genre", "dance_pop_genre", "rap_genre", "pop_rap_genre", 
                      "hip_hop_genre", "rnb_genre", "urban_contemporary_genre", 
                      "trap_genre", "southern_hip_hop_genre", "modern_rock_genre")

for(var in categorical_vars){
  Playlist[[var]] <- as.factor(Playlist[[var]])
}

# Check if the variables are converted to factors
column_names <- colnames(Playlist)
print(column_names)

result_list <- list()
for (column_name in column_names) {
  result_list[[column_name]] <- is.factor(Playlist[[column_name]])
}

print(result_list)

# Remove unnecessary variables
unnecessary_vars <- c("playlist_url", "track_id", "track_name", "album", "artist_id", "artist_name", "artist_genres")
Playlist_filtered <- Playlist[ , !names(Playlist) %in% unnecessary_vars]

print(colnames(Playlist_filtered))

# Encoding categorical variables using one-hot encoding - model.matrix()
Playlist_filtered <- model.matrix(~ . - 1, data = Playlist_filtered)
print(colnames(Playlist_filtered))

# Convert Playlist_filtered back to a data frame
Playlist_filtered <- as.data.frame(Playlist_filtered)
is.data.frame(Playlist_filtered)

# Step 3: Rebuild the model with the filtered dataset
model_filtered <- lm(track_popularity ~ year2000 + year2001 + year2002 + year2003 + 
                       year2004 + year2005 + year2006 + year2007 + year2008 + year2009 + 
                       year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + 
                       year2016 + year2017 + year2018 + year2019 + year2020 + year2021 + 
                       year2022 + artist_popularity + danceability + energy + key + 
                       loudness + mode + speechiness + acousticness + instrumentalness + 
                       liveness + valence + tempo + duration_ms + time_signature + 
                       pop_genreTRUE + dance_pop_genreTRUE + rap_genreTRUE + 
                       pop_rap_genreTRUE + hip_hop_genreTRUE + rnb_genreTRUE + 
                       urban_contemporary_genreTRUE + trap_genreTRUE + 
                       southern_hip_hop_genreTRUE + modern_rock_genreTRUE, 
                       data = Playlist_filtered)

# Step 4: Check the summary of the new model
summary(model_filtered)

# Step 5: Plot the residuals of the new model to check if the issue has been resolved
par(mfrow = c(2,2))
plot(model_filtered)

# Cross-Validation
cv_model <- glm(track_popularity ~ year2000 + year2001 + year2002 + year2003 + 
                     year2004 + year2005 + year2006 + year2007 + year2008 + year2009 + 
                     year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + 
                     year2016 + year2017 + year2018 + year2019 + year2020 + year2021 + 
                     year2022 + artist_popularity + danceability + energy + key + 
                     loudness + mode + speechiness + acousticness + instrumentalness + 
                     liveness + valence + tempo + duration_ms + time_signature + 
                     num_genres + pop_genreTRUE + dance_pop_genreTRUE + rap_genreTRUE + 
                     pop_rap_genreTRUE + hip_hop_genreTRUE + rnb_genreTRUE + 
                     urban_contemporary_genreTRUE + trap_genreTRUE + 
                     southern_hip_hop_genreTRUE + modern_rock_genreTRUE, 
                     data = Playlist_filtered)
set.seed(123) # for reproducibility
K <- 10 # number of folds
cv_results <- cv.glm(Playlist_filtered, cv_model, K = K)
print(cv_results)

# Other Metrics (RMSE, MAE)
data.frame(RMSE = sqrt(mean(residuals(model_filtered)^2)), 
          MAE = mean(abs(residuals(model_filtered))))

# Time Series Analysis - (decreasing trend in the average duration of songs over the years)
# Melting the data to get year column
Playlist_filtered_long <- Playlist_filtered %>%
  pivot_longer(cols = starts_with("year"), names_to = "year", values_to = "value") %>%
  filter(value == 1)

Playlist_filtered_long$year <- substr(Playlist_filtered_long$year, 5, 8)

avg_duration_per_year <- Playlist_filtered_long %>%
  group_by(year) %>%
  summarize(avg_duration = mean(duration_ms, na.rm = TRUE))

# Converting to time series object
time_series_data <- ts(avg_duration_per_year$avg_duration, start = min(avg_duration_per_year$year), end = max(avg_duration_per_year$year), frequency = 1)
print(time_series_data)

# Create a date sequence
date_seq <- seq.Date(from = as.Date("2000-01-01"), to = as.Date("2022-12-01"), by = "month")
year_seq_numeric <- seq(2000, 2022, length.out = length(date_seq))

# Step 2: Interpolate the monthly data points
# Assuming avg_duration_per_year is a data frame with columns 'year' and 'avg_duration'
year_seq <- seq(2000, 2022)
avg_duration_seq <- avg_duration_per_year$avg_duration

monthly_avg_duration <- approx(year_seq, avg_duration_seq, xout = year_seq_numeric, method = "linear")$y

# Step 3: Create a time series object
time_series_data <- ts(monthly_avg_duration, start = c(2000, 1), frequency = 12)

# Step 4: Decompose the time series
decomposed_data <- decompose(time_series_data)

# Plot the decomposed data
plot(decomposed_data)

# Tests
# Conduct the Augmented Dickey-Fuller Test
adf_test <- adf.test(time_series_data, alternative = "stationary")
print(adf_test)

# The output of the Augmented Dickey-Fuller test indicates that the time series is stationary. The p-value is 0.01, which is less than the common significance level of 0.05, allowing us to reject the null hypothesis that the time series has a unit root (i.e., is non-stationary).

# Forecasting
# Difference the time series
diff_time_series_data <- diff(time_series_data)

# Use auto.arima to automatically select the best ARIMA model
best_model <- auto.arima(time_series_data)

# Forecast the next 12 months
forecast_result <- forecast(best_model, h = 12)

# Plot the forecast
plot(forecast_result)
