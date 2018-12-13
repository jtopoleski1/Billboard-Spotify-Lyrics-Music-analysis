# Load all neccessary packages

library(gganimate)
library(dplyr)
library(ggplot2)
library(tidytext)
library(wordcloud)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(RCurl)
library(XML)
library(tidyverse)

# Read in the CSV data file and save it as a RDS
# This allows for easy incorperation into my shiny app

songs <- read_csv("songs.csv")
saveRDS(songs, file = "data.rds")

--
  
  # NOTE:
  # The following work is an active "brainstorm site".
  # All work here is simply me testing out ideas and examining
  # the appeal of different graphics. While some of the concepts
  # were incorperated into my shiny web app, you should look
  # to the app for all complete and clean work. THIS WORK IS 
  # INCOMPLETE AND SIMPLY A RAW WORK SPACE. It is NOT to be 
  # graded or reproduced!

# Number of Songs on the Top Charts per Year

num <- songs %>%
  count(year)

# Potentially animate??
ggplot(num, aes(x = year, y = n)) + 
  geom_line() +
  labs(title = "Number of Songs on the Billboard Top Charts",
       subtitle = "Per Year: 2000-2017",
       x = "Year",
       y = "Number of Songs") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  geom_vline(xintercept=2006, colour="grey") +
  geom_text(aes(x=2006, label="Spotify Created (2006)\n", y=525), colour="blue", angle=90) +
  geom_vline(xintercept=2015, colour="grey") +
  geom_text(aes(x=2015, label="Apple Music Created (2015)\n", y=525), colour="red", angle=90)

## Genre: #1 vs. the Rest (make other combos in shiny app maybe?)

# Plot 1: Compare Genre vs. Peak Position
genre_peak <- songs %>%
  filter(broad_genre != "unknown")

median <- genre_peak %>%
  group_by(broad_genre) %>%
  mutate(med = median(peak_pos))

ggplot(genre_peak, aes(x = broad_genre, y = peak_pos)) + 
  geom_boxplot() +
  geom_text(data = median, aes(x = broad_genre, y = med, label = med), 
            size = 3, vjust = -1.5) +
  labs(title = "Peak Billboard Hot 100 Position",
       subtitle = "Based Upon Genre: 2000-2017",
       x = "Genre",
       y = "Peak Position") +
  # How to get the y-axis to say 1 at top and NOT 0???
  scale_y_reverse() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.subtitle = element_text(hjust = 0.5))



# Plot 2: Compare Top 1 Songs vs. Genre
# Once in Shiny, allow user to select YEAR as 
# well as Top 1 vs 2-100 (peak_pos) vs others in a dropdown menu

top_1_new <- songs %>%
  filter(peak_pos == 1) %>%
  filter(broad_genre != "unknown")

top_10_new <- songs %>%
  filter(peak_pos <= 10) %>%
  filter(broad_genre != "unknown")

ggplot(top_1_new, aes(x = broad_genre)) + 
  geom_bar() +
  labs(title = "Songs Appearing in the Billboard Hot 100",
       subtitle = "Based Upon Genre: 2000-2017",
       x = "Genre",
       y = "Number of Appearances") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.subtitle = element_text(hjust = 0.5))

## Song Titles (length changes AND sentiment analysis?)

# Song Title Length Analysis
# In shiny, make this so user can pick ALL songs, #1 songs or non-#1 songs
# Also, make this hoverable for the data points

title_length <- songs %>%
  group_by(year) %>%
  mutate(tlength = nchar(title))

avg_title_length <- title_length %>%
  filter(peak_pos == 1) %>%
  group_by(year) %>%
  mutate(avgtlength = mean(tlength))

ggplot(avg_title_length, aes(x = year, y = avgtlength)) + 
  geom_smooth() +
  labs(title = "Billboard Hot 100 Song Title Length",
       subtitle = "By Year: 2000-2017",
       x = "Year",
       y = "Average Title Length (Characters)") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.subtitle = element_text(hjust = 0.5))

# Sentiment Analysis
# Do this for EVERY year and combine into a table / graph it
songs17 <- songs %>%
  filter(year == "2017")

tokens <- data_frame(text= songs17$lyrics) %>%
  unnest_tokens(word, text)

lyric_set <- tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  spread(sentiment, n) %>%
  mutate(positivity = positive - negative)

# Make a word cloud (STILL TO DO)
word_count <- data_frame(text= songs$lyrics) %>%
  unnest_tokens(word, text) %>%
  count(word) %>%
  filter(n > 1000)

## Songs Lengths (and densities)

# Song Length Analysis
# In shiny, make this so user can pick ALL songs, #1 songs or non-#1 songs
# Also, make this hoverable for the data points
duration <- songs %>%
  mutate(duration = as.integer(duration_ms)) %>%
  filter(duration != "NA")

avg_duration <- duration %>%
  group_by(year) %>%
  mutate(avgtime = mean(duration)) %>%
  mutate(avgtime = avgtime/60000)

ggplot(avg_duration, aes(x = year, y = avgtime)) + 
  geom_smooth() +
  labs(title = "Billboard Hot 100 Song Duration",
       subtitle = "By Year: 2000-2017",
       x = "Year",
       y = "Average Song Length (Minutes)") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.subtitle = element_text(hjust = 0.5))


# Components of a Song

# Shiny app with a selector of all the various components:
# energy, liveness, tempo, speechiness, acousticness, danceability

songs16 <-
  songs %>%
  filter(year == "2016") %>%
  filter(peak_pos == "1")

songs$tempo <- as.numeric(songs$tempo)
songs$liveness <- as.numeric(songs$liveness)
songs$energy <- as.numeric(songs$energy)
songs$speechiness <- as.numeric(songs$speechiness)
songs$danceability <- as.numeric(songs$danceability)

ggplot(songs, aes(x = tempo, y = peak_pos)) + 
  geom_point() +
  stat_smooth(method = "lm")

ggplot(songs16, aes(x = energy)) +
  geom_histogram(binwidth = ".2", stat = "count")


pickerInput("year",
            "Year:",
            choices = unique(songs$year),
            selected = unique(songs$year),
            options = list(`actions-box` = TRUE), 
            multiple = TRUE)
