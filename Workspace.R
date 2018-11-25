library(readr)
library(tidyverse)
songs <- read_csv("songs.csv")


# Plot 1: Compare Genre vs. Peak Position
genre_peak <- songs %>%
  filter(broad_genre != "unknown")

ggplot(genre_peak, aes(x = broad_genre, y = peak_pos)) + 
  geom_boxplot() + scale_y_reverse()

# Plot 2: Compare Top 10 Songs vs. Genre
top10_genre <- songs %>%
  filter(peak_pos <= "10") 

ggplot(top10_genre, aes(x = broad_genre)) + 
  geom_bar()

          # NOT Top 10
not_top10 <- songs %>%
  filter(peak_pos > "10")

ggplot(not_top10, aes(x = broad_genre)) + 
  geom_bar()




  