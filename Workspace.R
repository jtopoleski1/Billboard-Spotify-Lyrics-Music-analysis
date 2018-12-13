# The purpose of this workspace is to save the csv file as an rds
# to allow for easy incorperation into the shiny app

# Load the neccessary packages

library(readr)

# Read in the CSV data file and save it as a RDS

songs <- read_csv("songs.csv")
saveRDS(songs, file = "data.rds")
