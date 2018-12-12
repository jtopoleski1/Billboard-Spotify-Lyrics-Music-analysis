# Loaded all packages neccessary for the project

library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(tidytext)
library(ggridges)
library(sjPlot)
library(wordcloud)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(RCurl)
library(XML)
library(shinythemes)
library(stargazer)

# Loaded tinyverse last in order to make sure no other package overrides its functions

library(tidyverse)

# Read in the songs data, previously manipulated in a RMD workspace and converted
# to a .RDS for easy incorperation

songs <- read_rds("songs.rds")


# Define the navbar overall heading, which allows for clean presentation of tabs to switch between
# chose the lumen shiny theme for a clean presentation of data (it's a simple, clean theme)

ui <- navbarPage(strong("Bop to the Top: What Makes a Billboard Top Hit?"), 
                 theme = shinytheme("lumen"),
       
         # Create the first tab called "Overview" with a html text output (define in the server).
         # The fluidRow creates rows for the about output (with neat formatting).
         # This section allows the reader to gain an overall understanding of
         # what the proejct is about and is an asthetically pleasing intro page.
         # Finally, the "12" column definition refers to the width based upon the 
         # Bootstrap 12-wide grid system (all columns must add up to 12).
                 
       tabPanel("Overview",
                fluidRow(
                  column(12,
                         wellPanel(
                           htmlOutput("about")
                         ))
                )),          
       
       # Creates the second tab called "The Changing Music Industry", providing information 
       # on the number of songs that have appeared on the Hot 100 over time.
       # Sets up radio buttons in order to allow the user to easily click between a category 
       # of songs based upon their highest rating (this featured is maintained throughout).
       # The choices argument is used to give a clean output to the names the view sees within
       # the sidebar. Additionally, a text output explains the outcome.
       
       # Stylistic note: defining each sidebar panel within EVERY tab panel allows for a certain
       # panel to only appear within certain individual tabs and NOT on every tab.
                        
       tabPanel("The Changing Music Industry",
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("type", 
                                "Song's Highest Rating:",
                                choices = c("View All" = "all100",
                                  "Top 1" = "top1",
                                  "Top 10" = "top10",
                                  "Top 25" = "top25",
                                  "26-100" = "bot75")),
                    helpText("To better understand the Billboard Top 100, let's first explore the rapidly changing music industry."),
                    helpText("The emergence of new music sharing technology has led to a rapid increase in the distibution of music. As shown to the right, there have been more and more songs appearing on the chart. This is likely to to the proliferation of music sharing services that have given many more listeners access to a wider, more diverse array of music.")
        
                   ),
                  
                # Graph output in the main panel (defined in server), a line plot of the number of
                # songs appearing on the Hot 100 over time.
                
                  mainPanel(
                    plotOutput("plot1")
                  )
                )
       ),
       
       # Creates the third tab called "Genre", providing information on the genre of songs
       # that have appeared on the top charts over time.
       # Sets up radio buttons in order to allow the user to easily click between a category 
       # of songs based upon their highest rating (this featured is maintained throughout) for the 
       # bar graph output. Additionally, the sidebar sets up a picker with ALL years preselected for
       # the bottom box and whiskers plot in order to see popularity during certain years.
       # The choices argument is used to give a clean output to the names the view sees within
       # the sidebar. Additionally, a text output explains the outcome and gives advice on how to 
       # make a hit song.
       
       tabPanel("Genre",
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("type2", 
                                 "Song's Highest Rating (for the top graph):",
                                 c("View All" = "all100",
                                   "Top 1" = "top1",
                                   "Top 10" = "top10",
                                   "Top 25" = "top25",
                                   "26-100" = "bot75")),
                    pickerInput("year",
                                "Year (for the bottom graph):",
                                choices = unique(songs$year),
                                selected = unique(songs$year),
                                options = list(`actions-box` = TRUE),
                                multiple = TRUE),
                    helpText("The top graph establishes the dominance of the pop, r&b, and rap genres (all appearing the most in the top 1, top 10, and top 25). Although the 'View All' selection makes it appear country is also very popular, these songs seem to litter the bottom 26-100 spots."),
                    helpText("The bottom graph further iterates this point through displaying the higher median spot on the Top 100 list for r&b songs."),
                    helpText("Step 1 of 'How to Make a Top Hit Song': r&b, pop, and rap are the way to go.")
                  ),
                  
                  # For this main output, there are two graphs: a bar graph showing the number of 
                  # appearences by genre (2.1) and a box and whiskers plot showing the average peak
                  # position of songs on the charts in each genre (2.2).
                  
                  mainPanel(
                    plotOutput("plot2.1"),
                    plotOutput("plot2.2")
                  ))),
       
       # Creates the fourth tab called "Title Length", providing information on how the number of
       # charaters per song title has changed over time.
       # Sets up radio buttons in order to allow the user to easily click between a category 
       # of songs based upon their highest rating (this featured is maintained throughout).
       # The choices argument is used to give a clean output to the names the view sees within
       # the sidebar.
       # Additionally, a set of summary statistics is added to the sidebar panel, further defined 
       # within the server.
       # Finally, a text output explains the outcome and gives advice on how to make a hit song.
       
       
       tabPanel("Title Length",
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("type3", 
                                 "Song's Highest Rating:",
                                 c("View All" = "all100",
                                   "Top 1" = "top1",
                                   "Top 10" = "top10",
                                   "Top 25" = "top25",
                                   "26-100" = "bot75")),
                    htmlOutput("title_stats"),
                    helpText("Over time, song titles within the Top 100 have generally been getting shorter. While this does seem true in more recent years for the top #1 songs, there has been lots of flucuation within the past 17 years. As such, this is not a great explanatory value to consider"),
                    helpText("Step 2 of 'How to Make a Top Hit Song': a shorter title (12-13 characters) may be the way to go, but there isn't much statistical significance.")
                  ),
                  
                  # Graph output in the main panel (defined in server), a line plot of the average title
                  # legnth of songs based upon characters within the title over timel.
                  
                  mainPanel(
                    plotOutput("plot3")
                  )
                )),
       
       # Creates the fifth tab called "Duration", providing information on how the length of
       # sogns on the Hot 100 has changed over time.
       # Sets up radio buttons in order to allow the user to easily click between a category 
       # of songs based upon their highest rating (this featured is maintained throughout).
       # The choices argument is used to give a clean output to the names the view sees within
       # the sidebar.
       # Additionally, a set of summary statistics is added to the sidebar panel, further defined 
       # within the server.
       # Finally, a text output explains the outcome and gives advice on how to make a hit song.
       
       tabPanel("Duration",
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("type4", 
                                 "Song's Highest Rating:",
                                 c("View All" = "all100",
                                   "Top 1" = "top1",
                                   "Top 10" = "top10",
                                   "Top 25" = "top25",
                                   "26-100" = "bot75")),
                    htmlOutput("duration_stats"),
                    helpText("Over time, song lengths within the Top 100 have been getting significantly shorter. This relationship comes with strong statistical significance."),
                    helpText("Step 3 of 'How to Make a Top Hit Song': a shorter song (about 3:30-4 minutes) seems like the way to go.")
                  ),
                  
                  # Graph output in the main panel (defined in server), a line plot of the average song
                  # legnth (in minutes) and how it has changed over time.
                  
                  mainPanel(
                    plotOutput("plot4")
                  )
                )),
       
       # Creates the sixth tab called "Music Analysis", providing information on several
       # important components and their impact on songs' peak position on the chart.
            # These options are explained in-depth within the server.
       # Sets up radio buttons in order to allow the user to easily click between a category 
       # of songs based upon their highest rating (this featured is maintained throughout).
       # Additionally, the sidebar sets up a picker with ALL years preselected to allow
       # the reader to look at smaller, more readable sets of data.
       # The choices argument is used to give a clean output to the names the view sees within
       # the sidebar.
       # Additionally, a set of summary statistics is added to the sidebar panel, further defined 
       # within the server.
       # Finally, a text output explains the outcome and gives advice on how to make a hit song.
       
       
       
       # STILL NEED TO EDIT THE ADVICE AND RECAP OF WHAT HAPPENS
       
       

       tabPanel("Music Analysis",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("analysis", 
                                "Choose component to analyze:",
                    choices = c("Energy" = "energy",
                                "Liveness" = "liveness",
                                "Tempo" = "tempo", 
                                "Speechiness" = "speechiness",
                                "Danceability" = "danceability"),
                    selected = "energy"),
                    pickerInput("year2",
                                "Year:",
                                choices = unique(songs$year),
                                selected = unique(songs$year),
                                options = list(`actions-box` = TRUE),
                                multiple = TRUE),
                    htmlOutput("music_stats"),
                  helpText("Energy: represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. A value of 0.0 is least energetic and 1.0 is most energetic"),
                  helpText("Liveness: describes the probability that the song was recorded with a live audience. A value above 0.8 provides strong likelihood that the track is live."),
                  helpText("Tempo: describes the speed/rate the song is played, measure in beats per minute"),
                  helpText("Speechiness: detects the presence of spoken words in a track. If above 0.66, it is probably made of spoken words, a score between 0.33 and 0.66 is a song that may contain both music and words, and a score below 0.33 means the song does not have any speech."),
                  helpText("Danceability: describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable.")
                ),
                
                # For this main output, there are two graphs: a dot plot (with regression line) 
                # showing one of the compoents vs. a song's peak position (5.1) and a density plot 
                # showing the concentration of songs in the top and non-top position around a certain 
                # level of a measurement (like Energy) (5.2).
                
                mainPanel(
                  plotOutput("plot5.1"),
                  plotOutput("plot5.2")
                ))))

# Define plots and all of the information and output that is displayed with the UI
                

server <- function(input, output) {
  
  # About Page
  
  output$about <- renderUI({
    HTML(paste(
      h3("Summary"),
      p("What makes a hit song?"),
      p("Over the past 20 years, the Billboard Hot 100 has kept data on the popularity of songs, with numerous data categorizations."),
      p("Let's explore!"),
      p("*It is important to keep in mind that the following data are 
        showing correlative relationships, not causative ones. This site is attempting to shed light on what is (and isn't) important, NOT a causal relationship."),
      h3("Source"),
      a("Michael Tauberg: Billboard Hot 100", href = "https://github.com/taubergm/Billboard-Spotify-Lyrics-Music-analysis")
      ))
  })
  
  
  # Plot 1
  top_1 <- songs %>%
    filter(peak_pos == 1) %>%
    count(year)
  
  top_10 <- songs %>%
    filter(peak_pos <= 10) %>%
    count(year)
  
  top_25 <- songs %>%
    filter(peak_pos <= 25) %>%
    count(year)
  
  bot_75 <- songs %>%
    filter(peak_pos >= 26) %>%
    count(year)
  
  all_100 <- songs %>%
    count(year)
      
  
      output$plot1 <- renderPlot({
        if (input$type == "all100"){
          ggplot(all_100, aes(x = year, y = n)) + 
            geom_line() +
            labs(title = "Number of Songs on the Billboard Top Charts",
                 subtitle = "Per Year: 2000-2017",
                 x = "Year",
                 y = "Number of Songs") +
            theme(plot.title = element_text(hjust = 0.5)) + 
            theme(plot.subtitle = element_text(hjust = 0.5)) +
            geom_vline(xintercept=2005.15, colour="grey", linetype="dashed") +
            geom_text(aes(x=2005.15, label="YouTube Launched (2005)\n", y=535), colour="red", angle=90) +
            geom_vline(xintercept=2008, colour="grey", linetype="dashed") +
            geom_text(aes(x=2008, label="Spotify Launched (2008)\n", y=535), colour="blue", angle=90) +
            geom_vline(xintercept=2015.5, colour="grey", linetype="dashed") +
            geom_text(aes(x=2015.5, label="Apple Music Launched (2015)\n", y=535), colour="violet", angle=90) +
            ylim(280,675)
        } else if (input$type == "top1"){
        ggplot(top_1, aes(x = year, y = n)) + 
        geom_line() +
        labs(title = "Number of Songs on the Billboard Top Charts",
             subtitle = "Per Year: 2000-2017",
             x = "Year",
             y = "Number of Songs") +
        theme(plot.title = element_text(hjust = 0.5)) + 
        theme(plot.subtitle = element_text(hjust = 0.5)) +
            geom_vline(xintercept=2005.15, colour="grey", linetype="dashed") +
            geom_text(aes(x=2005.15, label="YouTube Launched (2005)\n", y=23), colour="red", angle=90) +
            geom_vline(xintercept=2008, colour="grey", linetype="dashed") +
            geom_text(aes(x=2008, label="Spotify Launched (2008)\n", y=23), colour="blue", angle=90) +
            geom_vline(xintercept=2015.5, colour="grey", linetype="dashed") +
            geom_text(aes(x=2015.5, label="Apple Music Launched (2015)\n", y=23), colour="violet", angle=90) +
            ylim(5, 30)
        } else if(input$type == "top10"){
          ggplot(top_10, aes(x = year, y = n)) + 
            geom_line() +
            labs(title = "Number of Songs on the Billboard Top Charts",
                 subtitle = "Per Year: 2000-2017",
                 x = "Year",
                 y = "Number of Songs") +
            theme(plot.title = element_text(hjust = 0.5)) + 
            theme(plot.subtitle = element_text(hjust = 0.5)) +
            geom_vline(xintercept=2005.15, colour="grey", linetype="dashed") +
            geom_text(aes(x=2005.15, label="YouTube Launched (2005)\n", y=100), colour="red", angle=90) +
            geom_vline(xintercept=2008, colour="grey", linetype="dashed") +
            geom_text(aes(x=2008, label="Spotify Launched (2008)\n", y=100), colour="blue", angle=90) +
            geom_vline(xintercept=2015.5, colour="grey", linetype="dashed") +
            geom_text(aes(x=2015.5, label="Apple Music Launched (2015)\n", y=100), colour="violet", angle=90) +
            ylim(25, 125)
        } else if(input$type == "top25"){
          ggplot(top_25, aes(x = year, y = n)) + 
            geom_line() +
            labs(title = "Number of Songs on the Billboard Top Charts",
                 subtitle = "Per Year: 2000-2017",
                 x = "Year",
                 y = "Number of Songs") +
            theme(plot.title = element_text(hjust = 0.5)) + 
            theme(plot.subtitle = element_text(hjust = 0.5)) +
            geom_vline(xintercept=2005.15, colour="grey", linetype="dashed") +
            geom_text(aes(x=2005.15, label="YouTube Launched (2005)\n", y=160), colour="red", angle=90) +
            geom_vline(xintercept=2008, colour="grey", linetype="dashed") +
            geom_text(aes(x=2008, label="Spotify Launched (2008)\n", y=160), colour="blue", angle=90) +
            geom_vline(xintercept=2015.5, colour="grey", linetype="dashed") +
            geom_text(aes(x=2015.5, label="Apple Music Launched (2015)\n", y=160), colour="violet", angle=90) +
            ylim(75, 200)
          } else if(input$type == "bot75"){
            ggplot(bot_75, aes(x = year, y = n)) + 
            geom_line() +
            labs(title = "Number of Songs on the Billboard Top Charts",
                 subtitle = "Per Year: 2000-2017",
                 x = "Year",
                 y = "Number of Songs") +
            theme(plot.title = element_text(hjust = 0.5)) + 
            theme(plot.subtitle = element_text(hjust = 0.5)) +
            geom_vline(xintercept=2005.15, colour="grey", linetype="dashed") +
            geom_text(aes(x=2005.15, label="YouTube Launched (2005)\n", y=385), colour="red", angle=90) +
            geom_vline(xintercept=2008, colour="grey", linetype="dashed") +
            geom_text(aes(x=2008, label="Spotify Launched (2008)\n", y=385), colour="blue", angle=90) +
            geom_vline(xintercept=2015.5, colour="grey", linetype="dashed") +
            geom_text(aes(x=2015.5, label="Apple Music Launched (2015)\n", y=410), colour="violet", angle=90) +
            ylim(175, 500)
          }
      })
      
      
      # Plot 2
      
      top_1_new <- songs %>%
        filter(peak_pos == 1) %>%
        filter(broad_genre != "unknown")
      
      top_10_new <- songs %>%
        filter(peak_pos <= 10) %>%
        filter(broad_genre != "unknown")
      
      top_25_new <- songs %>%
        filter(peak_pos <= 25) %>%
        filter(broad_genre != "unknown")
      
      bot_75_new <- songs %>%
        filter(peak_pos >= 26) %>%
        filter(broad_genre != "unknown")
      
      all_100_new <- songs %>%
        filter(broad_genre != "unknown")
      
        
        output$plot2.1 <- renderPlot({
          if (input$type2 == "all100"){
            ggplot(all_100_new, aes(x = broad_genre)) + 
              geom_bar() +
              labs(title = "Songs Appearing in the Billboard Hot 100",
                subtitle = "Based Upon Genre: 2000-2017",
                x = "Genre",
                y = "Number of Appearances") +
          theme(plot.title = element_text(hjust = 0.5)) + 
          theme(plot.subtitle = element_text(hjust = 0.5))
          } else if (input$type2 == "top1"){
            ggplot(top_1_new, aes(x = broad_genre)) + 
              geom_bar() +
              labs(title = "Songs Appearing in the Billboard Hot 100",
                   subtitle = "Based Upon Genre: 2000-2017",
                   x = "Genre",
                   y = "Number of Appearances") +
              theme(plot.title = element_text(hjust = 0.5)) + 
              theme(plot.subtitle = element_text(hjust = 0.5)) 
          } else if(input$type2 == "top10"){
            ggplot(top_10_new, aes(x = broad_genre)) + 
              geom_bar() +
              labs(title = "Songs Appearing in the Billboard Hot 100",
                   subtitle = "Based Upon Genre: 2000-2017",
                   x = "Genre",
                   y = "Number of Appearances") +
              theme(plot.title = element_text(hjust = 0.5)) + 
              theme(plot.subtitle = element_text(hjust = 0.5))
          } else if(input$type2 == "top25"){
            ggplot(top_25_new, aes(x = broad_genre)) + 
              geom_bar() +
              labs(title = "Songs Appearing in the Billboard Hot 100",
                   subtitle = "Based Upon Genre: 2000-2017",
                   x = "Genre",
                   y = "Number of Appearances") +
              theme(plot.title = element_text(hjust = 0.5)) + 
              theme(plot.subtitle = element_text(hjust = 0.5))
          } else if(input$type2 == "bot75"){
            ggplot(bot_75_new, aes(x = broad_genre)) + 
              geom_bar() +
              labs(title = "Songs Appearing in the Billboard Hot 100",
                   subtitle = "Based Upon Genre: 2000-2017",
                   x = "Genre",
                   y = "Number of Appearances") +
              theme(plot.title = element_text(hjust = 0.5)) + 
              theme(plot.subtitle = element_text(hjust = 0.5))
          }
        })
          
            
          output$plot2.2 <- renderPlot({
            year_songs <- 
              songs %>%
              filter(broad_genre != "unknown") %>%
              filter(year %in% input$year)
            
            ggplot(year_songs, aes(x = broad_genre, y = peak_pos)) + 
              geom_boxplot() +
              # geom_text(data = median, aes(x = broad_genre, y = med, label = med), 
              #           size = 3, vjust = -1.5) +
              labs(title = "Peak Billboard Hot 100 Position",
                   subtitle = "Based Upon Genre: 2000-2017",
                   x = "Genre",
                   y = "Peak Position") +
              # How to get the y-axis to say 1 at top and NOT 0???
              scale_y_reverse() +
              theme(plot.title = element_text(hjust = 0.5)) + 
              theme(plot.subtitle = element_text(hjust = 0.5))

            
      }) 
          
          # Plot 3
          
          output$plot3 <- renderPlot({
            
            title_length <- songs %>%
              group_by(year) %>%
              mutate(tlength = nchar(title))
            
            avg_title_length_100 <- title_length %>%
              group_by(year) %>%
              mutate(avgtlength = mean(tlength))
            
            avg_title_length_1 <- title_length %>%
              filter(peak_pos == 1) %>%
              group_by(year) %>%
              mutate(avgtlength = mean(tlength))
            
            avg_title_length_1 <- title_length %>%
              filter(peak_pos == 1) %>%
              group_by(year) %>%
              mutate(avgtlength = mean(tlength))
            
            avg_title_length_10 <- title_length %>%
              filter(peak_pos <= 10) %>%
              group_by(year) %>%
              mutate(avgtlength = mean(tlength))
            
            avg_title_length_25 <- title_length %>%
              filter(peak_pos <= 25) %>%
              group_by(year) %>%
              mutate(avgtlength = mean(tlength))
            
            avg_title_length_75 <- title_length %>%
              filter(peak_pos >= 26) %>%
              group_by(year) %>%
              mutate(avgtlength = mean(tlength))
            
            if (input$type3 == "all100"){
              ggplot(avg_title_length_100, aes(x = year, y = avgtlength)) + 
                geom_smooth(size = 0.4, linetype="dotted", se = FALSE) +
                geom_smooth(method = lm, se=FALSE) +
                labs(title = "Billboard Hot 100 Song Title Length",
                     subtitle = "By Year: 2000-2017",
                     x = "Year",
                     y = "Average Title Length (Characters)") +
                theme(plot.title = element_text(hjust = 0.5)) + 
                theme(plot.subtitle = element_text(hjust = 0.5))
            } else if (input$type3 == "top1"){
              ggplot(avg_title_length_1, aes(x = year, y = avgtlength)) + 
                geom_smooth(size = 0.4, linetype="dotted", se = FALSE) +
                geom_smooth(method = lm, se=FALSE) +
                labs(title = "Billboard Hot 100 Song Title Length",
                     subtitle = "By Year: 2000-2017",
                     x = "Year",
                     y = "Average Title Length (Characters)") +
                theme(plot.title = element_text(hjust = 0.5)) + 
                theme(plot.subtitle = element_text(hjust = 0.5)) 
            } else if(input$type3 == "top10"){
              ggplot(avg_title_length_10, aes(x = year, y = avgtlength)) + 
                geom_smooth(size = 0.4, linetype="dotted", se = FALSE) +
                geom_smooth(method = lm, se=FALSE) +
                labs(title = "Billboard Hot 100 Song Title Length",
                     subtitle = "By Year: 2000-2017",
                     x = "Year",
                     y = "Average Title Length (Characters)") +
                theme(plot.title = element_text(hjust = 0.5)) + 
                theme(plot.subtitle = element_text(hjust = 0.5))
            } else if(input$type3 == "top25"){
              ggplot(avg_title_length_25, aes(x = year, y = avgtlength)) + 
                geom_smooth(size = 0.4, linetype="dotted", se = FALSE) +
                geom_smooth(method = lm, se=FALSE) +
                labs(title = "Billboard Hot 100 Song Title Length",
                     subtitle = "By Year: 2000-2017",
                     x = "Year",
                     y = "Average Title Length (Characters)") +
                theme(plot.title = element_text(hjust = 0.5)) + 
                theme(plot.subtitle = element_text(hjust = 0.5))
            } else if(input$type3 == "bot75"){
              ggplot(avg_title_length_75, aes(x = year, y = avgtlength)) + 
                geom_smooth(size = 0.4, linetype="dotted", se = FALSE) +
                geom_smooth(method = lm, se=FALSE) +
                labs(title = "Billboard Hot 100 Song Title Length",
                     subtitle = "By Year: 2000-2017",
                     x = "Year",
                     y = "Average Title Length (Characters)") +
                theme(plot.title = element_text(hjust = 0.5)) + 
                theme(plot.subtitle = element_text(hjust = 0.5))
            }
          })
          
          output$title_stats <- renderUI({
            
            title_length <- songs %>%
              group_by(year) %>%
              mutate(tlength = nchar(title))
            
            avg_title_length_100 <- title_length %>%
              group_by(year) %>%
              mutate(avgtlength = mean(tlength))
            
            avg_title_length_1 <- title_length %>%
              filter(peak_pos == 1) %>%
              group_by(year) %>%
              mutate(avgtlength = mean(tlength))
            
            avg_title_length_1 <- title_length %>%
              filter(peak_pos == 1) %>%
              group_by(year) %>%
              mutate(avgtlength = mean(tlength))
            
            avg_title_length_10 <- title_length %>%
              filter(peak_pos <= 10) %>%
              group_by(year) %>%
              mutate(avgtlength = mean(tlength))
            
            avg_title_length_25 <- title_length %>%
              filter(peak_pos <= 25) %>%
              group_by(year) %>%
              mutate(avgtlength = mean(tlength))
            
            avg_title_length_75 <- title_length %>%
              filter(peak_pos >= 26) %>%
              group_by(year) %>%
              mutate(avgtlength = mean(tlength))
            
            if (input$type3 == "all100"){
              HTML(stargazer(lm(data = avg_title_length_100,
                                avgtlength ~ year),
                             type = "html",
                             covariate.labels = c("Title Length", "Year"),
                             dep.var.labels="Average Title Length"
              ))
            } else if (input$type3 == "top1"){
              HTML(stargazer(lm(data = avg_title_length_1,
                                avgtlength ~ year),
                             type = "html",
                             covariate.labels = c("Title Length", "Year"),
                             dep.var.labels="Average Title Length"
              ))
            } else if(input$type3 == "top10"){
              HTML(stargazer(lm(data = avg_title_length_10,
                                avgtlength ~ year),
                             type = "html",
                             covariate.labels = c("Title Length", "Year"),
                             dep.var.labels="Average Title Length"
              ))
            } else if(input$type3 == "top25"){
              HTML(stargazer(lm(data = avg_title_length_25,
                                avgtlength ~ year),
                             type = "html",
                             covariate.labels = c("Title Length", "Year"),
                             dep.var.labels="Average Title Length"
              ))
            } else if(input$type3 == "bot75"){
              HTML(stargazer(lm(data = avg_title_length_75,
                                avgtlength ~ year),
                             type = "html",
                             covariate.labels = c("Title Length", "Year"),
                             dep.var.labels="Average Title Length"
              ))
              
            }
            
          })
          
          # Plot 4
          
          output$plot4 <- renderPlot({
            
            duration <- songs %>%
              mutate(duration = as.integer(duration_ms)) %>%
              filter(duration != "NA")
            
            avg_duration_100 <- duration %>%
              group_by(year) %>%
              mutate(avgtime = mean(duration)) %>%
              mutate(avgtime = avgtime/60000)
              
              avg_duration_1 <- duration %>%
                filter(peak_pos == 1) %>%
                group_by(year) %>%
                mutate(avgtime = mean(duration)) %>%
                mutate(avgtime = avgtime/60000)
              
              avg_duration_10 <- duration %>%
                filter(peak_pos <= 10) %>%
                group_by(year) %>%
                mutate(avgtime = mean(duration)) %>%
                mutate(avgtime = avgtime/60000)
              
              avg_duration_25 <- duration %>%
                filter(peak_pos <= 25) %>%
                group_by(year) %>%
                mutate(avgtime = mean(duration)) %>%
                mutate(avgtime = avgtime/60000)
              
              avg_duration_75 <- duration %>%
                filter(peak_pos >= 25) %>%
                group_by(year) %>%
                mutate(avgtime = mean(duration)) %>%
                mutate(avgtime = avgtime/60000)
              
              if (input$type4 == "all100"){
                ggplot(avg_duration_100, aes(x = year, y = avgtime)) + 
                  geom_smooth(size = 0.4, linetype="dotted", se = FALSE) +
                  geom_smooth(method = lm, se=FALSE) +
                  labs(title = "Billboard Hot 100 Song Duration",
                       subtitle = "By Year: 2000-2017",
                       x = "Year",
                       y = "Average Song Length (Minutes)") +
                  theme(plot.title = element_text(hjust = 0.5)) + 
                  theme(plot.subtitle = element_text(hjust = 0.5))
              } else if (input$type4 == "top1"){
                ggplot(avg_duration_1, aes(x = year, y = avgtime)) + 
                  geom_smooth(size = 0.4, linetype="dotted", se = FALSE) +
                  geom_smooth(method = lm, se=FALSE) +
                  labs(title = "Billboard Hot 100 Song Duration",
                       subtitle = "By Year: 2000-2017",
                       x = "Year",
                       y = "Average Song Length (Minutes)") +
                  theme(plot.title = element_text(hjust = 0.5)) + 
                  theme(plot.subtitle = element_text(hjust = 0.5))
              } else if(input$type4 == "top10"){
                ggplot(avg_duration_10, aes(x = year, y = avgtime)) + 
                  geom_smooth(size = 0.4, linetype="dotted", se = FALSE) +
                  geom_smooth(method = lm, se=FALSE) +
                  labs(title = "Billboard Hot 100 Song Duration",
                       subtitle = "By Year: 2000-2017",
                       x = "Year",
                       y = "Average Song Length (Minutes)") +
                  theme(plot.title = element_text(hjust = 0.5)) + 
                  theme(plot.subtitle = element_text(hjust = 0.5))
              } else if(input$type4 == "top25"){
                ggplot(avg_duration_25, aes(x = year, y = avgtime)) + 
                  geom_smooth(size = 0.4, linetype="dotted", se = FALSE) +
                  geom_smooth(method = lm, se=FALSE) +
                  labs(title = "Billboard Hot 100 Song Duration",
                       subtitle = "By Year: 2000-2017",
                       x = "Year",
                       y = "Average Song Length (Minutes)") +
                  theme(plot.title = element_text(hjust = 0.5)) + 
                  theme(plot.subtitle = element_text(hjust = 0.5))
              } else if(input$type4 == "bot75"){
                ggplot(avg_duration_75, aes(x = year, y = avgtime)) + 
                  geom_smooth(size = 0.4, linetype="dotted", se = FALSE) +
                  geom_smooth(method = lm, se=FALSE) +
                  labs(title = "Billboard Hot 100 Song Duration",
                       subtitle = "By Year: 2000-2017",
                       x = "Year",
                       y = "Average Song Length (Minutes)") +
                  theme(plot.title = element_text(hjust = 0.5)) + 
                  theme(plot.subtitle = element_text(hjust = 0.5))
              }
          })
          
          output$duration_stats <- renderUI({
            
            duration <- songs %>%
              mutate(duration = as.integer(duration_ms)) %>%
              filter(duration != "NA")
            
            avg_duration_100 <- duration %>%
              group_by(year) %>%
              mutate(avgtime = mean(duration)) %>%
              mutate(avgtime = avgtime/60000)
            
            avg_duration_1 <- duration %>%
              filter(peak_pos == 1) %>%
              group_by(year) %>%
              mutate(avgtime = mean(duration)) %>%
              mutate(avgtime = avgtime/60000)
            
            avg_duration_10 <- duration %>%
              filter(peak_pos <= 10) %>%
              group_by(year) %>%
              mutate(avgtime = mean(duration)) %>%
              mutate(avgtime = avgtime/60000)
            
            avg_duration_25 <- duration %>%
              filter(peak_pos <= 25) %>%
              group_by(year) %>%
              mutate(avgtime = mean(duration)) %>%
              mutate(avgtime = avgtime/60000)
            
            avg_duration_75 <- duration %>%
              filter(peak_pos >= 25) %>%
              group_by(year) %>%
              mutate(avgtime = mean(duration)) %>%
              mutate(avgtime = avgtime/60000)
            
            if (input$type4 == "all100"){
              HTML(stargazer(lm(data = avg_duration_100,
                                avgtime ~ year),
                             type = "html",
                             covariate.labels = c("Duration", "Year"),
                             dep.var.labels="Average Length (min)"
              ))
            } else if (input$type4 == "top1"){
              HTML(stargazer(lm(data = avg_duration_1,
                                avgtime ~ year),
                             type = "html",
                             covariate.labels = c("Duration", "Year"),
                             dep.var.labels="Average Length (min)"
              ))
            } else if(input$type4 == "top10"){
              HTML(stargazer(lm(data = avg_duration_10,
                                avgtime ~ year),
                             type = "html",
                             covariate.labels = c("Duration", "Year"),
                             dep.var.labels="Average Length (min)"
              ))
            } else if(input$type4 == "top25"){
              HTML(stargazer(lm(data = avg_duration_25,
                                avgtime ~ year),
                             type = "html",
                             covariate.labels = c("Duration", "Year"),
                             dep.var.labels="Average Length (min)"
              ))
            } else if(input$type4 == "bot75"){
              HTML(stargazer(lm(data = avg_duration_75,
                                avgtime ~ year),
                             type = "html",
                             covariate.labels = c("Duration", "Year"),
                             dep.var.labels="Average Length (min)"
              ))
              
            }
            
          })
          
          
          
          output$plot5.1 <- renderPlot({
            
            songs$tempo <- as.numeric(songs$tempo)
            songs$liveness <- as.numeric(songs$liveness)
            songs$energy <- as.numeric(songs$energy)
            songs$speechiness <- as.numeric(songs$speechiness)
            songs$danceability <- as.numeric(songs$danceability)
            
            songs_filtered <- songs %>%
              filter(energy != "unknown") %>%
              filter(liveness != "unknown") %>%
              filter(tempo != "unknown") %>%
              filter(speechiness != "unknown") %>%
              filter(danceability != "unknown") %>%
              filter(year %in% input$year2)
            
            
            if (input$analysis == "energy"){
              ggplot(songs_filtered, aes(x = energy, y = peak_pos))  +
                geom_jitter(alpha = .3) +
                stat_smooth(method = "lm") +
                scale_y_reverse() +
                labs(title = "Energy Analysis",
                     subtitle = "Based Upon Top Position",
                     x = "Energy",
                     y = "Song's Top Position") +
                theme(plot.title = element_text(hjust = 0.5))
            } else if (input$analysis == "liveness"){
              ggplot(songs_filtered, aes(x = liveness, y = peak_pos)) + 
                geom_jitter(alpha = .3) +
                stat_smooth(method = "lm") +
                scale_y_reverse() +
                labs(title = "Liveness Analysis",
                     subtitle = "Based Upon Top Position",
                     x = "Liveness",
                     y = "Song's Top Position") +
                theme(plot.title = element_text(hjust = 0.5))
            } else if(input$analysis == "tempo"){
              ggplot(songs_filtered, aes(x = tempo, y = peak_pos)) + 
                geom_jitter(alpha = .3) +
                stat_smooth(method = "lm") +
                scale_y_reverse() +
                labs(title = "Tempo Analysis",
                     subtitle = "Based Upon Top Position",
                     x = "Tempo (bpm)",
                     y = "Song's Top Position") +
                theme(plot.title = element_text(hjust = 0.5))
            } else if(input$analysis == "speechiness"){
              ggplot(songs_filtered, aes(x = speechiness, y = peak_pos)) + 
                geom_jitter(alpha = .3) +
                stat_smooth(method = "lm") +
                scale_y_reverse() +
                labs(title = "Speechiness Analysis",
                     subtitle = "Based Upon Top Position",
                     x = "Speechiness",
                     y = "Song's Top Position") +
                theme(plot.title = element_text(hjust = 0.5))
            } else if(input$analysis == "danceability"){
              ggplot(songs_filtered, aes(x = danceability, y = peak_pos)) + 
                geom_jitter(alpha = .3) +
                stat_smooth(method = "lm") +
                scale_y_reverse() +
                labs(title = "Danceability Analysis",
                     subtitle = "Based Upon Top Position",
                     x = "Danceability",
                     y = "Song's Top Position") +
                theme(plot.title = element_text(hjust = 0.5))
            }
            
            
          })
          
          output$plot5.2 <- renderPlot({
            
            songs$tempo <- as.numeric(songs$tempo)
            songs$liveness <- as.numeric(songs$liveness)
            songs$energy <- as.numeric(songs$energy)
            songs$speechiness <- as.numeric(songs$speechiness)
            songs$danceability <- as.numeric(songs$danceability)
            
            songs_filtered1 <- songs %>%
              filter(energy != "unknown") %>%
              filter(liveness != "unknown") %>%
              filter(tempo != "unknown") %>%
              filter(speechiness != "unknown") %>%
              filter(danceability != "unknown") %>%
              filter(year %in% input$year2) %>%
              filter(peak_pos == 1)
            
            songs_filtered_rest <- songs %>%
              filter(energy != "unknown") %>%
              filter(liveness != "unknown") %>%
              filter(tempo != "unknown") %>%
              filter(speechiness != "unknown") %>%
              filter(danceability != "unknown") %>%
              filter(year %in% input$year2) %>%
              filter(peak_pos != 1)
            
            if (input$analysis == "energy"){
              ggplot(songs_filtered1) + 
                geom_density(aes(x = energy, colour="1"),
                             alpha = .2, 
                             fill="#0000CC") +
                geom_density(data = songs_filtered_rest,
                             aes(x = energy, colour="2-100"),
                             alpha = .2, 
                             fill = "#CC0000") +
                scale_colour_manual(name="Song's Top Position",values=c('1' = "#0000CC",
                                                                 '2-100' = "#CC0000")) +
                labs(subtitle = "Density Comparison: Top 1 vs. Rest",
                     x = "Energy",
                     y = "Density")
            } else if (input$analysis == "liveness"){
              ggplot(songs_filtered1) +
              geom_density(aes(x = liveness, colour="1"),
                     alpha = .2, 
                     fill="#0000CC") +
                geom_density(data = songs_filtered_rest,
                             aes(x = liveness, colour="2-100"),
                             alpha = .2, 
                             fill = "#CC0000") +
                scale_colour_manual(name="Song's Top Position",values=c('1' = "#0000CC",
                                                                        '2-100' = "#CC0000"))+
                labs(subtitle = "Density Comparison: Top 1 vs. Rest",
                     x = "Liveness",
                     y = "Density")
            } else if(input$analysis == "tempo"){
              ggplot(songs_filtered1) +
                geom_density(aes(x = tempo, colour="1"),
                     alpha = .2, 
                     fill="#0000CC") +
                geom_density(data = songs_filtered_rest,
                             aes(x = tempo, colour="2-100"),
                             alpha = .2, 
                             fill = "#CC0000") +
                scale_colour_manual(name="Song's Top Position",values=c('1' = "#0000CC",
                                                                        '2-100' = "#CC0000")) +
                labs(subtitle = "Density Comparison: Top 1 vs. Rest",
                     x = "Tempo (bpm)",
                     y = "Density")
            } else if(input$analysis == "speechiness"){
              ggplot(songs_filtered1) +
                geom_density(aes(x = speechiness, colour="1"),
                     alpha = .2, 
                     fill="#0000CC") +
                geom_density(data = songs_filtered_rest,
                             aes(x = speechiness, colour="2-100"),
                             alpha = .2, 
                             fill = "#CC0000") +
                scale_colour_manual(name="Song's Top Position",values=c('1' = "#0000CC",
                                                                        '2-100' = "#CC0000"))+
                labs(subtitle = "Density Comparison: Top 1 vs. Rest",
                     x = "Speechiness",
                     y = "Density")
            } else if(input$analysis == "danceability"){
              ggplot(songs_filtered1) +
                geom_density(aes(x = danceability, colour="1"),
                     alpha = .2, 
                     fill="#0000CC") +
                geom_density(data = songs_filtered_rest,
                             aes(x = danceability, colour="2-100"),
                             alpha = .2, 
                             fill = "#CC0000") +
                scale_colour_manual(name="Song's Top Position",
                                    values=c('1' = "#0000CC",'2-100' = "#CC0000"))+
                labs(subtitle = "Density Comparison: Top 1 vs. Rest",
                     x = "Danceability",
                     y = "Density")
            
            }
            
          })
          
          output$music_stats <- renderUI({
            
            songs$tempo <- as.numeric(songs$tempo)
            songs$liveness <- as.numeric(songs$liveness)
            songs$energy <- as.numeric(songs$energy)
            songs$speechiness <- as.numeric(songs$speechiness)
            songs$danceability <- as.numeric(songs$danceability)
            
            lm_data <- songs %>%
              filter(energy != "unknown") %>%
              filter(liveness != "unknown") %>%
              filter(tempo != "unknown") %>%
              filter(speechiness != "unknown") %>%
              filter(danceability != "unknown") %>%
              filter(year %in% input$year2)
          
            if (input$analysis == "energy"){
              HTML(stargazer(lm(data = lm_data,
                                energy ~ peak_pos),
                             type = "html",
                             covariate.labels = c("Energy", "Top Position"),
                             dep.var.labels="Energy"
              ))
            } else if (input$analysis == "liveness"){
              HTML(stargazer(lm(data = lm_data,
                                liveness ~ peak_pos),
                             type = "html",
                             covariate.labels = c("Liveness", "Top Position"),
                             dep.var.labels="Energy"
              ))
            } else if(input$analysis == "tempo"){
              HTML(stargazer(lm(data = lm_data,
                                tempo ~ peak_pos),
                             type = "html",
                             covariate.labels = c("Tempo", "Top Position"),
                             dep.var.labels="Energy"
              ))
            } else if(input$analysis == "speechiness"){
              HTML(stargazer(lm(data = lm_data,
                                speechiness ~ peak_pos),
                             type = "html",
                             covariate.labels = c("Speechiness", "Top Position"),
                             dep.var.labels="Energy"
              ))
            } else if(input$analysis == "danceability"){
              HTML(stargazer(lm(data = lm_data,
                                danceability ~ peak_pos),
                             type = "html",
                             covariate.labels = c("Danceability", "Top Position"),
                             dep.var.labels="Energy"
              ))
              
            }
            
          })
          
}
  
shinyApp(ui, server)
