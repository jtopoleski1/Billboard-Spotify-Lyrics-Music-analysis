# Loaded all packages neccessary for the project

library(shiny)
library(shinyWidgets)
library(ggridges)
library(sjPlot)
library(tm)
library(RColorBrewer)
library(RCurl)
library(XML)
library(shinythemes)
library(stargazer)
library(tidyverse)

# Loaded tinyverse last in order to make sure no other package overrides its functions

library(tidyverse)

# Read in the songs data, previously manipulated in a RMD workspace and converted
# to a .RDS for easy incorperation

songs <- read_rds("songs.rds")


# Define the navbar overall heading, which allows for clean presentation of tabs to switch between
# chose the lumen shiny theme for a clean presentation of data (it's a simple, clean theme)

ui <- navbarPage(strong("Bop to the Top: What Makes a Billboard Top Hit?"), 
                 theme = shinytheme("lumen"),
       
         # TAB 1
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
       
       # TAB 2
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
       
       # TAB 3
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
       
       # TAB 4
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
       
       # TAB 5 
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
       
       # TAB 6
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
                  helpText("Danceability: describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable."),
                  helpText("With so much data, it is hard to see a sustainable pattern. For most components, there seems to be no difference. However, dancability shows a relatively strong positive upward trend. Additionally, looking at the density plot shows that songs in the number one position are consistently more dancable than those in the 2-100 position. So, Step 4: put some boogie in it!")
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
  
  # TAB 1: OVERVIEW
  # This page sets up the first tab, titled "Overview". It provides a brief summary of
  # what the project is about (using HTML), along with a precautionary measure about the 
  # findings. I chose to include this because while this data may point to many conclusions, 
  # all findings MUST be understood as simply correlative.
  # Finally, I give a source to the data, using href to embed a link within the text so that 
  # readers can easily access the data for analysis and reference.
  
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
  
  
# TAB 2: THE CHANGING MUSIC INDUSTRY
  # To start this tab, I first define the data I am looking to analyze.
  # In specific, I need to count how many songs appear within the top charts
  # per year. Additionally, I create 5 different variations of this, each
  # stratified by the positions of the songs I wish to look at (and allow
  # manipulation of within the sidebar).
  
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
  
  # Next, I render the plot that appears within the first tab. In order to ensure
  # it is interactive, I then define "if then" and "else" argments to change the input of 
  # the graph depending on what stratification is selected within the sidebar. This
  # is all using the data I generated above. The "if" conditions are presented for
  # selection within the sidebar, which is further explained above in the UI.
  # Finally, each variation of the graph is a line graph with the addition of 
  # vertical lines added during the time periods when YouTube, Spotify, and Apple
  # Music were each launched. I chose to include this (and to put them at different
  # vertical levels) to assist the viewer in considering some salient factors to the 
  # situation we are analyzing.
  
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
      
      
#TAB 3: GENRE
   # To start this tab, I first define the data I am looking to analyze.
   # In specific, I need to filter the songs by the categories of peak 
   # position that I present as choices within the sidebar. 
   # STYLISTIC CHOICE: I decided to filter out all songs in the "unknown"
   # genre category because it added unncessary noise to my charts and added
   # uncertainty. In general, I am just trying to look at what genres make a 
   # difference, and "unknown" is irrelevant to this conversation.
      
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
      
      # Graph 1
      # I render the first plot that appears within the second tab. In order to ensure
      # it is interactive, I then define "if then" and "else" argments to change the input of 
      # the graph depending on what stratification is selected within the sidebar. This
      # is all using the data I generated above. The "if" conditions are presented for
      # selection within the sidebar, which is further explained above in the UI.
      # This graph is a bar chart that counts the number of songs that appeaer on the
      # top charts per genre. 
      
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
          
        # Graph 2
        # Next, I render the second plot that appears within the second tab. I first
        # define the the input to make the graph interactive. In this case, the input
        # is year, a selected input the viewer can choose within the sidebar panel.
        # Like above, a stylistically chose to filter to out the "unknown" genre for
        # the same reason. 
        # Fianlly, I render a box and whiskers plot that analyzes the IQR and median
        # of all of the songs that do appear on the charts (by genre). This allows the 
        # viewer to see where songs tend to peak at per genre when they do actually
        # appear on the top charts.
        # STYLISTIC NOTE: I chose to reverse the y-axis scale because it makes sense
        # from a viewer perspective for a higher song position to be closer to be at 
        # the top of the frame of reference. In this case, a lower number actually means
        # a higher position (which is why 1 is at the very top).
            
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
              scale_y_reverse() +
              theme(plot.title = element_text(hjust = 0.5)) + 
              theme(plot.subtitle = element_text(hjust = 0.5))

            
      }) 
          
#TAB 4: TITLE LENGTH
  # To start this tab, I first define the data I am looking to analyze.
  # In specific, I count the number of characters within every song's title.
  # Then, to stratify this to align with the input selections of the songs'
  # highest ratings, I find the mean number of characters per song within each
  # stratification.
          
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
            
            # Graph
            # I render the plot that appears within the fourth tab. In order to ensure
            # it is interactive, I then define "if then" and "else" argments to change the input of 
            # the graph depending on what stratification is selected within the sidebar. This
            # is all using the data I generated above. The "if" conditions are presented for
            # selection within the sidebar, which is further explained above in the UI.
            # The graphs rendered here are line plots that show the change of title length over
            # time. 
            # STYLISTIC NOTE: I chose to include a linear model to see the general trend as well
            # as a smoothed line in order to see the individual variation per year. This proved to
            # be a worthy addition for the "Top 1" category which has a linear model with a positive
            # slope, but has title lengths that have decreased in length within more recent years. Within
            # the linear model, I mute the standard error range because it adds too much clutter.
            
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
          
       # Summary Statistics
          # Again, I first define the data I am looking to analyze.
          # In specific, I count the number of characters within every song's title.
          # Then, to stratify this to align with the input selections of the songs'
          # highest ratings, I find the mean number of characters per song within each
          # stratification.
          
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
            
            # I render the stats table that appears within the fourth tab. In order to ensure
            # it is interactive, I then define "if then" and "else" argments to change the input 
            # info of the table depending on what stratification is selected within the sidebar. 
            # The "if" conditions are presented for selection within the sidebar, which is further 
            # explained above in the UI.
            # Within the conditional, I use stargzer, which allows for the creation of well-formatted
            # regression tables, including frame division. Into this, I simply apply a linear
            # model to the defined variables above and rename the labels to match the format of the graph.
            # I analyze the relationship between average title length and year to generate this data.
            
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
          
#TAB 5: DURATION
  # To start this tab, I first define the data I am looking to analyze.
  # In specific, I mutate the duration from a character vector to an integer,
  # allowing me to apply mathematical functions to it. Then, to stratify this 
  # to align with the input selections of the songs' highest ratings, I find 
  # the mean time for a song within each year based upon the song's peak position.
  # STYLISTIC NOTE: I chose to divide each of the mean times by 60,000 in order to
  # display the times within minutes, a varibale that is easier for the viewer to
  # understand. I also chose to filter out any song that has a duration of "NA" 
  # becuase it is not relevant to the overall trend I am attempting to analyze.
          
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
              
              # Graph
              # I render the plot that appears within the fifth tab. In order to ensure
              # it is interactive, I then define "if then" and "else" argments to change the input of 
              # the graph depending on what stratification is selected within the sidebar. This
              # is all using the data I generated above. The "if" conditions are presented for
              # selection within the sidebar, which is further explained above in the UI.
              # The graphs rendered here are line plots that show the change of song length over
              # time. 
              # STYLISTIC NOTE: Again, I chose to include a linear model to see the general trend 
              # as well as a smoothed line in order to see the individual variation per year. Within
              # the linear model, I mute the standard error range because it adds too much clutter.
              
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
          
        # Summary Statistics
          # Again, I first define the data I am looking to analyze.
          # I mutate the duration from a character vector to an integer,
          # allowing me to apply mathematical functions to it. Then, to stratify this 
          # to align with the input selections of the songs' highest ratings, I find 
          # the mean time for a song within each year based upon the song's peak position.
          # Like above, I also stylistically filter out songs lengths that are "NA".
        
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
            
            # I render the stats table that appears within the fifth tab. In order to ensure
            # it is interactive, I then define "if then" and "else" argments to change the input 
            # info of the table depending on what stratification is selected within the sidebar. 
            # The "if" conditions are presented for selection within the sidebar, which is further 
            # explained above in the UI.
            # Within the conditional, I use stargzer, which allows for the creation of well-formatted
            # regression tables, including frame division. Into this, I simply apply a linear
            # model to the defined variables above and rename the labels to match the format of the graph.
            # I analyze the relationship between average song length and year to generate this data.
            
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
          
#TAB 6: MUSIC ANALYSIS
  # Within this section, I chose five compoenents of songs to look at. Every song was
  # rated in each of these categories with some type of numeric by Billboard and are 
  # more specifically explained within the "Music Analysis" sidebar panel that appears 
  # within the UI.
  # First, I mutate each of the components into numerics in order to allow me to compute
  # statistical and mathematical functions with them.
  # STYLISTIC NOTE: I then chose to filter out all songs with "unknown" values for any
  # of the components. This filtered out a relatively small amount of songs and allowed 
  # the analysis to focus on the specific effects of the components.
          
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
            
            # Graph 1
            # I render the first plot that appears within the sixth tab. In order to ensure
            # it is interactive, I then define "if then" and "else" argments to change the input of 
            # the graph depending on what stratification is selected within the sidebar. This
            # is all using the data I generated above. The "if" conditions are presented for
            # selection within the sidebar, which is further explained above in the UI.
            # The graph output is a dot plot with a liner model line applied to it.
            # STYLISTIC NOTE: I chose a relatively low aplha and chose to jitter the dot plot
            # in order to allow the viewer to better see the concentration of points. Once again,
            # I chose to reverse the y-axis scale because it makes sense that (from the persepctive)
            # of the viewer) for a higher song position to be closer in position to the top of the 
            # frame of reference. In this case, a lower number actually means a higher position 
            # (which is why 1 is at the very top).
            
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
          
          # Graph 2
          # First, I mutate each of the components into numerics in order to allow me to compute
          # statistical and mathematical functions with them.
          # STYLISTIC NOTE: I then chose to filter out all songs with "unknown" values for any
          # of the components. This filtered out a relatively small amount of songs and allowed 
          # the analysis to focus on the specific effects of the components.
          # In "songs_filtered1" I also filter for only the songs that reach the top peak position.
          # Alternatively, in "songs_filtered_rest", I filter for songs within the 2-100 position.
          # This allows me to compare the two stratifications within my density plot that I created.
          
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
            
            # Then, I render the second plot that appears within the sixth tab. In order to ensure
            # it is interactive, I then define "if then" and "else" argments to change the input 
            # of the graph depending on what stratification is selected within the sidebar. This
            # is all using the data I generated above. The "if" conditions are presented for
            # selection within the sidebar, which is further explained above in the UI.
            # I create a density plot that looks at the concentration of either #1 or #2-100
            # songs based upon each component.
            # STYLISTIC NOTE: I define a relatively low alpha so that the viewer can easily see
            # the overlap that appears. Additionally, the peak position of songs are colored within
            # the key, which I defined manually using scale_colour_maual.
            
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
          
        # Summary Statistics
          # Again, I first define the data I am looking to analyze.
          # I mutate each of the components into numerics in order to allow me to compute
          # statistical and mathematical functions with them.
          # Like above, I then stylistically chose to filter out all songs with "unknown" values for
          # any of the components. This filtered out a relatively small amount of songs and allowed 
          # the analysis to focus on the specific effects of the components.
          
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
            
            # I render the stats table that appears within the sixth tab. In order to ensure
            # it is interactive, I then define "if then" and "else" argments to change the input 
            # info of the table depending on what stratification is selected within the sidebar. 
            # The "if" conditions are presented for selection within the sidebar, which is further 
            # explained above in the UI.
            # Within the conditional, I use stargzer, which allows for the creation of well-formatted
            # regression tables, including frame division. Into this, I simply apply a linear
            # model to the defined variables above and rename the labels to match the format of the graph.
            # I analyze the relationship between each of the components and the song's peak position to 
            # generate this data.
          
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

# Finally, I run the shiny app with both the UI and the server defined.
  
shinyApp(ui, server)
