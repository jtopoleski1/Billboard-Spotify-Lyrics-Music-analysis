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
library(tidyverse)

songs <- read_rds("songs.rds")


# Define UI for application that draws a histogram
ui <- navbarPage(strong("Bop to the Top: What Makes a Billboard Top Hit?"), 
                 theme = shinytheme("lumen"),
              
       tabPanel("Changing Music Industry",
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("type", 
                                "Song's Highest Rating:",
                                c("View All" = "all100",
                                  "Top 1" = "top1",
                                  "Top 10" = "top10",
                                  "Top 25" = "top25",
                                  "26-100" = "bot75")),
                    helpText("To better understand the Billboard Top 100, let's first explore the rapidly changing music industry."),
                    helpText("The emergence of new music sharing technology has led to a rapid increase in the distibution of music. As shown to the right, there have been more and more songs appearing on the chart. This is likely to to the proliferation of music sharing services that have given many more listeners access to a wider, more diverse array of music.")
        
                   ),
                  
                  mainPanel(
                    plotOutput("plot1")
                  )
                )
       ),
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
                  
                  mainPanel(
                    plotOutput("plot2.1"),
                    plotOutput("plot2.2")
                  ))),
       tabPanel("Title Length",
                sidebarLayout(
                  sidebarPanel(
                    radioButtons(radioButtons("type3", 
                                              "Song's Highest Rating:",
                                              c("View All" = "all100",
                                                "Top 1" = "top1",
                                                "Top 10" = "top10",
                                                "Top 25" = "top25",
                                                "26-100" = "bot75"))),
                    helpText("Over time, song titles within the Top 100 have been getting significantly shorter."),
                    helpText("Step 1 of 'How to Make a Top Hit Song': a shorter title is the way to go.")
                  ),
                  
                  mainPanel(
                    plotOutput("plot3")
                  )
                )))

server <- function(input, output) {

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
                geom_smooth() +
                labs(title = "Billboard Hot 100 Song Title Length",
                     subtitle = "By Year: 2000-2017",
                     x = "Year",
                     y = "Average Title Length (Characters)") +
                theme(plot.title = element_text(hjust = 0.5)) + 
                theme(plot.subtitle = element_text(hjust = 0.5))
            } else if (input$type3 == "top1"){
              ggplot(avg_title_length_1, aes(x = year, y = avgtlength)) + 
                geom_smooth() +
                labs(title = "Billboard Hot 100 Song Title Length",
                     subtitle = "By Year: 2000-2017",
                     x = "Year",
                     y = "Average Title Length (Characters)") +
                theme(plot.title = element_text(hjust = 0.5)) + 
                theme(plot.subtitle = element_text(hjust = 0.5)) 
            } else if(input$type3 == "top10"){
              ggplot(avg_title_length_10, aes(x = year, y = avgtlength)) + 
                geom_smooth() +
                labs(title = "Billboard Hot 100 Song Title Length",
                     subtitle = "By Year: 2000-2017",
                     x = "Year",
                     y = "Average Title Length (Characters)") +
                theme(plot.title = element_text(hjust = 0.5)) + 
                theme(plot.subtitle = element_text(hjust = 0.5))
            } else if(input$type3 == "top25"){
              ggplot(avg_title_length_25, aes(x = year, y = avgtlength)) + 
                geom_smooth() +
                labs(title = "Billboard Hot 100 Song Title Length",
                     subtitle = "By Year: 2000-2017",
                     x = "Year",
                     y = "Average Title Length (Characters)") +
                theme(plot.title = element_text(hjust = 0.5)) + 
                theme(plot.subtitle = element_text(hjust = 0.5))
            } else if(input$type3 == "bot75"){
              ggplot(avg_title_length_75, aes(x = year, y = avgtlength)) + 
                geom_smooth() +
                labs(title = "Billboard Hot 100 Song Title Length",
                     subtitle = "By Year: 2000-2017",
                     x = "Year",
                     y = "Average Title Length (Characters)") +
                theme(plot.title = element_text(hjust = 0.5)) + 
                theme(plot.subtitle = element_text(hjust = 0.5))
            }
          })
}
      
      
      

shinyApp(ui, server)
  

              
                    
    
                    
        

