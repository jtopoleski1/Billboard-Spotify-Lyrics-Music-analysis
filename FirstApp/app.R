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
              
       tabPanel("# of Songs on List",
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("type", 
                                "Song's Highest Rating:",
                                c("View All" = "all100",
                                  "Top 1" = "top1",
                                  "Top 10" = "top10",
                                  "Top 25" = "top25",
                                  "26-100" = "bot75")),
                    helpText("The emergence of new music sharing technology has led to a rapid increase in the distibution of music. As shown in all of the Billboard Hot 100 songs, there have been more and more songs appearing on the chart.")
        
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
                                 "Song's Highest Rating:",
                                 c("View All" = "all100",
                                   "Top 1" = "top1",
                                   "Top 10" = "top10",
                                   "Top 25" = "top25",
                                   "26-100" = "bot75")),
                    pickerInput("year",
                                "Year:",
                                choices = unique(songs$year),
                                selected = unique(songs$year),
                                options = list(`actions-box` = TRUE),
                                multiple = TRUE),
                    helpText("HERE.")
                  ),
                  
                  mainPanel(
                    plotOutput("plot2.1"),
                    plotOutput("plot2.2")
                  ))))

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
        
        
        # year2 <- reactive({
        #   req(input$year)
        #   if(input$year == "2017"){
        #     songs %>% filter(year == 2017)
        #   } else if(input$year == "2016"){
        #     songs %>% filter(year == 2016)
        #   } else if(input$year == "2015"){
        #     songs %>% filter(year == 2015)
        #   } else if(input$year == "2014"){
        #     songs %>% filter(year == 2014)
        #   } else if(input$year == "2013"){
        #     songs %>% filter(year == 2013)
        #   } else if(input$year == "2012"){
        #     songs %>% filter(year == 2012)
        #   } else if(input$year == "2011"){
        #     songs %>% filter(year == 2011)
        #   } else if(input$year == "2010"){
        #     songs %>% filter(year == 2010)
        #   } else if(input$year == "2009"){
        #     songs %>% filter(year == 2009)
        #   } else if(input$year == "2008"){
        #     songs %>% filter(year == 2008)
        #   } else if(input$year == "2007"){
        #     songs %>% filter(year == 2007)
        #   } else if(input$year == "2006"){
        #     songs %>% filter(year == 2006)
        #   } else if(input$year == "2005"){
        #     songs %>% filter(year == 2005)
        #   } else if(input$year == "2004"){
        #     songs %>% filter(year == 2004)
        #   } else if(input$year == "2003"){
        #     songs %>% filter(year == 2003)
        #   } else if(input$year == "2002"){
        #     songs %>% filter(year == 2002)
        #   } else if(input$year == "2001"){
        #     songs %>% filter(year == 2001)
        #   } else if(input$year == "2000"){
        #     songs %>% filter(year == 2000)
        #   }})
      
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

}
      
      
      

shinyApp(ui, server)
  

              
                    
    
                    
        

