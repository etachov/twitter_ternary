

library(shiny)
library(dplyr)
library(markdown)
library(ggtern)
library(Cairo)

# using cairo for that crystal clear look
options(shiny.usecairo = T)

# https://twitter.com/united/status/851471781827420160

# using a sliiiighly modified version of https://bootswatch.com/cosmo/ for styling
ui <- fluidPage(theme = "bootstrap.css",
   
   fluidRow(
     column(3),
     column(6, align = "center", titlePanel("Did you make a Bad Tweet?"))
     ),
   
   
   fluidRow(
     column(3),
     # set all inputs to NA initially 
     column(2, align = "center", numericInput("replies", "# of replies", NA, min = 0, max = 1000000)),
     column(2, align = "center", numericInput("retweets", "# of retweets", NA, min = 0, max = 1000000)),
     column(2, align = "center", numericInput("favs", "# of favorites", NA, min = 0, max = 1000000))
     ), 
   
   fluidRow(
     column(3),
     column(6, align= "center", submitButton("Find out!"))
   ), 
   
   fluidRow(
     column(3),
     column(6, align = "center", plotOutput("twitter_tern"))
   ),
   
   fluidRow(),

   fluidRow(
     column(3),
     column(6, includeMarkdown("background.md"))
   )
   
  )


server <- function(input, output, session) {
   
  
   dat <- reactive({
     
     # if NA fill with a zero so that if people leave one blank and submit it will still work.
     data.frame(replies = ifelse(is.na(input$replies), 0, input$replies),
                retweets = ifelse(is.na(input$retweets), 0, input$retweets),
                favs = ifelse(is.na(input$favs), 0, input$favs)) %>%
       # calculate the %s
       mutate(replies = replies,
              total.interactions = replies + retweets + favs, 
              replies.pct = replies/total.interactions, 
              retweets.pct = retweets/total.interactions, 
              favs.pct = favs/total.interactions)
     
   })
  

   output$twitter_tern <- renderPlot({

     # coordinates for the background shading
     background_dat <- data_frame(
       point.id = c(rep(1:3,2)),
       y = c(0, 0.4, 0,  0, 0.6, 0), 
       x = c(1, 0.6, 0.6,  1, 0.4, 0.4), 
       z= c( 0, 0, 0.4,  0, 0, 0.6), 
       label = c(rep("A", 3), rep("B", 3))
     )
     
     
    submitted_data <- dat()
     
    p <- ggtern() +
      geom_polygon(data = background_dat, aes(x,y,z, fill=label,group=label), fill = "red", alpha=.2) +
      geom_crosshair_tern(data = submitted_data, aes(x = replies.pct,
                                                     y = retweets.pct,
                                                     z = favs.pct), size = .7)+
      geom_point(data = submitted_data, aes(x = replies.pct,
                                            y = retweets.pct,
                                            z = favs.pct), pch = 21, fill = "white", size = 5) +
      scale_T_continuous(breaks = c(0, 1), labels = c("", "")) +
      scale_L_continuous(breaks = c(0, 1), labels = c("", "")) +
      scale_R_continuous(breaks = c(0, 1), labels = c("", "")) +
      labs(title = ifelse(is.na(dat()$replies.pct[1]), "\n",
                          ifelse(dat()$total.interactions[1] < 50, "\nToo soon to tell.",
                           ifelse(dat()$replies.pct[1] > .6, "\nWoah that's a Bad Tweet", 
                                  ifelse(dat()$replies.pct[1] > .4,  "\nDelete now!",
                                         ifelse(dat()$replies.pct[1] > .3, "\nNot looking good...",
                                                "\nCongrats! This isn't a Bad Tweet"))))),
           # need to redo this with glue https://github.com/tidyverse/glue
           subtitle = ifelse(is.na(dat()$replies.pct[1]), "\n", 
                                   paste0(dat()$total.interactions[1], " interactions\n",
                                          round(100*dat()$replies.pct[1]), "% replies ",
                                          round(100*dat()$retweets.pct[1]), "% retweets ",
                                          round(100*dat()$favs.pct[1]), "% favs")),
            x = "", xarrow = "Higher % replies",
            y = "", yarrow = "Higher % retweets",
            z = "", zarrow = "Higher % favs") +
      theme_arrowlong() +
      # so that points near the edge won't be cut off
      theme_nomask() +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
            plot.subtitle = element_text(size = 14, hjust = 0.5), 
            axis.text = element_text(size = 14), 
            panel.background =  element_rect(fill = NA, colour = "black", size = 0.25), 
            panel.grid.major =  element_line(colour = "black", size = .2))
    
   print(p)


   })
   
   

}

# Run the application 
shinyApp(ui = ui, server = server)


