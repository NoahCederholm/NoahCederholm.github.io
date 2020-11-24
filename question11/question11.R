library(tidyverse)
library(shiny)

d = read_csv('Teams.csv')

variables_names = names(d)

ui <- fluidPage(
  
  titlePanel("Density Plot"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      checkboxGroupInput(inputId = "teamID", label = "Select AL East Team",
                         c("BOS", "NYA", "TBA", "BAL", "TOR"), inline = TRUE
                         
      ),
      selectInput(
        inputId ="var1",
        label = "Select a Variable",
        choices = variables_names, selected = "yearID"
      ),
      selectInput(
        inputId ="var2",
        label = "Select a Variable",
        choices = variables_names, selected = "HR"
      ),
      
      sliderInput(inputId = "yearID",
                  "Select Age Range:",
                  min = min(d$yearID, na.rm=TRUE),
                  max = max(d$yearID, na.rm=TRUE),
                  value= c(min(d$yearID),max(d$yearID))
      ),
      
      radioButtons(inputId = "plot_choice", 
                   label = h3("Select Plot:"),
                   choices = c("Scatter Plot" = "point",
                               "Line Plot" = "line"),
                   selected = 'point'
      ), 
      
      
      
      
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      
      plotOutput(outputId = 'show_plot')
    )
  )
)

# server is a function! 
server <- function(input, output) {
  
  
  output$show_plot <- renderPlot({
    
    v1 = input$var1
    v2 = input$var2
    v3 = input$teamID
    
    
    
    library(ggplot2)
    
    if(input$plot_choice == 'point')
      
    {
      d <- d %>% filter(teamID %in% input$teamID, yearID>input$yearID[1], yearID<input$yearID[2])
      ggplot(d, aes(x = d[[v1]], y = d[[v2]]))+
        geom_point()+
        labs(x = v1, y = v2)
    }
    
    else
    {
      d <- d %>% filter(teamID %in% input$teamID, yearID>input$yearID[1], yearID<input$yearID[2])
      ggplot(d, aes(x = d[[v1]], y = d[[v2]]))+
        geom_line()+
        labs(x = v1, y = v2)
    }
    
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)