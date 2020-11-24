library(tidyverse)
library(shiny)

d <- read_csv('titanic.csv')

variables_names = names(d)

ui <- fluidPage(
  
  titlePanel("Two Categorical Variable Bar Plot"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        inputId ="var1",
        label = "Select a Categorical Variable",
        choices = variables_names, selected = "Sex"
      ),
      
      selectInput(
        inputId ="var2",
        label = "Select another Categorical Variable",
        choices = variables_names,
        selected = "Survived"
      )
      
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
    d = read_csv('titanic.csv')
    v1 = input$var1
    v2 = input$var2
    
    
    library(ggplot2)
    
    r = ggplot(d, aes(x = d[[v1]], color = as.factor(d[[v2]])))+
      geom_bar()+
      labs(x = v1, color = v2)
    
    return(r)
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)