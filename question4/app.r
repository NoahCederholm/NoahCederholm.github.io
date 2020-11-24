library(tidyverse)
library(shiny)

d <- read_csv('titanic.csv')

variables_names = names(d)

ui <- fluidPage(
  
  titlePanel("Numerical Bar Plot"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        inputId ="var1",
        label = "Select a Numerical Variable",
        choices = variables_names, selected = "Age"
      ),
      
      selectInput(
        inputId ="var2",
        label = "Select another Numerical Variable",
        choices = variables_names,
        selected = "Fare"
      ),
      selectInput(
        inputId ="var3",
        label = "Select a Categorical Variable",
        choices = variables_names, selected = "Sex"
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
    v3 = input$var3
    
    
    library(ggplot2)
    
    r = ggplot(d, aes(x = d[[v1]], y = d[[v2]],color = as.factor(d[[v3]])))+
      geom_point()+
      labs(x = v1, y = v2, color = v3)
    
    return(r)
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)