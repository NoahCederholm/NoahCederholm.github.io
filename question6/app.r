library(tidyverse)
library(shiny)

d = read_csv('titanic.csv')

variables_names = names(d)

ui <- fluidPage(
  
  titlePanel("Line or Scatter Plot"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        inputId ="var1",
        label = "Select a Numeric Variable",
        choices = variables_names, selected = "Age"
      ),
      
      selectInput(
        inputId ="var2",
        label = "Select a Numerical Variable",
        choices = variables_names,
        selected = "Fare"
      ),
      
      selectInput(
        inputId ="var3",
        label = "Select a Categorical Variables",
        choices = variables_names,
        selected = "Sex"
      ),
      
      radioButtons(inputId = "plot_choice", 
                   label = h3("Select Plot:"),
                   choices = c("Scatter Plot" = "point",
                               "Line Plot" = "line"),
                   selected = 'point')
      
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
    v3 = input$var3
    
    
    library(ggplot2)
    
    if(input$plot_choice == 'point')
      
    {
      ggplot(d, aes(x = d[[v1]], y = d[[v2]], color = as.factor(d[[v3]])))+
        geom_point()+
        labs(x = v1, y = v2, color = v3)
    }
    
    else
    {
      ggplot(d, aes(x = d[[v1]], y = d[[v2]], color = as.factor(d[[v3]])))+
        geom_line()+
        labs(x = v1, y = v2, color = v3)
    }
    
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)