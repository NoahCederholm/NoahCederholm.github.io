library(tidyverse)
library(shiny)

d = read_csv('titanic.csv')

variables_names = names(d)

ui <- fluidPage(
  
  titlePanel("Age Variation"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        inputId ="var1",
        label = "Select a Categorical Variable",
        choices = variables_names, selected = "Survived"
      ),
      
      selectInput(
        inputId ="var2",
        label = "Select a Categorical Variable",
        choices = variables_names,
        selected = "Sex"
      ), 
      
      sliderInput(inputId = "Age",
                  "Select Age Range:",
                  min = min(d$Fare, na.rm=TRUE),
                  max = max(d$Fare, na.rm=TRUE),
                  value= c(0, 100))
      
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
    
    
    library(ggplot2)
    
    d <- d %>% filter(Fare>input$Age[1], Fare<input$Age[2])
    
    ggplot(d, aes(x = d[[v1]], color = as.factor(d[[v2]])))+
      geom_bar()+
      labs(x = v1, color = v2)
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)