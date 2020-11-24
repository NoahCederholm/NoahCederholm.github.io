library(tidyverse)
library(shiny)

d = read_csv('titanic.csv')

variables_names = names(d)

ui <- fluidPage(
  
  titlePanel("Density Plot"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        inputId ="var1",
        label = "Select a Numeric Variables",
        choices = variables_names, selected = "Age"
      ),
      
      selectInput(
        inputId ="var2",
        label = "Select a Categorical Variables",
        choices = variables_names,
        selected = "Sex"
      ),
      
      radioButtons(inputId = "plot_choice", 
                   label = h3("Select Plot:"),
                   choices = c("Density Plot" = "density",
                               "Histogram Plot" = "histogram"),
                   selected = 'line'
    ), 
    
    sliderInput(inputId = "Age",
                "Select Age Range:",
                min = min(d$Age, na.rm=TRUE),
                max = max(d$Age, na.rm=TRUE),
                value= c(0, 100)
    ),
    
    
    checkboxGroupInput(inputId = "survive", label = "Select Survival Category",
                       choices = names(table(d$Survived)), inline = TRUE),
    
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
    
    if(input$plot_choice == 'density')
      
    {
      d <- d %>% filter(Survived %in% input$survive, Age>input$Age[1], Age<input$Age[2])
      ggplot(d, aes(x = d[[v1]], color = as.factor(d[[v2]])))+
        geom_density()+
        labs(x = v1, color = v2)
    }
    
    else
    {
      d <- d %>% filter(Survived %in% input$survive, Age>input$Age[1], Age<input$Age[2])
      ggplot(d, aes(x = d[[v1]], color = as.factor(d[[v2]])))+
        geom_histogram()+
        labs(x = v1, color = v2)
    }
    
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)