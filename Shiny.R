library(shiny)
library(tidyverse) 
library(gridExtra)
load("data/movies.Rdata")
install.packages("shinythemes")

ui <- fluidPage()
server <- function(input, output) {} 
shinyApp(ui = ui, server = server)

movies

# Define UI
ui <- fluidPage(
  titlePanel("Check correlation"),
  
  # Sidebar layout with a input and output definitions 
  sidebarLayout(
  # Inputs: Select variables to plot 
    sidebarPanel(
    
  # Select variable for y-axis 
      selectInput(inputId = "y", label = "Y-axis:",
  choices = c('Age', 'Household_Size', 'Income_to_Pov', 'Systolic_BP1', 'Systolic_BP2',
              'Diastolic_BP1', 'Diastolic_BP2', 'BMXWT', 'BMXHT', 'BMXBMI', 'BMXWAIST'),
  selected = "audience_score"), # Select variable for x-axis
  selectInput(inputId = "x", label = "X-axis:",
            choices = c('Age', 'Household_Size', 'Income_to_Pov', 'Systolic_BP1', 'Systolic_BP2',
              'Diastolic_BP1', 'Diastolic_BP2', 'BMXWT', 'BMXHT', 'BMXBMI', 'BMXWAIST'), selected = "critics_score"),
  theme = shinytheme("united"), 
  
),

# Output: Show scatterplot 
mainPanel(
plotOutput(outputId = "scatterplot"),
plotOutput(outputId = "boxplot"))
) )

# Define server function
server <- function(input, output) {
  
  # Create the scatterplot object the plotOutput function is expecting 
  output$scatterplot <- renderPlot({
  ggplot(data = nhanes2, aes_string(x = input$x, y = input$y, group = "Gender",
                                    colour = "Gender")) +geom_point()
    
    
  }) 
  output$boxplot <- renderPlot({
    ggplot(data = nhanes2, aes_string(x = input$x, y = input$y, group = "Gender",
                                      colour = "Gender")) +geom_boxplot()
  })
  
  }


library(shinythemes)

fluidPage(theme = shinytheme("cerulean"),
)



