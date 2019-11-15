library(shiny)
library(tidyverse) 
library(gridExtra)
#install.packages("shinythemes")
library(shinythemes)

ui <- fluidPage()
server <- function(input, output) {} 
shinyApp(ui = ui, server = server)

colnames(nhanes2)

# Define UI
ui <- fluidPage(
  titlePanel("Nhanes Dashboard"),
  themeSelector(),
  
  # Sidebar layout with a input and output definitions 
  sidebarLayout(
    # Inputs: Select variables to plot 
    sidebarPanel(
      
      # Select variable for y-axis 
      selectInput(inputId = "x", label = "Scatter-X:",
                  choices = c('Age', 'Household_Size', 'Income_to_Pov', 'Systolic_BP1', 'Systolic_BP2',
                              'Diastolic_BP1', 'Diastolic_BP2', 'BMXWT', 'BMXHT', 'BMXBMI', 'BMXWAIST'),
                  selected = "audience_score"), # Select variable for x-axis
      selectInput(inputId = "y", label = "Scatter-Y",
                  choices = c('Age', 'Household_Size', 'Income_to_Pov', 'Systolic_BP1', 'Systolic_BP2',
                              'Diastolic_BP1', 'Diastolic_BP2', 'BMXWT', 'BMXHT', 'BMXBMI', 'BMXWAIST'), selected = "critics_score"),
      selectInput(inputId = "x2", label = "Box-X:",
                  choices = c('agegroup', "Alcohol_Year", "Smoked_100", "Race", "Education", "Marital_Status",
                              'Household_Size', 'Income_to_Pov'),
                  selected = "audience_score"), # Select variable for x-axis
      selectInput(inputId = "y2", label = "Box-Y:",
                  choices = c('Systolic_BP1', 'Systolic_BP2', 'Diastolic_BP1', 'Diastolic_BP2', 
                              'BMXWT', 'BMXHT', 'BMXBMI', 'BMXWAIST'), selected = "critics_score")
      
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
    ggplot(data = nhanes2, aes_string(x = input$x2, y = input$y2,
                                      colour = 'Gender')) +geom_boxplot()
  })
  
}

