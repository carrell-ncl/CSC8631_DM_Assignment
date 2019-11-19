#library(shiny)
#library(tidyverse) 
#library(gridExtra)
#install.packages("shinythemes")
#library(shinythemes)
#library(rlang)

ui <- fluidPage()
server <- function(input, output) {} 



# Define UI
ui <- fluidPage(
  titlePanel("Nhanes Dashboard"),

  # Sidebar layout with a input and output definitions 
  sidebarLayout(
    
    # Inputs: Select variables to plot 
    sidebarPanel(
      
      # Select variable for y-axis 
      
      selectInput(inputId = "x", label = "Scatter - Variable in X axis:",
                  choices = c('Age', 'Systolic_BP1', 'Systolic_BP2',
                              'Diastolic_BP1', 'Diastolic_BP2', 'BMXWT', 'BMXHT', 'BMXBMI', 'BMXWAIST'),
                  selected = "Systolic_BP1"), # Select variable for x-axis
      selectInput(inputId = "y", label = "Scatter - Variable in Y Axis",
                  choices = c('Age', 'Income_to_Pov', 'Systolic_BP1', 'Systolic_BP2',
                              'Diastolic_BP1', 'Diastolic_BP2', 'BMXWT', 'BMXHT', 'BMXBMI', 'BMXWAIST'), 
                  selected = "Systolic_BP2"),
      
      sliderInput("range", 
                  label = "Scatter X interval:",
                  min = 0, max = 240, value = c(80, 240)),
      
      selectInput(inputId = "x2", label = "Box - Variable in X axis:",
                  choices = c('agegroup', "Alcohol_Year", "Smoked_100", "Race", "Education", "Marital_Status",
                              'Household_Size', 'Income_to_Pov'),
                  selected = "Household_Size"), # Select variable for x-axis
      selectInput(inputId = "y2", label = "Box - Variable in Y axis:",
                  choices = c('Systolic_BP1', 'Systolic_BP2', 'Diastolic_BP1', 'Diastolic_BP2', 
                              'BMXWT', 'BMXHT', 'BMXBMI', 'BMXWAIST', 'Income_to_Pov'), 
                  selected = "Income_to_Pov"),
      
      
      
      radioButtons("checkGroup", label = h3("Select data by:"), 
                         choices = list("Gender" = "Gender", "Age Group" = "agegroup",
                                        "Smoked 100" = "Smoked_100", "Alcohol Year" = "Alcohol_Year"),
                         selected = "Gender"),
    
    ),
    
    
    # Output 
    mainPanel(
      tabsetPanel(type="tab",
      tabPanel("Data", plotOutput(outputId = "scatterplot"),
      plotOutput(outputId = "boxplot")), 
      tabPanel("Summary of Nhanes dataset", verbatimTextOutput("summ")),
      tabPanel("Group table with proportions", radioButtons("Grp_prop", label = h3("Select data by group:"), 
                                                         choices = list("Age Group" = "agegroup",
                                                                        "Race" = "Race", "Education" = "Education", 
                                                                        "Marital Status" = "Marital_Status", "Household Size" = "Household_Size"),
                                                         selected = "agegroup"),
               radioButtons("analyse_prop", label = h3("Select data by:"), 
                            choices = list("Smoked 100" = "Smoked_100", "Alcohol Year" = "Alcohol_Year"),
                            selected = "Smoked_100"),dataTableOutput("prop"))),
      
  
      
      )
    ) 
  )

# Define server function
server <- function(input, output) {

  
  # Create the scatterplot object the plotOutput function is expecting 
  
  output$scatterplot <- renderPlot({
    nh3 = filter(nhanes2, !!sym(input$checkGroup) != "Don't know" & !!sym(input$checkGroup)!= "Refused")
    
    ggplot(data = nh3, aes_string(x = input$x, y = input$y, group = input$checkGroup,
                                      colour = input$checkGroup)) + geom_point(alpha = 0.5) +
                                      xlim(input$range[1], input$range[2])
      
      
    
    
  }) 
  output$boxplot <- renderPlot({
    nh3 = filter(nhanes2, !!sym(input$checkGroup) != "Don't know" & !!sym(input$checkGroup)!= "Refused" & !!sym(input$x2)!= "9" & !!sym(input$x2)!= "77" & !!sym(input$x2)!="Don't know" & !!sym(input$x2)!="Refused")
    nh3 = filter(nh3, !is.na(!!sym(input$checkGroup)))
    ggplot(data = nh3, aes_string(x = input$x2, y = input$y2,
                                      colour = input$checkGroup)) +geom_boxplot() 
  })
  
  # You can access the value of the widget with input$checkbox, e.g.
  output$value <- renderPrint({ input$checkGroup })
  
  output$range <- renderPrint({ input$slider })
  
  output$prop = renderDataTable({prop_func(nhanes2, input$Grp_prop, input$analyse_prop)})
  
  output$summ = renderPrint({summary(nhanes2)})
  
  
}
  
shinyApp(ui = ui, server = server)

