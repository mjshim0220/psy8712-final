library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(rsconnect)

data_s <- readRDS("data_s.rds")

# Define UI for application
ui <- fluidPage(
  titlePanel("Engagement Histogram"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("place", "Work Mode:", 
                         choices = c("Remote", "Office")), #By default, no options are selected.
      checkboxGroupInput("Contract", "Contract:", 
                         choices = c("Full", "Part")),
      checkboxGroupInput("Side Work", "Side Work:", 
                         choices = c("Single Job Holder", "Multiple Job Holder")),
      radioButtons("plotType", "Plot Type:",
                   choices = c("Histogram", "Box Plot"),#By default, the first option is selected.
                   selected = "Histogram"),
      radioButtons("plotColor", "Plot Color:",
                   choices = c("Blue", "Red"),
                   selected = "Blue")
    ),
    
    mainPanel(
      plotOutput("engagementPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    data <- data_s
    
    # Filter based on work mode
    if ("Remote" %in% input$place) {
      data <- data %>% filter(emp_place == 1) #if the reader click the remote, then use the remote worker data only, the same logic below
    } else if ("Office" %in% input$place) {
      data <- data %>% filter(emp_place == 2)
    }
    
    # Filter based on contract type
    if ("Full" %in% input$Contract) {
      data <- data %>% filter(emp_fptime == 1)
    } else if ("Part" %in% input$Contract) {
      data <- data %>% filter(emp_fptime == 2)
    }
    
    # Filter based on job type
    if ("Single Job Holder" %in% input$`Side Work`) {
      data <- data %>% filter(job1 == 1)
    } else if ("Multiple Job Holder" %in% input$`Side Work`) {
      data <- data %>% filter(job1 %in% c(2, 3))
    }
    data
  })
  
  # Plotting based on filtered data
  output$engagementPlot <- renderPlot({
    if (input$plotType == "Histogram") {
      hist <- ggplot(filtered_data(), aes(x = engagement)) +
        geom_histogram(binwidth = 0.5, fill = ifelse(input$plotColor == "Red", "red", "skyblue"), color = "black") +
        labs(title = "Engagement Histogram",
             x = "Engagement",
             y = "Frequency")
    } else if (input$plotType == "Box Plot") {
      hist <- ggplot(filtered_data(), aes(y = engagement)) +
        geom_boxplot(fill = ifelse(input$plotColor == "Red", "red", "skyblue"), color = "black") +
        labs(title = "Engagement Box Plot")
    }
    
    hist
  })
}

# Run the application
shinyApp(ui = ui, server = server)

