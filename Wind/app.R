# Load necessary libraries
library(shiny)
library(openair)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Wind Speed and Direction Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv")  # File input
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Wind Rose", plotOutput("windRosePlot")),
        tabPanel("Frequency Table", tableOutput("freqTable")),
        tabPanel("Footprint Analysis", textOutput("footprintAnalysis"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to read the uploaded data
  dataInput <- reactive({
    req(input$file1)  # Ensure the file is uploaded before proceeding
    
    # Attempt to read the file with different options
    data <- tryCatch({
      # Try reading with default read.csv (comma-separated)
      read.csv(input$file1$datapath)
    }, error = function(e) {
      # If an error occurs, attempt to read with read.csv2 (semicolon-separated)
      read.csv2(input$file1$datapath)
    })
    
    # Check if data is a valid data frame
    if (!is.data.frame(data)) {
      stop("Uploaded file is not a valid data frame.")
    }
    
    # Clean the data by removing NA values and ensuring valid wind direction
    data_clean <- data %>%
      filter(!is.na(wind_speed), !is.na(wind_direction)) %>%  # Remove NA values
      filter(wind_direction >= 0 & wind_direction <= 360)  # Ensure wind direction is within range
    
    return(data_clean)  # Return cleaned data
  })
  
  # Generate wind rose plot
  output$windRosePlot <- renderPlot({
    data <- dataInput()  # Get the cleaned data
    req("wind_speed" %in% colnames(data), "wind_direction" %in% colnames(data))
    windRose(data, ws = "wind_speed", wd = "wind_direction", angle = 30)
  })
  
  # Generate frequency and relative frequency table
  output$freqTable <- renderTable({
    data <- dataInput()  # Get the cleaned data
    freq_data <- data %>%
      group_by(wind_direction) %>%   # Group by wind direction
      summarise(frequency = n()) %>%  # Count occurrences in each wind direction
      mutate(relative_frequency = frequency / sum(frequency) * 100)  # Calculate relative frequency
    
    # Return the resulting table
    freq_data
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)