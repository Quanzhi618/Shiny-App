# Load necessary libraries 
library(shiny)
library(openair)
library(dplyr)
library(ggplot2)

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
        tabPanel("Frequency Table", 
                 tableOutput("freqTable"),
                 plotOutput("freqPlot")  # Add frequency plot
        ),
        tabPanel("Footprint Analysis", textOutput("footprintAnalysis"),
                 plotOutput("footprintPlot"))  # Add footprint plot
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
    
    # Clean the data by replacing NA values with 0 for wind_speed and wind_direction
    data_clean <- data %>%
      mutate(
        wind_speed = ifelse(is.na(wind_speed), 0, wind_speed),  # Replace NA in wind_speed with 0
        wind_direction = ifelse(is.na(wind_direction), 0, wind_direction)  # Replace NA in wind_direction with 0
      ) %>% 
      filter(wind_direction >= 0 & wind_direction <= 360) %>%  # Ensure wind direction is within range
      filter(wind_speed >= 0)  # Ensure wind speed is non-negative
    
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
  
  # Generate frequency plot (bar chart of wind direction frequencies)
  output$freqPlot <- renderPlot({
    data <- dataInput()  # Get the cleaned data
    freq_data <- data %>%
      group_by(wind_direction) %>%
      summarise(frequency = n())
    
    ggplot(freq_data, aes(x = wind_direction, y = frequency)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      theme_minimal() +
      labs(
        title = "Wind Direction Frequency", 
        x = "Wind Direction (°)", 
        y = "Frequency (Count)"  # Add units to the y-axis
      ) +
      theme(
        axis.title.y = element_text(angle = 0),  # Rotate y-axis title to horizontal
        plot.title = element_text(hjust = 0.5, vjust = -1)  # Move title to bottom of the plot
      )
  })
  
  # Footprint Analysis logic
  output$footprintAnalysis <- renderText({
    data <- dataInput()  # Get the cleaned data
    
    # Calculate additional statistics
    avg_wind_speed <- mean(data$wind_speed, na.rm = TRUE)
    sd_wind_speed <- sd(data$wind_speed, na.rm = TRUE)
    avg_wind_direction <- mean(data$wind_direction, na.rm = TRUE)
    mode_wind_direction <- as.numeric(names(sort(table(data$wind_direction), decreasing = TRUE))[1])
    
    # Return analysis text
    paste("Average Wind Speed: ", round(avg_wind_speed, 2), " units\n",
          "Standard Deviation of Wind Speed: ", round(sd_wind_speed, 2), " units\n",
          "Average Wind Direction: ", round(avg_wind_direction, 2), " degrees\n",
          "Mode Wind Direction: ", round(mode_wind_direction, 2), " degrees")
  })
  
  # Footprint Analysis plot (wind speed and wind direction distributions)
  output$footprintPlot <- renderPlot({
    data <- dataInput()  # Get the cleaned data
    
    ggplot(data, aes(x = wind_speed)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
      theme_minimal() +
      labs(
        title = "Wind Speed Distribution", 
        x = "Wind Speed (m/s)",  # Add units to the x-axis
        y = "Frequency (Count)"  # Add units to the y-axis
      ) +
      theme(
        axis.title.y = element_text(angle = 0),  # Rotate y-axis title to horizontal
        plot.title = element_text(hjust = 0.5, vjust = -1)  # Move title to bottom of the plot
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)