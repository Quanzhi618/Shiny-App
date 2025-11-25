# Load necessary libraries
library(shiny)
library(openair)


# Define UI
ui <- fluidPage(
  titlePanel("Wind Speed and Direction Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv")
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
    read.csv(input$file1$datapath)  # Read the uploaded CSV file
  })
  
  # Generate wind rose plot
  output$windRosePlot <- renderPlot({
    data <- dataInput()  # Get the uploaded data
    req("wind_speed" %in% colnames(data), "wind_direction" %in% colnames(data))
    windRose(data, ws = "wind_speed", wd = "wind_direction", angle = 30)
  })
  
  # Placeholder for logic later
}

# Run the Shiny app
shinyApp(ui = ui, server = server)