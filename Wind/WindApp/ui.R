# Load necessary libraries
library(shiny)
library(openair)
library(dplyr)
library(ggplot2)
library(stats)  

# Define UI
ui <- fluidPage(
  titlePanel("Wind Speed and Direction Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),  # File input
      sliderInput("angle", "Adjust Wind Rose Angle", min = 10, max = 60, value = 30, step = 5),  # Angle slider
      selectInput("plotType", "Select Plot Type", choices = c("Bar Chart", "Line Chart")),  # Wind speed plot type
      downloadButton("downloadData", "Download Processed Data"),  # Data download button
      
      # Statistical tests and analysis
      checkboxInput("linearRegression", "Perform Linear Regression", FALSE)  # Linear regression checkbox
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Wind Rose", plotOutput("windRosePlot")),
        tabPanel("Frequency Table", 
                 tableOutput("freqTable"),
                 plotOutput("freqPlot")
        ),
        tabPanel("Footprint Analysis", textOutput("footprintAnalysis"),
                 plotOutput("footprintPlot"),
                 plotOutput("regressionPlot")),  # Added regression plot here
        tabPanel("Statistical Results", verbatimTextOutput("statisticalResults"))  # Display statistical test results
      )
    )
  )
)