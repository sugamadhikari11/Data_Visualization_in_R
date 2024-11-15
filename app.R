#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

library(reshape2)  # Load the reshape2 library


library(shinythemes) 
library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)

df <- read.csv("wdbc.data", col.names = c(
  "ID", "Diagnosis", "Radius_Mean", "Texture_Mean", "Perimeter_Mean", 
  "Area_Mean", "Smoothness_Mean", "Compactness_Mean", "Concavity_Mean", 
  "Concave_Points_Mean", "Symmetry_Mean", "Fractal_Dimension_Mean", 
  "Radius_SE", "Texture_SE", "Perimeter_SE", "Area_SE", "Smoothness_SE", 
  "Compactness_SE", "Concavity_SE", "Concave_Points_SE", "Symmetry_SE", 
  "Fractal_Dimension_SE", "Radius_Worst", "Texture_Worst", "Perimeter_Worst", 
  "Area_Worst", "Smoothness_Worst", "Compactness_Worst", "Concavity_Worst", 
  "Concave_Points_Worst", "Symmetry_Worst", "Fractal_Dimension_Worst"
))

df$Diagnosis <- factor(df$Diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Breast Cancer Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("featureX", "Select X-axis Feature:", choices = colnames(df)[3:32]),
      selectInput("featureY", "Select Y-axis Feature:", choices = colnames(df)[3:32]),
      selectInput("plotType", "Select Plot Type:", choices = c("Boxplot", "Density", "Scatterplot", "3D Scatterplot")),
      checkboxInput("normalize", "Normalize Features", value = FALSE),
      downloadButton("downloadData", "Download Filtered Data"),
      checkboxInput("showCorr", "Show Correlation Matrix", value = FALSE),
      actionButton("btnPlot", "Plot Visualization")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Visualizations", plotlyOutput("dynamicPlot")),
        tabPanel("Correlation Matrix", plotlyOutput("corrPlot")),
        tabPanel("Filtered Data", tableOutput("filteredTable"))
      )
    )
  )
)

server <- function(input, output) {
  reactive_data <- reactive({
    data <- df
    if (input$normalize) {
      data_norm <- as.data.frame(lapply(data[, 3:32], function(x) (x - min(x)) / (max(x) - min(x))))
      data_norm$Diagnosis <- data$Diagnosis
      return(data_norm)
    } else {
      return(data)
    }
  })
  
  output$dynamicPlot <- renderPlotly({
    input$btnPlot
    isolate({
      data <- reactive_data()
      p <- ggplot(data, aes_string(x = input$featureX, y = input$featureY, color = "Diagnosis"))
      
      if (input$plotType == "Boxplot") {
        p <- ggplot(data, aes_string(x = "Diagnosis", y = input$featureX, fill = "Diagnosis")) +
          geom_boxplot() +
          theme_minimal()
      } else if (input$plotType == "Density") {
        p <- ggplot(data, aes_string(x = input$featureX, fill = "Diagnosis")) +
          geom_density(alpha = 0.5) +
          theme_minimal()
      } else if (input$plotType == "3D Scatterplot") {
        p <- plot_ly(data, x = ~get(input$featureX), y = ~get(input$featureY), z = ~get("Area_Mean"),
                     color = ~Diagnosis, colors = c("skyblue", "tomato"), type = "scatter3d", mode = "markers")
      } else if (input$plotType == "Scatterplot") {
        p <- p + geom_point(alpha = 0.7) + theme_minimal()
      }
      
      ggplotly(p)
    })
  })
  
  output$corrPlot <- renderPlotly({
    req(input$showCorr)
    data <- reactive_data()
    corr_matrix <- cor(data[, 3:32])
    p <- ggplot(melt(corr_matrix), aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
      coord_fixed() +
      labs(title = "Correlation Matrix")
    ggplotly(p)
  })
  
  output$filteredTable <- renderTable({
    reactive_data()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_data(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
