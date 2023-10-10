

library(shiny)
library(leaflet)
library(sf)
library(tidyverse)


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(
    id = "controls",
    class = "panel panel-default",
    fixed = TRUE,
    draggable = TRUE,
    top = 60, left = "auto", right = 20, bottom = "auto",
    width = 330, height = "auto",
    fileInput("file", "Upload your RDS file", accept = c(".rds")),
    uiOutput("geometry_ui"),
    uiOutput("time_ui"),
    uiOutput("admin_ui"),
    uiOutput("admin2_ui"),
    uiOutput("value_ui"),
    uiOutput("slider_ui")
  )
)

server <- function(input, output, session) {
  
  data_reactive <- reactive({
    req(input$file)
    readRDS(input$file$datapath)
  })
  
  output$geometry_ui <- renderUI({
    req(data_reactive())
    selectInput("geometry", "Geometry:", choices = names(data_reactive()))
  })
  
  output$time_ui <- renderUI({
    req(data_reactive())
    selectInput("time", "Time:", choices = names(data_reactive()))
  })
  
  output$admin_ui <- renderUI({
    req(data_reactive())
    selectInput("admin", "Admin:", choices = names(data_reactive()))
  })
  
  output$admin2_ui <- renderUI({
    req(data_reactive())
    selectInput("admin2", "Admin 2:", choices = names(data_reactive()))
  })
  
  output$value_ui <- renderUI({
    req(data_reactive())
    selectInput("value", "Value:", choices = names(data_reactive()))
  })
  
  output$slider_ui <- renderUI({
    req(input$time)
    df <- data_reactive()
    time_values <- df[[input$time]]
    sliderInput("time_slider", "Time:",
                min = min(time_values, na.rm = TRUE),
                max = max(time_values, na.rm = TRUE),
                value = min(time_values, na.rm = TRUE),
                step = 1)
  })
  
  # Create a new reactive function for the filtered data
  filtered_data <- reactive({
    req(input$time_slider)
    df <- data_reactive()
    df[df[[input$time]] == input$time_slider,]
  })
  
  output$map <- renderLeaflet({
    req(data_reactive(), input$geometry, input$time, input$admin, input$admin2, input$value)
    df <- filtered_data()  # Get the filtered data using the new reactive function
    
    geometry_column <- input$geometry
    time_column <- input$time
    admin_column <- input$admin
    admin2_column <- input$admin2
    value_column <- input$value
    
    pal <- colorNumeric(palette = "YlGnBu", domain = df[[value_column]])
    
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(df[[value_column]]),
        fillOpacity = 0.7, 
        color = "#BDBDC3", 
        weight = 1,
        label = ~paste0(df[[admin_column]], " ", df[[admin2_column]], ": ", df[[value_column]]),  # Adjusted label
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = pal, values = df[[value_column]], 
                title = paste("Values of", value_column),
                position = "bottomright")
  })
  
}

shinyApp(ui, server)
