#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sf)

Sunflower <- readRDS("Sunflower_shinylive.RDS") %>% 
                          dplyr::filter(STATE %in% c("ND","SD","MN"))

# Assuming your data is loaded and preprocessed as Sunflower

# UI
ui <- fluidPage(
  titlePanel("YieldScape USA"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select State:",
                  choices = unique(Sunflower$STATE)),
      
      sliderInput("year", "Select Year:",
                  min = min(Sunflower$YEAR), 
                  max = max(Sunflower$YEAR),
                  value = min(Sunflower$YEAR),
                  sep = "",
                  step = 1)
    ),
    
    mainPanel(
      plotOutput("choroplethMap")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive expression to filter data based on input
  filtered_data <- reactive({
    Sunflower %>%
      filter(STATE == input$state & YEAR == input$year)
  })
  
  # Render plot
  output$choroplethMap <- renderPlot({
    ggplot(data = filtered_data()) +
      geom_sf(aes(geometry = geometry, fill = YIELD)) + 
      scale_fill_viridis_c(direction = -1) +  
      theme_minimal() +
      labs(title = paste("Sunflower Yield in", input$year),
           fill = "Yield (lb/acre)")
  })
  
  # Update the year slider range dynamically based on the selected state
  observeEvent(input$state, {
    year_range <- range(Sunflower$YEAR[Sunflower$STATE == input$state])
    updateSliderInput(session, "year",
                      min = year_range[1],
                      max = year_range[2],
                      value = year_range[1])
  })
}

# Run the app
shinyApp(ui, server)
