#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


### running the packages ##

library(shiny)
library(leaflet)
library(tidyverse)
library(sf)

Sunflower <- readRDS("Sunflower.RDS") %>% 
                         dplyr::filter(STATE %in% c("ND","SD","MN"))


# Assuming Sunflower is your dataset

# UI code


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput("state", "State:", 
                            choices = unique(Sunflower$STATE),
                            selected = unique(Sunflower$STATE)[1]),
                sliderInput("year", "Year:",
                            min = min(Sunflower$YEAR), 
                            max = max(Sunflower$YEAR),
                            value = min(Sunflower$YEAR),
                            sep = "",
                            step = 1)
  )
)

# Server code
server <- function(input, output, session) {  # Added session argument
  
  df <- reactive({
    Sunflower %>% 
      filter(YEAR == input$year & STATE == input$state) %>% 
      st_transform(crs=4326)
  })
  
  observeEvent(input$state, {
    year_range <- range(Sunflower$YEAR[Sunflower$STATE == input$state])
    updateSliderInput(session, "year",
                      min = year_range[1],
                      max = year_range[2],
                      value = year_range[1])
  })
  
  output$map <- renderLeaflet({
    req(nrow(df()) > 0)  # Ensure there is data to plot
    pal <- colorNumeric(palette = "YlGnBu", 
                        domain = df()$YIELD)
    leaflet(df()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(YIELD),
        fillOpacity = 0.7, 
        color = "#BDBDC3", 
        weight = 1,
        label = ~paste0("County: ", COUNTYNAME, 
                        " Yield: ", YIELD),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = pal, values = ~YIELD, 
                title = "Yield per County",
                position = "bottomright")
  })
}

# Run the Shiny app
shinyApp(ui, server)
