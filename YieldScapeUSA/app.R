
# Load necessary libraries
library(shiny)
library(leaflet)
library(sf)
library(tidyverse)

# UI
ui <- fluidPage(
  titlePanel("Interactive Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload your RDS file", accept = c(".rds")),
      uiOutput("geometry_ui"),
      uiOutput("time_ui"),
      uiOutput("admin_ui"),
      uiOutput("admin2_ui"),
      uiOutput("value_ui"),
      uiOutput("slider_ui"),
      selectInput("colorPalette", "Color Palette:",
                  choices = rownames(RColorBrewer::brewer.pal.info),
                  selected = "Set3")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map", width = "100%", height = "600px")),
        tabPanel("Admin Level Analysis",
                 plotOutput("line_plot", width = "100%", height = "600px")),
        tabPanel("Admin 2 Level Analysis",
                 plotOutput("bar_plot",width = "100%", height = "600px")),
        tabPanel("About",
                 HTML("
                 <h2>Welcome to YieldScapeUSA!</h2>
                 <p>YieldScapeUSA is an interactive platform crafted to visualize agricultural yield data across the United States. Initially designed to display oilseed sunflower yield data across counties in North Dakota, South Dakota, and Minnesota, the app has evolved to allow users to upload and visualize their own yield data across various crops and regions.</p>
                 <h3>How to Use:</h3>
                 <ol>
                   <li><strong>Data Upload:</strong> Upload your own RDS data file to explore yield statistics for different crops and locations.</li>
                   <li><strong>Dynamic Selections:</strong> Select the appropriate columns for geometry, time, administrative divisions, and yield values to tailor the analysis to your data.</li>
                   <li><strong>Interactive Map:</strong> Engage with the dynamic map to explore yield data across different geographical regions. Hover over a region to view detailed yield information.</li>
                   <li><strong>Time Analysis:</strong> Use the time slider to view yield data for specific years, which dynamically adjusts based on your data.</li>
                   <li><strong>Admin Level Analysis:</strong> Visualize average yield trends over time with line plots, faceted by the first level of administrative division.</li>
                   <li><strong>Admin 2 Level Analysis:</strong> Explore the top 5 counties in terms of total yield with bar plots, faceted by the first level of administrative division.</li>
                   <li><strong>Color Palette Selection:</strong> Customize the color palette for the line and bar plots to better understand yield variations.</li>
                 </ol>
                 <h3>Input File Expectations:</h3>
                 <ul>
                   <li><strong>File Format:</strong> Your data file should be in RDS format.</li>
                   <li><strong>Spatial Data:</strong> Ensure your data contains spatial geometry information for accurate mapping.</li>
                   <li><strong>Required Columns:</strong> 
                       <ul>
                         <li>A Geometry column for spatial geometry.</li>
                         <li>A Time column for the time dimension (e.g., year).</li>
                         <li>An Admin column for the first level of administrative division (e.g., State).</li>
                         <li>An Admin 2 column for the second level of administrative division (e.g., County).</li>
                         <li>A Value column for the yield values or any other values you wish to visualize.</li>
                       </ul>
                   </li>
                 </ul>
                 <h3>Example Data Structure:</h3>
                 <table border='1'>
                   <thead>
                     <tr>
                       <th>Geometry</th><th>Time</th><th>Admin</th><th>Admin 2</th><th>Value</th>
                     </tr>
                   </thead>
                   <tbody>
                     <tr>
                       <td>...</td><td>1976</td><td>ND</td><td>Stark</td><td>1234</td>
                     </tr>
                     <tr>
                       <td>...</td><td>1976</td><td>ND</td><td>Dunn</td><td>5678</td>
                     </tr>
                     <tr>
                       <td>...</td><td>1977</td><td>SD</td><td>Jones</td><td>9101</td>
                     </tr>
                     <tr>
                       <td>...</td><td>1977</td><td>SD</td><td>Lyman</td><td>1121</td>
                     </tr>
                   </tbody>
                 </table>
                 "))
      )
    )
  )
)


server <- function(input, output, session) {
  
  library(sf)  # Ensure the sf package is loaded
  
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
  # Create a new reactive function for the filtered data
  filtered_data <- reactive({
    req(input$time_slider)
    df <- data_reactive()
    df[df[[input$time]] == input$time_slider,]
  })
  
  output$map <- renderLeaflet({
    req(input$geometry, input$time, input$admin, input$admin2, input$value, input$time_slider)
    df <- filtered_data()  # Get the filtered data using the reactive function
    
    # Ensure the data is in sf format
    df_sf <- st_as_sf(df, wkt = df[[input$geometry]])
    
    # Set the CRS to WGS 84
    df_sf <- st_set_crs(df_sf, 4326)
    
    geometry_column <- input$geometry
    time_column <- input$time
    admin_column <- input$admin
    admin2_column <- input$admin2
    value_column <- input$value
    
    pal <- colorNumeric(palette = "YlGnBu", domain = df[[value_column]])
    
    leaflet(df_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(df[[value_column]]),
        fillOpacity = 0.7, 
        color = "#BDBDC3", 
        weight = 1,
        label = ~paste0(df[[admin_column]], " ", df[[admin2_column]], ": ", df[[value_column]]),
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
  
  
  output$line_plot <- renderPlot({
    req(data_reactive(), input$value, input$time, input$admin)
    df <- data_reactive()
    
    avg_df <- df %>%
      group_by(across(c(input$admin, input$time))) %>%
      summarise(avg_value = mean(.data[[input$value]], na.rm = TRUE), .groups = 'drop')
    
    ggplot(avg_df, aes_string(x = input$time, y = "avg_value", colour = input$admin)) +
      geom_line(size = 1) +
      geom_point() +
      facet_wrap(as.formula(paste("~", input$admin))) +
      labs(x = "Years", y = paste("Average", input$value)) +
      theme(text = element_text(size = 10)) +
      theme_bw() +
      scale_color_brewer(palette = input$colorPalette)
    
  }) 
  
  output$bar_plot <- renderPlot({
    req(data_reactive(), input$value, input$admin, input$admin2)
    df <- data_reactive()  # Get the filtered data using the reactive function
    
    # First, calculate the total value for each Admin2 unit across all timeframes
    summed_values <- df %>%
      group_by(across(c(input$admin, input$admin2))) %>%
      summarise(total_value = mean(.data[[input$value]], na.rm = TRUE), .groups = 'drop')
    
    # Filter to keep only the top 5 Admin2 units within each Admin based on total value
    top_5_values <- summed_values %>%
      group_by(.data[[input$admin]]) %>%
      arrange(desc(total_value)) %>%
      slice_head(n = 5) %>%
      ungroup()
    
    # Reorder Admin2 within each Admin based on the total value and plot
    top_5_values %>%
      mutate(reorder_col = tidytext::reorder_within(.data[[input$admin2]], total_value, .data[[input$admin]])) %>%
      ggplot(aes(x = reorder_col, y = total_value, fill = .data[[input$admin]])) +
      geom_bar(stat = "identity") +
      coord_flip() +
      facet_wrap(as.formula(paste("~", input$admin)), scales = "free_y") +
      tidytext::scale_x_reordered() +
      scale_fill_brewer(palette = input$colorPalette) +
      labs(x = "Admin 2 Units", y = "Total Value") +
      theme(text = element_text(size = 10)) +
      theme_bw()
    
  })
}

shinyApp(ui, server)



  
