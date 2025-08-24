  library(shiny)
  library(leaflet)
  library(sf)
  
  ui <- fluidPage(
    titlePanel("Click on the Field Boundary to Get Coordinates"),
    leafletOutput("map", height = 600),
    verbatimTextOutput("click_info")
  )
  
  server <- function(input, output, session) {
    
    shapefile_path <- "C:/Users/acer/OneDrive - Texas State University/ChoLab/USDA Crop yield Stability Study/BARC_Yield_Analysis/Data_Raw/v1.1/2021_farmcrew_n76_v1.1/USDA_South Farm_SH-7_2021_CORN_1/SouthFarm_SH-7_2021_CORN_sph_proc_field.shp"
    shapefile_data <- st_read(shapefile_path)
    shapefile_data <- st_transform(shapefile_data, crs = 4326)  # Make sure it's in lat/lon for Leaflet
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = shapefile_data, color = "blue", weight = 2, fillOpacity = 0.4)
    })
    
    output$click_info <- renderPrint({
      click <- input$map_click
      if (!is.null(click)) {
        cat("Clicked Coordinates:\n")
        cat("(X, Y): (", click$lng, ",", click$lat,")\n")
        
      }
    })
  }
  
  shinyApp(ui, server)
  
