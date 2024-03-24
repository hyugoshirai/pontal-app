library(shiny)
library(leaflet)
library(raster)
library(leaflet.extras)
library(shinyWidgets)
library(sf)
library(raster)
library(rgdal)

# Define UI for application that draws a map
ui <- fluidPage(
  titlePanel("Draw Polygons and Add Labels"),
  mainPanel(
    leafletOutput("map"),
    uiOutput("label_input")  # Add this line to include the dynamic UI element
  )
)


server <- function(input, output, session) {
  # Initialize reactive values to store drawn polygons
  drawn_polygons <- reactiveValues(polygons = list())
  label_text <- reactiveVal(NULL)
  
  # Render leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = -52.320349, lat = -22.513868, zoom = 12) %>%
      addScaleBar() %>%
      addDrawToolbar(polylineOptions = FALSE,
                     polygonOptions = drawPolygonOptions(),
                     circleOptions = FALSE,
                     rectangleOptions = FALSE,
                     markerOptions = FALSE,
                     circleMarkerOptions = FALSE, 
                     editOptions = FALSE,
                     singleFeature = FALSE)
  })
  
  # Dynamically render text input for marker label
  observeEvent(input$map_draw_new_feature, {
    # Get the features drawn on the map
    polygon_coordinates <- input$map_draw_new_feature$geometry$coordinates[[1]]
    
    # Transform coordinates to an sp Polygon
    drawn_polygon <- Polygon(do.call(rbind, lapply(polygon_coordinates, function(x) c(x[[1]][1], x[[2]][1]))))
    drawn_polygons$polygons <- c(drawn_polygons$polygons, list(drawn_polygon))
    
    # Create UI for label input and add label button dynamically
    output$label_input <- renderUI({
      fluidRow(
        textInput("label_text_input", "Enter label text:", ""),
        actionButton("add_label_button", "Add Label")
      )
    })
  })
  
  observeEvent(input$add_label_button, {
    # Get the last drawn polygon
    last_polygon <- drawn_polygons$polygons[[length(drawn_polygons$polygons)]]
    
    # Calculate centroid of the last drawn polygon
    centroid <- colMeans(coordinates(last_polygon))
    print(centroid)
    
    # Get the content for the marker label from the text input
    label_text_input <- input$label_text_input
    
    # Add marker at the centroid of the last drawn polygon
    leafletProxy("map") %>%
      addMarkers(lng = centroid[1], lat = centroid[2],
                 label = label_text_input, # Use the text input as label content
                 labelOptions = labelOptions(noHide = TRUE, direction = "bottom",
                                             style = list(
                                               color = "red",
                                               "font-family" = "serif",
                                               "font-style" = "italic",
                                               "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)"
                                             )
                 )
      )
  })
}

shinyApp(ui = ui, server = server)
