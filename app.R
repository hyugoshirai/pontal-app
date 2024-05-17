# List of required packages
packages <- c(
  "googledrive",
  "leaflet",
  "leaflet.extras",
  "raster",
  "sf",
  "shiny",
  "shinyWidgets",
  "shinyjs",
  "terra",
  "tidyverse"
)

# Install packages if they are not already installed
install_packages <- packages[!packages %in% installed.packages()]
if (length(install_packages) > 0) {
  install.packages(install_packages)
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))

# Define UI for application that draws a map
ui <- fluidPage(
  titlePanel("Mapping the Pontal"),
  sidebarLayout(
    sidebarPanel(
      HTML("<h3> Draw Polygons and Add Labels </h3>"),
      p("This tool is designed to draw polygons on the map and add labels to them."),
      tags$hr(),
      p("With the 'Draw a polygon' tool, add polygons to the map."),
      tags$hr(),
      p("Finish the polygon and enter the label in the text box."),
      tags$hr(),
      p("After finishing the text, click 'Add Label' button to add it to the centroid of the polygon."),
      tags$hr(),
      p("Then, click the 'Upload' button."),
      tags$hr(),
      p("Finally, follow the instructions on the Console menu."),
      selectInput("basemap", "Select Basemap:", 
                  choices = c("OpenStreetMap", "Esri.WorldImagery"),
                  selected = "OpenStreetMap")
    ),
    mainPanel(
      leafletOutput("map"),
      uiOutput("label_input")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Initialize reactive values to store drawn and edited polygons
  drawn_polygons <- reactiveValues(polygons = list())
  edited_polygon <- reactiveValues(polygons = list())
  
  # Render leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = input$basemap) %>%
      setView(lng = -52.320349, lat = -22.513868, zoom = 9) %>%
      addScaleBar() %>%
      addDrawToolbar(
        polylineOptions = FALSE,
        polygonOptions = drawPolygonOptions(showArea = TRUE, 
                                            metric = TRUE,
                                            shapeOptions = drawShapeOptions(clickable = TRUE), 
                                            repeatMode = FALSE),
        circleOptions = FALSE,
        rectangleOptions = drawRectangleOptions(showArea = TRUE, 
                                                metric = TRUE,
                                                shapeOptions = drawShapeOptions(clickable = TRUE), 
                                                repeatMode = FALSE),
        markerOptions = FALSE,
        circleMarkerOptions = FALSE, 
        singleFeature = FALSE,
        editOptions = editToolbarOptions()
      )
  })
  
  # Dynamically render text input for marker label and add label button
  output$label_input <- renderUI({
    fluidRow(
      textInput("label_text_input", "Enter label text:", ""),
      actionButton("add_label_button", "Add Label"),
      actionButton("UpldButton", "Upload")
    )
  })
  
  # Function to add polygons to the map
  addPolygonsToMap <- function(map, polygons) {
    for (polygon in polygons) {
      coords <- st_coordinates(polygon)[, 1:2]
      leafletProxy(map) %>%
        addPolygons(lng = coords[, 1], lat = coords[, 2], color = "blue", weight = 2, fill = TRUE)
    }
  }
  
  # Observe basemap change and re-render polygons
  observeEvent(input$basemap, {
    leafletProxy("map") %>%
      clearTiles() %>%
      addProviderTiles(provider = input$basemap)
    addPolygonsToMap("map", drawn_polygons$polygons)
  })
  
  # Handle polygon drawing
  observeEvent(input$map_draw_new_feature, {
    polygon_coordinates <- input$map_draw_new_feature$geometry$coordinates[[1]]
    new_polygon <- st_polygon(list(do.call(rbind, lapply(polygon_coordinates, function(x) c(x[[1]], x[[2]])))))
    drawn_polygons$polygons <- append(drawn_polygons$polygons, list(new_polygon))
    edited_polygon$polygons <- list()  # Reset edited polygon
    addPolygonsToMap("map", list(new_polygon))  # Add the new polygon to the map
  })
  
  # Handle polygon editing
  observeEvent(input$map_draw_edited_features, {
    edited_polygon_coords <- input$map_draw_edited_features$features[[1]]$geometry$coordinates[[1]]
    edited_polygon_sf <- st_polygon(list(do.call(rbind, lapply(edited_polygon_coords, function(x) c(x[[1]], x[[2]])))))
    edited_polygon$polygons <- list(edited_polygon_sf)
    addPolygonsToMap("map", list(edited_polygon_sf))  # Add the edited polygon to the map
  })
  
  # Handle adding label to the last drawn or edited polygon
  observeEvent(input$add_label_button, {
    label_text_input <- input$label_text_input
    if (length(edited_polygon$polygons) > 0) {
      last_polygon <- edited_polygon$polygons[[1]]
    } else if (length(drawn_polygons$polygons) > 0) {
      last_polygon <- drawn_polygons$polygons[[length(drawn_polygons$polygons)]]
    } else {
      return()  # Return if there are no drawn or edited polygons
    }
    
    centroid <- st_centroid(st_sfc(last_polygon))
    centroid_coords <- st_coordinates(centroid)
    
    leafletProxy("map") %>%
      addMarkers(lng = centroid_coords[1], lat = centroid_coords[2],
                 label = label_text_input,
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
  
  # Handle uploading the last drawn or edited polygon
  observeEvent(input$UpldButton, {
    if (length(edited_polygon$polygons) > 0) {
      shp <- edited_polygon$polygons[[1]]
    } else if (length(drawn_polygons$polygons) > 0) {
      shp <- drawn_polygons$polygons[[length(drawn_polygons$polygons)]]
    } else {
      return()  # Return if there are no drawn or edited polygons
    }
    
    # Convert to sf object
    shp_sf <- st_sfc(shp, crs = 4326) %>% st_sf()
    
    shapefileName <- input$label_text_input
    temp_shp <- tempdir()
    
    # Write shapefile
    tryCatch({
      st_write(shp_sf, file.path(temp_shp, paste0(shapefileName, ".shp")), delete_dsn = TRUE)
      prj_content <- 'GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]'
      writeLines(prj_content, file.path(temp_shp, paste0(shapefileName, ".prj")))
      
      drive_auth()
      td <- drive_get("https://drive.google.com/drive/folders/15d_6NxHaXK2qzBzyI9SJcOQYJFMQA_Fn")
      
      files_to_upload <- c(".shp", ".shx", ".dbf", ".prj")
      lapply(files_to_upload, function(ext) {
        drive_put(media = file.path(temp_shp, paste0(shapefileName, ext)),
                  name = paste0(shapefileName, ext),
                  path = as_id(td))
      })
      
      showNotification("Upload complete! Shapefile named: ", shapefileName, type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
