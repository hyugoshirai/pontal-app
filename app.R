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
        # Add text to the sidebar
        HTML("<h3> Draw Polygons and Add Labels </h3>"),
        p("This tool is designed to draw polygon into the map and adding labels to them."),
        tags$hr(),  # Add a horizontal line
        p("With the 'Draw a polygon' tool, add polygons to the map."),
        tags$hr(),  # Add a horizontal line
        p("Finish the polygon and enter the label in the text box."),
        tags$hr(),  # Add a horizontal line
        p("After finishing the text, click 'Ádd Label' button to add it to the centroid of the polygon"),
        tags$hr(),  # Add a horizontal line
        p("Finally, click the 'Upload' button"),
        selectInput("basemap", "Select Basemap:", 
                    choices = c("OpenStreetMap", "Esri.WorldImagery"),
                    selected = "OpenStreetMap")
      ),
    mainPanel(
      leafletOutput("map"),
      uiOutput("label_input")  # Add this line to include the dynamic UI element
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Render instructions text
  output$instructions <- renderText({
    "Insert your instructions here..."
  })
  
  # Initialize reactive values to store drawn polygons
  drawn_polygons <- reactiveValues(polygons = list())
  label_text <- reactiveVal(NULL)
  
  # Render leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = input$basemap) %>%
      setView(lng = -52.320349, lat = -22.513868, zoom = 9) %>%
      addScaleBar() %>%
     addDrawToolbar(
      polylineOptions = FALSE,
      polygonOptions = drawPolygonOptions(
        showArea = FALSE, 
        metric = TRUE,
        shapeOptions = drawShapeOptions(clickable = TRUE), 
        repeatMode = FALSE
      ),
      circleOptions = FALSE,
      rectangleOptions = FALSE,
      markerOptions = FALSE,
      circleMarkerOptions = FALSE, 
      singleFeature = FALSE,
      editOptions = editToolbarOptions()
    )
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
        actionButton("add_label_button", "Add Label"),
        actionButton("UpldButton", "Upload")
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

    # Define a reactive value to keep track of button clicks
    clickCount <- reactiveVal(0)

    # Observe the button click event
    observeEvent(input$UpldButton, {
      # Increment the click count
      clickCount(clickCount() + 1)
      print(clickCount)
      
      # Call your custom function or code here
      customFunction(clickCount())
    })
    
    # Define the custom function to be executed on button click
    customFunction <- function(clickCount) {
      # Get the features drawn on the map
      polygon_coordinates <- input$map_draw_new_feature$geometry$coordinates[[1]]
      # Perform your desired actions here
      print("funciona")
      temp_shp <- tempdir()
      lng <- map_dbl(polygon_coordinates, `[[`, 1)
      lat <- map_dbl(polygon_coordinates, `[[`, 2)
      print(lng)
      print(lat)
      
      
      shapefileName <- input$label_text_input  # Extract shapefile name from input
      shp <- st_as_sf(tibble(lon = lng, lat = lat),
                      coords = c("lon", "lat"),
                      crs = 4326) %>%
        summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")
      
      # Write the shapefile components into the specified folder with an explicit driver
      st_write(shp, file.path(temp_shp, paste0(shapefileName, ".shp")), driver = "ESRI Shapefile", append=TRUE)
      st_write(shp, file.path(temp_shp, paste0(shapefileName, ".shx")), driver = "ESRI Shapefile", append=TRUE)
      st_write(shp, file.path(temp_shp, paste0(shapefileName, ".dbf")), driver = "ESRI Shapefile", append=TRUE)
      
      # Manually create and save the .prj file
      prj_content <- 'GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]'
      writeLines(prj_content, file.path(temp_shp, paste0(shapefileName, ".prj")))
      
      # Authenticate with Google Drive using OAuth
      drive_auth()
      td <- drive_get("https://drive.google.com/drive/folders/15d_6NxHaXK2qzBzyI9SJcOQYJFMQA_Fn")
      
      # Upload the zipped file to Google Drive
      drive_put(media = file.path(temp_shp, paste0(shapefileName, ".shp")),
                name = paste0(shapefileName, ".shp"),
                path = as_id(td))
      drive_put(media = file.path(temp_shp, paste0(shapefileName, ".shx")),
                name = paste0(shapefileName, ".shx"),
                path = as_id(td))
      drive_put(media = file.path(temp_shp, paste0(shapefileName, ".dbf")),
                name = paste0(shapefileName, ".dbf"),
                path = as_id(td))
      drive_put(media = file.path(temp_shp, paste0(shapefileName, ".prj")),
                name = paste0(shapefileName, ".prj"),
                path = as_id(td))

      # Render a text output to indicate completion
      renderText(paste("Done! Shapefile named:", shapefileName))
    }
    })
 
}

shinyApp(ui = ui, server = server)

