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
      HTML("<h3> Draw Shapes and Add Labels </h3>"),
      p("This tool is designed to draw shapes (polygons, circles, and lines) on the map and add labels to them."),
      tags$hr(),
      p("With the drawing tools, add shapes to the map."),
      tags$hr(),
      p("Finish drawing and enter the label in the text box."),
      tags$hr(),
      p("After finishing the text, click 'Add Label' button to add it to the centroid of the shape."),
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
  
  # Initialize reactive values to store drawn and edited shapes and markers
  drawn_shapes <- reactiveValues(shapes = list())
  edited_shape <- reactiveValues(shapes = list())
  drawn_markers <- reactiveValues(markers = list())
  
  # Render leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = input$basemap) %>%
      setView(lng = -52.320349, lat = -22.513868, zoom = 9) %>%
      addScaleBar() %>%
      addDrawToolbar(
        polylineOptions = drawPolylineOptions(),
        polygonOptions = drawPolygonOptions(showArea = TRUE, 
                                            metric = TRUE,
                                            shapeOptions = drawShapeOptions(clickable = TRUE), 
                                            repeatMode = FALSE),
        circleOptions = drawCircleOptions(),
        rectangleOptions = drawRectangleOptions(showArea = TRUE, 
                                                metric = TRUE,
                                                shapeOptions = drawShapeOptions(clickable = TRUE), 
                                                repeatMode = FALSE),
        markerOptions = drawMarkerOptions(), # Enable marker drawing
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
  
  # Function to add shapes and markers to the map
  addFeaturesToMap <- function(map, shapes, markers) {
    for (shape in shapes) {
      if (shape$type == "polygon") {
        coords <- st_coordinates(shape$geometry)[, 1:2]
        leafletProxy(map) %>%
          addPolygons(lng = coords[, 1], lat = coords[, 2], color = "blue", weight = 2, fill = TRUE)
      } else if (shape$type == "polyline") {
        coords <- st_coordinates(shape$geometry)[, 1:2]
        leafletProxy(map) %>%
          addPolylines(lng = coords[, 1], lat = coords[, 2], color = "blue", weight = 2)
      } else if (shape$type == "circle") {
        center <- st_coordinates(shape$geometry)[1, 1:2]
        radius <- shape$radius
        leafletProxy(map) %>%
          addCircles(lng = center[1], lat = center[2], radius = radius, color = "blue", weight = 2, fill = TRUE)
      }
    }
    for (marker in markers) {
      leafletProxy(map) %>%
        addMarkers(lng = marker$lng, lat = marker$lat, label = marker$label,
                   labelOptions = marker$labelOptions)
    }
  }
  
  # Observe basemap change and re-render shapes and markers
  observeEvent(input$basemap, {
    leafletProxy("map") %>%
      clearTiles() %>%
      addProviderTiles(provider = input$basemap)
    addFeaturesToMap("map", drawn_shapes$shapes, drawn_markers$markers)
  })
  
  # Handle shape drawing
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature
    if (feature$geometry$type == "Polygon") {
      polygon_coordinates <- feature$geometry$coordinates[[1]]
      new_polygon <- st_polygon(list(do.call(rbind, lapply(polygon_coordinates, function(x) c(x[[1]], x[[2]])))))
      drawn_shapes$shapes <- append(drawn_shapes$shapes, list(list(type = "polygon", geometry = new_polygon)))
      edited_shape$shapes <- list()  # Reset edited shape
      addFeaturesToMap("map", list(list(type = "polygon", geometry = new_polygon)), list())  # Add the new polygon to the map
    } else if (feature$geometry$type == "LineString") {
      line_coordinates <- feature$geometry$coordinates
      new_polyline <- st_linestring(do.call(rbind, lapply(line_coordinates, function(x) c(x[[1]], x[[2]]))))
      drawn_shapes$shapes <- append(drawn_shapes$shapes, list(list(type = "polyline", geometry = new_polyline)))
      edited_shape$shapes <- list()  # Reset edited shape
      addFeaturesToMap("map", list(list(type = "polyline", geometry = new_polyline)), list())  # Add the new polyline to the map
    } else if (feature$geometry$type == "Point" && !is.null(feature$properties$radius)) {
      circle_center <- feature$geometry$coordinates
      new_circle <- st_point(circle_center)
      drawn_shapes$shapes <- append(drawn_shapes$shapes, list(list(type = "circle", geometry = new_circle, radius = feature$properties$radius)))
      edited_shape$shapes <- list()  # Reset edited shape
      addFeaturesToMap("map", list(list(type = "circle", geometry = new_circle, radius = feature$properties$radius)), list())  # Add the new circle to the map
    } else if (feature$geometry$type == "Point") {
      new_marker <- list(
        lng = feature$geometry$coordinates[[1]],
        lat = feature$geometry$coordinates[[2]],
        label = "",
        labelOptions = NULL
      )
      drawn_markers$markers <- append(drawn_markers$markers, list(new_marker))
      addFeaturesToMap("map", list(), list(new_marker))  # Add the new marker to the map
    }
  })
  
  # Handle shape editing
  observeEvent(input$map_draw_edited_features, {
    edited_features <- input$map_draw_edited_features$features
    edited_shapes <- list()
    for (feature in edited_features) {
      if (feature$geometry$type == "Polygon") {
        edited_polygon_coords <- feature$geometry$coordinates[[1]]
        edited_polygon_sf <- st_polygon(list(do.call(rbind, lapply(edited_polygon_coords, function(x) c(x[[1]], x[[2]])))))
        edited_shapes <- append(edited_shapes, list(list(type = "polygon", geometry = edited_polygon_sf)))
      } else if (feature$geometry$type == "LineString") {
        edited_polyline_coords <- feature$geometry$coordinates
        edited_polyline_sf <- st_linestring(do.call(rbind, lapply(edited_polyline_coords, function(x) c(x[[1]], x[[2]]))))
        edited_shapes <- append(edited_shapes, list(list(type = "polyline", geometry = edited_polyline_sf)))
      } else if (feature$geometry$type == "Point" && !is.null(feature$properties$radius)) {
        edited_circle_center <- feature$geometry$coordinates
        edited_circle_sf <- st_point(edited_circle_center)
        edited_shapes <- append(edited_shapes, list(list(type = "circle", geometry = edited_circle_sf, radius = feature$properties$radius)))
      }
    }
    edited_shape$shapes <- edited_shapes
    addFeaturesToMap("map", edited_shapes, list())  # Add the edited shapes to the map
  })
  
  # Handle adding label to the last drawn or edited shape
  observeEvent(input$add_label_button, {
    label_text_input <- input$label_text_input
    if (length(edited_shape$shapes) > 0) {
      last_shape <- edited_shape$shapes[[1]]
    } else if (length(drawn_shapes$shapes) > 0) {
      last_shape <- drawn_shapes$shapes[[length(drawn_shapes$shapes)]]
    } else if (length(drawn_markers$markers) > 0) {
      last_marker <- drawn_markers$markers[[length(drawn_markers$markers)]]
      last_marker$label <- label_text_input
      last_marker$labelOptions <- labelOptions(noHide = TRUE, direction = "bottom",
                                               style = list(
                                                 color = "red",
                                                 "font-family" = "serif",
                                                 "font-style" = "italic",
                                                 "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                 "font-size" = "12px",
                                                 "border-color" = "rgba(0,0,0,0.5)"
                                               ))
      drawn_markers$markers[[length(drawn_markers$markers)]] <- last_marker
      addFeaturesToMap("map", list(), list(last_marker))
      return()
    } else {
      return()  # Return if there are no drawn or edited shapes
    }
    
    centroid <- st_centroid(st_sfc(last_shape$geometry))
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
  
  # Handle uploading the last drawn or edited shape
  observeEvent(input$UpldButton, {
    tryCatch({
      if (length(edited_shape$shapes) > 0) {
        shp <- edited_shape$shapes[[1]]
      } else if (length(drawn_shapes$shapes) > 0) {
        shp <- drawn_shapes$shapes[[length(drawn_shapes$shapes)]]
      } else {
        showNotification("No shapes to upload!", type = "error")
        return()  # Return if there are no drawn or edited shapes
      }
      
      # Convert to sf object
      shp_sf <- st_sfc(shp$geometry, crs = 4326) %>% st_sf()
      
      shapefileName <- input$label_text_input
      temp_shp <- tempdir()
      
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
      
      showNotification(paste("Upload complete! Shapefile named: ", shapefileName), type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
