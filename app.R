# Load necessary packages
packages <- c("googledrive", "leaflet", "leaflet.extras", "raster", "sf", "shiny", "shinyWidgets", "shinyjs", "terra", "tidyverse", "viridis", "DT")
install_packages <- packages[!packages %in% installed.packages()]
if (length(install_packages) > 0) {
  install.packages(install_packages)
}
invisible(lapply(packages, library, character.only = TRUE))

# source("helper-functions.R")
#### Source Modules
# Define the directory containing the R script files
modules_directory <- "modules"

# List all .R files in the directory
script_files <- list.files(modules_directory, pattern = "\\.R$", full.names = TRUE)

# Loop through each R file and source it
for (script_file in script_files) {
  source(script_file)
}

# UI definition
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  titlePanel("Mapping Tool"),
    mainPanel(
      leafletOutput("map"),
      DTOutput("features_table"),
      downloadButton("download_shapefile", "Download Selected Shapefile")  # Download button for shapefile
  )
)

# Server Function ----------------------------------------------------------
# Reactive values to store features
features_list <- reactiveVal(list())
# Create a reactive value for the features table
features_table_data <- reactiveVal(data.frame(ID = character(), Name = character(), Feature = character(), stringsAsFactors = FALSE))
rendered_table <- renderDT({
  features_table_data()
}, editable = list(target = "cell", disable = list(columns = c(0, 1, 3))), server = FALSE) # Render the features table


# Define server logic
server <- function(input, output, session) {
  
  # Initialize leaflet map
  output$map <- renderLeaflet({
    initLeafletMap(input)
  })
  
  # Update the basemap when selection changes
  observeEvent(input$basemap, {
    leafletProxy("map") %>%
      clearTiles() %>%
      addProviderTiles(providers[[input$basemap]])
  })
  
  # Update the features list when a new feature is drawn
  observeEvent(input$map_draw_new_feature, {
    handleNewFeature(input, input$map_draw_new_feature)
    # Render features table
    output$features_table <- rendered_table
  })

  # Update the features list when a feature is edited
  observeEvent(input$map_draw_edited_features, {
    handleFeatureEdit(input, input$map_draw_edited_features)
    # Render features table
    output$features_table <- rendered_table
  })
  
  # Observer for cell edits in the data table
  observeEvent(input$features_table_cell_edit, {
    handleCellEdit(input, input$features_table_cell_edit)
    # Render features table
    output$features_table <- rendered_table
  })
  
  # Handle feature deletion
  observeEvent(input$map_draw_deleted_features, {
    handleFeatureDeletion(input, input$map_draw_deleted_features)
    # Render features table
    output$features_table <- rendered_table
  })

  output$download_shapefile <- downloadHandler(
    filename = function() {
      paste("drawn_features", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      # Combine all features into one sf object
      combined_sf <- combineFeatures()
      # Check CRS
      crs_info <- st_crs(combined_sf)
      # If CRS is missing, set it to the known CRS
      if (is.na(st_crs(combined_sf))) {
        st_crs(combined_sf) <- 4326  # Set CRS to WGS 84 if missing
      } else {
        # Reproject the features to WGS 84 if CRS is already defined
        combined_sf <- st_transform(combined_sf, crs = 4326)
      }
      
      # Separate points and polygons
      points_sf <- combined_sf[st_geometry_type(combined_sf) == "POINT", ]
      polygons_sf <- combined_sf[st_geometry_type(combined_sf) == "POLYGON", ]
      polylines_sf <- combined_sf[st_geometry_type(combined_sf) == "LINESTRING", ]
      
      
      # Create a temporary directory to save the shapefile components
      temp_shapefile_dir <- tempdir()
      
      shapefile_components <- c()
      
      # Write the points shapefile if it exists
      if (nrow(points_sf) > 0) {
        points_shapefile_name <- "drawn_points"
        st_write(points_sf, dsn = file.path(temp_shapefile_dir, points_shapefile_name), driver = "ESRI Shapefile", delete_dsn = TRUE)
        shapefile_components <- c(shapefile_components, list.files(temp_shapefile_dir, pattern = paste0(points_shapefile_name, ".*$"), full.names = TRUE))
      }
      
      # Write the polygons shapefile if it exists
      if (nrow(polygons_sf) > 0) {
        polygons_shapefile_name <- "drawn_polygons"
        st_write(polygons_sf, dsn = file.path(temp_shapefile_dir, polygons_shapefile_name), driver = "ESRI Shapefile", delete_dsn = TRUE)
        shapefile_components <- c(shapefile_components, list.files(temp_shapefile_dir, pattern = paste0(polygons_shapefile_name, ".*$"), full.names = TRUE))
      }
      
      # Write the polylines shapefile if it exists
      if (nrow(polylines_sf) > 0) {
        polylines_shapefile_name <- "drawn_polylines"
        st_write(polylines_sf, dsn = file.path(temp_shapefile_dir, polylines_shapefile_name), driver = "ESRI Shapefile", delete_dsn = TRUE)
        shapefile_components <- c(shapefile_components, list.files(temp_shapefile_dir, pattern = paste0(polylines_shapefile_name, ".*$"), full.names = TRUE))
      }
      
      # Create a zip file with the shapefile components
      zip::zipr(zipfile = file, files = shapefile_components)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
