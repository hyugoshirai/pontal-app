# Helper Functions ---------------------------------------------------------

# Initialize the Leaflet map
initLeafletMap <- function(input) {
  leaflet() %>%
    addProviderTiles(providers$OpenStreetMap) %>%
    setView(lng = -52.320349, lat = -22.513868, zoom = 9) %>%
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
}

# Function to update the features table
updateFeaturesTable <- function() {
  features_df <- data.frame(
    ID = names(features_list()),
    Name = sapply(features_list(), function(x) x$name),
    Feature = sapply(features_list(), function(x) as.character(st_geometry_type(x$geometry))),
    stringsAsFactors = FALSE
  )
  features_table_data(features_df)
}

# Update feature labels on the map
updateFeatureLabels <- function() {
  leafletProxy("map") %>% clearMarkers()  # Clear existing labels
  
  current_features <- features_list()
  
  for (feature_id in names(current_features)) {
    feature <- current_features[[feature_id]]
    coords <- st_coordinates(st_centroid(feature$geometry))
    
    leafletProxy("map") %>%
      addLabelOnlyMarkers(
        lng = coords[1],
        lat = coords[2],
        label = feature$name,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "auto",
          textOnly = TRUE
        )
      )
  }
}

# Handle new feature draw
handleNewFeature <- function(input, feature) {
  coords <- feature$geometry$coordinates[[1]] # Extract the coordinates
  coords_matrix <- do.call(rbind, lapply(coords, unlist)) # Convert coordinates to matrix
  
  feature_sf <- st_sfc(st_polygon(list(coords_matrix))) # Create the sf object
  feature_id <- feature$properties$`_leaflet_id`# Extract the feature ID
  feature_name <- paste("Feature", feature_id) # Initialize a feature name
  feature_sf <- st_sf(id = feature_id, name = feature_name, geometry = feature_sf) # Add the name as a column in the sf object
  
  current_features <- features_list() # Retrieve the current features list
  current_features[[as.character(feature_id)]] <- feature_sf # Add the new feature to the list
  features_list(current_features) # Save the updated list back to the reactive value
  
  updateFeaturesTable() # Update the data table
  updateFeatureLabels() # Update feature labels
  
  showNotification(paste("Feature", feature_id, "has been added"), duration = 5, type = "message") # Inform the user that the feature has been added
}

# Handle feature edit
handleFeatureEdit <- function(input, edited_features) {
  edited_feature_id <- edited_features$features[[1]]$properties$`_leaflet_id` # Extract the feature ID
  edited_coords <- edited_features$features[[1]]$geometry$coordinates[[1]] # Extract the coordinates
  edited_coords_matrix <- do.call(rbind, lapply(edited_coords, unlist)) # Convert coordinates to matrix
  edited_feature_sf <- st_sfc(st_polygon(list(edited_coords_matrix))) # Create the sf object
  
  current_features <- features_list() # Retrieve the current features list
  current_features[[as.character(edited_feature_id)]]$geometry <- edited_feature_sf # Update the feature geometry
  features_list(current_features) # Save the updated list back to the reactive value
  
  updateFeaturesTable() # Update the data table
  updateFeatureLabels() 
  
  showNotification(paste("Feature", edited_feature_id, "has been updated."), duration = 5, type = "message")
}

# Handle cell edit in the data table
handleCellEdit <- function(input, new_data) {
  row <- new_data$row # Extract the row number
  col <- new_data$col # Extract the column number
  new_value <- new_data$value # Extract the new value
  
  current_features <- features_list()  # Retrieve the current features list
  feature_id <- names(current_features)[row]  # Extract the feature ID
  current_features[[feature_id]]$name <- new_value  # Update the feature name
  features_list(current_features)  # Save the updated list back to the reactive value
  
  updateFeaturesTable()  # Update the data table
  updateFeatureLabels()  # Update feature labels

  # Inform the user that the feature name has been updated
  showNotification(paste("Feature name has been updated to:", new_value), duration = 5, type = "message")
}


# Handle feature deletion
handleFeatureDeletion <- function(input, deleted_features) {
  feature_ids <- sapply(deleted_features$features, function(f) f$properties$`_leaflet_id`) # Extract the feature IDs
  
  current_features <- features_list() # Retrieve the current features list
  current_features <- current_features[!names(current_features) %in% feature_ids] # Remove the deleted features
  features_list(current_features) # Save the updated list back to the reactive value
  
  updateFeaturesTable() # Update the data table
  updateFeatureLabels() # Update feature labels
  
  showNotification(paste("Features", paste(feature_ids, collapse = ", "), "have been deleted."), duration = 5, type = "message") # Inform the user that the features have been deleted
}
# Combining All sf Objects:
combineFeatures <- function() {
  features <- features_list()
  combined_sf <- do.call(rbind, features)
  return(combined_sf)
}
