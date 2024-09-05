# Helper Functions ---------------------------------------------------------

# Initialize the Leaflet map
initLeafletMap <- function(input) {
  # Read the ROI shapefile
  roi <- st_read("../LeastCostPath/BD_LeastCostTool/ROI.shp")
  roi <- st_transform(roi, crs = 4326)
  # Calculate the centroid of the ROI shapefile
  roi_centroid <- st_centroid(st_union(roi))
  # Extract the coordinates of the centroid
  centroid_coords <- st_coordinates(roi_centroid)
  
  leaflet() %>%
    addProviderTiles(providers$OpenStreetMap) %>%
    addTiles (urlTemplate = "https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}", group = "Satellite") %>%
    addPolygons(data = roi, color = "blue", weight = 2, opacity = 0.8, fillOpacity = 0, group = "ROI") %>%
    setView(lng = centroid_coords[1], lat = centroid_coords[2], zoom = 10) %>%
    addDrawToolbar(
      polylineOptions = TRUE,
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
    ) |> 
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Satellite"),
      options = layersControlOptions(collapsed = FALSE)
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
        label = HTML(paste0('<div style="background-color: white; border: 2px solid black; padding: 1px; border-radius: 1px;">
                        <span style="color: black; font-size: 14px; font-weight: bold;">', feature$name, '</span>
                      </div>')),
        # label = feature$name,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "auto",
          textOnly = TRUE,
          # style = list(
          #   "color" = "black",                    # Label text color
          #   "font-size" = "16px",                 # Increase font size
          #   "font-weight" = "bold",               # Make the font bold
          #   "-webkit-text-stroke" = "0.2px white"   # Create a white outline
          # )
        )
      )
  }
}

# Handle new feature draw
handleNewFeature <- function(input, feature) {
  feature_type <- feature$properties$feature_type  # Get the type of the geometry
  print (feature)
  print(feature_type)
  if (feature_type == "rectangle" | feature_type == "polygon") {
    coords <- feature$geometry$coordinates[[1]] # Extract the coordinates
    coords_matrix <- do.call(rbind, lapply(coords, unlist)) # Convert coordinates to matrix
    
    feature_sf <- st_sfc(st_polygon(list(coords_matrix))) # Create the sf object
  } else if (feature_type == "marker") {
    coords <- unlist(feature$geometry$coordinates) # Extract and unlist the coordinates for point
    print(coords)
    feature_sf <- st_sfc(st_point(coords)) # Create the sf object for point
  } else if (feature_type == "circle") {
    center <- unlist(feature$geometry$coordinates)  # Extract and unlist the center coordinates
    radius <- feature$properties$radius  # Extract the radius in meters
    # Create a point geometry in WGS 84 (longitude/latitude)
    point <- st_sfc(st_point(center), crs = 4326)
    # Transform the point to a projected CRS that uses meters (e.g., UTM Zone 23S)
    point_projected <- st_transform(point, crs = 32723)
    # Apply the buffer in the projected CRS to create the circle as a polygon
    circle_polygon_projected <- st_buffer(point_projected, dist = radius)
    # Transform the circle polygon back to WGS 84
    circle_polygon <- st_transform(circle_polygon_projected, crs = 4326)
    circle_polygon <- st_set_crs(circle_polygon_projected, NA)
    feature_sf <- st_sfc(circle_polygon)  # Create the sf object
    # Optionally update the feature type to "POLYGON"
    feature_type <- "POLYGON"
  }
  else if (feature_type == "polyline") {
    coords <- feature$geometry$coordinates
    print (coords)
    coords_matrix <- do.call(rbind, lapply(coords, function(coord) unlist(coord)))
    print (coords_matrix)
    
    feature_sf <- st_sfc(st_linestring(coords_matrix)) # Create the sf object for polyline
  }
  print(feature_sf)
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
  # Extract the feature ID and coordinates
  edited_feature_id <- edited_features$features[[1]]$properties$`_leaflet_id`
  feature_type <- edited_features$features[[1]]$geometry$type
  edited_coords <- edited_features$features[[1]]$geometry$coordinates
  
  # Initialize the sf object based on the feature type
  if (feature_type == "Point") {
    coords <- unlist(edited_coords)  # Flatten the list if necessary
    edited_feature_sf <- st_sfc(st_point(coords))
    
  } else if (feature_type == "LineString") {
    # Convert the coordinates to a matrix
    edited_coords_matrix <- do.call(rbind, lapply(edited_coords, unlist))
    edited_feature_sf <- st_sfc(st_linestring(edited_coords_matrix))
    
  } else if (feature_type == "Polygon") {
    edited_coords <- edited_features$features[[1]]$geometry$coordinates[[1]] # Extract the coordinates
    edited_coords_matrix <- do.call(rbind, lapply(edited_coords, unlist)) # Convert coordinates to matrix
    edited_feature_sf <- st_sfc(st_polygon(list(edited_coords_matrix))) # Create the sf object
  } else {
    showNotification("Unsupported feature type.", type = "error")
    return()
  }
  
  # Retrieve and update the current features list
  current_features <- features_list()
  current_features[[as.character(edited_feature_id)]]$geometry <- edited_feature_sf
  features_list(current_features)
  
  # Update the data table and feature labels
  updateFeaturesTable()
  updateFeatureLabels()
  
  # Notify the user about the update
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
