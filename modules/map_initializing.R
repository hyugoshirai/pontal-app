# Initialize the Leaflet map
initLeafletMap <- function(input) {
  # Read the ROI shapefile
  roi <- st_read("../LeastCostKraken/LeastCostPath/BD_LeastCostTool/cities.shp")
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