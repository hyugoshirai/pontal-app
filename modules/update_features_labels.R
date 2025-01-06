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