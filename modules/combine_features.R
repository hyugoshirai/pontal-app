# Combining All sf Objects:
combineFeatures <- function() {
  features <- features_list()
  combined_sf <- do.call(rbind, features)
  return(combined_sf)
}