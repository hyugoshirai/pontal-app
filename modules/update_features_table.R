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