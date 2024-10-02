# Creates a simple black and white map of shapefiles. Useful for visualizing the number
# of CPUMAs in a region. Cannot graph data other than simple shapefiles.
map_geographies <- function(data) {
  ggplot(data) +
    geom_sf(fill = "white", color = "black") +
    theme_void()
}

# Creates a colored map of shapefiles, with the color indicating the gradient of the
# data. Data must contain a column called `diff` for this function to work as intended.
map_data <- function(
    data,
    borders = TRUE
) {
  
  # If borders are turned on, assign this setting to black
  border_color <- if (borders) "black" else NA
  
  ggplot(data) +
    geom_sf(aes(fill = diff), color = border_color) +  # Remove borders
    scale_fill_gradient2(low = "#0552f7", mid = "white", high = "#e68f0e", midpoint = 0) +
    theme_void()
}