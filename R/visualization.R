#' Create a Black and White Map of Shapefiles
#'
#' This function generates a simple black and white map using shapefiles. It is useful for visualizing 
#' the number of CPUMAs or other geographic boundaries in a region. The function cannot graph data other 
#' than simple shapefiles.
#'
#' @param data A `sf` object (simple feature) containing the shapefile data to be plotted.
#'
#' @return A `ggplot2` plot object displaying a black and white map of the shapefile.
#' @examples
#' # Example usage:
#' # Assuming `sf_data` is a simple feature (sf) object
#' map_geographies(sf_data)
#'
#' @export
map_geographies <- function(data) {
  ggplot(data) +
    geom_sf(fill = "white", color = "black") +
    theme_void()
}


#' Create a Colored Map of Shapefiles with Gradient
#'
#' This function generates a colored map using shapefiles, where the color indicates the gradient of the data. 
#' The data must contain a column named `diff` for the function to work as intended. Optionally, the function 
#' can add borders to the shapes.
#'
#' @param data A `sf` object (simple feature) containing the shapefile data to be plotted. It must contain 
#'   a column called `diff` to represent the gradient.
#' @param borders A logical indicating whether to include black borders around the shapes. Defaults to `TRUE`.
#'
#' @return A `ggplot2` plot object displaying a colored map of the shapefile with gradient fill based on the `diff` column.
#' @examples
#' # Example usage:
#' # Assuming `sf_data` is a simple feature (sf) object with a `diff` column
#' map_data(sf_data, borders = TRUE)
#'
#' @export
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
