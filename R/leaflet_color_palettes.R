#' Vector of viridis color palettes supported by Leaflet
#'
#' Helper function
#'
#' In a DrivePlotR plot map, the map uses the same color palette as
#'   the companion graphs.
#' @returns Vector of viridis color palettes supported by Leaflet.
#' @export
#' @examples
#' leaflet_color_palettes()

leaflet_color_palettes <- function() {
  c("viridis", "magma", "inferno", "plasma")
}
