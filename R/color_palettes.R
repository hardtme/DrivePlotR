# Available color palettes from viridis and Color Brewer

#' Vector of viridis color palettes supported by Leaflet
#'
#' Helper function
#'
#' In a DrivePlotR plot map, the map uses the same color palette as
#'   the companion graphs.
#' @returns Vector of viridis color palettes supported by Leaflet.
#' @export
#' @examples
#' viridis_color_palettes()
viridis_color_palettes <- function() {
  c("viridis", "magma", "inferno", "plasma",
    "cividis", "rocket", "mako", "turbo")
}

#' Vector of ColorBrewer color palettes supported by Leaflet
#'
#' Helper function
#' In a DrivePlotR plot map, the map uses the same color palette as
#'   the companion graphs.
#' @returns Vector of ColorBrewer color palettes supported by Leaflet.
#' @export
#' @examples
#' colorbrewer_color_palettes()
colorbrewer_color_palettes <- function() {
  c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn",
    "Spectral", "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
    "Set2", "Set3", "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys",
    "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds",
    "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")
}
