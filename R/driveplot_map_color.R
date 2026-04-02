#' Create the color palette to be used in `driveplot_map()`.
#'
#' Helper function for internal use only
#' @param colorvarnumeric An indication of whether the color variable is
#'   numeric (TRUE), not numeric (FALSE), or not provided (NULL)
#' @param colorpalette The color palette to use; either a single color
#'   (e.g., "red") or one of "viridis", "inferno", "magma", or "plasma".
#' @param quocolor A quosure containing the color variable.
#' @param ogdata The data used to create the domain for the color palette.
#' @returns The leaflet color palette to be used in `driveplot_map()`.
#' @importFrom leaflet colorFactor colorNumeric
#' @importFrom viridisLite viridis
#' @importFrom rlang eval_tidy
#' @importFrom dplyr n_distinct
driveplot_map_color <- function(colorvarnumeric,
                                colorpalette,
                                quocolor,
                                ogdata) {
  if (is.null(colorvarnumeric) &&
      colorpalette %in% leaflet_color_palettes()) {
    colorpal <- colorFactor(palette = viridis(n = 1, option = colorpalette),
                            domain = NULL,
                            na.color = "dimgray")
  } else if (is.null(colorvarnumeric) &&
             !(colorpalette %in% leaflet_color_palettes())) {
    colorpal <- colorFactor(palette = colorpalette,
                            domain = NULL,
                            na.color = "dimgray")
  } else if (isTRUE(colorvarnumeric)) {
    colorpal <- colorNumeric(palette = colorpalette,
                             domain = eval_tidy(quocolor, data = ogdata),
                             na.color = "dimgray")
  } else if (isFALSE(colorvarnumeric)) {
    ncolors <- eval_tidy(quocolor, data = ogdata) |>
      n_distinct(na.rm = TRUE)
    colorpal <- colorFactor(palette = viridis(n = ncolors,
                                              option = colorpalette),
                            domain = eval_tidy(quocolor, data = ogdata),
                            na.color = "dimgray")
  }
  colorpal
}
