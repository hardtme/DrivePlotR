#' Create the color palette to be used in `driveplot_map()`
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
#' @keywords internal
driveplot_map_color <- function(colorvarnumeric,
                                colorpalette,
                                quocolor,
                                ogdata) {

  palettelength <- length(colorpalette)

  if (palettelength > 1) {
  if (is.null(colorvarnumeric)) {
    colorpal <- colorFactor(
      palette = colorpalette[1],
      domain = NULL,
      na.color = "#00000000"
    )
  } else if (isTRUE(colorvarnumeric)) {
    colorpal <- colorNumeric(
      palette = colorpalette,
      domain = eval_tidy(quocolor, data = ogdata),
      na.color = "#00000000"
    )
  } else if (isFALSE(colorvarnumeric)) {
    colorvals <- eval_tidy(quocolor, data = ogdata)
    ncolors <- colorvals |>
      n_distinct(na.rm = TRUE)
    if (is.factor(colorvals)) {
      colorvals <- droplevels(colorvals)
    }
    if (palettelength == ncolors) {
      # number of colors in palette = number of colors needed
      colorpal <- colorFactor(
        palette = colorpalette, # keep all colors
        domain = colorvals,
        na.color = "#00000000"
      )
    } else if (palettelength > ncolors) {
      # number of colors in palette > number of colors needed
      colorpal <- colorFactor(
        palette = colorpalette[1:ncolors], # keep only necessary colors
        domain = colorvals,
        na.color = "#00000000"
      )
    } else {
      # number of colors in palette < number of colors needed
      stop(
        paste0("Insufficient colors in manual scale. ", ncolors,
               " needed but only ", palettelength, " provided."),
        call. = FALSE
      )
    }
  }
  } else {
  if (is.null(colorvarnumeric) &&
      !(colorpalette %in% viridis_color_palettes()) &&
      !(colorpalette %in% colorbrewer_color_palettes())) {
    colorpal <- colorFactor(
      palette = colorpalette,
      domain = NULL,
      na.color = "#00000000"
    )
  } else if (is.null(colorvarnumeric) &&
    colorpalette %in% viridis_color_palettes()) {
    colorpal <- colorFactor(
      palette = viridis(n = 1, option = colorpalette),
      domain = NULL,
      na.color = "#00000000"
    )
  } else if (!is.null(colorvarnumeric) &&
             !(colorpalette %in% viridis_color_palettes()) &&
             !(colorpalette %in% colorbrewer_color_palettes())) {
      # {{ colorvar }} is a column in the data and a viridis or
      # ColorBrewer palette isn't specified
      # Throw an error if a color variable is specified, but not a color palette
      stop(
      "When specifying colorvar, please use a viridis or ColorBrewer palette.",
      call. = FALSE
      )
  } else if (is.null(colorvarnumeric) &&
             colorpalette %in% colorbrewer_color_palettes()) {
    colorpal <- colorFactor(
      palette = brewer.pal(n = 3, name = colorpalette)[1],
      domain = NULL,
      na.color = "#00000000"
    )
  } else if (isTRUE(colorvarnumeric)) {
    colorpal <- colorNumeric(
      palette = colorpalette,
      domain = eval_tidy(quocolor, data = ogdata),
      na.color = "#00000000"
    )
  } else if (isFALSE(colorvarnumeric) &&
             colorpalette %in% viridis_color_palettes()) {
    colorvals <- eval_tidy(quocolor, data = ogdata)
    ncolors <- colorvals |>
      n_distinct(na.rm = TRUE)
    if (is.factor(colorvals)) {
      colorvals <- droplevels(colorvals)
    }
    colorpal <- colorFactor(
      palette = viridis(
        n = ncolors,
        option = colorpalette
      ),
      domain = colorvals,
      na.color = "#00000000"
    )
  } else if (isFALSE(colorvarnumeric) &&
             colorpalette %in% colorbrewer_color_palettes()) {
    colorvals <- eval_tidy(quocolor, data = ogdata)
    ncolors <- colorvals |>
      n_distinct(na.rm = TRUE)
    if (is.factor(colorvals)) {
      colorvals <- droplevels(colorvals)
    }
    brewercols <- suppressWarnings(
      brewer.pal(n = ncolors, name = colorpalette)
    )
    if (ncolors < 3) {
      brewercols <- brewercols[1:ncolors]
    }
    colorpal <- colorFactor(
      palette = brewercols,
      domain = colorvals,
      na.color = "#00000000"
    )
  }
  }
  colorpal
}
