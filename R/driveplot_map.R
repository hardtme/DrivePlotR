#' Create a standalone map.
#'
#' @param shareddata A SharedData object containing observations to be plotted.
#' @param lng The bare (unquoted) column of shareddata containing longitude
#'   (only required if shareddata does not have a geometry column).
#' @param lat The bare (unquoted) column of shareddata containing latitude
#'   (only required if shareddata does not have a geometry column).
#' @param colorvar The bare (unquoted) column in shareddata to which color
#'   should be mapped.
#' @param label An optional label for the map points.
#' @param colorpalette The color palette for the plot; either a single
#'   color (e.g., "red") or one of the viridis color palettes compatible
#'   with leaflet. Run `leaflet_color_palettes()` to see the available options
#'   for viridis color palettes. If `colorvar` and `colorpalette` are both not
#'   provided, `colorpalette` defaults to blue. If `colorvar` is provided but
#'   `colorpalette` is not, `colorpalette` defaults to the color palette
#'   "viridis".
#' @param fillopacity The opacity of the fill of the map points (0 to 1).
#' @param mapheight The height of the map in CSS units, e.g, "98vh".
#' @returns A leaflet map.
#' @importFrom crosstalk SharedData is.SharedData
#' @importFrom rlang enquo eval_tidy quo_squash quo
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @export
#' @examples
#' library(crosstalk)
#' data(drive7)
#' shared_drive <- SharedData$new(drive7)
#'
#' # Basic map of one drive
#' driveplot_map(shareddata = shared_drive)
#'
#' # Color drive points by direction of car
#' driveplot_map(
#'   shareddata = shared_drive,
#'   colorvar = gyro_heading,
#'   label = gyro_heading,
#'   colorpalette = "viridis"
#' )
driveplot_map <- function(shareddata,
                          lng = NULL,
                          lat = NULL,
                          colorvar = NULL,
                          label = NA,
                          colorpalette = NULL,
                          fillopacity = 1,
                          mapheight = "98vh") {
  shareddata <- convert_to_SharedData(shareddata)

  # Get original data from shareddata so we can check the type of colorvar
  # and set color palette domains
  # We can't directly access columns in a SharedData object
  ogdata <- shareddata$origData()
  quolng <- enquo(lng)
  quolat <- enquo(lat)
  quocolor <- enquo(colorvar)
  quolabel <- enquo(label)

  colorvarnumeric <- check_colorvar(
    shareddata = shareddata,
    colorvar = {{ quocolor }}
  )

  colorpalette <- check_colorpalette(
    shareddata = shareddata,
    colorvar = {{ quocolor }},
    colorpalette = colorpalette
  )

  lnglat <- check_lnglat(
    shareddata = shareddata,
    lng = {{ quolng }},
    lat = {{ quolat }}
  )

  if (isFALSE(grepl("vh", mapheight))) {
    stop("Must specify `mapheight` in CSS units, e.g., '98vh'")
  }

  # Create color palettes
  if (is.null(colorvarnumeric)) {
    quocolor <- 0
    colorpal <- driveplot_map_color(
      colorvarnumeric = colorvarnumeric,
      colorpalette = colorpalette,
      quocolor = {{ quocolor }},
      ogdata = ogdata
    )
  } else {
    colorpal <- driveplot_map_color(
      colorvarnumeric = colorvarnumeric,
      colorpalette = colorpalette,
      quocolor = {{ quocolor }},
      ogdata = ogdata
    )
  }

  if (!is.null(lnglat)) {
    lng <- lnglat$lng
    lat <- lnglat$lat
    plot_map <- eval_tidy(quo_squash(quo({
      leaflet(data = shareddata, height = mapheight, width = "100%") |>
        addTiles() |>
        addCircleMarkers(
          lat = lat, lng = lng, stroke = TRUE,
          weight = 2, color = "dimgray",
          label = ~ (!!quolabel),
          fillColor = ~ colorpal(!!quocolor),
          fillOpacity = fillopacity
        )
    })))
    plot_map
  } else {
    plot_map <- eval_tidy(quo_squash(quo({
      leaflet(data = shareddata, height = mapheight, width = "100%") |>
        addTiles() |>
        addCircleMarkers(
          lat = ~ (!!quolat), lng = ~ (!!quolng),
          stroke = TRUE, weight = 2, color = "dimgray",
          label = ~ (!!quolabel),
          fillColor = ~ colorpal(!!quocolor),
          fillOpacity = fillopacity
        )
    })))
    plot_map
  }
}
