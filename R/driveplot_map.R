#' Create a standalone map
#'
#' @param shareddata a SharedData object containing observations to be plotted
#' @param lng the column of shareddata containing longitude
#' (only required if shareddata does not have a geometry column)
#' @param lat the column of shareddata containing latitude
#' (only required if shareddata does not have a geometry column)
#' @param colorvar the variable in shareddata to which color should be mapped
#' @param label an optional label for the map points
#' @param colorpalette either a single color (e.g., "red") or one of
#' "viridis", "inferno", "magma", or "plasma"
#' @param fillOpacity the opacity of the fill (0 to 1)
#' @param mapheight the height of the map in CSS units, e.g, "98vh"
#' @returns leaflet map
#' @importFrom crosstalk SharedData
#' @importFrom dplyr pull filter
#' @importFrom rlang enquo eval_tidy quo_squash quo
#' @importFrom leaflet colorFactor derivePoints colorNumeric leaflet
#' addTiles addCircleMarkers
#' @importFrom viridisLite viridis
#' @export
#' @examples
#' library(crosstalk)
#' library(dplyr)
#' data(nds_data)
#' nds_sf7 <- nds_data |>
#'   filter(drive==7) |>
#'   sf::st_as_sf(coords = c("gps_long", "gps_lat"), crs = "WGS84")
#' nds_sf7_sd <- SharedData$new(nds_sf7)
#'
#' # basic map of one drive
#' driveplot_map(shareddata = nds_sf7_sd)
#'
#' # color drive points by direction of car
#' driveplot_map(nds_sf7_sd, colorvar=gyro_heading, colorpalette="viridis")
driveplot_map <- function(shareddata,
                          lng = NULL,
                          lat = NULL,
                          colorvar = NULL,
                          label = NA,
                          colorpalette = "#03F",
                          fillOpacity = 1,
                          mapheight = "98vh") {
  # Get original data from shareddata so we can check the type of colorvar
  # and set color palette domains
  # We can't directly access columns in a SharedData object
  ogdata <- shareddata$origData()
  lngcheck <- tryCatch(ogdata |> pull({{ lng }}),
                       error = function(e){},
                       finally = NULL)
  latcheck <- tryCatch(ogdata |> pull({{ lat }}),
                       error = function(e){},
                       finally = NULL)
  quolabel <- enquo(label)
  colorvarnumeric <- tryCatch(ogdata |> pull({{ colorvar }}) |> is.numeric(),
                              error = function(e){},
                              finally = NULL)
  # Create color palettes
  if (is.null(colorvarnumeric) &&
     colorpalette %in% c("viridis", "magma", "inferno", "plasma")) {
    colorarg <- 0
    # We need to make sure the same color is used on the map and the plots
    colorpal <- colorFactor(palette = viridis(n = 1, option = colorpalette),
                            domain = NULL,
                            na.color = "dimgray")
  }
  if (is.null(colorvarnumeric) &&
     !(colorpalette %in% c("viridis", "magma", "inferno", "plasma"))) {
    colorarg <- 0
    colorpal <- colorFactor(palette = colorpalette,
                            domain = NULL,
                            na.color = "dimgray")
  }

  if (isTRUE(colorvarnumeric)) {
    colorarg <- enquo(colorvar)
    colorpal <- colorNumeric(palette = colorpalette,
                             domain = ogdata |> pull({{ colorvar }}),
                             na.color = "dimgray")

  }
  if (isFALSE(colorvarnumeric)) {
    colorarg <- enquo(colorvar)
    colorpal <- colorFactor(palette = colorpalette,
                                     domain = ogdata |> pull({{ colorvar }}),
                                     na.color = "dimgray")
  }
  if (is.null(lngcheck) && is.null(latcheck)) {
    lnglat <- derivePoints(shareddata)
    lng <- lnglat$lng
    lat <- lnglat$lat
    plot_map <- eval_tidy(quo_squash(quo({
      leaflet(data = shareddata, height = mapheight, width = "100%") |>
        addTiles() |>
        addCircleMarkers(lat = lat, lng = lng, stroke = T,
                         weight = 2, color = "dimgray",
                         label = ~(!!quolabel),
                         fillColor = ~colorpal(!!colorarg),
                         fillOpacity = fillOpacity)
    })))
    return(plot_map)
  }else{
    quolng <- enquo(lng)
    quolat <- enquo(lat)

    plot_map <- eval_tidy(quo_squash(quo({
      leaflet(data = shareddata, height = mapheight, width = "100%") |>
        addTiles() |>
        addCircleMarkers(lat = ~(!!quolat), lng = ~(!!quolng),
                         stroke = T, weight = 2, color = "dimgray",
                         label = ~(!!quolabel),
                         fillColor = ~colorpal(!!colorarg),
                         fillOpacity = fillOpacity)
    })))
    return(plot_map)
  }
}
