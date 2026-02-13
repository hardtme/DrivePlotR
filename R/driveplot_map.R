#' Create a standalone map
#'
#' @param shareddata A SharedData object containing observations to be plotted.
#' @param lng The column of shareddata containing longitude
#'   (only required if shareddata does not have a geometry column).
#' @param lat the column of shareddata containing latitude
#'   (only required if shareddata does not have a geometry column).
#' @param colorvar The variable in shareddata to which color should be mapped.
#' @param label An optional label for the map points.
#' @param colorpalette The color palette for the map; either a single color
#'   (e.g., "red") or one of "viridis", "inferno", "magma", or "plasma".
#' @param fillOpacity The opacity of the fill of the map points (0 to 1).
#' @param mapheight The height of the map in CSS units, e.g, "98vh".
#' @returns A leaflet map.
#' @importFrom crosstalk SharedData is.SharedData
#' @importFrom dplyr pull filter
#' @importFrom rlang enquo eval_tidy quo_squash quo
#' @importFrom leaflet colorFactor derivePoints colorNumeric leaflet
#'    addTiles addCircleMarkers
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

  if (isFALSE(crosstalk::is.SharedData(shareddata))) {
    stop("`shareddata` must be a SharedData object.", call. = FALSE)
  }
  # Get original data from shareddata so we can check the type of colorvar
  # and set color palette domains
  # We can't directly access columns in a SharedData object
  ogdata <- shareddata$origData()
  columns <- colnames(ogdata)
  lngname <- as_label(enquo(lng))
  latname <- as_label(enquo(lat))

  lngcheck <- if (lngname == "NULL") FALSE else TRUE
  latcheck <- if (latname == "NULL") FALSE else TRUE
  if ((isFALSE(lngcheck) && isTRUE(latcheck)) ||
      (isTRUE(lngcheck) && isFALSE(latcheck))) {
    stop("If providing `lng` and `lat`, must provide both.",
         call. = FALSE)
  }

  if (isTRUE(lngcheck) && !(lngname) %in% columns) {
    stop(paste0("Can't find column `", lngname, "` in `shareddata`."),
         call. = FALSE)
  }

  if (isTRUE(latcheck) && !(latname) %in% columns) {
    stop(paste0("Can't find column `", latname, "` in `shareddata`."),
         call. = FALSE)
  }

  if (isFALSE(grepl("vh", mapheight))) {
    stop("Must specify `mapheight` in CSS units, e.g., '98vh'")
  }

  quolabel <- enquo(label)
  colorvarnumeric <- tryCatch(
    ogdata |>
      pull({{ colorvar }}) |>
      is.numeric(),
    error = function(e){},
    finally = NULL
  )
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
    ncolors <- ogdata |>
      filter(!is.na({{ colorvar }})) |>
      pull({{ colorvar }}) |>
      unique() |>
      length()
    colorpal <- colorFactor(palette = viridis(n = ncolors,
                                              option = colorpalette),
                            domain = ogdata |> pull({{ colorvar }}),
                            na.color = "dimgray")
  }
  if (isFALSE(lngcheck) && isFALSE(latcheck)) {
    sfgeomcheck <- attr(ogdata, "sf_column")
    if (is.null(sfgeomcheck)) {
      stop("Can't find a geometry column and `lng` and `lat` not provided.",
           call. = FALSE)
    }

    lnglat <- tryCatch(
      derivePoints(shareddata),
      error = function(e) {
        stop("Geometry column must have type POINT.",
             call. = FALSE)
        }
    )
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
