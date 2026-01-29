#' Make a stack of companion graphs from shared drive data
#'
#' @param shareddata a SharedData object containing observations to be plotted
#' @param lng the column of shareddata containing longitude
#' (only required if shareddata does not have a geometry column)
#' @param lat the column of shareddata containing latitude
#' (only required if shareddata does not have a geometry column)
#' @param x variable from shareddata to be plotted on the horizontal axis
#' @param y1 variable from shareddata to be plotted on the vertical axis
#' for the first graph
#' @param y2 variable from shareddata to be plotted on the vertical axis
#' for the second graph
#' @param y3 variable from shareddata to be plotted on the vertical axis
#' for the third graph
#' @param y4 variable from shareddata to be plotted on the vertical axis
#' for the fourth graph
#' @param colorvar the variable in shareddata to which color should be mapped
#' @param maplabel an optional label for the map points
#' @param colorpalette either a single color (e.g., "red") or one of
#' "viridis", "inferno", "magma", or "plasma"
#' @param fillOpacity the opacity of the fill (0 to 1)
#' @param xlabel the label for the variable on the horizontal axis
#' @param y1label the label for the variable on the vertical axis
#' for the first graph
#' @param y2label the label for the variable on the vertical axis
#' for the second graph
#' @param y3label the label for the variable on the vertical axis
#' for the third graph
#' @param y4label the label for the variable on the vertical axis
#' for the fourth graph
#' @param showlegend show the plot legend (TRUE) or not (FALSE)
#' @param legendtitle the title for the plot legend
#' @param plottitle the title for the plot map
#' @param spacing value between 0 and 1 for the space between the graphs
#' @returns linked plot map
#' @importFrom crosstalk SharedData bscols
#' @importFrom dplyr pull ungroup select
#' @importFrom rlang enquo eval_tidy quo_squash quo
#' @importFrom leaflet colorFactor derivePoints colorNumeric leaflet
#' addTiles addCircleMarkers
#' @importFrom viridisLite viridis
#' @importFrom ggplot2 ggplot geom_point theme_bw scale_fill_viridis_c
#' scale_fill_viridis_d ylab xlab labs
#' @importFrom sf st_drop_geometry
#' @importFrom plotly ggplotly layout highlight hide_guides subplot style
#' @importFrom htmltools h1
#' @export
#' @examples
#' # example code
#' library(crosstalk)
#' library(dplyr)
#' data(nds_data)
#' nds_sf7 <- nds_data |>
#'   filter(drive==7) |>
#'   sf::st_as_sf(coords = c("gps_long", "gps_lat"), crs = "WGS84")
#' nds_sf7_sd <- SharedData$new(nds_sf7)
#'
#' driveplot(shareddata = nds_sf7_sd, #lng = gps_long, lat = gps_lat,
#'           x = time_cst, y1 = speed_mph, y2 = gyro_heading,
#'           y3 = gps_heading, colorvar = gyro_heading,
#'           maplabel = time_cst, colorpalette = "viridis", fillOpacity = 1,
#'           xlabel = "Time", y1label = "Speed (mph)",
#'           y2label = "Gyro Heading (degrees)",
#'           y3label = "GPS Heading (degrees)")
driveplot <- function(shareddata, lng = NULL, lat = NULL,
                      x, y1, y2 = NULL, y3 = NULL, y4 = NULL, colorvar = NULL,
                      maplabel = NA, colorpalette = "#03F", fillOpacity = 1,
                      xlabel = NULL, y1label = NULL, y2label = NULL,
                      y3label = NULL, y4label = NULL,
                      showlegend = TRUE, legendtitle = NULL,
                      plottitle = NULL, spacing = 0.05){
  notitle <- is.null(plottitle)

  if(isTRUE(notitle)){
    height <- "98vh"
  }
  else{
    height <- "85vh" # Leave more space for the title
  }
  plot_map <- driveplot_map(shareddata = shareddata, lng = {{ lng }},
                            lat = {{ lat }}, colorvar = {{ colorvar }},
                            label = {{ maplabel }},
                            colorpalette = colorpalette,
                            fillOpacity = fillOpacity, mapheight = height)

  plot_graphs <- driveplot_companions(shareddata = shareddata, x = {{ x }},
                                      y1 = {{ y1 }}, y2 = {{ y2 }},
                                      y3 = {{ y3 }},y4 = {{ y4 }},
                                      colorvar = {{ colorvar }},
                                      xlabel = xlabel, y1label = y1label,
                                      y2label = y2label, y3label = y3label,
                                      y4label = y4label,
                                      colorpalette = colorpalette,
                                      showlegend = showlegend,
                                      legendtitle = legendtitle,
                                      spacing = spacing, plotheight = height)

  if(isTRUE(notitle)){
    final_viz <- bscols(plot_map, plot_graphs, widths = c(6, 6))
  }
  else{
    # suppressWarnings so we don't see bscols complain that
    # the sum of the widths is greater than 12
    final_viz <- suppressWarnings(
      bscols(
        h1(
          plottitle,
          .noWS = c("before", "after", "outside", "after-begin", "before-end"),
          align = "center", style="font-family: sans-serif; color:black"),
        plot_map, plot_graphs, widths = c(12, 6, 6)
        )
      )
  }
  return(final_viz)
}
