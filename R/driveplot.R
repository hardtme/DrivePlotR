#' Create a vehicle trajectory plot map
#'
#' @param shareddata A SharedData object containing observations to be plotted.
#' @param lng The column of shareddata containing longitude
#'   (only required if shareddata does not have a geometry column).
#' @param lat The column of shareddata containing latitude
#'   (only required if shareddata does not have a geometry column)
#' @param x The variable from shareddata to be plotted on the horizontal axis.
#' @param ys A vector or list of 1 to 4 variables from shareddata to be
#'   plotted on the vertical axes of the companion graphs.
#' @param colorvar The variable in shareddata to which color should be mapped.
#' @param maplabel An optional label for the map points.
#' @param colorpalette The color palette for the plot map; either a single
#'   color (e.g., "red") or one of "viridis", "inferno", "magma", or "plasma".
#' @param fillOpacity The opacity of the fill of the map points (0 to 1).
#' @param xlabel The label for the variable on the horizontal axis.
#' @param ylabels A vector or list of 1 to 4 labels for the vertical axes
#'   of the companion graphs.
#' @param showlegend Show the plot legend (TRUE) or not (FALSE).
#' @param legendtitle The title for the plot legend.
#' @param plottitle The title for the plot map.
#' @param spacing A value between 0 and 1 for the space between the
#'   companion graphs.
#' @returns A linked plot map.
#' @importFrom crosstalk SharedData is.SharedData bscols
#' @importFrom htmltools h1
#' @export
#' @examples
#' library(crosstalk)
#' data(drive7)
#' shared_drive <- SharedData$new(drive7)
#'
#' driveplot(shareddata = shared_drive,
#'           x = time_cst,
#'           ys = c(speed_mph, gyro_heading, gps_heading),
#'           colorvar = gyro_heading,
#'           maplabel = time_cst,
#'           colorpalette = "viridis",
#'           fillOpacity = 1,
#'           xlabel = "Time",
#'           ylabels = c("Speed (mph)", "Gyro Heading (degrees)",
#'                       "GPS Heading (degrees)"))
driveplot <- function(shareddata,
                      lng = NULL,
                      lat = NULL,
                      x,
                      ys,
                      colorvar = NULL,
                      maplabel = NA,
                      colorpalette = "#03F",
                      fillOpacity = 1,
                      xlabel = NULL,
                      ylabels = NULL,
                      showlegend = TRUE,
                      legendtitle = NULL,
                      plottitle = NULL,
                      spacing = 0.05) {

  if (isFALSE(is.SharedData(shareddata))) {
    stop("`shareddata` must be a SharedData object.", call. = FALSE)
  }

  notitle <- is.null(plottitle)

  if (isTRUE(notitle)) {
    height <- "98vh"
  }
  else{
    height <- "85vh" # Leave more space for the title
  }
  plot_map <- driveplot_map(shareddata = shareddata,
                            lng = {{ lng }},
                            lat = {{ lat }},
                            colorvar = {{ colorvar }},
                            label = {{ maplabel }},
                            colorpalette = colorpalette,
                            fillOpacity = fillOpacity,
                            mapheight = height)

  plot_graphs <- driveplot_companions(shareddata = shareddata,
                                      x = {{ x }},
                                      ys = {{ ys }},
                                      colorvar = {{ colorvar }},
                                      xlabel = xlabel,
                                      ylabels = ylabels,
                                      colorpalette = colorpalette,
                                      showlegend = showlegend,
                                      legendtitle = legendtitle,
                                      spacing = spacing,
                                      plotheight = height)

  if (isTRUE(notitle)) {
    final_viz <- bscols(plot_map, plot_graphs, widths = c(6, 6))
  }
  else{
    # suppressWarnings so we don't see bscols warn that
    # the sum of the widths is greater than 12
    # We need this behavior to put the title on its own line
    final_viz <- suppressWarnings(
      bscols(
        h1(plottitle,
           .noWS = c("before", "after", "outside", "after-begin", "before-end"),
           align = "center", style="font-family: sans-serif; color:black"),
        plot_map, plot_graphs, widths = c(12, 6, 6)
      )
    )
  }
  return(final_viz)
}
