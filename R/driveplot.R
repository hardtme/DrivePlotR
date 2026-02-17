#' Create a vehicle trajectory plot map
#'
#' @param shareddata A SharedData object containing observations to be plotted.
#' @param lng The column of shareddata containing longitude
#'   (only required if shareddata does not have a geometry column).
#' @param lat The column of shareddata containing latitude
#'   (only required if shareddata does not have a geometry column)
#' @param x The variable from shareddata to be plotted on the horizontal axis.
#' @param y1 The variable from shareddata to be plotted on the vertical axis
#'   for the first graph.
#' @param y2 The variable from shareddata to be plotted on the vertical axis
#'   for the second graph.
#' @param y3 The variable from shareddata to be plotted on the vertical axis
#'   for the third graph.
#' @param y4 The variable from shareddata to be plotted on the vertical axis
#'   for the fourth graph.
#' @param colorvar The variable in shareddata to which color should be mapped.
#' @param maplabel An optional label for the map points.
#' @param colorpalette The color palette for the plot map; either a single
#'   color (e.g., "red") or one of "viridis", "inferno", "magma", or "plasma".
#' @param fillOpacity The opacity of the fill of the map points (0 to 1).
#' @param xlabel The label for the variable on the horizontal axis.
#' @param y1label The label for the variable on the vertical axis
#'   for the first companion graph.
#' @param y2label The label for the variable on the vertical axis
#'   for the second companion graph.
#' @param y3label The label for the variable on the vertical axis
#'   for the third companion graph.
#' @param y4label The label for the variable on the vertical axis
#'   for the fourth companion graph.
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
#'           y1 = speed_mph,
#'           y2 = gyro_heading,
#'           y3 = gps_heading,
#'           colorvar = gyro_heading,
#'           maplabel = time_cst,
#'           colorpalette = "viridis",
#'           fillOpacity = 1,
#'           xlabel = "Time",
#'           y1label = "Speed (mph)",
#'           y2label = "Gyro Heading (degrees)",
#'           y3label = "GPS Heading (degrees)")
driveplot <- function(shareddata,
                      lng = NULL,
                      lat = NULL,
                      x,
                      y1,
                      y2 = NULL,
                      y3 = NULL,
                      y4 = NULL,
                      colorvar = NULL,
                      maplabel = NA,
                      colorpalette = "#03F",
                      fillOpacity = 1,
                      xlabel = NULL,
                      y1label = NULL,
                      y2label = NULL,
                      y3label = NULL,
                      y4label = NULL,
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
                                      y1 = {{ y1 }},
                                      y2 = {{ y2 }},
                                      y3 = {{ y3 }},
                                      y4 = {{ y4 }},
                                      colorvar = {{ colorvar }},
                                      xlabel = xlabel,
                                      y1label = y1label,
                                      y2label = y2label,
                                      y3label = y3label,
                                      y4label = y4label,
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
