#' Create a vehicle trajectory plot map
#'
#' @param shareddata A SharedData object containing observations to be plotted.
#' @param lng The bare (unquoted) column of shareddata containing longitude
#'   (only required if shareddata does not have a geometry column).
#' @param lat The bare (unquoted) column of shareddata containing latitude
#'   (only required if shareddata does not have a geometry column)
#' @param x The bare (unquoted) column from shareddata to be plotted
#'   on the horizontal axis.
#' @param ys A vector or list of bare (unquoted) columns from shareddata to be
#'   plotted on the vertical axes of the companion graphs.
#' @param colorvar The bare (unquoted) column in shareddata to which color
#'   should be mapped.
#' @param maplabel An optional label for the map points.
#' @param colorpalette The color palette for the plot; either a single
#'   color (e.g., "red") or one of the viridis color palettes compatible
#'   with leaflet. Run `leaflet_color_palettes()` to see the available options
#'   for viridis color palettes. If `colorvar` and `colorpalette` are both not
#'   provided, `colorpalette` defaults to blue. If `colorvar` is provided but
#'   `colorpalette` is not, `colorpalette` defaults to the color palette
#'   "viridis".
#' @param fillopacity The opacity of the fill of the map points (0 to 1).
#' @param xlabel The label for the variable on the horizontal axis.
#' @param ylabels A vector or list of labels for the vertical axes
#'   of the companion graphs. If provided, it should be the same length as ys.
#' @param showlegend Show the plot legend (TRUE) or not (FALSE).
#' @param legendtitle The title for the plot legend.
#' @param plottitle The title for the plot map.
#' @param spacing A value between 0 and 1 for the space between the
#'   companion graphs.
#' @param width The width of the plot map, provided as a string with one of the
#'   following units: %, vh, vw, or px (e.g., "100%" or "400px")
#' @param height The height of the plot map, provided as a string with one of
#'   the following units: %, vh, vw, or px (e.g., "100%" or "400px")
#' @returns A linked plot map.
#' @importFrom crosstalk SharedData is.SharedData
#' @importFrom htmltools tags HTML browsable
#' @export
#' @examples
#' library(crosstalk)
#' data(drive7)
#' shared_drive <- SharedData$new(drive7)
#'
#' driveplot(
#'   shareddata = shared_drive,
#'   x = time_cst,
#'   ys = c(speed_mph, gyro_heading, gps_heading),
#'   colorvar = gyro_heading,
#'   maplabel = time_cst,
#'   colorpalette = "viridis",
#'   fillopacity = 1,
#'   xlabel = "Time",
#'   ylabels = c(
#'     "Speed (mph)", "Gyro Heading (degrees)",
#'     "GPS Heading (degrees)"
#'   ),
#'   showlegend = TRUE,
#'   legendtitle = "Gyro Heading",
#'   plottitle = "A drive with points colored by gyro heading",
#'   height = "500px"
#' )
driveplot <- function(shareddata,
                      lng = NULL,
                      lat = NULL,
                      x,
                      ys,
                      colorvar = NULL,
                      maplabel = NA,
                      colorpalette = NULL,
                      fillopacity = 1,
                      xlabel = NULL,
                      ylabels = NULL,
                      showlegend = TRUE,
                      legendtitle = NULL,
                      plottitle = NULL,
                      spacing = 0.05,
                      width = "100%",
                      height = "100vh") {
  shareddata <- convert_to_SharedData(shareddata)

  if (is.null(plottitle)) {
    plotmap_title <- NULL
  } else {
    plotmap_title <- tags$h2(
      plottitle,
      style = "text-align: center; margin-top: 0px; margin-bottom: 20px;
      font-family: Arial, sans-serif; flex-shrink: 0;"
    )
  }

  if (is.numeric(width) || is.numeric(height)) {
    stop(
      'Arguments `width` and `height` must be provided as strings,
      e.g., "100%" or "400px".',
      call. = FALSE
    )
  }

  if (isFALSE(grepl("%|vh|vw|px", width)) ||
    isFALSE(grepl("%|vh|vw|px", height))) {
    stop("Arguments `width` and `height` must be provided as strings
          with one of the following units: %, vh, vw, or px.",
      call. = FALSE
    )
  }

  plot_map <- driveplot_map(
    shareddata = shareddata,
    lng = {{ lng }},
    lat = {{ lat }},
    colorvar = {{ colorvar }},
    label = {{ maplabel }},
    colorpalette = colorpalette,
    fillopacity = fillopacity
  )

  plot_graphs <- driveplot_companions(
    shareddata = shareddata,
    x = {{ x }},
    ys = {{ ys }},
    colorvar = {{ colorvar }},
    xlabel = xlabel,
    ylabels = ylabels,
    colorpalette = colorpalette,
    showlegend = showlegend,
    legendtitle = legendtitle,
    spacing = spacing
  )

  # Place the map and companion graphs side by side using flexbox,
  # not bscols()
  side_by_side <- tags$div(
    style = "display: flex; flex-direction: row; flex-grow: 1;
    min-height: 0; width: 100%;",

    # Map
    tags$div(
      style = "flex: 1; position: relative; margin-right: 10px;",
      tags$div(style = "position: absolute; top: 0; bottom: 0;
               left: 0; right: 0;", plot_map)
    ),

    # Companion Graphs
    tags$div(
      style = "flex: 1; position: relative; margin-left: 10px;",
      tags$div(style = "position: absolute; top: 0; bottom: 0;
               left: 0; right: 0;", plot_graphs)
    )
  )

  # CSS styling
  css_styling <- tags$style(HTML("
    .plotmap-container .html-widget {
      height: 100% !important; width: 100% !important;
    }
    html, body { margin: 0; padding: 0; width: 100%; height: 100%; }
  "))

  # Main container for the plot map
  container <- tags$div(
    class = "plotmap-container",
    style = sprintf("width: %s; height: %s; display: flex;
                    flex-direction: column; box-sizing: border-box;
                    overflow: hidden; padding: 10px;", width, height),
    css_styling,
    plotmap_title,
    side_by_side
  )

  final_plotmap <- browsable(container)

  final_plotmap
}
