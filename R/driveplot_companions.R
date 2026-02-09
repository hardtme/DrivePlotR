#' Make a stack of companion graphs from shared drive data
#'
#' @param shareddata A SharedData object containing observations to be plotted.
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
#' @param xlabel The label for the variable on the horizontal axis.
#' @param y1label The label for the variable on the vertical axis
#'   for the first graph.
#' @param y2label The label for the variable on the vertical axis
#'   for the second graph.
#' @param y3label The label for the variable on the vertical axis
#'   for the third graph.
#' @param y4label The label for the variable on the vertical axis
#'   for the fourth graph.
#' @param colorpalette The color palette for the plot map; either a single
#'   color (e.g., "red") or one of "viridis", "inferno", "magma", or "plasma".
#' @param showlegend Show the plot legend (TRUE) or not (FALSE).
#' @param legendtitle The title for the plot legend.
#' @param spacing A value between 0 and 1 for the space between the
#'   companion graphs.
#' @param plotheight The height of the stack of companion graphs in CSS units,
#'   e.g, "98vh"
#' @returns A stack of plotly scatterplots.
#' @importFrom crosstalk SharedData is.SharedData
#' @importFrom dplyr pull ungroup select
#' @importFrom rlang enquo eval_tidy quo_squash quo
#' @importFrom leaflet colorFactor derivePoints colorNumeric leaflet
#' addTiles addCircleMarkers
#' @importFrom viridisLite viridis
#' @importFrom ggplot2 ggplot geom_point theme_bw scale_fill_viridis_c
#' scale_fill_viridis_d ylab xlab labs
#' @importFrom sf st_drop_geometry
#' @importFrom plotly ggplotly layout highlight hide_guides subplot style
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
#' # Linked time series of speed, headings (in GPS and gyro), and GPS quality
#' driveplot_companions(
#'      shareddata = nds_sf7_sd,
#'      x = time_cst,
#'      y1 = speed_mph,
#'      y2 = gyro_heading,
#'      y3 = gps_heading,
#'      colorvar = gps_pdop,
#'      xlabel = "Time",
#'      y1label = "Speed (mph)",
#'      y2label = "Gyro Heading (degrees)",
#'      y3label = "GPS Heading (degrees)",
#'      colorpalette = "viridis")
#'
driveplot_companions <- function(shareddata,
                                 x,
                                 y1,
                                 y2 = NULL,
                                 y3 = NULL,
                                 y4 = NULL,
                                 colorvar = NULL,
                                 xlabel = NULL,
                                 y1label = NULL,
                                 y2label = NULL,
                                 y3label = NULL,
                                 y4label = NULL,
                                 colorpalette = "#03F",
                                 showlegend = TRUE,
                                 legendtitle = NULL,
                                 spacing = 0.05,
                                 plotheight = "98vh") {

  if (isFALSE(crosstalk::is.SharedData(shareddata))) {
    stop("`shareddata` must be a SharedData object.")
  }
  # Get original data from shareddata so we can check column existence and type
  # We can't directly access columns in a SharedData object
  ogdata <- shareddata$origData()
  y2check <- tryCatch(
    ogdata |>
      pull({{ y2 }}),
    error = function(e){},
    finally = NULL
  )
  y3check <- tryCatch(
    ogdata |>
      pull({{ y3 }}),
    error = function(e){},
    finally = NULL
  )
  y4check <- tryCatch(
    ogdata |>
      pull({{ y4 }}),
    error = function(e){},
    finally = NULL
  )

  colorvarnumeric <- tryCatch(
    ogdata |>
      pull({{ colorvar }}) |>
      is.numeric(),
    error = function(e){},
    finally = NULL
  )
  # colovarnumeric = NULL if {{ colorvar }} isn't a column in ogdata
  # colorvarnumeric = TRUE if {{ colorvar }} is a numeric column in ogdata
  # colorvarnumeric = FALSE if {{ colorvar }} is not a numeric column in ogdata
  if (isFALSE(colorvarnumeric)) {
    ncolors <- ogdata |> pull({{ colorvar }}) |> unique() |> length()
  }
  if (is.null(y2check) && is.null(y3check) && is.null(y4check)) {
    plot1 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y1 }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = y1label,
                                 showlegend = showlegend,
                                 legendtitle = legendtitle)
    plot1 <- plot1 |>
      layout(font = list(family = "sans-serif"))
    plot1$sizingPolicy$defaultHeight <- plotheight
    plot1$sizingPolicy$defaultWidth <- "100%"
    return(plot1)
  }else if (!is.null(y2check) && is.null(y3check) && is.null(y4check)) {
    plot1 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y1 }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = y1label,
                                 showlegend = showlegend,
                                 legendtitle = legendtitle)
    plot2 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y2 }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = y2label,
                                 showlegend = FALSE,
                                 legendtitle = NULL)
    if (isTRUE(showlegend)) {
      plotlysubplot <- subplot(plot1, plot2, nrows = 2, shareX = TRUE,
                               titleY = TRUE, which_layout = 1, margin = spacing)
    }
    else{
      plotlysubplot <- subplot(plot1, plot2, nrows = 2, shareX = TRUE,
                               titleY = TRUE, margin = spacing)
    }
    if (isFALSE(colorvarnumeric) && isTRUE(showlegend)) {
      nplots <- 2
      plotlysubplot <- plotlysubplot |>
        style(showlegend = FALSE, traces = (ncolors + 1):(ncolors*nplots))
    }
    plotlysubplot <- plotlysubplot |>
      layout(font = list(family = "sans-serif"))
    plotlysubplot$sizingPolicy$defaultHeight <- plotheight
    plotlysubplot$sizingPolicy$defaultWidth <- "100%"
    return(plotlysubplot)
  }else if (!is.null(y2check) && !is.null(y3check) && is.null(y4check)) {
    plot1 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y1 }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = y1label,
                                 showlegend = showlegend,
                                 legendtitle = legendtitle)
    plot2 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y2 }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = y2label,
                                 showlegend = FALSE,
                                 legendtitle = NULL)
    plot3 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y3 }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = y3label,
                                 showlegend = FALSE,
                                 legendtitle = NULL)
    if (isTRUE(showlegend)) {
      plotlysubplot <- subplot(plot1,
                               plot2,
                               plot3,
                               nrows = 3,
                               shareX = TRUE,
                               titleY = TRUE,
                               which_layout = 1,
                               margin = spacing)
    }
    else{
      plotlysubplot <- subplot(plot1,
                               plot2,
                               plot3,
                               nrows = 3,
                               shareX = TRUE,
                               titleY = TRUE,
                               margin = spacing)
    }
    if (isFALSE(colorvarnumeric) && isTRUE(showlegend)) {
      nplots <- 3
      plotlysubplot <- plotlysubplot |>
        style(showlegend = FALSE, traces = (ncolors + 1):(ncolors*nplots))
    }
    plotlysubplot <- plotlysubplot |>
      layout(font = list(family = "sans-serif"))
    plotlysubplot$sizingPolicy$defaultHeight <- plotheight
    plotlysubplot$sizingPolicy$defaultWidth <- "100%"
    return(plotlysubplot)
  }else if (!is.null(y2check) && !is.null(y3check) && !is.null(y4check)) {
    plot1 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y1 }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = y1label,
                                 showlegend = showlegend,
                                 legendtitle = legendtitle)
    plot2 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y2 }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = y2label,
                                 showlegend = FALSE,
                                 legendtitle = NULL)
    plot3 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y3 }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = y3label,
                                 showlegend = FALSE,
                                 legendtitle = NULL)
    plot4 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y4 }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = y4label,
                                 showlegend = FALSE,
                                 legendtitle = NULL)
    if (isTRUE(showlegend)) {
      plotlysubplot <- subplot(plot1,
                               plot2,
                               plot3,
                               plot4,
                               nrows = 4,
                               shareX = TRUE,
                               titleY = TRUE,
                               which_layout = 1,
                               margin = spacing)
    }
    else{
      plotlysubplot <- subplot(plot1,
                               plot2,
                               plot3,
                               plot4,
                               nrows = 4,
                               shareX = TRUE,
                               titleY = TRUE,
                               margin = spacing)
    }
    if (isFALSE(colorvarnumeric) && isTRUE(showlegend)) {
      nplots <- 4
      plotlysubplot <- plotlysubplot |>
        style(showlegend = FALSE, traces = (ncolors + 1):(ncolors*nplots))
    }
    plotlysubplot <- plotlysubplot |>
      layout(font = list(family = "sans-serif"))
    plotlysubplot$sizingPolicy$defaultHeight <- plotheight
    plotlysubplot$sizingPolicy$defaultWidth <- "100%"
    return(plotlysubplot)
  }
}
