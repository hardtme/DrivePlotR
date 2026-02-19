#' Make a stack of companion graphs from shared drive data
#'
#' @param shareddata A SharedData object containing observations to be plotted.
#' @param x The variable from shareddata to be plotted on the horizontal axis.
#' @param ys A vector or list of 1 to 4 variables from shareddata to be
#'   plotted on the vertical axes of the companion graphs.
#' @param colorvar The variable in shareddata to which color should be mapped.
#' @param xlabel The label for the variable on the horizontal axis.
#' @param ylabels A vector or list of 1 to 4 labels for the vertical axes
#'   of the companion graphs.
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
#' @importFrom dplyr pull
#' @importFrom rlang enquo quo_squash as_label sym
#' @importFrom plotly layout subplot style
#' @export
#' @examples
#' library(crosstalk)
#' data(drive7)
#' shared_drive <- SharedData$new(drive7)
#'
#' # Linked time series of speed, headings (in GPS and gyro), and GPS quality
#' driveplot_companions(
#'      shareddata = shared_drive,
#'      x = time_cst,
#'      ys = c(speed_mph, gyro_heading, gps_heading),
#'      colorvar = gps_pdop,
#'      xlabel = "Time",
#'      ylabels = c("Speed (mph)", "Gyro Heading (degrees)",
#'                  "GPS Heading (degrees)"),
#'      colorpalette = "viridis")
#'
driveplot_companions<- function(shareddata,
                                x,
                                ys,
                                colorvar = NULL,
                                xlabel = NULL,
                                ylabels = NULL,
                                colorpalette = "#03F",
                                showlegend = TRUE,
                                legendtitle = NULL,
                                spacing = 0.05,
                                plotheight = "98vh") {
  #browser()
  if (isFALSE(is.SharedData(shareddata))) {
    stop("`shareddata` must be a SharedData object.", call. = FALSE)
  }

  ysquo <- as.list(quo_squash(enquo(ys)))
  # Allow different syntax for a single y variable
  # E.g., ys = c(speed_mph) and ys = speed_mph both work
  ysquo <- if(length(ysquo) == 1) ysquo else ysquo[-1]

  if (length(ysquo) < 1 || length(ysquo) > 4) {
    stop("`ys` must be a vector or list with 1 to 4 `shareddata` column names.",
         call. = FALSE)
  }

  if ((!is.null(ylabels)) && (length(ylabels) < 1 || length(ylabels) > 4)) {
    stop("`ylabels` must be a vector or list of 1 to 4 character strings.",
         call. = FALSE)
  }

  # Get original data from shareddata so we can check column existence and type
  # We can't directly access columns in a SharedData object
  ogdata <- shareddata$origData()
  columns <- colnames(ogdata)
  xname <- as_label(enquo(x))
  yslength <- length(ysquo)
  if (yslength == 1) {
    y1name <- as_label(ysquo[[1]])
  } else if (yslength == 2) {
    y1name <- as_label(ysquo[[1]])
    y2name <- as_label(ysquo[[2]])
  } else if (yslength == 3) {
    y1name <- as_label(ysquo[[1]])
    y2name <- as_label(ysquo[[2]])
    y3name <- as_label(ysquo[[3]])
  } else {
    y1name <- as_label(ysquo[[1]])
    y2name <- as_label(ysquo[[2]])
    y3name <- as_label(ysquo[[3]])
    y4name <- as_label(ysquo[[4]])
  }

  if (!(xname %in% colnames(ogdata))) {
    stop(paste0("Can't find column `", xname, "` in `shareddata`."),
         call. = FALSE)
  } else if (!(y1name %in% colnames(ogdata))) {
    stop(paste0("Can't find column `", y1name, "` in `shareddata`."),
         call. = FALSE)
  }

  if (yslength >= 2 && !(y2name) %in% columns) {
    stop(paste0("Can't find column `", y2name, "` in `shareddata`."),
         call. = FALSE)
  } else if (yslength >= 3 && !(y3name) %in% columns) {
    stop(paste0("Can't find column `", y3name, "` in `shareddata`."),
         call. = FALSE)
  } else if (yslength == 4 && !(y4name) %in% columns) {
    stop(paste0("Can't find column `", y4name, "` in `shareddata`."),
         call. = FALSE)
  }

  if (!is.null(ylabels) && length(ylabels) != yslength) {
    stop("If providing `ylabels`, `ys` and `ylabels` must be the same length.",
         call. = FALSE)
  }

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
    ncolors <- ogdata |>
      pull({{ colorvar }}) |>
      unique() |>
      length()
  }

  if (yslength == 1) {
    y1sym <- sym(ysquo[[1]])
    plot1 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y1sym }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = ylabels[1],
                                 showlegend = showlegend,
                                 legendtitle = legendtitle)
    plot1 <- plot1 |>
      layout(font = list(family = "sans-serif"))
    plot1$sizingPolicy$defaultHeight <- plotheight
    plot1$sizingPolicy$defaultWidth <- "100%"
    return(plot1)
  } else if (yslength == 2) {
    y1sym <- sym(ysquo[[1]])
    y2sym <- sym(ysquo[[2]])
    plot1 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y1sym }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = ylabels[1],
                                 showlegend = showlegend,
                                 legendtitle = legendtitle)
    plot2 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y2sym }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = ylabels[2],
                                 showlegend = FALSE,
                                 legendtitle = NULL)
    if (isTRUE(showlegend)) {
      plotlysubplot <- subplot(plot1, plot2, nrows = 2, shareX = TRUE,
                               titleY = TRUE, which_layout = 1, margin = spacing)
    } else {
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
  } else if (yslength == 3) {
    y1sym <- sym(ysquo[[1]])
    y2sym <- sym(ysquo[[2]])
    y3sym <- sym(ysquo[[3]])
    plot1 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y1sym }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = ylabels[1],
                                 showlegend = showlegend,
                                 legendtitle = legendtitle)
    plot2 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y2sym }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = ylabels[2],
                                 showlegend = FALSE,
                                 legendtitle = NULL)
    plot3 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y3sym }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = ylabels[3],
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
    } else{
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
  } else if (yslength == 4) {
    y1sym <- sym(ysquo[[1]])
    y2sym <- sym(ysquo[[2]])
    y3sym <- sym(ysquo[[3]])
    y4sym <- sym(ysquo[[4]])
    plot1 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y1sym }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = ylabels[1],
                                 showlegend = showlegend,
                                 legendtitle = legendtitle)
    plot2 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y2sym }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = ylabels[2],
                                 showlegend = FALSE,
                                 legendtitle = NULL)
    plot3 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y3sym }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = ylabels[3],
                                 showlegend = FALSE,
                                 legendtitle = NULL)
    plot4 <- driveplot_companion(shareddata = shareddata,
                                 x = {{ x }},
                                 y = {{ y4sym }},
                                 colorvar = {{ colorvar }},
                                 colorpalette = colorpalette,
                                 xlabel = xlabel,
                                 ylabel = ylabels[4],
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
    } else {
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
