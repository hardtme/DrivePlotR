#' Make a stack of companion graphs from shared drive data
#'
#' @param shareddata A SharedData object containing observations to be plotted.
#' @param x The column from shareddata to be plotted on the horizontal axis.
#' @param ys A vector or list of 1 to 4 column from shareddata to be
#'   plotted on the vertical axes of the companion graphs.
#' @param colorvar The column in shareddata to which color should be mapped.
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
#' @importFrom rlang enexpr enquo as_label ensym
#' @importFrom plotly layout subplot style
#' @importFrom purrr map map2 list_c
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
driveplot_companions <- function(shareddata,
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

  if (isFALSE(is.SharedData(shareddata))) {
    stop("`shareddata` must be a SharedData object.", call. = FALSE)
  }

  yexpr <- as.list(enexpr(ys))
  # Allow different syntax for a single y variable
  # E.g., ys = c(speed_mph) and ys = speed_mph both work
  yexpr <- if(length(yexpr) == 1) yexpr else yexpr[-1]

  # Get original data from shareddata so we can check column existence and type
  # We can't directly access columns in a SharedData object
  ogdata <- shareddata$origData()
  columns <- colnames(ogdata)
  xname <- as_label(enquo(x))
  xsym <- ensym(xname)
  ylength <- length(yexpr)
  ynames <- yexpr |>
    map(\(y) as_label(y)) |>
    list_c()

  if (ylength > 4) {
    warning("4+ columns were passed in `ys`, so graphs may be compressed.",
            call. = FALSE)
  }

  if (!(xname %in% columns)) {
    stop(paste0("Can't find column `", xname, "` in `shareddata`."),
         call. = FALSE)
  } else if (length(setdiff(ynames, columns)) > 0) {
    missing_ys <- paste0("`", setdiff(ynames, columns), "`", collapse = ", ")
    stop(paste0("Can't find ", missing_ys, " in `shareddata`."),
         call. = FALSE)
  }

  if (any(is.na(ylabels))) {
    stop("`ylabels` cannot be NA.",
         call. = FALSE)
  }

  if (!is.null(ylabels) && length(ylabels) != ylength) {
    stop("If providing `ylabels`, `ys` and `ylabels` must be the same length.",
         call. = FALSE)
  }

  if (is.null(ylabels)) {
    ylabels <- vector(mode = "list", length = ylength)
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

  companion_graphs <- map2(
    .x = yexpr, .y = ylabels,
    .f = \(x, y) driveplot_companion(
      shareddata = shareddata,
      x = {{ xsym }},
      y = {{ x }},
      colorvar = {{ colorvar }},
      colorpalette = colorpalette,
      xlabel = xlabel,
      ylabel = y,
      showlegend = showlegend,
      legendtitle = legendtitle
    )
  )

  if (isTRUE(showlegend)) {
    plotlysubplot <- subplot(companion_graphs,
                             nrows = ylength,
                             shareX = TRUE,
                             titleY = TRUE,
                             which_layout = 1,
                             margin = spacing)
  } else{
    plotlysubplot <- subplot(companion_graphs,
                             nrows = ylength,
                             shareX = TRUE,
                             titleY = TRUE,
                             margin = spacing)
  }
  if (isFALSE(colorvarnumeric) && isTRUE(showlegend)) {
    plotlysubplot <- plotlysubplot |>
      style(showlegend = FALSE, traces = (ncolors + 1):(ncolors*ylength))
  }
  plotlysubplot <- plotlysubplot |>
    layout(font = list(family = "sans-serif"))
  plotlysubplot$sizingPolicy$defaultHeight <- plotheight
  plotlysubplot$sizingPolicy$defaultWidth <- "100%"
  return(plotlysubplot)
}
