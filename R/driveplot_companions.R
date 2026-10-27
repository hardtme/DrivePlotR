#' Make a stack of companion graphs from shared drive data
#'
#' @param shareddata A SharedData object containing observations to be plotted.
#' @param x The bare (unquoted) column from shareddata to be plotted
#'   on the horizontal axis.
#' @param ys A vector or list of bare (unquoted) columns from shareddata to be
#'   plotted on the vertical axes of the companion graphs.
#' @param colorvar The bare (unquoted) column in shareddata to which color
#'   should be mapped.
#' @param group The bare (unquoted) column from shareddata for the grouping
#'   variable.
#' @param xlabel The label for the variable on the horizontal axis.
#' @param ylabels A vector or list of labels for the vertical axes
#'   of the companion graphs. If provided, it should be the same length as ys.
#' @param colorpalette The color palette for the plot; either a single
#'   color (e.g., "red") or one of the viridis color palettes compatible
#'   with leaflet. Run `viridis_color_palettes()` to see the available options
#'   for viridis color palettes. If `colorvar` and `colorpalette` are both not
#'   provided, `colorpalette` defaults to blue. If `colorvar` is provided but
#'   `colorpalette` is not, `colorpalette` defaults to the color palette
#'   "viridis".
#' @param showlegend Show the plot legend (TRUE) or not (FALSE).
#' @param legendtitle The title for the plot legend.
#' @param spacing A value between 0 and 1 for the space between the
#'   companion graphs.
#' @param plotheight The height of the stack of companion graphs, e.g., "100vh"
#'   or "400px".
#' @returns A stack of plotly scatterplots.
#' @importFrom crosstalk SharedData is.SharedData
#' @importFrom rlang enquo as_label quo_is_call call_args quo_get_expr is_call
#'   quo_is_missing
#' @importFrom plotly layout subplot style
#' @export
#' @examples
#' library(crosstalk)
#' data(drive7)
#' shared_drive <- SharedData$new(drive7)
#'
#' # Linked time series of speed, headings (in GPS and gyro), and GPS quality
#' driveplot_companions(
#'   shareddata = shared_drive,
#'   x = time_cst,
#'   ys = c(speed_mph, gyro_heading, gps_heading),
#'   colorvar = gps_pdop,
#'   xlabel = "Time",
#'   ylabels = c(
#'     "Speed (mph)", "Gyro Heading (degrees)",
#'     "GPS Heading (degrees)"
#'   ),
#'   colorpalette = "viridis",
#'   legendtitle = "GPS PDOP"
#' )
#'
driveplot_companions <- function(shareddata,
                                 x,
                                 ys,
                                 colorvar = NULL,
                                 group = NULL,
                                 xlabel = NULL,
                                 ylabels = NULL,
                                 colorpalette = NULL,
                                 showlegend = TRUE,
                                 legendtitle = NULL,
                                 spacing = 0.05,
                                 plotheight = "100vh") {
  shareddata <- convert_to_SharedData(shareddata)

  quoys <- enquo(ys)
  if(quo_is_missing(quoys)) {
    stop("argument is missing, with no default",
         call. = FALSE)
  } else {
    exprys <- quo_get_expr(quoys)
  }
  # Check for values of ys like c(y) * 2 or list(y) * 2
  if (is_call(exprys) && is_call(exprys[[2]], name = c("c", "list"))) {
    stop("Transformations must occur inside `c()` or `list()`,
          e.g., `c(y * 2)` or `list(y * 2)`, not `c(y) * 2` or `list(y) * 2`.",
         call. = FALSE)
    }
  # Allow different syntax for a single y variable
  # E.g., ys = c(speed_mph) and ys = speed_mph both work
  if (quo_is_call(quoys, name = c("c", "list"))) {
    yslist <- call_args(quoys)
  } else {
    yslist <- list(exprys)
  }

  ogdata <- shareddata$origData()
  ylength <- length(yslist)
  quocolor <- enquo(colorvar)

  ylabels <- check_ylabels(ylabels = ylabels, ylength = ylength)

  tidy_color <- eval_tidy(quocolor, data = ogdata)
  if (is.null(tidy_color)) {
    colorvarnumeric <- NULL
  } else {
    colorvarnumeric <- if (is.numeric(tidy_color)) TRUE else FALSE
  }

  if (isFALSE(colorvarnumeric)) {
    ncolors <- tidy_color |>
      n_distinct(na.rm = TRUE)
  }

  companion_graphs <- mapply(
    FUN = function(var, label) {
      driveplot_companion(
        shareddata = shareddata,
        x = {{ x }},
        y = {{ var }},
        group = {{ group }},
        colorvar = {{ colorvar }},
        colorpalette = colorpalette,
        xlabel = xlabel,
        ylabel = label,
        showlegend = showlegend,
        legendtitle = legendtitle
      )
    },
    yslist, ylabels,
    SIMPLIFY = FALSE
  )

  if (isTRUE(showlegend)) {
    plotlysubplot <- subplot(companion_graphs,
      nrows = ylength,
      shareX = TRUE,
      titleY = TRUE,
      which_layout = 1,
      margin = spacing
    )
  } else {
    plotlysubplot <- subplot(companion_graphs,
      nrows = ylength,
      shareX = TRUE,
      titleY = TRUE,
      margin = spacing
    )
  }

  if (isFALSE(colorvarnumeric) && isTRUE(showlegend) && ylength > 1) {
    if (any(is.na(tidy_color))) {
      ncolors <- ncolors + 1
    }
    tracesperplot <- 1 + ncolors # (line + colors)
    plotlysubplot <- plotlysubplot |>
      style(showlegend = FALSE,
            traces = c(1, (tracesperplot + 1):(tracesperplot * ylength)))
  }
  # Identify the traces that are not points
  non_point_traces <- which(sapply(plotlysubplot$x$data, function(trace) {
    is.null(trace$mode) || !grepl("markers", trace$mode)
  }))
  # Keep non-point traces out of the hover box
  if (length(non_point_traces > 0)){
  plotlysubplot <- plotlysubplot |>
    style(hoverinfo = "skip", traces = non_point_traces)
  }

  plotlysubplot <- plotlysubplot |>
    layout(font = list(family = "sans-serif"),
           hovermode = "x unified",
           hoverlabel = list(
             bgcolor = 'rgba(255, 255, 255, 0.75)',
             bordercolor = "black"))
  plotlysubplot$sizingPolicy$defaultHeight <- plotheight
  plotlysubplot$sizingPolicy$defaultWidth <- "100%"
  plotlysubplot
}
