#' Make a single plotly scatter plot from shared drive data
#'
#' @importFrom crosstalk SharedData
#' @importFrom dplyr pull ungroup select
#' @importFrom rlang enquo eval_tidy quo_squash quo as_label
#' @importFrom leaflet colorFactor derivePoints colorNumeric leaflet addTiles
#'   addCircleMarkers
#' @importFrom viridisLite viridis
#' @importFrom ggplot2 ggplot geom_point theme_bw scale_fill_viridis_c
#'   scale_fill_viridis_d ylab xlab labs aes
#' @importFrom plotly ggplotly layout highlight hide_guides
#' @param shareddata A SharedData object containing observations to be plotted.
#' @param x The variable from shareddata to be plotted on the horizontal axis.
#' @param y The variable from shareddata to be plotted on the vertical axis.
#' @param colorvar The variable in shareddata to which color should be mapped.
#' @param colorpalette The color palette for the plot; either a single
#'   color (e.g., "red") or one of "viridis", "inferno", "magma", or "plasma".
#' @param xlabel The label for the variable on the horizontal axis.
#' @param ylabel The label for the variable on the vertical axis.
#' @param showlegend Show the plot legend (TRUE) or not (FALSE).
#' @param legendtitle The title for the plot legend.
#' @returns A plotly scatterplot.
#' @export
#' @examples
#' library(crosstalk)
#' data(drive7)
#' shared_drive <- SharedData$new(drive7)
#'
#' # Time series of speed
#' driveplot_companion(shareddata = shared_drive,
#'                     x = time_cst,
#'                     y = speed_mph)
#'
#' # Color points by direction of car
#' driveplot_companion(shareddata = shared_drive,
#'                     x = time_cst,
#'                     y = speed_mph,
#'                     colorvar = gyro_heading,
#'                     colorpalette = "viridis")
driveplot_companion <- function(shareddata,
                                x,
                                y,
                                colorvar = NULL,
                                colorpalette = "#03F",
                                xlabel = NULL,
                                ylabel = NULL,
                                showlegend = TRUE,
                                legendtitle = NULL) {

  if (isFALSE(crosstalk::is.SharedData(shareddata))) {
    stop("`shareddata` must be a SharedData object.", call. = FALSE)
  }

  # Get original data from shareddata so we can check the existence of
  # x, y, and colorvar along with the type of colorvar
  # We can't directly access columns in a SharedData object
  ogdata <- shareddata$origData()
  columns <- colnames(ogdata)
  xname <- as_label(enquo(x))
  yname <- as_label(enquo(y))
  colorvarname <- as_label(enquo(colorvar))

  if (!(xname %in% columns)) {
    stop(paste0("Can't find column `", xname, "` in `shareddata`."),
         call. = FALSE)
  } else if (!(yname %in% columns)) {
    stop(paste0("Can't find column `", yname, "` in `shareddata`."),
         call. = FALSE)
  } else if (colorvarname != "NULL" && !(colorvarname %in% columns)) {
    stop(paste0("Can't find column `", colorvarname, "` in `shareddata`."),
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

  # Use viridis color palettes allowed by leaflet:
  # "viridis", "magma", "inferno", or "plasma"
  if (is.null(colorvarnumeric)  &&
     !(colorpalette %in% c("viridis", "magma", "inferno", "plasma"))) {
    # {{ colorvar }} isn't a column in ogdata and
    # viridis palette isn't specified
    gg <- ggplot(data = shareddata) +
      geom_point(aes(x = {{ x }}, y = {{ y }}),
                 shape = 21,
                 stroke = 0.05,
                 fill = colorpalette,
                 color = "dimgray",
                 show.legend = showlegend) +
      theme_bw()
  }else if (!is.null(colorvarnumeric) &&
           !(colorpalette %in% c("viridis", "magma", "inferno", "plasma"))) {
    # {{ colorvar }} is a column in ogdata and
    # a viridis palette isn't specified
    # Throw an error if a color variable is specified, but not a color palette
    stop('When specifying colorvar, please use
         colorpalette = "viridis", "magma", "inferno", or "plasma".',
         call. = FALSE)
  }else if (is.null(colorvarnumeric) &&
           colorpalette %in% c("viridis", "magma", "inferno", "plasma")) {
    # {{ colorvar }} is not a column in ogdata and
    # a viridis palette is specified
    # Make plot using the first color from the specified viridis color scale
    gg <- ggplot(data = shareddata) +
      geom_point(aes(x = {{ x }}, y = {{ y }}),
                 shape = 21,
                 stroke = 0.05,
                 fill = viridis(n = 1, option = colorpalette),
                 color = "dimgray",
                 show.legend = showlegend) +
      theme_bw()
  }else if (isTRUE(colorvarnumeric) &&
           colorpalette %in% c("viridis", "magma", "inferno", "plasma")) {
    # {{ colorvar }} is a numeric column in ogdata and
    # a viridis palette is specified
    gg <- ggplot(data = shareddata) +
      geom_point(aes(x = {{ x }}, y = {{ y }}, fill = {{ colorvar }}),
                 shape = 21,
                 stroke = 0.05,
                 color = "dimgray",
                 show.legend = showlegend) +
      scale_fill_viridis_c(option = colorpalette, na.value = "dimgray") +
      theme_bw()
  }else if (isFALSE(colorvarnumeric) &&
           colorpalette %in% c("viridis", "magma", "inferno", "plasma")) {
    # {{ colorvar }} is a non-numeric column in ogdata and
    # a viridis palette is specified
    gg <- ggplot(data = shareddata) +
      geom_point(aes(x = {{ x }}, y = {{ y }}, fill = {{ colorvar }}),
                 shape = 21,
                 stroke = 0.05,
                 color = "dimgray",
                 show.legend = showlegend) +
      scale_fill_viridis_d(option = colorpalette, na.value = "dimgray") +
      theme_bw()
  }

  # Remove the y-axis title because we will use a plotly annotation for it
  gg <- gg + ylab(NULL)

  if (!is.null(xlabel)) {
    gg <- gg +
      xlab(xlabel)
  }

  if (!is.null(legendtitle)) {
    gg <- gg +
      labs(fill = legendtitle)
  }

  if (is.null(ylabel)) {
    ylabel <- yname
  }

  plot_annotations <- list(
    text = ylabel,
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "left",
    align = "left",
    x = 0,
    y = 1,
    showarrow = FALSE
  )

  scatterplotly <- gg |>
    ggplotly(dynamicTicks = TRUE) |>
    layout(annotations = plot_annotations) |>
    highlight(on = "plotly_selected", off = "plotly_deselect", dynamic = FALSE)

  if (isFALSE(showlegend)) {
    scatterplotly <- scatterplotly |> hide_guides()
  }

  return(scatterplotly)
}
