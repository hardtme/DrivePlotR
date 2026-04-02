#' Make a single plotly scatter plot from shared drive data
#'
#' @param shareddata A SharedData object containing observations to be plotted.
#' @param x The bare (unquoted) column from shareddata to be plotted
#'   on the horizontal axis.
#' @param y The bare (unquoted) column from shareddata to be plotted
#'   on the vertical axis.
#' @param colorvar The bare (unquoted) column in shareddata to which
#'   color should be mapped.
#' @param colorpalette The color palette for the plot; either a single
#'   color (e.g., "red") or one of the viridis color palettes compatible
#'   with leaflet. Run `leaflet_color_palettes()` to see the available options.
#' @param xlabel The label for the variable on the horizontal axis.
#' @param ylabel The label for the variable on the vertical axis.
#' @param showlegend Show the plot legend (TRUE) or not (FALSE).
#' @param legendtitle The title for the plot legend.
#' @returns A plotly scatterplot.
#' @importFrom crosstalk SharedData is.SharedData
#' @importFrom dplyr pull
#' @importFrom rlang enquo quo_set_env global_env get_expr as_label
#' @importFrom viridisLite viridis
#' @importFrom ggplot2 ggplot geom_point theme_bw scale_fill_viridis_c
#'   scale_fill_viridis_d ylab xlab labs aes
#' @importFrom plotly ggplotly layout highlight hide_guides
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
#'                     colorpalette = "viridis",
#'                     xlabel = "Time",
#'                     ylabel = "Speed (MPH)",
#'                     showlegend = TRUE,
#'                     legendtitle = "Gyro Heading")
driveplot_companion <- function(shareddata,
                                x,
                                y,
                                colorvar = NULL,
                                colorpalette = "#03F",
                                xlabel = NULL,
                                ylabel = NULL,
                                showlegend = TRUE,
                                legendtitle = NULL) {

  if (isFALSE(is.SharedData(shareddata))) {
    stop("`shareddata` must be a SharedData object.", call. = FALSE)
  }

  # Get original data from shareddata so we can check the existence of
  # x, y, and colorvar along with the type of colorvar
  # We can't directly access columns in a SharedData object
  ogdata <- shareddata$origData()
  quox <- enquo(x)
  quox <- quo_set_env(quox, global_env())
  if (is.character(get_expr(quox))) {
    stop("Do not put argument `x` in quotes.",
         call. = FALSE)
  }
  quoy <- enquo(y)
  quoy <- quo_set_env(quoy, global_env())
  if (is.character(get_expr(quoy))) {
    stop("Do not put argument `y` in quotes.",
         call. = FALSE)
  }
  yname <- as_label(quoy)
  quocolor <- enquo(colorvar)
  if (is.character(get_expr(quocolor))) {
    stop("Do not put argument `colorvar` in quotes.
    Did you mean to use `colorpalette` instead?",
         call. = FALSE)
  }
  colorvarname <- as_label(quocolor)

  tidy_x <- eval_tidy(quox, data = ogdata)
  if (length(tidy_x) == 1 && is.na(tidy_x)) {
    stop("Argument `x` cannot be NA.",
         call. = FALSE)
  } else if (length(tidy_x) == 1 && is.null(tidy_x)) {
    stop("Argument `x` cannot be NULL.",
         call. = FALSE)
  }

  tidy_y <- eval_tidy(quoy, data = ogdata)
  if (length(tidy_y) == 1 && is.na(tidy_y)) {
    stop("Argument `y` cannot be NA.",
         call. = FALSE)
  } else if (length(tidy_y) == 1 && is.null(tidy_y)) {
    stop("Argument `y` cannot be NULL.",
         call. = FALSE)
  }

  tidy_color <- eval_tidy(quocolor, data = ogdata)
  if (is.null(tidy_color)) {
    colorvarnumeric <- NULL
  } else {
    colorvarnumeric <- if (is.numeric(tidy_color)) TRUE else FALSE
  }
  # colovarnumeric = NULL if {{ colorvar }} isn't a column in ogdata
  # colorvarnumeric = TRUE if {{ colorvar }} is a numeric column in ogdata
  # colorvarnumeric = FALSE if {{ colorvar }} is not a numeric column in ogdata

  # Use viridis color palettes allowed by leaflet:
  # "viridis", "magma", "inferno", or "plasma"
  if (is.null(colorvarnumeric)  &&
        !(colorpalette %in% leaflet_color_palettes())) {
    # {{ colorvar }} isn't a column in ogdata and
    # viridis palette isn't specified
    gg <- ggplot(data = shareddata) +
      geom_point(aes(x = !!quox, y = !!quoy),
                 shape = 21,
                 stroke = 0.05,
                 fill = colorpalette,
                 color = "dimgray",
                 show.legend = showlegend) +
      theme_bw()
  }else if (!is.null(colorvarnumeric) &&
              !(colorpalette %in% leaflet_color_palettes())) {
    # {{ colorvar }} is a column in ogdata and
    # a viridis palette isn't specified
    # Throw an error if a color variable is specified, but not a color palette
    palettes <- leaflet_color_palettes()
    error_msg <- paste(paste(shQuote(palettes[-length(palettes)]),
                                    collapse = ", "), "or",
                              shQuote(palettes[length(palettes)]))
    stop(paste0("When specifying colorvar, please use
         colorpalette = ", error_msg),
         call. = FALSE)
  }else if (is.null(colorvarnumeric) &&
              colorpalette %in% leaflet_color_palettes()) {
    # {{ colorvar }} is not a column in ogdata and
    # a viridis palette is specified
    # Make plot using the first color from the specified viridis color scale
    gg <- ggplot(data = shareddata) +
      geom_point(aes(x = !!quox, y = !!quoy),
                 shape = 21,
                 stroke = 0.05,
                 fill = viridis(n = 1, option = colorpalette),
                 color = "dimgray",
                 show.legend = showlegend) +
      theme_bw()
  }else if (isTRUE(colorvarnumeric) &&
              colorpalette %in% leaflet_color_palettes()) {
    # {{ colorvar }} is a numeric column in ogdata and
    # a viridis palette is specified
    gg <- ggplot(data = shareddata) +
      geom_point(aes(x = !!quox, y = !!quoy, fill = !!quocolor),
                 shape = 21,
                 stroke = 0.05,
                 color = "dimgray",
                 show.legend = showlegend) +
      scale_fill_viridis_c(option = colorpalette, na.value = "dimgray") +
      theme_bw()
  }else if (isFALSE(colorvarnumeric) &&
              colorpalette %in% leaflet_color_palettes()) {
    # {{ colorvar }} is a non-numeric column in ogdata and
    # a viridis palette is specified
    gg <- ggplot(data = shareddata) +
      geom_point(aes(x = !!quox , y = !!quoy, fill = !!quocolor),
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
    scatterplotly <- scatterplotly |>
      hide_guides()
  }

  scatterplotly
}
