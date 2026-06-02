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
#'   with leaflet. Run `leaflet_color_palettes()` to see the available options
#'   for viridis color palettes. If `colorvar` and `colorpalette` are both not
#'   provided, `colorpalette` defaults to blue. If `colorvar` is provided but
#'   `colorpalette` is not, `colorpalette` defaults to the color palette
#'   "viridis".
#' @param xlabel The label for the variable on the horizontal axis.
#' @param ylabel The label for the variable on the vertical axis.
#' @param showlegend Show the plot legend (TRUE) or not (FALSE).
#' @param legendtitle The title for the plot legend.
#' @returns A plotly scatterplot.
#' @importFrom crosstalk SharedData is.SharedData
#' @importFrom rlang enquo quo_set_env global_env as_label
#' @importFrom viridisLite viridis
#' @importFrom ggplot2 ggplot geom_point theme_bw scale_fill_viridis_c
#'   scale_fill_viridis_d ylab xlab labs aes
#' @importFrom plotly ggplotly layout highlight hide_guides config
#' @export
#' @examples
#' library(crosstalk)
#' data(drive7)
#' shared_drive <- SharedData$new(drive7)
#'
#' # Time series of speed
#' driveplot_companion(
#'   shareddata = shared_drive,
#'   x = time_cst,
#'   y = speed_mph
#' )
#'
#' # Color points by direction of car
#' driveplot_companion(
#'   shareddata = shared_drive,
#'   x = time_cst,
#'   y = speed_mph,
#'   colorvar = gyro_heading,
#'   colorpalette = "viridis",
#'   xlabel = "Time",
#'   ylabel = "Speed (MPH)",
#'   showlegend = TRUE,
#'   legendtitle = "Gyro Heading"
#' )
driveplot_companion <- function(shareddata,
                                x,
                                y,
                                colorvar = NULL,
                                colorpalette = NULL,
                                xlabel = NULL,
                                ylabel = NULL,
                                showlegend = TRUE,
                                legendtitle = NULL) {
  shareddata <- convert_to_SharedData(shareddata)

  quox <- enquo(x)
  quox <- quo_set_env(quox, global_env())
  quoy <- enquo(y)
  quoy <- quo_set_env(quoy, global_env())
  quocolor <- enquo(colorvar)

  check_x <- check_xy(
    shareddata = shareddata,
    arg = {{ quox }},
    argname = "x"
  )

  check_y <- check_xy(
    shareddata = shareddata,
    arg = {{ quoy }},
    argname = "y"
  )
  yname <- as_label(quoy)

  colorvarnumeric <- check_colorvar(
    shareddata = shareddata,
    colorvar = {{ quocolor }}
  )

  colorpalette <- check_colorpalette(
    shareddata = shareddata,
    colorvar = {{ quocolor }},
    colorpalette = colorpalette
  )

  if (is.null(colorvarnumeric) &&
    !(colorpalette %in% leaflet_color_palettes())) {
    # {{ colorvar }} isn't a column in the data and
    # viridis palette isn't specified
    gg <- ggplot(data = shareddata) +
      geom_point(aes(x = !!quox, y = !!quoy),
        shape = 21,
        stroke = 0.05,
        fill = colorpalette,
        color = "dimgray",
        show.legend = showlegend
      ) +
      theme_bw()
  } else if (!is.null(colorvarnumeric) &&
    !(colorpalette %in% leaflet_color_palettes())) {
    # {{ colorvar }} is a column in the data and
    # a viridis palette isn't specified
    # Throw an error if a color variable is specified, but not a color palette
    palettes <- leaflet_color_palettes()
    error_msg <- paste(
      paste(shQuote(palettes[-length(palettes)]),
        collapse = ", "
      ), "or",
      shQuote(palettes[length(palettes)])
    )
    stop(paste0("When specifying colorvar, please use
         colorpalette = ", error_msg),
      call. = FALSE
    )
  } else if (is.null(colorvarnumeric) &&
    colorpalette %in% leaflet_color_palettes()) {
    # {{ colorvar }} is not a column in the data and
    # a viridis palette is specified
    # Make plot using the first color from the specified viridis color scale
    gg <- ggplot(data = shareddata) +
      geom_point(aes(x = !!quox, y = !!quoy),
        shape = 21,
        stroke = 0.05,
        fill = viridis(n = 1, option = colorpalette),
        color = "dimgray",
        show.legend = showlegend
      ) +
      theme_bw()
  } else if (isTRUE(colorvarnumeric) &&
    colorpalette %in% leaflet_color_palettes()) {
    # {{ colorvar }} is a numeric column in the data and
    # a viridis palette is specified
    gg <- ggplot(data = shareddata) +
      geom_point(aes(x = !!quox, y = !!quoy, fill = !!quocolor),
        shape = 21,
        stroke = 0.05,
        color = "dimgray",
        show.legend = showlegend
      ) +
      scale_fill_viridis_c(option = colorpalette, na.value = "dimgray") +
      theme_bw()
  } else if (isFALSE(colorvarnumeric) &&
    colorpalette %in% leaflet_color_palettes()) {
    # {{ colorvar }} is a non-numeric column in the data and
    # a viridis palette is specified
    gg <- ggplot(data = shareddata) +
      geom_point(aes(x = !!quox, y = !!quoy, fill = !!quocolor),
        shape = 21,
        stroke = 0.05,
        color = "dimgray",
        show.legend = showlegend
      ) +
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

  scatterplotly <- scatterplotly |>
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "toImage",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )

  scatterplotly
}
