#' Make a single plotly scatter plot from shared drive data
#'
#' @param shareddata A SharedData object containing observations to be plotted.
#' @param x The bare (unquoted) column from shareddata to be plotted
#'   on the horizontal axis.
#' @param y The bare (unquoted) column from shareddata to be plotted
#'   on the vertical axis.
#' @param group The bare (unquoted) column from shareddata for the grouping
#'   variable.
#' @param colorvar The bare (unquoted) column in shareddata to which
#'   color should be mapped.
#' @param colorpalette The color palette for the plot; either a single
#'   color (e.g., "red") or one of the viridis color palettes compatible
#'   with leaflet. Run `viridis_color_palettes()` to see the available options
#'   for viridis color palettes. If `colorvar` and `colorpalette` are both not
#'   provided, `colorpalette` defaults to blue. If `colorvar` is provided but
#'   `colorpalette` is not, `colorpalette` defaults to the color palette
#'   "viridis".
#' @param fillopacity The opacity of the fill of the points (0 to 1).
#' @param xlabel The label for the variable on the horizontal axis.
#' @param ylabel The label for the variable on the vertical axis.
#' @param showlegend Show the plot legend (TRUE) or not (FALSE).
#' @param legendtitle The title for the plot legend.
#' @returns A plotly scatterplot.
#' @importFrom crosstalk SharedData is.SharedData
#' @importFrom rlang enquo quo_set_env global_env as_label quo_is_null
#' @importFrom viridisLite viridis
#' @importFrom ggplot2 ggplot geom_point theme_bw scale_fill_viridis_c
#'   scale_fill_viridis_d ylab xlab labs aes scale_fill_brewer
#'   scale_fill_distiller scale_fill_manual scale_fill_gradientn geom_line
#' @importFrom plotly ggplotly layout highlight hide_guides config
#' @importFrom RColorBrewer brewer.pal
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
                                group = NULL,
                                colorvar = NULL,
                                colorpalette = NULL,
                                fillopacity = 1,
                                xlabel = NULL,
                                ylabel = NULL,
                                showlegend = TRUE,
                                legendtitle = NULL) {
  shareddata <- convert_to_SharedData(shareddata)

  quox <- enquo(x)
  quox <- quo_set_env(quox, global_env())
  quoy <- enquo(y)
  quoy <- quo_set_env(quoy, global_env())
  quogroup <- enquo(group)
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

  palettelength <- length(colorpalette)

  gg <- ggplot(data = shareddata,
               mapping = aes(x = !!quox, y = !!quoy))

  # Add the grouping variable, if provided
  if (quo_is_null(quogroup)) {
    # No grouping variable provided
    gg <- gg + aes(group = 1)
  } else {
    # Grouping variable provided
    gg <- gg + aes(group = !!quogroup)
  }

  if (palettelength > 1){
    if (is.null(colorvarnumeric)) {
      gg <- gg +
        geom_line(color = "dimgray", linewidth = 0.25, alpha = fillopacity/2) +
        geom_point(
                   shape = 21,
                   stroke = 0.05,
                   fill = colorpalette[1],
                   color = "dimgray",
                   alpha = fillopacity,
                   show.legend = showlegend
        ) +
        theme_bw()
    } else if(isTRUE(colorvarnumeric)) {
      gg <- gg +
        geom_line(color = "dimgray", linewidth = 0.25, alpha = fillopacity/2) +
        geom_point(aes(fill = !!quocolor),
                   shape = 21,
                   stroke = 0.05,
                   color = "dimgray",
                   alpha = fillopacity,
                   show.legend = showlegend
        ) +
        scale_fill_gradientn(colors = colorpalette, na.value = "#00000000") +
        theme_bw()
    } else if(isFALSE(colorvarnumeric)) {
      gg <- gg +
        geom_line(color = "dimgray", linewidth = 0.25, alpha = fillopacity/2) +
        geom_point(aes(fill = !!quocolor),
                   shape = 21,
                   stroke = 0.05,
                   color = "dimgray",
                   alpha = fillopacity,
                   show.legend = showlegend
        ) +
        scale_fill_manual(values = colorpalette, na.value = "#00000000") +
        theme_bw()
    }
  } else {
  # Single color or viridis or ColorBrewer palette
  if (is.null(colorvarnumeric) &&
      !(colorpalette %in% viridis_color_palettes()) &&
      !(colorpalette %in% colorbrewer_color_palettes())) {
    # {{ colorvar }} isn't a column in the data and a viridis or
    # ColorBrewer palette isn't specified
    gg <- gg +
      geom_line(color = "dimgray", linewidth = 0.25, alpha = fillopacity/2) +
      geom_point(
        shape = 21,
        stroke = 0.05,
        fill = colorpalette,
        color = "dimgray",
        alpha = fillopacity,
        show.legend = showlegend
      ) +
      theme_bw()
  } else if (!is.null(colorvarnumeric) &&
    !(colorpalette %in% viridis_color_palettes()) &&
    !(colorpalette %in% colorbrewer_color_palettes())) {
    # {{ colorvar }} is a column in the data and a viridis or
    # ColorBrewer palette isn't specified
    # Throw an error if a color variable is specified, but not a color palette
    stop(
      "When specifying colorvar, please use a viridis or ColorBrewer palette.",
      call. = FALSE
    )
  } else if (is.null(colorvarnumeric) &&
    colorpalette %in% viridis_color_palettes()) {
    # {{ colorvar }} is not a column in the data and
    # a viridis palette is specified
    # Make plot using the first color from the specified viridis color scale
    gg <- gg +
      geom_line(color = "dimgray", linewidth = 0.25, alpha = fillopacity/2) +
      geom_point(
        shape = 21,
        stroke = 0.05,
        fill = viridis(n = 1, option = colorpalette),
        color = "dimgray",
        alpha = fillopacity,
        show.legend = showlegend
      ) +
      theme_bw()
  } else if (is.null(colorvarnumeric) &&
             colorpalette %in% colorbrewer_color_palettes()) {
    # {{ colorvar }} is not a column in the data and
    # a ColorBrewer palette is specified
    # Make plot using the first color from the specified ColorBrewer color scale
    gg <- gg +
      geom_line(color = "dimgray", linewidth = 0.25, alpha = fillopacity/2) +
      geom_point(
                 shape = 21,
                 stroke = 0.05,
                 fill = brewer.pal(n = 3, name = colorpalette)[1],
                 color = "dimgray",
                 alpha = fillopacity,
                 show.legend = showlegend
      ) +
      theme_bw()
  } else if (isTRUE(colorvarnumeric)) {
    # {{ colorvar }} is a numeric column in the data
    gg <- gg +
      geom_line(color = "dimgray", linewidth = 0.25) +
      geom_point(aes(fill = !!quocolor),
        shape = 21,
        stroke = 0.05,
        color = "dimgray",
        alpha = fillopacity,
        show.legend = showlegend
      )
    if (colorpalette %in% viridis_color_palettes()){
      # Use a viridis palette
      gg <- gg +
      scale_fill_viridis_c(option = colorpalette, na.value = "#00000000") +
      theme_bw()
    } else {
      # Use a ColorBrewer palette
      gg <- gg +
        scale_fill_distiller(palette = colorpalette, na.value = "#00000000",
                             direction = 1) +
        theme_bw()
    }
  } else if (isFALSE(colorvarnumeric)) {
    # {{ colorvar }} is a non-numeric column in the data
    gg <- gg +
      geom_line(color = "dimgray", linewidth = 0.25, alpha = fillopacity/2) +
      geom_point(aes(fill = !!quocolor),
        shape = 21,
        stroke = 0.05,
        color = "dimgray",
        alpha = fillopacity,
        show.legend = showlegend
      )
    if (colorpalette %in% viridis_color_palettes()){
      # Use a viridis palette
      gg <- gg +
        scale_fill_viridis_d(option = colorpalette, na.value = "#00000000") +
        theme_bw()
    } else {
      # Use a ColorBrewer palette
      gg <- gg +
        scale_fill_brewer(palette = colorpalette, na.value = "#00000000",
                          direction = 1) +
        theme_bw()
    }
  }
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
    ggplotly(dynamicTicks = TRUE, tooltip = c("x", "y")) |>
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
