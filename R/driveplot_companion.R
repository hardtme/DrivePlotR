#' Make a single plotly scatter plot from shared drive data
#'
#' @importFrom crosstalk SharedData
#' @importFrom dplyr pull ungroup select
#' @importFrom rlang enquo eval_tidy quo_squash quo
#' @importFrom leaflet colorFactor derivePoints colorNumeric leaflet addTiles addCircleMarkers
#' @importFrom viridisLite viridis
#' @importFrom ggplot2 ggplot geom_point theme_bw scale_fill_viridis_c scale_fill_viridis_d ylab xlab labs aes
#' @importFrom sf st_drop_geometry
#' @importFrom plotly ggplotly layout highlight hide_guides
#' @param shareddata a SharedData object containing observations to be plotted
#' @param x variable from shareddata to be plotted on the horizontal axis
#' @param y variable from shareddata to be plotted on the vertical axis
#' @param colorvar the variable in shareddata to which color should be mapped
#' @param colorpalette either a single color (e.g., "red") or one of "viridis", "inferno", "magma", or "plasma"
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param showlegend show the plot legend (TRUE) or not (FALSE)
#' @param legendtitle the title for the plot legend
#' @returns plotly scatterplot
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
#' # Time series of speed
#' driveplot_companion(nds_sf7_sd, time_cst, speed_mph)
#'
#' # color points by direction of car
#' driveplot_companion(nds_sf7_sd, time_cst, speed_mph, colorvar=gyro_heading, colorpalette="viridis")
driveplot_companion <- function(shareddata, x, y, colorvar = NULL,
                                colorpalette = "#03F", xlab = NULL, ylab = NULL, showlegend = TRUE,
                                legendtitle = NULL){
  # Get original data from shareddata so we can check the existence and type of colorvar
  # We can't directly access columns in a SharedData object
  ogdata <- shareddata$origData()
  colorvarnumeric <- tryCatch(ogdata |> pull({{ colorvar }}) |> is.numeric(),
                              error = function(e){},
                              finally = NULL)
  # colovarnumeric = NULL if {{ colorvar }} isn't a column in ogdata
  # colorvarnumeric = TRUE if {{ colorvar }} is a numeric column in ogdata
  # colorvarnumeric = FALSE if {{ colorvar }} is not a numeric column in ogdata

  # Use viridis color palettes allowed by leaflet: "viridis", "magma", "inferno", or "plasma"
  if(is.null(colorvarnumeric)  & !(colorpalette %in% c("viridis", "magma", "inferno", "plasma"))){
    # {{ colorvar }} isn't a column in ogdata and viridis palette isn't specified
    gg <- ggplot(data = shareddata)+
      geom_point(aes(x = {{ x }}, y = {{ y }}), shape = 21, stroke = 0.05, fill = colorpalette, color = "dimgray", show.legend = showlegend)+
      theme_bw()
  }else if(!is.null(colorvarnumeric) & !(colorpalette %in% c("viridis", "magma", "inferno", "plasma"))){
    # {{ colorvar }} is a column in ogdata and a viridis palette isn't specified
    # Throw an error if a color variable is specified, but not a color scale
    stop('When specifying colorvar, please use colorpalette = "viridis", "magma", "inferno", or "plasma".', call. = FALSE)
  }else if(is.null(colorvarnumeric) & colorpalette %in% c("viridis", "magma", "inferno", "plasma")){
    # {{ colorvar }} is not a column in ogdata and a viridis palette is specified
    # Make plot using the first color from the specified viridis color scale
    gg <- ggplot(data = shareddata)+
      geom_point(aes(x = {{ x }}, y = {{ y }}), shape = 21, stroke = 0.05, fill = viridis(n = 1, option = colorpalette), color = "dimgray", show.legend = showlegend)+
      theme_bw()
  }else if(isTRUE(colorvarnumeric) & colorpalette %in% c("viridis", "magma", "inferno", "plasma")){
    # {{ colorvar }} is a numeric column in ogdata and a viridis palette is specified
    gg <- ggplot(data = shareddata)+
      geom_point(aes(x = {{ x }}, y = {{ y }}, fill = {{ colorvar }}), shape = 21, stroke = 0.05, color = "dimgray", show.legend = showlegend)+
      scale_fill_viridis_c(option = colorpalette, na.value = "dimgray")+
      theme_bw()
  }else if(isFALSE(colorvarnumeric) & colorpalette %in% c("viridis", "magma", "inferno", "plasma")){
    # {{ colorvar }} is a non-numeric column in ogdata and a viridis palette is specified
    gg <- ggplot(data = shareddata)+
      geom_point(aes(x = {{ x }}, y = {{ y }}, fill = {{ colorvar }}), shape = 21, stroke = 0.05, color = "dimgray", show.legend = showlegend)+
      scale_fill_viridis_d(option = colorpalette, na.value = "dimgray")+
      theme_bw()
  }

  # Remove the y-axis title because we will use a plotly annotation for it
  gg <- gg + ylab(NULL)

  if(!is.null(xlab)){
    gg <- gg + xlab(xlab)
  }

  if(!is.null(legendtitle)){
    gg <- gg + labs(fill = legendtitle)
  }

  if(is.null(ylab)){
    ylab <- ogdata |> ungroup() |> st_drop_geometry() |> select({{ y }}) |> colnames()
  }

  plot_annotations <- list(
    text = ylab,
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

  if(isFALSE(showlegend)){
    scatterplotly <- scatterplotly |> hide_guides()
  }

  return(scatterplotly)
}
