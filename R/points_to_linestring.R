#' Given a dataframe of trajectory points, create a linestring for each
#'   trajectory
#'
#' @param df A dataframe containing point observations to be
#'   converted into linestrings.
#' @param group The bare (unquoted) column from df for the grouping
#'   variable.
#' @param lng The bare (unquoted) column of df containing longitude
#'   (only required if df does not have a geometry column).
#' @param lat The bare (unquoted) column of df containing latitude
#'   (only required if df does not have a geometry column).
#' @param crs The coordinate reference system (CRS) for the data.
#' @returns A dataframe with one linestring for each group in df
#' @importFrom sf st_as_sf st_crs st_cast
#' @importFrom dplyr group_by summarize
#' @importFrom rlang quo_is_null as_name
#' @keywords internal
points_to_linestring <- function(df,
                                 group,
                                 lng,
                                 lat,
                                 crs = 4326) {
  quogroup <- enquo(group)
  quolng <- enquo(lng)
  quolat <- enquo(lat)

  # Check if df has an sf geometry column and convert it to sf if not
  sfgeom <- attr(df, "sf_column")
  if (is.null(sfgeom)) {
    if (quo_is_null(quolng) || quo_is_null(quolat)) {
      stop("Can't convert provided data to LINESTRING.",
           call. = FALSE)
    }
    # Extract lng/lat column names from quosures
    lng_name <- as_name(quolng)
    lat_name <- as_name(quolat)

    df <- df |>
      st_as_sf(coords = c(lng_name, lat_name), crs = crs)
  }

  if (quo_is_null(group)) {
    df_ls <- df |>
      summarize(do_union = FALSE, .groups = "drop") |>
      st_cast("LINESTRING")
  } else {
  # Group and create linestrings
  df_ls <- df |>
    group_by({{ group }}) |>
    summarize(do_union = FALSE, .groups = "keep") |>
    st_cast("LINESTRING")
  }
  df_ls
}
