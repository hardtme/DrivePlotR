#' Check the arguments provided to `driveplot_companion()`, `driveplot_map()`,
#'   and `driveplot_map()`

#' Check the x and y arguments for `driveplot_companion()`
#'
#' Helper function for internal use only.
#' @param shareddata The SharedData object provided to the calling function.
#' @param arg The `x` or `y` argument provided to the calling function.
#' @param argname The name (x or y) of the argument to be checked.
#' @returns The result of the argument tidily evaluated in the environment
#'   of the original data from `shareddata`.
#' @importFrom rlang enquo quo_set_env global_env get_expr eval_tidy quo_is_null
#' @keywords internal
check_xy <- function(shareddata, arg, argname) {
  ogdata <- shareddata$origData()

  quoarg <- enquo(arg)
  quoarg <- quo_set_env(quoarg, global_env())

  if (quo_is_null(quoarg)) {
    stop(
      sprintf("Argument `%s` cannot be NULL.", argname),
      call. = FALSE
    )
  }

  if (is.character(get_expr(quoarg))) {
    stop(
      sprintf("Do not put argument `%s` in quotes.", argname),
      call. = FALSE
    )
  }

  tidy_arg <- eval_tidy(quoarg, ogdata)
  if (length(tidy_arg) == 1 && is.na(tidy_arg)) {
    stop(
      sprintf("Argument `%s` cannot be NA.", argname),
      call. = FALSE
    )
  }
  tidy_arg
}

#' Check the colorvar argument for `driveplot_companion()`
#'   and `driveplot_map()`
#'
#' Helper function for internal use only.
#' @param shareddata The SharedData object provided to the calling function.
#' @param colorvar The `colorvar` argument provided to the calling function.
#' @returns A value indicating whether colorvar is not a column in the data
#'   (NULL), colorvar is a numeric column in the data (TRUE), or colorvar is
#'   not a numeric column in the data (FALSE).
#' @importFrom rlang enquo quo_set_env global_env get_expr eval_tidy
#' @keywords internal
check_colorvar <- function(shareddata, colorvar) {
  ogdata <- shareddata$origData()

  quocolor <- enquo(colorvar)
  quocolor <- quo_set_env(quocolor, global_env())

  if (is.character(get_expr(quocolor))) {
    stop(
      "Do not put argument `colorvar` in quotes.\nDid you mean `colorpalette`?",
      call. = FALSE
    )
  }

  tidy_color <- eval_tidy(quocolor, data = ogdata)
  if (is.null(tidy_color)) {
    colorvarnumeric <- NULL
  } else if (isTRUE(is.numeric(tidy_color))) {
    colorvarnumeric <- TRUE
  } else if (isFALSE(is.numeric(tidy_color))) {
    colorvarnumeric <- FALSE
  }

  colorvarnumeric
}

#' Check the latitude and longitude for `driveplot_map()`
#'
#' Helper function for internal use only.
#' @param shareddata The SharedData object provided to the calling function.
#' @param lng The `lng` argument provided to the calling function.
#' @param lat The `lat` argument provided to the calling function.
#' @returns If `lng` and `lat` are correctly provided, return NULL. If an sf
#'  geometry column is provided, return a dataframe of the points derived from
#'  the geometry column.
#' @importFrom rlang enquo as_label quo_is_null
#' @importFrom leaflet derivePoints
#' @keywords internal
check_lnglat <- function(shareddata, lng, lat) {
  ogdata <- shareddata$origData()

  quolng <- enquo(lng)
  quolat <- enquo(lat)
  lngname <- as_label(quolng)
  latname <- as_label(quolat)
  columns <- colnames(ogdata)

  lng_provided <- !quo_is_null(quolng)
  lat_provided <- !quo_is_null(quolat)

  if (xor(lng_provided, lat_provided)) {
    stop("If providing `lng` and `lat`, must provide both.", call. = FALSE)
  }

  if (lng_provided && !(lngname %in% columns)) {
    stop(
      sprintf("Can't find `%s` in `shareddata`.", lngname),
      call. = FALSE
    )
  }

  if (lat_provided && !(latname %in% columns)) {
    stop(
      sprintf("Can't find `%s` in `shareddata`.", latname),
      call. = FALSE
    )
  }

  if (!lng_provided && !lat_provided && is.null(attr(ogdata, "sf_column"))) {
    lnglat <- guess_lat_long_cols(columns)
  } else {
    lnglat <- NULL
  }
  lnglat
}

#' Guess latitude and longitude column names
#'
#' Adapted from the leaflet function `guessLatLongCols`. Rather than looking for
#'   the exact variable names lat or latitude, the regular expression allows for
#'   variables that start or end with lat/latitude for latitude; similarly for
#'   longitude.
#'
#' Helper function for internal use only.
#'
#' @param names Vector of column names.
#' @param stopOnFailure Should the function throw an error when it cannot
#'   identify latitude and longitude columns in the data (TRUE) or not (FALSE)?
#' @returns List with latitude and longitude column names.
#' @keywords internal
guess_lat_long_cols <- function (names, stopOnFailure = TRUE) {
  lats <- names[grep("^(lat|latitude)|(lat|latitude)$",
                     names, ignore.case = TRUE)]
  lngs <- names[grep("^(lon|lng|long|longitude)|(lon|lng|long|longitude)$",
                     names, ignore.case = TRUE)]
  if (length(lats) == 1 && length(lngs) == 1) {
    if (length(names) > 2) {
      message("Assuming \"", lngs, "\" and \"", lats,
              "\" are longitude and latitude, respectively")
    }
    return(list(lng = lngs, lat = lats))
  }
  if (stopOnFailure) {
    stop("Couldn't infer longitude/latitude columns",
         call. = FALSE)
  }
  list(lng = NA, lat = NA)
}


#' Check the `ylabels` argument for `driveplot_companions()`
#'
#' Helper function for internal use only.
#' @param ylabels A vector or list of labels provided to the calling function.
#' @param ylength The length of the ys argument from the calling function.
#' @returns Either `ylabels` or a list of NULL of length `ylength`.
#' @keywords internal
check_ylabels <- function(ylabels, ylength) {
  if (ylength > 4) {
    warning("4+ columns were passed in `ys`, so graphs may be compressed.",
      call. = FALSE
    )
  }

  if (any(is.na(ylabels))) {
    stop("`ylabels` cannot be NA.",
      call. = FALSE
    )
  }

  if (!is.null(ylabels) && length(ylabels) != ylength) {
    stop("If providing `ylabels`, `ys` and `ylabels` must be the same length.",
      call. = FALSE
    )
  }

  if (is.null(ylabels)) {
    ylabels <- vector(mode = "list", length = ylength)
  }

  ylabels
}

#' Establish the default behavior for colorpalette
#'
#' Helper function for internal use only.
#' @param shareddata The SharedData object provided to the calling function.
#' @param colorvar The `colorvar` argument provided to the calling function.
#' @param colorpalette The `colorpalette` argument provided to the calling
#'   function.
#' @returns The color palette to be used for the visualization.
#' @importFrom rlang enquo quo_set_env global_env quo_is_null
#' @keywords internal
check_colorpalette <- function(shareddata,
                               colorvar = NULL,
                               colorpalette = NULL) {
  quocolor <- enquo(colorvar)
  quocolor <- quo_set_env(quocolor, global_env())

  if (is.null(colorpalette)) {
    if (missing(colorvar) || quo_is_null(quocolor)) {
      colorpalette <- "#03F"
    } else {
      colorpalette <- "viridis"
    }
  }
  colorpalette
}

