#' Check the arguments provided to a function and throw an error if there is
#'  a problem.
#'
#' Helper function for internal use only.
#' @param shareddata The SharedData object provided to the calling function.
#' @param checks A vector of the checks to be performed; defaults to
#'  `c("x", "y", "colorvar", "lat", "lng")`
#' @param x The `x` argument provided to the calling function.
#' @param y The `y` argument provided to the calling function.
#' @param colorvar The `colorvar` argument provided to the calling function.
#' @param lng The `lng` argument provided to the calling function.
#' @param lat The `lat` argument provided to the calling function.
#' @returns TRUE if all function arguments pass the checks.
#' @importFrom crosstalk SharedData
#' @importFrom rlang enquo quo_set_env global_env get_expr eval_tidy as_label
#' @importFrom leaflet derivePoints
#'
check_function_arguments <- function(shareddata,
                                     checks = c("x", "y", "colorvar",
                                                "lat", "lng"),
                                     x = NULL,
                                     y = NULL,
                                     colorvar = NULL,
                                     lng = NULL,
                                     lat = NULL) {

  results_of_checks <- list(colorvarnumeric = NULL,
                            lnglat = NULL)

  ogdata <- shareddata$origData()

  # Checks associated with `driveplot_companion()`
  if ("x" %in% checks) {
    quox <- enquo(x)
    quox <- quo_set_env(quox, global_env())
    if (is.character(get_expr(quox))) {
      stop("Do not put argument `x` in quotes.",
           call. = FALSE)
    }

    tidy_x <- eval_tidy(quox, data = ogdata)
    if (length(tidy_x) == 1 && is.na(tidy_x)) {
      stop("Argument `x` cannot be NA.",
           call. = FALSE)
    } else if (is.null(tidy_x)) {
      stop("Argument `x` cannot be NULL.",
           call. = FALSE)
    }
  }

  if ("y" %in% checks) {
    quoy <- enquo(y)
    quoy <- quo_set_env(quoy, global_env())
    if (is.character(get_expr(quoy))) {
      stop("Do not put argument `y` in quotes.",
           call. = FALSE)
    }
    tidy_y <- eval_tidy(quoy, data = ogdata)
    if (length(tidy_y) == 1 && is.na(tidy_y)) {
      stop("Argument `y` cannot be NA.",
           call. = FALSE)
    } else if (is.null(tidy_y)) {
      stop("Argument `y` cannot be NULL.",
           call. = FALSE)
    }
  }

  # Checks for the color variable
  if ("colorvar" %in% checks) {
    quocolor <- enquo(colorvar)
    if (is.character(get_expr(quocolor))) {
      stop("Do not put argument `colorvar` in quotes.
    Did you mean to use `colorpalette` instead?",
           call. = FALSE)
    }
    tidy_color <- eval_tidy(quocolor, data = ogdata)
    if (is.null(tidy_color)) {
      results_of_checks$colorvarnumeric <- NULL
    } else if (isTRUE(is.numeric(tidy_color))) {
      results_of_checks$colorvarnumeric <- TRUE
    } else if (isFALSE(is.numeric(tidy_color))) {
      results_of_checks$colorvarnumeric <- FALSE
    }
  }

  # Checks associated with `driveplot_map()`
  if ("lng" %in% checks && "lat" %in% checks) {
    quolng <- enquo(lng)
    quolat <- enquo(lat)
    lngname <- as_label(quolng)
    latname <- as_label(quolat)
    columns <- colnames(ogdata)

    lngcheck <- if (lngname == "NULL") FALSE else TRUE
    latcheck <- if (latname == "NULL") FALSE else TRUE

    if ((isFALSE(lngcheck) && isTRUE(latcheck)) ||
          (isTRUE(lngcheck) && isFALSE(latcheck))) {
      stop("If providing `lng` and `lat`, must provide both.",
           call. = FALSE)
    }

    if (isTRUE(lngcheck) && !(lngname) %in% columns) {
      stop(paste0("Can't find `", lngname, "` in `shareddata`."),
           call. = FALSE)
    }

    if (isTRUE(latcheck) && !(latname) %in% columns) {
      stop(paste0("Can't find `", latname, "` in `shareddata`."),
           call. = FALSE)
    }

    # Check for sf geometry column
    if (isFALSE(lngcheck) && isFALSE(latcheck)) {
      sfgeomcheck <- attr(ogdata, "sf_column")
      if (is.null(sfgeomcheck)) {
        stop("Can't find a geometry column and `lng` and `lat` not provided.",
             call. = FALSE)
      }

      lnglat <- tryCatch(
        derivePoints(shareddata),
        error = function(e) {
          stop("Geometry column must have type POINT.",
               call. = FALSE)
        }
      )
      results_of_checks$lnglat <- lnglat
    }
  }
  results_of_checks
}
