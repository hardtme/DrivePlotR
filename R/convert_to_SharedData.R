#' Convert a data frame to a SharedData object and warn the user
#'   this was done
#'
#' Helper function for internal use only.
#' @param df The provided data object to be converted to a SharedData object.
#' @returns A SharedData object.
#' @importFrom crosstalk SharedData is.SharedData
#' @keywords internal
convert_to_SharedData <- function(df) {
  if (isFALSE(is.SharedData(df))) {
    warning("Converted `shareddata` to a SharedData object.",
      call. = FALSE
    )
    df <- SharedData$new(df)
  }
  df
}
