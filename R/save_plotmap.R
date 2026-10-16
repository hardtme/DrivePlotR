#' Save a plot map created with `driveplot()` as a standalone HTML file
#'
#' @param plotmap The plot map to be saved.
#' @param filepath The path for the HTML file to be created, e.g.,
#'   "myplotmap.html".
#'
#' @returns The file path (invisibly).
#' @importFrom rmarkdown pandoc_available pandoc_self_contained_html
#' @importFrom htmltools save_html
#' @export
#' @examples
#' \dontrun{
#' library(crosstalk)
#' data(drive7)
#' shared_drive <- SharedData$new(drive7)
#'
#' myplotmap <- driveplot(
#'   shareddata = shared_drive,
#'   x = time_cst,
#'   ys = c(speed_mph, gyro_heading, gps_heading),
#'   colorvar = gyro_heading,
#'   maplabel = time_cst,
#'   colorpalette = "viridis",
#'   fillopacity = 1,
#'   xlabel = "Time",
#'   ylabels = c(
#'     "Speed (mph)", "Gyro Heading (degrees)",
#'     "GPS Heading (degrees)"
#'   ),
#'   showlegend = TRUE,
#'   legendtitle = "Gyro Heading",
#'   plottitle = "A drive with points colored by gyro heading",
#'   height = "500px"
#' )
#' save_plotmap(plotmap = myplotmap, filepath = "myplotmap.html")
#' }
save_plotmap <- function(plotmap, filepath) {

  # Make sure pandoc is available
  if (!pandoc_available()) {
    stop("Pandoc is required to create self-contained HTML files.",
         call. = FALSE)
  }

  # Make sure the directory for the output file exists
  dir.create(dirname(filepath), showWarnings = FALSE, recursive = TRUE)

  # Set the dependency library directory relative to the file path
  libdir <- file.path(dirname(filepath), "lib")

  # Initially save the plot map and its dependencies
  save_html(html = plotmap, file = filepath, libdir = libdir)

  # Clean up the HTML file
  htmllines <- readLines(filepath, warn = FALSE)
  htmllines <- gsub("<!DOCTYPE html>", "", htmllines, ignore.case = TRUE)
  writeLines(htmllines, filepath)

  # Combine the plot map and its dependencies into one self-contained file
  pandoc_self_contained_html(filepath, filepath)

  # Clean up the external dependency folder
  if (dir.exists(libdir)) {
    unlink(libdir, recursive = TRUE)
  }

  invisible(filepath)
}
