% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/driveplot_companions.R
\name{driveplot_companions}
\alias{driveplot_companions}
\title{Make a stack of companion graphs from shared drive data}
\usage{
driveplot_companions(
  shareddata,
  time,
  y1,
  y2 = NULL,
  y3 = NULL,
  y4 = NULL,
  timelabel = NULL,
  y1label = NULL,
  y2label = NULL,
  y3label = NULL,
  y4label = NULL,
  colorvar = NULL,
  colorpalette = "#03F",
  showlegend = TRUE,
  legendtitle = NULL,
  spacing = 0.05,
  plotheight = "98vh"
)
}
\arguments{
\item{shareddata}{a SharedData object containing observations to be plotted}

\item{time}{time variable from shareddata to be plotted on the horizontal axis}

\item{y1}{variable from shareddata to be plotted on the vertical axis for the first graph}

\item{y2}{variable from shareddata to be plotted on the vertical axis for the second graph}

\item{y3}{variable from shareddata to be plotted on the vertical axis for the third graph}

\item{y4}{variable from shareddata to be plotted on the vertical axis for the fourth graph}

\item{timelabel}{the label for the time variable on the horizontal axis}

\item{y1label}{the label for the variable on the vertical axis for the first graph}

\item{y2label}{the label for the variable on the vertical axis for the second graph}

\item{y3label}{the label for the variable on the vertical axis for the third graph}

\item{y4label}{the label for the variable on the vertical axis for the fourth graph}

\item{colorvar}{the variable in shareddata to which color should be mapped}

\item{colorpalette}{either a single color (e.g., "red") or one of "viridis", "inferno", "magma", or "plasma"}

\item{showlegend}{show the plot legend (TRUE) or not (FALSE)}

\item{legendtitle}{the title for the plot legend}

\item{spacing}{value between 0 and 1 for the space between the graphs}

\item{plotheight}{the height of the stack of companion graphs in CSS units, e.g, "98vh"}
}
\value{
plotly scatterplot
}
\description{
Make a stack of companion graphs from shared drive data
}
\examples{
library(crosstalk)
library(dplyr)
data(nds_data)
nds_sf7 <- nds_data \%>\%
  filter(drive==7) \%>\%
  sf::st_as_sf(coords = c("gps_long", "gps_lat"), crs = "WGS84")
nds_sf7_sd <- SharedData$new(nds_sf7)

# Linked time series of speed, headings (in GPS and gyro), and GPS quality
driveplot_companions(
     nds_sf7_sd, time = time_cst, timelabel="Time",
     y1 = speed_mph, y1label = "Speed (mph)",
     y2 = gyro_heading, y2label = "Gyro Heading (degree)",
     y3 = gps_heading, y3label = "GPS Heading (degree)",
     colorvar = gps_pdop, colorpalette = "viridis" )

}
