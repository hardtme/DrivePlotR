% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/driveplot_map.R
\name{driveplot_map}
\alias{driveplot_map}
\title{Create a standalone map}
\usage{
driveplot_map(
  shareddata,
  lng = NULL,
  lat = NULL,
  label = NA,
  colorvar = NULL,
  colorpalette = "#03F",
  fillOpacity = 1,
  mapheight = "98vh"
)
}
\arguments{
\item{shareddata}{a SharedData object containing observations to be plotted}

\item{lng}{the column of shareddata containing longitude (only required if shareddata does not have a geometry column)}

\item{lat}{the column of shareddata containing latitude (only required if shareddata does not have a geometry column)}

\item{label}{an optional label for the map points}

\item{colorvar}{the variable in shareddata to which color should be mapped}

\item{colorpalette}{either a single color (e.g., "red") or one of "viridis", "inferno", "magma", or "plasma"}

\item{fillOpacity}{the opacity of the fill (0 to 1)}

\item{mapheight}{the height of the map in CSS units, e.g, "98vh"}
}
\value{
leaflet map
}
\description{
Create a standalone map
}
\examples{
library(crosstalk)
library(dplyr)
data(nds_data)
nds_sf7 <- nds_data \%>\%
  filter(drive==7) \%>\%
  sf::st_as_sf(coords = c("gps_long", "gps_lat"), crs = "WGS84")
nds_sf7_sd <- SharedData$new(nds_sf7)

# basic map of one drive
driveplot_map(nds_sf7_sd)

# color drive points by direction of car
driveplot_map(nds_sf7_sd, colorvar=gyro_heading, colorpalette="viridis")
}
