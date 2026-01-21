
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DrivePlotR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/hardtme/DrivePlotR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hardtme/DrivePlotR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of DrivePlotR is to produce linked plot maps for multivariate
high resolution spatio-temporal data such as data from naturalistic
driving studies and connected vehicles.

## Installation

You can install the development version of DrivePlotR from
[GitHub](https://github.com/hardtme/DrivePlotR) with:

``` r
# install.packages("pak")
pak::pak("hardtme/DrivePlotR")
```

## Example

This example shows the basic workflow for making visualizations with
`DrivePlotR`.

<!-- Examples borrowed from the DrivePlotR paper -->

1.  Load `DrivePlotR` and make drive data available in the `R` session.
    We have made non-sensitive drives from a naturalistic driving study
    available as part of the `DrivePlotR` package, but any other data
    with both geographic and temporal components would also be suitable.

``` r
library(DrivePlotR)
data("drive7")
```

2.  Establish the geographic variables and projection. The code below
    creates a simple feature variable `geometry` of all points. Setting
    the geographic projection to WGS84 ensures that points in the data
    line up with maps in Leaflet. This is already done for the example
    dataset `drive7`.

3.  Convert the drive data into a shared data frame.

``` r
shared_drive <- crosstalk::SharedData$new(drive7)
```

4.  Create visualizations.

Although the main purpose of `DrivePlotR` is for users to create linked
visualizations with a map and one to four companion graphs, users can
also separately create maps or companion graphs.

You can create a basic standalone map as follows:

``` r
driveplot_map(shareddata = shared_drive)
```

You can also create a standalone companion graph as follows:

``` r
driveplot_companions(
  shareddata = shared_drive, 
  x = time_cst, y1 = speed_mph, 
  xlabel = "Time", 
  y1label = "Speed (MPH)")
```

`DrivePlotR` offers many options for customization. For example, the
following code creates a more complex linked plot map. The companion
graphs include speed, gyroscopic heading (the direction the vehicle is
going), and GPS position dilution of precision (PDOP, which is a measure
of the quality of the GPS location data). The points on both the map and
the companion graphs are colored by the minute of the drive during which
they occurred.

``` r
driveplot(
  shareddata = shared_drive, 
  x = time_cst, y1 = speed_mph, 
  y2 = gyro_heading, y3 = gps_pdop,
  colorvar = gps_minute, maplabel = time_cst, 
  colorpalette = "viridis", 
  xlabel = "Time", y1label = "Speed (MPH)", 
  y2label = "Gyro Heading", y3label = "GPS PDOP", 
  showlegend = TRUE, legendtitle = "Minute", 
  plottitle = "A Drive in Omaha, NE")
```
