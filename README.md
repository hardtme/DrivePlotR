
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DrivePlotR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/hardtme/DrivePlotR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hardtme/DrivePlotR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of DrivePlotR is to produce linked plot maps for multivariate
high-resolution spatio-temporal data such as vehicle trajectories from
naturalistic driving studies and connected vehicles.

## Installation

You can install the development version of DrivePlotR from
[GitHub](https://github.com/hardtme/DrivePlotR) with:

``` r
# install.packages("pak")
pak::pak("hardtme/DrivePlotR")
```

## Example

This example shows the basic workflow for visualizing vehicle
trajectories with DrivePlotR.

1.  Load DrivePlotR and make drive data available in the R session. We
    have made non-sensitive drives from a naturalistic driving study
    available as part of the DrivePlotR package, but any other data with
    both geographic and temporal components would also be suitable. Each
    row in the data must represent one observation from the vehicle
    trajectory.

``` r
library(DrivePlotR)
data("drive7")
```

2.  Establish the geographic variables and projection. Setting the
    geographic projection to WGS84 ensures that data points are
    compatible with Leaflet maps. This is already done for the example
    dataset `drive7` included with the package.

3.  Convert the drive data into a shared data frame.

``` r
shared_drive <- crosstalk::SharedData$new(drive7)
```

4.  Create visualizations.

Although the main purpose of DrivePlotR is to create linked
visualizations with a map and one to four companion graphs, users can
also create maps or companion graphs separately.

You can create a basic standalone map with the following code:

``` r
driveplot_map(
  shareddata = shared_drive
)
```

You can also create a standalone companion graph as follows:

``` r
driveplot_companion(
  shareddata = shared_drive,
  x = time_cst,
  y = speed_mph,
  xlabel = "Time",
  ylabel = "Speed (MPH)"
)
```

DrivePlotR offers many options for customization. For example, the
following code creates a more complex linked plot map. The companion
graphs include speed, gyroscopic heading (the direction the vehicle is
moving), and GPS position dilution of precision (PDOP,a measure of the
quality of the GPS location data). The points on both the map and the
companion graphs are colored by the minute of the drive during which
they occurred.

``` r
driveplot(
  shareddata = shared_drive,
  x = time_cst,
  ys = c(speed_mph, gyro_heading, gps_pdop),
  colorvar = gps_minute,
  maplabel = time_cst,
  colorpalette = "viridis",
  xlabel = "Time",
  ylabels = c("Speed (MPH)", "Gyro Heading", "GPS PDOP"),
  showlegend = TRUE,
  legendtitle = "Minute",
  plottitle = "A Drive in Omaha, NE"
)
```

<!-- Delete?-->

DrivePlotR provides users with a straightforward way to transform data
columns right when they are specified. In the example below, speed in
miles per hour (MPH) is converted to kilometers per hour (KPH), while
gyro heading numbers are translated into general directions (N, S, E,
W).

``` r
driveplot(
  shareddata = shared_drive,
  x = time_cst,
  ys = c(
    speed_mph * 1.60934,
    c("N", "E", "S", "W")[findInterval(
      (gyro_heading + 45) %% 360,
      c(90, 180, 270)
    ) + 1],
    gps_pdop
  ),
  colorvar = gps_minute,
  maplabel = time_cst,
  colorpalette = "viridis",
  xlabel = "Time",
  ylabels = c("Speed (KPH)", "General Gyro Heading", "GPS PDOP"),
  showlegend = TRUE,
  legendtitle = "Minute",
  plottitle = "A Drive in Omaha, NE"
)
```

To activate the selection box on the map, click the selection button
below the +/- zoom buttons in the upper left corner of the map. A
selection box will then appear on the map. You can move the selection
box by clicking on the 9 dots in the upper left corner of the selection
box and dragging the selection box to the desired location. Use the
white squares in the corners of the selection box to resize the box. Any
points that fall within the selection box on the map will also be
selected on the companion graphs. To remove the selection box from the
map, click on the X button below the +/- zoom buttons in the upper left
corner of the map. To clear the corresponding selection on the companion
graphs, double-click anywhere within the plotting area of one of the
companion graphs.

To see the interaction tools for the companion graphs, hover over the
top of the first companion graph in the stack. A modebar with a
selection of buttons will appear. From left to right, the buttons are:
Zoom, Pan, Box Select, Lasso Select, Zoom in, Zoom out, Autoscale, and
Reset axes. Click a button to interact with the plot map. Points you
select on the companion graphs will also be selected on the map.
Double-click anywhere within the plotting area of one of the companion
graphs to reset the companion graphs.
