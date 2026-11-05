library(tidyverse)
library(sf)
library(geosphere)

angle_dist <- function(x, y, type = "degree") {
  # ensure that x and y are in degrees between 0 and 360)
  if (type == "degree") {
    x <- x %% 360 # just make sure that we are really in [0, 360)
    y <- y %% 360
    angle_diff <- (x - y) %% 360
    angle_diff <- ifelse(angle_diff > 180, angle_diff - 360, angle_diff)
  }

  if (type == "radians") {
    x <- x %% (2*pi) # just make sure that we are really in [0, 2pi)
    y <- y %% (2*pi)
    angle_diff <- (x - y) %% (2*pi)
    angle_diff <- ifelse(angle_diff > pi, angle_diff - (2*pi), angle_diff)
  }
  return(angle_diff)
}

loopdrives <- loops

loopdrives <- loopdrives |>
  group_by(loop) |>
  arrange(time, .by_group = TRUE) |>
  mutate(
    heading_raw = bearing(
      p1 = cbind(lon, lat), # current point
      p2 = cbind(lead(lon), lead(lat)) # next point
    ),
    # Convert headings from (-180, 180) to (0, 360)
    heading_raw = (heading_raw + 360) %% 360,
    heading = heading_raw,
    heading_diff = angle_dist(heading, lag(heading, 1)),
    heading_diff = ifelse(is.na(heading_diff),
                                   0, heading_diff),
    heading = cumsum(heading_diff) + heading_raw[1]
    ) |>
  select(-heading_diff) |>
  relocate(loop, .before = everything())

loops <- loopdrives

usethis::use_data(loops, overwrite = TRUE)
## ------------------------------------------------------------------

source("inst/roxify-helper.R")
roxify(loops)




