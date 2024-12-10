library(tidyverse)
library(readxl)
library(gpsdriving)
library(sf)

nds_data <- read_xlsx(here::here("data-raw/nds_data.xlsx"))
nds_data <- nds_data %>% select(-subj, -file, -device)

nds_data <- nds_data %>% mutate(time_cst = ymd_hms(time_cst, tz = "US/Central"),
                     gps_minute = as.factor(minute(time_cst)))


nds_data <- nds_data %>%
  group_by(drive) %>%
  arrange() %>%
  mutate(
  gps_heading_raw = gps_heading,
  gps_heading_diff = angle_dist(gps_heading, lag(gps_heading,1)),
  gps_heading_diff = ifelse(is.na(gps_heading_diff), 0, gps_heading_diff),
#  gps_heading_diff_dampen = ifelse(between(gps_heading_diff,-20,20), gps_heading_diff, sign(gps_heading_diff)*20),
  gps_heading = cumsum(gps_heading_diff),
#  gps_heading_corrected = cumsum(gps_heading_diff_dampen),
  gyro_heading_raw = gyro_heading,
  gyro_heading_diff = angle_dist(gyro_heading, lag(gyro_heading,1)),
  gyro_heading_diff = ifelse(is.na(gyro_heading_diff), 0, gyro_heading_diff),
  gyro_heading = cumsum(gyro_heading_diff)
)

nds_sf <- nds_data %>% st_as_sf(coords = c("gps_long", "gps_lat"),
                                  crs = "WGS84")

usethis::use_data(nds_data, overwrite = TRUE)
## ------------------------------------------------------------------

source("inst/roxify-helper.R")
roxify(nds_data)
