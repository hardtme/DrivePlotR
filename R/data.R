#' Blackbox-Driving Data for a number of drives
#'
#' This dataset contains 28 drives ranging in length from just over 2 hours to just 45 secs.
#' @format A tibble with 23187 rows and 37 columns
#' \describe{
#'   \item{drive}{integer, identifier for a drive; a drive is defined as the time between starting the car and turning it off again.}
#'   \item{time_utc}{POSIXct time stamp for each observation in UTC}
#'   \item{time_cst}{POSIXct time stamp for each observation in Central time}
#' }
#' GPS based measurements for each observation:
#' \describe{
#'   \item{gps_long}{numeric, geographic longitude}
#'   \item{gps_lat}{numeric, geographic latidude}
#'   \item{gps_elevation}{numeric elevation above sea-level (in m)}
#'   \item{gps_heading}{numeric direction of the car (in degrees)}
#'   \item{gps_fix}{numeric }
#'   \item{gps_sats}{integer of the number of satellits covering a position.}
#'   \item{gps_pdop}{numeric total dilution of precision }
#'   \item{gps_hdop}{numeric horizontal dilution of precision}
#'   \item{gps_vdop}{numeric vertical dilution of precision}
#'   \item{speed_mph}{numeric}
#'   \item{speed_source}{character one of "gps" or "obd"}
#'   \item{distance_miles}{numeric distance travelled during a drive (in miles)}
#'   }
#' Measurements by the on-board gyroscope:
#'   \describe{
#'   \item{accel_x}{numeric, forward acceleration (in g)}
#'   \item{accel_y}{numeric, sidewards acceleration (in g)}
#'   \item{accel_z}{numeric, upwards acceleration (in g)}
#'   \item{accel_event}{binary, did an acceleration event occur? 0/1}
#'   \item{accel_event_cat}{character, categorization of acceleration event in low, medium, and high.}
#'   \item{gyro_x}{numeric, Angular rotation rate about the right-to-left axis (in degrees/sec).}
#'   \item{gyro_y}{numeric, Angular rotation rate about the forward-to-backward axis.}
#'   \item{gyro_z}{numeric, Angular rotation rate about the up-to-down axis.}
#'   \item{gyro_heading}{numeric, Calculated heading utilizing compass data and gyro data (in degrees). Note that we use non-standard congruence classes for continuity of consecutive values. }
#'   \item{grav_x}{numeric, lateral gravity (in g)}
#'   \item{grav_y}{numeric, longitudinal gravity}
#'   \item{grav_z}{numeric, gravity (up-down)}
#'   \item{roll}{numeric, Angle about the forward-to-backward axis. 0 = level, positive = tilted to the left}
#'   \item{pitch}{numeric, Angle about the left-to-right axis. 0 = level, positive = tilted downward (i.e., vehicle going downhill).}
#'   \item{engine_rpm}{numeric, current engine revolutions per minute value.}
#'   \item{engine_throttle}{numeric, percentage of engine throttle (0-100).}
#'   \item{cumulative_drive_dist_mi}{numeric}
#'   \item{gps_minute}{factor}
#'   \item{gps_heading_raw}{numeric, gps heading modulo 360 }
#'   \item{gyro_heading_raw}{numeric, gyro heading modulo 360}
#' }
"nds_data"
