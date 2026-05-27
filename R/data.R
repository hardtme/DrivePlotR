#' Blackbox-Driving Data for a number of drives
#'
#' This dataset contains 28 drives ranging in length
#' from just over 2 hours to just 45 secs.
#' @format A tibble with 23187 rows and 35 columns
#' \describe{
#'   \item{drive}{integer, identifier for a drive; a drive is defined as the
#'   time between starting the car and turning it off again.}
#'   \item{time_utc}{POSIXct time stamp for each observation in UTC}
#'   \item{time_cst}{POSIXct time stamp for each observation in Central time}
#' }
#' GPS-based measurements for each observation:
#' \describe{
#'   \item{gps_long}{numeric, geographic longitude in degrees with CRS WGS84}
#'   \item{gps_lat}{numeric, geographic latitude in degrees with CRS WGS84}
#'   \item{gps_elevation}{numeric, elevation above sea-level (in m)}
#'   \item{gps_heading}{numeric, continuity-corrected direction of the car
#'   (in degrees with north at 0 degrees and increasing clockwise) as measured
#'   by GPS. The continuity correction takes the initial GPS heading and adds
#'   the cumulative sum of the changes in GPS heading for the subsequent
#'   observations.}
#'   \item{gps_fix}{numeric, value specifying the adequacy of the GPS fix, where
#'   0 = no fix, 1 = time only, 2 = 2D fix, 3 = 3D fix. A gps_fix of 3 is
#'   needed for good GPS data.}
#'   \item{gps_sats}{integer, the number of satellites covering a position.}
#'   \item{gps_pdop}{numeric, position dilution of precision, an overall GPS
#'   data quality measure (lower is better)}
#'   \item{gps_hdop}{numeric, horizontal dilution of precision, a GPS quality
#'   measure for latitude/longitude data (lower is better)}
#'   \item{gps_vdop}{numeric, vertical dilution of precision, a GPS quality
#'   measure for elevation data (lower is better)}
#'   \item{speed_mph}{numeric, speed of the vehicle (in miles per hour)}
#'   \item{speed_source}{character, the source of the reported speed, either
#'   "gps" or "obd"}
#'   \item{distance_miles}{numeric, distance traveled during a drive
#'   (in miles)}
#'   }
#' Measurements by the on-board gyroscope:
#'   \describe{
#'   \item{accel_x}{numeric, measure of vehicle acceleration along the
#'   front-back (longitudinal) axis (in g)}
#'   \item{accel_y}{numeric, measure of vehicle acceleration along the
#'   right-left (lateral) axis (in g)}
#'   \item{accel_z}{numeric, measure of acceleration of the vehicle in the
#'   up-down plane (normal/yaw axis) (in g)}
#'   \item{accel_event}{numeric, sequential (over time) variable marking when
#'   consecutive acceleration values (absolute value of accel_x/y) exceed a
#'   threshold of 0.35g for a minimum of 1 second}
#'   \item{accel_event_cat}{character, severity categorization of acceleration event
#'   into low (0.35-0.44g), medium (0.45-0.59g), and high (>0.6g)}
#'   \item{gyro_x}{numeric, angular rotation rate about the right-to-left
#'   axis (in degrees/sec)}
#'   \item{gyro_y}{numeric, angular rotation rate about the
#'   forward-to-backward axis (in degrees/sec)}
#'   \item{gyro_z}{numeric, angular rotation rate about the up-to-down axis (in
#'   degrees/sec)}
#'   \item{gyro_heading}{numeric, calculated heading utilizing compass data
#'   and gyro data (in degrees with north at 0 degrees and increasing
#'   clockwise). Note that we use non-standard congruence classes for continuity
#'   of consecutive values. Due to errors in the raw gyro heading, we use the
#'   initial GPS heading value as the initial heading value and then subtract
#'   the cumulative sum of gyro_z for the subsequent observations to calculate
#'   gyro_heading.}
#'   \item{grav_x}{numeric, lateral gravity (in g)}
#'   \item{grav_y}{numeric, longitudinal gravity (in g)}
#'   \item{grav_z}{numeric, gravity (up-down) (in g)}
#'   \item{roll}{numeric, angle about the forward-to-backward axis.
#'   0 = level, positive = tilted to the left (in degrees)}
#'   \item{pitch}{numeric, angle about the left-to-right axis where
#'   0 = level, positive = tilted downward (i.e., vehicle going downhill) (in
#'   degrees)}
#'   \item{engine_rpm}{numeric, current engine revolutions per minute value}
#'   \item{engine_throttle}{numeric, engine throttle as a percentage (0-100%)}
#'   \item{cumulative_drive_dist_mi}{numeric, the cumulative distance driven in
#'   miles at each observation}
#'   \item{gps_minute}{factor, the minute component of the timestamp for each
#'   observation (00 to 59)}
#' }
#' Raw heading measurements:
#' \describe{
#'   \item{gps_heading_raw}{numeric, raw GPS heading without continuity
#'   correction (in degrees)}
#'   \item{gyro_heading_raw}{numeric, raw gyro heading without continuity
#'   correction (in degrees)}
#' }
"nds_data"

#' Blackbox-Driving Data for one drive
#'
#' This dataset contains drive 7 from the dataset nds_data.
#' @format A tibble with 321 rows and 34 columns
#' \describe{
#'   \item{drive}{integer, identifier for a drive; a drive is defined as the
#'   time between starting the car and turning it off again.}
#'   \item{time_utc}{POSIXct time stamp for each observation in UTC}
#'   \item{time_cst}{POSIXct time stamp for each observation in Central time}
#' }
#' GPS-based measurements for each observation:
#' \describe{
#'   \item{gps_elevation}{numeric, elevation above sea-level (in m)}
#'   \item{gps_heading}{numeric, continuity-corrected direction of the car
#'   (in degrees with north at 0 degrees and increasing clockwise) as measured
#'   by GPS. The continuity correction takes the initial GPS heading and adds
#'   the cumulative sum of the changes in GPS heading for the subsequent
#'   observations.}
#'   \item{gps_fix}{numeric, value specifying the adequacy of the GPS fix, where
#'   0 = no fix, 1 = time only, 2 = 2D fix, 3 = 3D fix. A gps_fix of 3 is
#'   needed for good GPS data.}
#'   \item{gps_sats}{integer, the number of satellites covering a position.}
#'   \item{gps_pdop}{numeric, position dilution of precision, an overall GPS
#'   data quality measure (lower is better)}
#'   \item{gps_hdop}{numeric, horizontal dilution of precision, a GPS quality
#'   measure for latitude/longitude data (lower is better)}
#'   \item{gps_vdop}{numeric, vertical dilution of precision, a GPS quality
#'   measure for elevation data (lower is better)}
#'   \item{speed_mph}{numeric, speed of the vehicle (in miles per hour)}
#'   \item{speed_source}{character, the source of the reported speed, either
#'   "gps" or "obd"}
#'   \item{distance_miles}{numeric, distance traveled during a drive
#'   (in miles)}
#'   }
#' Measurements by the on-board gyroscope:
#'   \describe{
#'   \item{accel_x}{numeric, measure of vehicle acceleration along the
#'   front-back (longitudinal) axis (in g)}
#'   \item{accel_y}{numeric, measure of vehicle acceleration along the
#'   right-left (lateral) axis (in g)}
#'   \item{accel_z}{numeric, measure of acceleration of the vehicle in the
#'   up-down plane (normal/yaw axis) (in g)}
#'   \item{accel_event}{numeric, sequential (over time) variable marking when
#'   consecutive acceleration values (absolute value of accel_x/y) exceed a
#'   threshold of 0.35g for a minimum of 1 second}
#'   \item{accel_event_cat}{character, severity categorization of acceleration event
#'   into low (0.35-0.44g), medium (0.45-0.59g), and high (>0.6g)}
#'   \item{gyro_x}{numeric, angular rotation rate about the right-to-left
#'   axis (in degrees/sec)}
#'   \item{gyro_y}{numeric, angular rotation rate about the
#'   forward-to-backward axis (in degrees/sec)}
#'   \item{gyro_z}{numeric, angular rotation rate about the up-to-down axis (in
#'   degrees/sec)}
#'   \item{gyro_heading}{numeric, calculated heading utilizing compass data
#'   and gyro data (in degrees with north at 0 degrees and increasing
#'   clockwise). Note that we use non-standard congruence classes for continuity
#'   of consecutive values. Due to errors in the raw gyro heading, we use the
#'   initial GPS heading value as the initial heading value and then subtract
#'   the cumulative sum of gyro_z for the subsequent observations to calculate
#'   gyro_heading.}
#'   \item{grav_x}{numeric, lateral gravity (in g)}
#'   \item{grav_y}{numeric, longitudinal gravity (in g)}
#'   \item{grav_z}{numeric, gravity (up-down) (in g)}
#'   \item{roll}{numeric, angle about the forward-to-backward axis.
#'   0 = level, positive = tilted to the left (in degrees)}
#'   \item{pitch}{numeric, angle about the left-to-right axis where
#'   0 = level, positive = tilted downward (i.e., vehicle going downhill) (in
#'   degrees)}
#'   \item{engine_rpm}{numeric, current engine revolutions per minute value}
#'   \item{engine_throttle}{numeric, engine throttle as a percentage (0-100%)}
#'   \item{cumulative_drive_dist_mi}{numeric, the cumulative distance driven in
#'   miles at each observation}
#'   \item{gps_minute}{factor, the minute component of the timestamp for each
#'   observation (00 to 59)}
#' }
#' Raw heading measurements:
#' \describe{
#'   \item{gps_heading_raw}{numeric, raw GPS heading without continuity
#'   correction (in degrees)}
#'   \item{gyro_heading_raw}{numeric, raw gyro heading without continuity
#'   correction (in degrees)}
#' }
#' GPS coordinates in a simple features column:
#'  \describe{
#'  \item{geometry}{list-column, simple features geometry column with
#'  geometry type POINT and CRS WGS84}
#'  }
"drive7"
