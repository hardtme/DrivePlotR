test_that("dataframe throws error", {
  expect_error(
    driveplot_companions(shareddata = drive7, x = time_cst, y1 = speed_mph),
    "`shareddata` must be a SharedData object."
  )
})

test_that("check provided variables", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  # Misspelled x
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                        x = time_est,
                        y1 = speed_mph),
    "Can't find column `time_est` in `shareddata`."
  )
  # Misspelled y1
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                        x = time_cst,
                        y1 = speed_kph),
    "Can't find column `speed_kph` in `shareddata`."
  )
  # Misspelled y2
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         y1 = speed_mph,
                         y2 = gps_headin),
    "Can't find column `gps_headin` in `shareddata`."
  )
  # Misspelled y3
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         y1 = speed_mph,
                         y2 = gps_heading,
                         y3 = gyro_headin),
    "Can't find column `gyro_headin` in `shareddata`."
  )
  # Misspelled y4
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         y1 = speed_mph,
                         y2 = gps_heading,
                         y3 = gyro_heading,
                         y4 = gps_dop),
    "Can't find column `gps_dop` in `shareddata`."
  )
  # Specify y1 only
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         y1 = speed_mph)
  )
  # Specify y1 and y4 only
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         y1 = speed_mph,
                         y4 = gps_pdop),
    "Must provide `y2` before providing `y3` or `y4`."
  )
  # Specify y1 and y3 only
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         y1 = speed_mph,
                         y3 = gyro_heading),
    "Must provide `y2` before providing `y3` or `y4`."
  )
  # Specify y1, y3, and y4
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         y1 = speed_mph,
                         y3 = gyro_heading,
                         y4 = gps_pdop),
    "Must provide `y2` before providing `y3` or `y4`."
  )
  # Specify y1 and y2 only
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         y1 = speed_mph,
                         y2 = gps_heading)
  )
  # Specify y1, y2, and y4
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         y1 = speed_mph,
                         y2 = gps_heading,
                         y4 = gps_pdop),
    "Must provide `y3` before providing `y4`."
  )
  # Specify y1, y2, and y3
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         y1 = speed_mph,
                         y2 = gps_heading,
                         y3 = gyro_heading)
  )
  # Specify y1, y2, y3, and y4
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         y1 = speed_mph,
                         y2 = gps_heading,
                         y3 = gyro_heading,
                         y4 = gps_pdop)
  )
})
