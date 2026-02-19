test_that("dataframe throws error", {
  expect_error(
    driveplot_companions(shareddata = drive7,
                         x = time_cst,
                         ys = speed_mph),
    "`shareddata` must be a SharedData object."
  )
})

test_that("check provided ys", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  # Misspelled x
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                        x = time_est,
                        ys = speed_mph),
    "Can't find column `time_est` in `shareddata`."
  )
  # Misspelled first y
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                        x = time_cst,
                        ys = speed_kph),
    "Can't find column `speed_kph` in `shareddata`."
  )
  # Misspelled second y
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gps_headin)),
    "Can't find column `gps_headin` in `shareddata`."
  )
  # Misspelled third y
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gps_heading, gyro_headin)),
    "Can't find column `gyro_headin` in `shareddata`."
  )
  # Misspelled fourth y
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gps_heading,
                                gyro_heading, gps_dop)),
    "Can't find column `gps_dop` in `shareddata`."
  )
  # Specify only one y without c() or list()
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = speed_mph)
  )
  # Specify only one y with c()
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph))
  )
  # Specify only one y with list()
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = list(speed_mph))
  )
  # Specify two ys with c()
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gyro_heading))
  )
  # Specify two ys with list()
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = list(speed_mph, gyro_heading))
  )
  # Specify three ys with c()
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gyro_heading, gps_heading))
  )
  # Specify three ys with list()
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = list(speed_mph, gyro_heading, gps_heading))
  )
  # Specify four ys with c()
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gyro_heading,
                                gps_heading, gps_pdop))
  )
  # Specify four ys with list()
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = list(speed_mph, gyro_heading,
                                   gps_heading, gps_pdop))
  )
  # Specify five ys with c()
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gyro_heading,
                                gps_heading, gps_pdop,
                                gps_hdop)),
    "`ys` must be a vector or list with 1 to 4 `shareddata` column names."
  )
  # Specify five ys with list()
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = list(speed_mph, gyro_heading,
                                   gps_heading, gps_pdop,
                                   gps_hdop)),
    "`ys` must be a vector or list with 1 to 4 `shareddata` column names."
  )
})

  test_that("check ylabels", {
    shared_drive <- crosstalk::SharedData$new(drive7)
    # Provide two ys and one label
    expect_error(
      driveplot_companions(shareddata = shared_drive,
                           x = time_cst,
                           ys = list(speed_mph, gyro_heading),
                           ylabels = "Speed"),
      "If providing `ylabels`, `ys` and `ylabels` must be the same length."
    )

    # Provide only one y label without c() or list()
    expect_no_error(
      driveplot_companions(shareddata = shared_drive,
                           x = time_cst,
                           ys = speed_mph,
                           ylabels = "Speed")
    )
    # Provide only one y label with c()
    expect_no_error(
      driveplot_companions(shareddata = shared_drive,
                           x = time_cst,
                           ys = speed_mph,
                           ylabels = c("Speed"))
    )
    # Provide only one y label with list()
    expect_no_error(
      driveplot_companions(shareddata = shared_drive,
                           x = time_cst,
                           ys = speed_mph,
                           ylabels = list("Speed"))
    )
    # Specify two y labels with c()
    expect_no_error(
      driveplot_companions(shareddata = shared_drive,
                           x = time_cst,
                           ys = c(speed_mph, gyro_heading),
                           ylabels = c("Speed", "Gyro Heading"))
    )
    # Specify two y labels with list()
    expect_no_error(
      driveplot_companions(shareddata = shared_drive,
                           x = time_cst,
                           ys = c(speed_mph, gyro_heading),
                           ylabels = list("Speed", "Gyro Heading"))
    )
    # Specify ylabels without quotes
    expect_error(
      driveplot_companions(shareddata = shared_drive,
                           x = time_cst,
                           ys = c(speed_mph, gyro_heading),
                           ylabels = list(Speed, Heading))
    )
  })


