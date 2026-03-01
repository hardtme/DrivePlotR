test_that("dataframe throws error", {
  expect_error(
    driveplot_companions(shareddata = drive7,
                         x = time_cst,
                         ys = speed_mph),
    "`shareddata` must be a SharedData object."
  )
})

test_that("x not provided", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         ys = speed_mph),
  "Can't find column `<empty>` in `shareddata`.")
})

test_that("ys not provided", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst),
    "Can't find `<empty>` in `shareddata`.")
})

test_that("misspelled x", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                        x = time_est,
                        ys = speed_mph),
    "Can't find column `time_est` in `shareddata`."
  )
})

test_that("misspelled first y", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                        x = time_cst,
                        ys = speed_kph),
    "Can't find `speed_kph` in `shareddata`."
  )
})

test_that("misspelled second y", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gps_headin)),
    "Can't find `gps_headin` in `shareddata`."
  )
})

test_that("misspelled third y", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  # Misspelled third y
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gps_heading, gyro_headin)),
    "Can't find `gyro_headin` in `shareddata`."
  )
})

test_that("misspelled fourth y", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  # Misspelled fourth y
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gps_heading,
                                gyro_heading, gps_dop)),
    "Can't find `gps_dop` in `shareddata`."
  )
})

test_that("specify only one y without c() or list()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = speed_mph)
  )
})

test_that("specify only one y with c()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  # Specify only one y with c()
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph))
  )
})

test_that("specify only one y with list()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  # Specify only one y with list()
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = list(speed_mph))
  )
})

test_that("specify two ys with c()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gyro_heading))
  )
})

test_that("specify two ys with list()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = list(speed_mph, gyro_heading))
  )
})

test_that("specify three ys with c()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gyro_heading, gps_heading))
  )
})

test_that("specify three ys with list()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = list(speed_mph, gyro_heading, gps_heading))
  )
})

test_that("specify four ys with c()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gyro_heading,
                                gps_heading, gps_pdop))
  )
})

test_that("specify four ys with list()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = list(speed_mph, gyro_heading,
                                   gps_heading, gps_pdop))
  )
})

test_that("specify five ys with c()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_warning(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gyro_heading,
                                gps_heading, gps_pdop,
                                gps_hdop))
  )
})

test_that("specify five ys with list()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_warning(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = list(speed_mph, gyro_heading,
                                   gps_heading, gps_pdop,
                                   gps_hdop))
  )
})

test_that("provide two ys and one label", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                          x = time_cst,
                          ys = list(speed_mph, gyro_heading),
                          ylabels = "Speed"),
    "If providing `ylabels`, `ys` and `ylabels` must be the same length."
  )
})

test_that("provide only one y label without c() or list()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                          x = time_cst,
                          ys = speed_mph,
                          ylabels = "Speed")
  )
})

test_that("provide only one y label with c()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                          x = time_cst,
                          ys = speed_mph,
                          ylabels = c("Speed"))
    )
})

test_that("provide only one y label with list()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                          x = time_cst,
                          ys = speed_mph,
                          ylabels = list("Speed"))
    )
})

test_that("specify two y labels with c()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                          x = time_cst,
                          ys = c(speed_mph, gyro_heading),
                          ylabels = c("Speed", "Gyro Heading"))
    )
})

test_that("specify two y labels with list()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gyro_heading),
                         ylabels = list("Speed", "Gyro Heading"))
    )
})

test_that("specify ylabels without quotes", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                          x = time_cst,
                          ys = c(speed_mph, gyro_heading),
                          ylabels = list(Speed, Heading))
  )
})

test_that("NA ylabels", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gyro_heading),
                         ylabels = NA),
    "`ylabels` cannot be NA."
  )
})



