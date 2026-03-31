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
    "argument is missing, with no default"
  )
})

test_that("ys not provided", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst),
    "argument is missing, with no default"
  )
})

test_that("misspelled x", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_est,
                         ys = speed_mph),
    "object 'time_est' not found"
  )
})

test_that("quoted x", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = "time_est",
                         ys = speed_mph),
    "Do not put argument `x` in quotes."
  )
})

test_that("misspelled first y", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = speed_kph),
    "object 'speed_kph' not found"
  )
})

test_that("quoted y", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_est,
                         ys = "speed_mph"),
    "Do not put argument `y` in quotes."
  )
})

test_that("misspelled second y", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gps_headin)),
    "object 'gps_headin' not found"
  )
})

test_that("quoted second y", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, "gps_heading")),
    "Do not put argument `y` in quotes."
  )
})

test_that("misspelled third y", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  # Misspelled third y
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gps_heading, gyro_headin)),
    "object 'gyro_headin' not found"
  )
})

test_that("quoted third y", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gps_heading, "gyro_heading")),
    "Do not put argument `y` in quotes."
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
    "object 'gps_dop' not found"
  )
})

test_that("quoted fourth y", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph, gps_heading,
                                gyro_heading, "gps_pdop")),
    "Do not put argument `y` in quotes."
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

test_that("can modify function arguments", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  # Modify x
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = as.POSIXct(time_cst, tz = "UTC"),
                         ys = c(speed_mph, gyro_heading))
  )
  # Modify one y
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph * 1.609, gyro_heading))
  )
  # Modify two ys
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = c(speed_mph * 1.609, gyro_heading %% 360))
  )
  # Modify colorvar
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = time_cst,
                         ys = list(speed_mph, gps_pdop),
                         colorvar = gyro_heading %% 360,
                         colorpalette = "viridis")
  )
  # Many modifications
  expect_no_error(
    driveplot_companions(shareddata = shared_drive,
                         x = as.POSIXct(time_cst, tz = "UTC"),
                         ys = list(speed_mph * 1.609, gyro_heading %% 360),
                         colorvar = paste("Minute:", gps_minute),
                         colorpalette = "viridis")
  )
})
