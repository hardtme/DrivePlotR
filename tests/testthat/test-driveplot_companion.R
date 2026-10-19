test_that("dataframe causes warning", {
  expect_warning(
    driveplot_companion(shareddata = drive7, x = time_cst, y = speed_mph),
    "Converted `shareddata` to a SharedData object."
  )
})

test_that("x is not in shareddata", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_est,
      y = speed_mph
    ),
    "object 'time_est' not found"
  )
})

test_that("y is not in shareddata", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_cst,
      y = speed_kph
    ),
    "object 'speed_kph' not found"
  )
})

test_that("colorvar is not in shareddata", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  # Supplied colorvar instead of colorpalette
  expect_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_cst,
      y = speed_mph,
      colorvar = "red"
    ),
    "Do not put argument `colorvar` in quotes.\nDid you mean `colorpalette`?"
  )
  # Misspelled colorvar
  expect_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_cst,
      y = speed_mph,
      colorvar = gps_dop,
      colorpalette = "viridis"
    ),
    "object 'gps_dop' not found"
  )
})

test_that("throw error when x or y is quoted", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  # Quoted x
  expect_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = "time_cst",
      y = speed_mph
    ),
    "Do not put argument `x` in quotes."
  )
  # Quoted y
  expect_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_cst,
      y = "speed_mph"
    ),
    "Do not put argument `y` in quotes."
  )
})

test_that("throw error when x or y is NULL", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  # x is NULL
  expect_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = NULL,
      y = speed_mph
    ),
    "Argument `x` cannot be NULL."
  )
  # y is NULL
  expect_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_cst,
      y = NULL
    ),
    "Argument `y` cannot be NULL."
  )
})

test_that("throw error when colorvar is specified, but wrong colorpalette", {
  shared_drive <- crosstalk::SharedData$new(drive7)

  # Specify a single color for colorpalette along with colorvar
  expect_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_cst,
      y = speed_mph,
      colorvar = gps_pdop,
      colorpalette = "black"
    ),
    "When specifying colorvar, please use a viridis or ColorBrewer palette."
  )
  # Specify a ColorBrewer palette
  expect_no_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_cst,
      y = speed_mph,
      colorvar = gps_pdop,
      colorpalette = "Blues"
    )
  )
  # Misspell a viridis color palette
  expect_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_cst,
      y = speed_mph,
      colorvar = gps_pdop,
      colorpalette = "mamga"
    ),
    "When specifying colorvar, please use a viridis or ColorBrewer palette."
  )
  # Misspell a ColorBrewer color palette
  expect_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_cst,
      y = speed_mph,
      colorvar = gps_pdop,
      colorpalette = "Blus"
    ),
    "When specifying colorvar, please use a viridis or ColorBrewer palette."
  )
})

test_that("no error when colorpalette is specified without colorvar", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  # Specify a single color
  expect_no_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_cst,
      y = speed_mph,
      colorpalette = "black"
    )
  )
  # Specify a viridis color palette
  expect_no_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_cst,
      y = speed_mph,
      colorpalette = "plasma"
    )
  )
  # Specify a ColorBrewer color palette
  expect_no_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_cst,
      y = speed_mph,
      colorpalette = "Set2"
    )
  )
})

test_that("default viridis palette takes over when colorvar is specified
          without colorpalette", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_cst,
      y = speed_mph,
      colorvar = gps_pdop
    )
  )
})

test_that("check custom colors from user", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  drive7mod <- drive7
  drive7mod$accel_event_cat[200] <- "medium"
  drive7mod$accel_event_cat[250] <- "high"
  shared_drivemod <- crosstalk::SharedData$new(drive7mod)
  # colorvar is not specified
  expect_no_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_cst,
      y = speed_mph,
      colorpalette = c("black", "red")
    )
  )
  # numeric colorvar is specified with two colors
  expect_no_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_cst,
      y = speed_mph,
      colorvar = gyro_heading,
      colorpalette = c("black", "red")
    )
  )
  # numeric colorvar is specified with four colors
  expect_no_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_cst,
      y = speed_mph,
      colorvar = gyro_heading,
      colorpalette = c("orange", "yellow", "green", "blue")
    )
  )
  # character colorvar with more colors than non-NA values
  expect_no_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_cst,
      y = speed_mph,
      colorvar = accel_event_cat,
      colorpalette = c("orange", "yellow", "green", "blue")
    )
  )
  # character colorvar with fewer colors than non-NA values
  expect_error(
    driveplot_companion(
      shareddata = shared_drivemod,
      x = time_cst,
      y = speed_mph,
      colorvar = accel_event_cat,
      colorpalette = c("orange", "blue")
    )
  )
})

test_that("can perform operations within function arguments", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  # Modify x
  expect_no_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = as.POSIXct(time_cst, tz = "UTC"),
      y = speed_mph
    )
  )
  # Modify y
  expect_no_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_cst,
      y = speed_mph * 1.609
    )
  )
  # Modify colorvar
  expect_no_error(
    driveplot_companion(
      shareddata = shared_drive,
      x = time_cst,
      y = speed_mph,
      colorvar = gyro_heading %% 360,
      colorpalette = "viridis"
    )
  )
})
