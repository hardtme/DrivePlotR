test_that("dataframe throws error", {
  expect_error(
    driveplot_companion(shareddata = drive7, x = time_cst, y = speed_mph),
    "`shareddata` must be a SharedData object."
  )
})

test_that("x is not in shareddata", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_est,
                        y = speed_mph),
    "object 'time_est' not found"
  )
})

test_that("y is not in shareddata", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_cst,
                        y = speed_kph),
    "object 'speed_kph' not found"
  )
})

test_that("colorvar is not in shareddata", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  # Supplied colorvar instead of colorpalette
  expect_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_cst,
                        y = speed_mph,
                        colorvar = "red"),
    "Do not put argument `colorvar` in quotes.
    Did you mean to use `colorpalette` instead?"
  )
  # Misspelled colorvar
  expect_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_cst,
                        y = speed_mph,
                        colorvar = gps_dop,
                        colorpalette = "viridis"),
    "object 'gps_dop' not found"
  )
})

test_that("throw error when x or y is quoted", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  # Quoted x
  expect_error(
    driveplot_companion(shareddata = shared_drive,
                        x = "time_cst",
                        y = speed_mph),
    "Do not put argument `x` in quotes."
  )
  # Quoted y
  expect_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_cst,
                        y = "speed_mph"),
    "Do not put argument `y` in quotes."
  )
})

test_that("throw error when colorvar is specified, but wrong colorpalette", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  # Construct comparison error message
  palettes <- leaflet_color_palettes()
  error_msg <- paste(paste(shQuote(palettes[-length(palettes)]),
                           collapse = ", "), "or",
                     shQuote(palettes[length(palettes)]))
  compare_msg <- paste0("When specifying colorvar, please use
         colorpalette = ", error_msg)

  # Specify colorvar, but not colorpalette
  expect_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_cst,
                        y = speed_mph,
                        colorvar = gps_pdop),
    compare_msg
  )
  # Specify a single color for colorpalette
  expect_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_cst,
                        y = speed_mph,
                        colorvar = gps_pdop,
                        colorpalette = "black"),
    compare_msg
  )
  # Specify a different color palette, such as a ColorBrewer palette
  expect_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_cst,
                        y = speed_mph,
                        colorvar = gps_pdop,
                        colorpalette = "Blues"),
    compare_msg
  )
  # Misspell a viridis color palette
  expect_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_cst,
                        y = speed_mph,
                        colorvar = gps_pdop,
                        colorpalette = "mamga"),
    compare_msg
  )
})

test_that("no error when colorpalette is specified without colorvar", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  # Specify a single color
  expect_no_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_cst,
                        y = speed_mph,
                        colorpalette = "black")
  )
  # Specify a viridis color palette
  expect_no_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_cst,
                        y = speed_mph,
                        colorpalette = "plasma")
  )
})

test_that("can perform operations within function arguments", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  # Modify x
  expect_no_error(
    driveplot_companion(shareddata = shared_drive,
                        x = as.POSIXct(time_cst, tz = "UTC"),
                        y = speed_mph)
  )
  # Modify y
  expect_no_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_cst,
                        y = speed_mph * 1.609)
  )
  # Modify colorvar
  expect_no_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_cst,
                        y = speed_mph,
                        colorvar = gyro_heading %% 360,
                        colorpalette = "viridis")
  )
})
