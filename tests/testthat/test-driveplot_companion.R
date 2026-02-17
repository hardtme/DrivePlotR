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
    "Can't find column `time_est` in `shareddata`."
  )
})

test_that("y is not in shareddata", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_cst,
                        y = speed_kph),
    "Can't find column `speed_kph` in `shareddata`."
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
    "Can't find column `\"red\"` in `shareddata`."
  )
  # Misspelled colorvar
  expect_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_cst,
                        y = speed_mph,
                        colorvar = gps_dop),
    "Can't find column `gps_dop` in `shareddata`."
  )
})

test_that("throw error when colorvar is specified, but incorrect colorpalette", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  # Specify colorvar, but not colorpalette
  expect_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_cst,
                        y = speed_mph,
                        colorvar = gps_pdop),
    'When specifying colorvar, please use
         colorpalette = "viridis", "magma", "inferno", or "plasma".'
  )
  # Specify a single color for colorpalette
  expect_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_cst,
                        y = speed_mph,
                        colorvar = gps_pdop,
                        colorpalette = "black"),
    'When specifying colorvar, please use
         colorpalette = "viridis", "magma", "inferno", or "plasma".'
  )
  # Specify a different color palette, such as a ColorBrewer palette
  expect_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_cst,
                        y = speed_mph,
                        colorvar = gps_pdop,
                        colorpalette = "Blues"),
    'When specifying colorvar, please use
         colorpalette = "viridis", "magma", "inferno", or "plasma".'
  )
  # Misspell a viridis color palette
  expect_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_cst,
                        y = speed_mph,
                        colorvar = gps_pdop,
                        colorpalette = "mamga"),
    'When specifying colorvar, please use
         colorpalette = "viridis", "magma", "inferno", or "plasma".'
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
