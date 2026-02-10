test_that("dataframe throws error", {
  expect_error(
    driveplot_companion(shareddata = drive7, x = time_cst, y = speed_mph),
    "`shareddata` must be a SharedData object."
    )
})

test_that("x column is not in shareddata", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_est,
                        y = speed_mph),
    "Can't find column `x` in `shareddata`."
  )
})

test_that("y column is not in shareddata", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_companion(shareddata = shared_drive,
                        x = time_cst,
                        y = speed_kph),
    "Can't find column `y` in `shareddata`."
  )
})
