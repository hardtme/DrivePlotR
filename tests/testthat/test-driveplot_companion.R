test_that("dataframe throws error", {
  expect_error(
    driveplot_companion(shareddata = drive7, x = time_cst, y = speed_mph),
    "`shareddata` must be a SharedData object."
    )
})
