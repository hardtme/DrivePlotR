test_that("dataframe throws error", {
  expect_error(
    driveplot(shareddata = drive7, x = time_cst, y1 = speed_mph),
    "`shareddata` must be a SharedData object."
  )
})
