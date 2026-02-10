test_that("dataframe throws error", {
  expect_error(
    driveplot_map(shareddata = drive7),
    "`shareddata` must be a SharedData object."
  )
})
