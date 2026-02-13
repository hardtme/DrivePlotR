test_that("dataframe throws error", {
  expect_error(
    driveplot_map(shareddata = drive7),
    "`shareddata` must be a SharedData object."
  )
})

test_that("if providing lng/lat, must provide both", {
  drive1 <- nds_data |>
    dplyr::filter(drive == 1)
  drive1shared <- crosstalk::SharedData$new(drive1)
  # lng is provided, but not lat
  expect_error(
    driveplot_map(shareddata = drive1shared,
                  lng = gps_long,
                  lat = NULL),
    "If providing `lng` and `lat`, must provide both."
  )
  # lat is provided, but not lng
  expect_error(
    driveplot_map(shareddata = drive1shared,
                  lng = NULL,
                  lat = gps_lat),
    "If providing `lng` and `lat`, must provide both."
  )
})

test_that("check spelling of lng/lat", {
  drive1 <- nds_data |>
    dplyr::filter(drive == 1)
  drive1shared <- crosstalk::SharedData$new(drive1)
  # Misspelled lng
  expect_error(
    driveplot_map(shareddata = drive1shared,
                  lng = gps_lng,
                  lat = gps_lat),
    "Can't find column `gps_lng` in `shareddata`."
  )
  # Misspelled lat
  expect_error(
    driveplot_map(shareddata = drive1shared,
                  lng = gps_long,
                  lat = gps_lt),
    "Can't find column `gps_lt` in `shareddata`."
  )
})

test_that("missing geometry column throws error when lat/lng not provided", {
  drive1 <- nds_data |>
    dplyr::filter(drive == 1)
  drive1shared <- crosstalk::SharedData$new(drive1)
  expect_error(
    driveplot_map(shareddata = drive1shared),
    "Can't find a geometry column and `lng` and `lat` not provided."
  )
})

test_that("throw error when geometry type isn't POINT", {
  skip_if_not_installed("sf")
  drive7_ls <- drive7 |>
    sf::st_cast("LINESTRING")
  drive7_ls_sd <- crosstalk::SharedData$new(drive7_ls)
  expect_error(
    driveplot_map(shareddata = drive7_ls_sd),
    "Geometry column must have type POINT."
  )
})

test_that("mapheight is in CSS units", {
  drive1 <- nds_data |>
    dplyr::filter(drive == 1)
  drive1shared <- crosstalk::SharedData$new(drive1)
  expect_error(
    driveplot_map(shareddata = drive1shared,
                  mapheight = "100"),
    "Must specify `mapheight` in CSS units, e.g., '98vh'"
  )
  expect_error(
    driveplot_map(shareddata = drive1shared,
                  mapheight = 100),
    "Must specify `mapheight` in CSS units, e.g., '98vh'"
  )
})
