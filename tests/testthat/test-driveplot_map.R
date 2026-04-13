test_that("dataframe causes warning", {
  expect_warning(
    driveplot_map(shareddata = drive7),
    "Converted `shareddata` to a SharedData object."
  )
})

test_that("if providing lng/lat, must provide both", {
  drive1 <- nds_data |>
    dplyr::filter(drive == 1)
  drive1shared <- crosstalk::SharedData$new(drive1)
  # lng is provided, but not lat
  expect_error(
    driveplot_map(
      shareddata = drive1shared,
      lng = gps_long,
      lat = NULL
    ),
    "If providing `lng` and `lat`, must provide both."
  )
  # lat is provided, but not lng
  expect_error(
    driveplot_map(
      shareddata = drive1shared,
      lng = NULL,
      lat = gps_lat
    ),
    "If providing `lng` and `lat`, must provide both."
  )
})

test_that("check spelling of lng/lat", {
  drive1 <- nds_data |>
    dplyr::filter(drive == 1)
  drive1shared <- crosstalk::SharedData$new(drive1)
  # Misspelled lng
  expect_error(
    driveplot_map(
      shareddata = drive1shared,
      lng = gps_lng,
      lat = gps_lat
    ),
    "Can't find `gps_lng` in `shareddata`."
  )
  # Misspelled lat
  expect_error(
    driveplot_map(
      shareddata = drive1shared,
      lng = gps_long,
      lat = gps_lt
    ),
    "Can't find `gps_lt` in `shareddata`."
  )
})

test_that("throw error when colorvar is misspecified", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot_map(
      shareddata = shared_drive,
      colorvar = "red"
    ),
    "Do not put argument `colorvar` in quotes.\nDid you mean `colorpalette`?"
  )
})

test_that("default viridis palette takes over when colorvar is specified
          without colorpalette", {
            shared_drive <- crosstalk::SharedData$new(drive7)
            expect_no_error(
              driveplot_map(
                shareddata = shared_drive,
                colorvar = gps_pdop
    )
  )
})

test_that("specify a viridis color palette without specifying colorvar", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot_map(
      shareddata = shared_drive,
      colorpalette = "magma"
    )
  )
})

test_that("missing sf geometry column throws error when
          lat/lng not provided and not inferred", {
  drive1 <- nds_data |>
    dplyr::filter(drive == 1)
  drive1shared <- crosstalk::SharedData$new(drive1)
  expect_error(
    driveplot_map(shareddata = drive1shared),
    "Unable to derive points for map. Likely causes:
          couldn't infer latitude/longitude columns or
          sf geometry column does not have type POINT."
  )
})

test_that("don't throw error when lat/lng can be inferred", {
  drive1 <- nds_data |>
    dplyr::filter(drive == 1) |>
    dplyr::mutate(latitude = gps_lat,
                  longitude = gps_long)
  drive1shared <- crosstalk::SharedData$new(drive1)
  expect_message(
    driveplot_map(shareddata = drive1shared),
  'Assuming "longitude" and "latitude" are longitude and latitude, respectively'
  )
})

test_that("throw error when sf column geometry type isn't POINT", {
  skip_if_not_installed("sf")
  drive7_ls <- drive7 |>
    sf::st_cast("LINESTRING")
  drive7_ls_sd <- crosstalk::SharedData$new(drive7_ls)
  expect_error(
    driveplot_map(shareddata = drive7_ls_sd),
    "Unable to derive points for map. Likely causes:
          couldn't infer latitude/longitude columns or
          sf geometry column does not have type POINT."
  )
})

test_that("mapheight is in CSS units", {
  drive1 <- nds_data |>
    dplyr::filter(drive == 1)
  drive1shared <- crosstalk::SharedData$new(drive1)
  expect_error(
    driveplot_map(
      shareddata = drive1shared,
      lng = gps_long,
      lat = gps_lat,
      mapheight = "100"
    ),
    "Must specify `mapheight` in CSS units, e.g., '98vh'"
  )
  expect_error(
    driveplot_map(
      shareddata = drive1shared,
      lng = gps_long,
      lat = gps_lat,
      mapheight = 100
    ),
    "Must specify `mapheight` in CSS units, e.g., '98vh'"
  )
})
