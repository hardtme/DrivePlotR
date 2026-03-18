test_that("dataframe throws error", {
  expect_error(
    driveplot(shareddata = drive7,
              x = time_cst,
              ys = speed_mph),
    "`shareddata` must be a SharedData object."
  )
})

# Test calls to driveplot_map() ----
test_that("if providing lng/lat, must provide both", {
  drive1 <- nds_data |>
    dplyr::filter(drive == 1)
  drive1shared <- crosstalk::SharedData$new(drive1)
  # lng is provided, but lat is NULL
  expect_error(
    driveplot(shareddata = drive1shared,
              lng = gps_long,
              lat = NULL,
              x = time_cst,
              ys = speed_mph),
    "If providing `lng` and `lat`, must provide both."
  )
  # lat is provided, but not lng
  expect_error(
    driveplot(shareddata = drive1shared,
              lat = gps_lat,
              x = time_cst,
              ys = speed_mph),
    "If providing `lng` and `lat`, must provide both."
  )
})

test_that("check spelling of lng/lat", {
  drive1 <- nds_data |>
    dplyr::filter(drive == 1)
  drive1shared <- crosstalk::SharedData$new(drive1)
  # Misspelled lng
  expect_error(
    driveplot(shareddata = drive1shared,
              lng = gps_lng,
              lat = gps_lat,
              x = time_cst,
              ys = speed_mph),
    "Can't find `gps_lng` in `shareddata`."
  )
  # Misspelled lat
  expect_error(
    driveplot(shareddata = drive1shared,
              lng = gps_long,
              lat = gps_lt,
              x = time_cst,
              ys = speed_mph),
    "Can't find `gps_lt` in `shareddata`."
  )
})

test_that("missing geometry column throws error when lat/lng not provided", {
  drive1 <- nds_data |>
    dplyr::filter(drive == 1)
  drive1shared <- crosstalk::SharedData$new(drive1)
  expect_error(
    driveplot(shareddata = drive1shared,
              x = time_cst,
              ys = speed_mph),
    "Can't find a geometry column and `lng` and `lat` not provided."
  )
})

test_that("throw error when geometry type isn't POINT", {
  skip_if_not_installed("sf")
  drive7_ls <- drive7 |>
    sf::st_cast("LINESTRING")
  drive7_ls_sd <- crosstalk::SharedData$new(drive7_ls)
  expect_error(
    driveplot(shareddata = drive7_ls_sd,
              x = time_cst,
              ys = speed_mph),
    "Geometry column must have type POINT."
  )
})

test_that("works for both ways to specify coordinates", {
  drive1 <- nds_data |>
    dplyr::filter(drive == 1)
  drive1shared <- crosstalk::SharedData$new(drive1)
  shared_drive <- crosstalk::SharedData$new(drive7)
  # Specify lat/lng
  expect_no_error(
    driveplot(shareddata = drive1shared,
              lng = gps_long,
              lat = gps_lat,
              x = time_cst,
              ys = speed_mph)
  )

  # Provide a geometry column with type POINT
  expect_no_error(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = speed_mph)
  )
})

test_that("provide lat/lng but not x and ys", {
  drive1 <- nds_data |>
    dplyr::filter(drive == 1)
  drive1shared <- crosstalk::SharedData$new(drive1)
  expect_error(
    driveplot(shareddata = drive1shared,
              lng = gps_long,
              lat = gps_lat),
    "Can't find `<empty>` in `shareddata`."
  )
})

test_that("provide geometry column but not x and ys", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot(shareddata = shared_drive),
    "Can't find `<empty>` in `shareddata`."
  )
})

# Test calls to driveplot_companions() ----
test_that("x not provided", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot(shareddata = shared_drive, ys = speed_mph),
    "Can't find `<empty>` in `shareddata`."
  )
})

test_that("ys not provided", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot(shareddata = shared_drive, x = time_cst),
    "Can't find `<empty>` in `shareddata`."
  )
})

test_that("misspelled x", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot(shareddata = shared_drive,
              x = time_est,
              ys = speed_mph),
    "Can't find `time_est` in `shareddata`."
  )
})

test_that("misspelled first y", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = speed_kph),
    "Can't find `speed_kph` in `shareddata`."
  )
})

test_that("misspelled second y", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = c(speed_mph, gps_headin)),
    "Can't find `gps_headin` in `shareddata`."
  )
})

test_that("misspelled third y", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = c(speed_mph, gps_heading, gyro_headin)),
    "Can't find `gyro_headin` in `shareddata`."
  )
})

test_that("misspelled fourth y", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = c(speed_mph, gps_heading,
                     gyro_heading, gps_dop)),
    "Can't find `gps_dop` in `shareddata`."
  )
})

test_that("specify only one y without c() or list()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = speed_mph)
  )
})

test_that("specify only one y with c()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = c(speed_mph))
  )
})

test_that("specify only one y with list()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = list(speed_mph))
  )
})

test_that("specify two ys with c()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = c(speed_mph, gyro_heading))
  )
})

test_that("specify two ys with list()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = list(speed_mph, gyro_heading))
  )
})

test_that("specify five ys with c()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_warning(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = c(speed_mph, gyro_heading,
                     gps_heading, gps_pdop,
                     gps_hdop))
  )
})

test_that("specify five ys with list()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_warning(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = list(speed_mph, gyro_heading,
                        gps_heading, gps_pdop,
                        gps_hdop))
  )
})

test_that("provide two ys and one label", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = list(speed_mph, gyro_heading),
              ylabels = "Speed"),
    "If providing `ylabels`, `ys` and `ylabels` must be the same length."
  )
})

test_that("provide only one y label without c() or list()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = speed_mph,
              ylabels = "Speed")
  )
})

test_that("provide only one y label with c()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = speed_mph,
              ylabels = c("Speed"))
  )
})

test_that("provide only one y label with list()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = speed_mph,
              ylabels = list("Speed"))
  )
})

test_that("specify two y labels with c()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = c(speed_mph, gyro_heading),
              ylabels = c("Speed", "Gyro Heading"))
  )
})

test_that("specify two y labels with list()", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_no_error(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = c(speed_mph, gyro_heading),
              ylabels = list("Speed", "Gyro Heading"))
  )
})

test_that("specify ylabels without quotes", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = c(speed_mph, gyro_heading),
              ylabels = list(Speed, Heading))
  )
})

test_that("NA ylabel", {
  shared_drive <- crosstalk::SharedData$new(drive7)
  expect_error(
    driveplot(shareddata = shared_drive,
              x = time_cst,
              ys = c(speed_mph, gyro_heading),
              ylabels = NA),
    "`ylabels` cannot be NA."
  )
})
