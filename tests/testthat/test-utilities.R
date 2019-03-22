context("Utilities & Tools")

test_that("df_check flags problem dataframes", {
  df <- dplyr::tibble(
    x = c(100, NA, 102:105),
    y = c(800, NA, 802:805),
    date = 1:6
  )

  # data frames with missing values
  expect_error(df_check(df))

  # data frames without regular diff
  irr_df <- na.omit(df)
  expect_error(df_check(irr_df))

  # data frames without the correct columns
  df <- dplyr::rename(AG195, xx = x)
  expect_error(df_check(df))

  # gives no error with correct columns and regular diff
  expect_silent(df_check(AG195))
})

df <- dplyr::filter(elephants, id == "AG268")
reg_df <- suppressWarnings(regularize(df, dt = 20))

test_that("regularize regularizes trajectories", {
  expect_true(diff(range(diff(reg_df$date))) == 0)
})

test_that("kalman interpolates NAs", {
  expect_true(any(is.na(reg_df$x)))
  expect_false(any(is.na(kalman(reg_df)$x)))
  expect_false(any(is.na(kalman(reg_df)$y)))
})

test_that("kalman warns when over interpolating", {
  df <- dplyr::tibble(
    x = c(100, NA, 102:105),
    y = c(800, NA, 802:805),
    date = 1:6
  )

  expect_warning(kalman(df))
  expect_silent(kalman(df, warn = F))
})

test_that("create_telemetry outputs a telemetry object", {
  expect_s4_class(create_telemetry(AG195, "+proj=utm +zone=33 +south"), "telemetry")
})
