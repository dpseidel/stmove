context("Rolling & Interval statistics")

AG <- head(AG195, 100)
AG1hr <- dl(AG) %>% adehabitatLT::subsample(dt = 1, units = "hour") %>% ld() %>% dplyr::select(x, y, date)
AG4hr <- dl(AG) %>% adehabitatLT::subsample(dt = 4, units = "hour") %>% ld() %>% dplyr::select(x, y, date)

test_that("rolling stats warns with dt > 4 hours", {
  expect_warning(rolling_stats(AG4hr))
  expect_silent(rolling_stats(AG4hr, n_roll = 4))
})

test_that("n_roll works", {
  roll <- rolling_stats(AG)
  roll2 <- rolling_stats(AG, n_roll = 2)
  expect_equal(sum(is.na(roll$mean_dist)), 8)
  expect_equal(sum(is.na(roll2$mean_dist)), 1)
})

test_that("default n_roll is 3hr for dt < 1hr and 6 hr for dt >= 1hr", {
  roll <- rolling_stats(AG)
  roll1 <- rolling_stats(AG1hr)
  expect_equal(sum(is.na(roll$mean_dist)), 8) # 3hr rolls 9 20 min fixes, 8 NAs
  expect_equal(sum(is.na(roll1$mean_dist)), 5) # 6hr rolls 6 1hr fixes, 5 NA
})

test_that("rolling stats outputs consistent column names", {
  # rolling stats outputs consistent column names:
  expect_named(rolling_stats(AG), c(
    "x", "y", "date", "dx", "dy", "dist",
    "dt", "R2n", "abs.angle", "rel.angle",
    "mean_dist", "sd_dist", "acf_dist",
    "mean_ang", "sd_ang", "acf_ang", "ccf"
  ))
})

AG24 <- dl(AG195) %>%
  adehabitatLT::subsample(dt = 24, units = "hour") %>%
  ld() %>%
  dplyr::select(x, y, date)

test_that("diurnal function warns when dt is too big", {
  expect_silent(interval_stats(AG))
  expect_warning(interval_stats(AG24))
})

test_that("diurnal function cuts at 0:00 and 12:00", {
  expect_equal(unique(lubridate::hour(interval_stats(AG)$interval_start)), c(0, 12))

  # cuts into 12hr intervals
  expect_equal(nrow(interval_stats(AG1hr)), round(nrow(AG1hr) / 12))
})

test_that("lunar intervals work", {
  # with small interval at end?
  df <- dplyr::filter(AG195, date < ymd("2010-1-13"))
  expect_equal(as.character(interval_stats(df, "lunar")$interval_start), c("2010-01-01", "2010-01-12"))

  # at beginning
  df <- dplyr::filter(AG195, date > ymd("2010-1-11"))
  expect_equal(as.character(interval_stats(df, "lunar")$interval_start)[1:2], c("2010-01-11", "2010-01-12"))
})

test_that("season intervals work", {
  df <- head(AG195, 366)
  df$date <- seq(lubridate::ymd_hms("2012-5-15 12:00:00"),
    lubridate::ymd_hms("2013-5-15 12:00:00"),
    length.out = 366
  )

  # seas vector format is checked
  expect_error(interval_stats(df, type = "seasonal", seas = c("2012-06-01", "12-01-2012")))

  # when seas vector dates are internal to traj, the earlier fixes are included in separate interval
  expect_equal(nrow(interval_stats(df, type = "seasonal", seas = c("2012-06-01", "2012-12-01"))), 3)

  # when seas vector dates are outside traj range
  expect_equal(nrow(interval_stats(df,
    type = "seasonal",
    seas = c("2012-05-01", "2012-12-01", "2013-06-01")
  )), 2)
})
