context("Distributions")


test_that("dist functions return a plot and numeric vector", {
  vdiffr::expect_doppelganger("turning angle dist", {
    ta <- head(ta_dist(AG195))
  })
  # expect_equal(length(ta), 1)
  # expect_equal(class(ta)[1], "ltraj")
  expect_equal(class(ta), "numeric")

  vdiffr::expect_doppelganger("step length dist", {
    ss <- head(ss_dist(AG195))
  })
  # expect_equal(length(ss), 1)
  # expect_equal(class(ss)[1], "ltraj")
  expect_equal(class(ta), "numeric")
})
