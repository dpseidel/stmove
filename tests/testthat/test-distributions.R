context("Distributions")


test_that("dist functions return a plot and a length 1 ltraj", {
  vdiffr::expect_doppelganger("turning angle dist", {
    ta <- ta_dist(AG195)
  })
  expect_equal(length(ta), 1)
  expect_equal(class(ta)[1], "ltraj")


  vdiffr::expect_doppelganger("step length dist", {
    ss <- ss_dist(AG195)
  })
  expect_equal(length(ss), 1)
  expect_equal(class(ss)[1], "ltraj")
})
