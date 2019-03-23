context("test-population")

test_that("dist_map works", {
  vdiffr::expect_doppelganger(
     "dist_map",
  means <- dist_map(elephants, label = F,
                    "+proj=utm +zone=33 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  )

  expect_is(means, "sf")
})


test_that("timeline plot works", {
  vdiffr::expect_doppelganger(
    "timeline",
    plot_timeline(elephants)
  )
})
