context("test-population")

test_that("dist_map works", {
  # ggrepel means that the dist_map labels are placed a little differently each time. causing spurious failures
  # vdiffr::expect_doppelganger(
  #   "dist_map",
  means <- dist_map(elephants, "+proj=utm +zone=33 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  # )
  expect_is(means, "sf")
})


test_that("timeline plot works", {
  vdiffr::expect_doppelganger(
    "timeline",
    plot_timeline(elephants)
  )
})
