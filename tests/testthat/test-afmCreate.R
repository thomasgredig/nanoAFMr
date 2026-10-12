test_that("AFM create", {
  a <- AFM.artificialImage(8,5,-2,2, type="gradient", verbose=FALSE)
  d <- AFM.raster(a)
  d$z = d$z * 4
  # create a new AFM image from a data frame
  AFM.create(d, description="5x5 artificial image", channelName = "z") -> a2
  expect_equal(a2@x.nm, a@x.nm)
  expect_equal(a2@x.conv, a@x.conv)
})
