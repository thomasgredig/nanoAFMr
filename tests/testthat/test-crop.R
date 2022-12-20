test_that("AFM image cropping", {
  d <- AFM.artificialImage(type='calibration', width=100, height=100, verbose=FALSE)
  d1 = AFM.crop(d, x0=45, y0=45, width.pixels = 50, height.pixels = 50)
  q = summary(d1)
  expect_equal(q$resolution, "50 x 50")
  expect_equal(q$size, "490 x 490 nm")

  # try cropping beyond margins  
  d1 = AFM.crop(d, x0=85, y0=45, width.pixels = 50, height.pixels = 50)
  q = summary(d1)
  expect_equal(q$resolution, "15 x 50")
  expect_equal(q$size, "140 x 490 nm")
})
