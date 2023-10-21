test_that("Check artificial AFM image", {
  a = AFM.artificialImage(height=11, width=10, verbose=FALSE)
  expect_equal(summary(a)$resolution[1], "10 x 11")
})


test_that("Flattening", {
  a = AFM.artificialImage(type='gradient', verbose=FALSE)
  a2 = AFM.flatten(a)
  expect_equal(sum(a2@data$z[[1]]),0)
})


test_that("Flattening with zShift", {
  a = AFM.artificialImage(type='gradient', verbose=FALSE)
  a2 = AFM.flatten(a, zShift=20)
  # Make sure entire image is shifted upwards
  expect_equal(sum(a2@data$z[[1]])/a2@x.pixels/a2@y.pixels,20)
})


test_that("Flattening with Line by Line and Slope", {
  a = AFM.artificialImage(100,100, type='calibration', verbose=FALSE)
  a2 = AFM.flatten(a, method='lineByLine')
  expect_equal(sum(a2@data$z[[1]])/a2@x.pixels/a2@y.pixels, 28.3, tolerance = 0.01)
  
  df = AFM.flattenLine(a, tau_lower=0.05)
  expect_equal(mean(df$m), 0, tolerance = 1e-4)
  a2 = AFM.flatten(a, method='slope', slope = df)
  expect_equal(sum(a2@data$z[[1]])/a2@x.pixels/a2@y.pixels, 28.3, tolerance = 0.01)
})


test_that("Check artificial AFM image", {
  a = AFM.artificialImage(height=256, width=256, verbose=FALSE, imageWidth = 1000, imageHeight = 500)
  expect_equal(summary(a)$size[1], "996 x 498 nm")
})

test_that("Check flattening is not inverting the image", {
  a= AFM.artificialImage(width=50, height=50, type='calibration',verbose=FALSE)
  AFM.histogram(a, binNo=100, dataOnly = TRUE)-> d1
  a2=AFM.flatten(a)
  AFM.histogram(a2, binNo=100, dataOnly = TRUE)-> d2

  # calibration grid is mostly background, so should be larger than 0.5
  expect_true(sum(d1$zDensity[1:50])>0)
  expect_equal(sum(d1$zDensity[1:50]), sum(d2$zDensity[1:50]))
})



test_that("Set Line Data in AFM Image", {
  a = AFM.artificialImage(height=10, width=10, verbose=FALSE)
  b = AFM.getLine(a, yPixel = 1, dataOnly = TRUE)

  lineData = rep(b$z[1], length(b$x))
  a1 <- AFM.setLine(a, lineData, yPixel = 10)
  
  roughness.prev = AFM.math.params(a)$Ra
  roughness.after = AFM.math.params(a1)$Ra
  
  # new image roughness should be lower
  expect_true(roughness.after < roughness.prev)
})
