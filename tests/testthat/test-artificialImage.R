test_that("Check artificial AFM image", {
  a = AFM.artificialImage(height=11, width=10, verbose=FALSE)
  expect_equal(summary(a)$resolution[1], "10 x 11")
})


test_that("Flattening", {
  a = AFM.artificialImage(type='gradient', verbose=FALSE)
  a2 = AFM.flatten(a)
  expect_equal(sum(a2@data$z[[1]]),0)
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
