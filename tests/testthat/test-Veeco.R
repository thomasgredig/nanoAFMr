context("Veeco AFM image check")
filename = AFM.getSampleImages(type='veeco')

test_that("check Veeco AFM image exists", {
  expect_true(file.exists(filename))
})



test_that("use general AFM reading function to read file", {
  d = AFM.import(filename)

  expect_equal(summary(d)$resolution,"256 x 256")
  expect_equal(max(d@data$z[[1]]), 10.46655, tolerance=1e-4)
})


test_that("Veeco image roughness check", {
  d = AFM.math.params(AFM.import(filename))
  expect_true(inherits(d, "AFMmath"))
  
  expect_equal(d$Ra, 1.983689, tolerance = 1e-3)
  expect_equal(d$Rq, 2.458281, tolerance = 1e-3)
})

