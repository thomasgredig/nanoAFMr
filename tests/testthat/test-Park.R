context("Park AFM image check")

filename = AFM.getSampleImages(type='tiff')

test_that("check Park AFM image exists", {
  expect_true(file.exists(filename))
})



test_that("Park image import", {
  afmobj = AFM.import(filename)
  expect_equal(afmobj@x.pixels, 256)
})


test_that("Park AFM image roughness check", {
  d = AFM.math.params(AFM.import(filename))
  expect_equal(d$Ra, 0.3788, tolerance = 1e-4)
  expect_equal(d$Rq, 0.6351, tolerance = 1e-4)
})
