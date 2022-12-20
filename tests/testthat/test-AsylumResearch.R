context("Asylum Research AFM image check")

filename = AFM.getSampleImages(type='ibw')

test_that("check AR AFM image exists", {
  expect_true(file.exists(filename))
})

test_that("AR: filefound", {
  expect_true(file.exists(filename))
})

test_that("AR: check version 2 channel list ", {
  h1 = read.AR_eofHeader.V2(filename)
  expect_equal(length(h1), 13)
})

test_that("AR: check AFM import ", {
  obj = AFM.import(filename)
  expect_equal(obj@x.pixels, 128)
  expect_equal(length(obj@data$z), 4)
  expect_equal( length(obj@data$z[[1]]), 128*128)
})


test_that("AR: check image roughness ", {
  d = AFM.math.params(AFM.import(filename))
  expect_equal(d$Ra, 6.365067, tolerance = 1e-5)
  expect_equal(d$Rq, 7.865308, tolerance = 1e-5)
})
