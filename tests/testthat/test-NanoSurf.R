context("NanoSurf AFM image check")


filename = AFM.getSampleImages(type='nid')

test_that("check NanoSurf AFM image exists", {
  expect_true(file.exists(filename))
})


test_that("check NID file reads correctly", {
  d = NID.checkFile(filename)
  expect_equal(d,0)
})


test_that("use general AFM reading function to read NanoSurf file", {
  d = AFM.import(filename)
  expect_equal(d@x.pixels,128)
  expect_equal(d@y.pixels,128)
  expect_equal(d@x.nm,10000)
  expect_equal(d@y.nm,10000)
  expect_equal(summary(d)$z.min, c(-2.9418e-7,2.8534e-2,-3.01208e-7, 2.89917e-02), tolerance = 1e-5)
  expect_equal(d@instrument,"NanoSurf")
})


test_that("NanoSurf image roughness check", {
  d = AFM.math.params(AFM.import(filename))
  expect_equal(d$Ra, 23.67e-9, tolerance = 1e-4)
  expect_equal(d$Rq, 31.85e-9, tolerance = 1e-4)
})


