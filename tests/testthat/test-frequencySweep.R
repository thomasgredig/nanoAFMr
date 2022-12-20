context("Frequency Sweep")

filesAFM = AFM.getSampleImages()
freqAFM = filesAFM[sapply(filesAFM, function(x) { (AFM.dataType(AFM.import(x)) == 'frequency') })]

test_that("Check for 1 Frequency Sweep file", {
  expect_equal(length(freqAFM), 1)
})

test_that("Check summary for Frequency Sweep file", {
  a = AFM.import(freqAFM)
  expect_equal(summary(a)$resolution, "301")
})

test_that("Raster data from Frequency Sweep file", {
  a = AFM.import(freqAFM)
  d = AFM.raster(a)
  expect_equal(sum(d$z.V), 19.24835, tolerance = 1e-5)
  expect_equal(sum(d$freq.Hz), 54009935, tolerance = 1e-5)
})
