context("Load Park AFM Spectroscopy Data")

test_that("", {
  a = AFM.import(AFM.getSampleImages('force'))
  specHeader = AFM.specHeader(a)
  expect_equal(nrow(specHeader), 8)
  
  specData = AFM.specData(a)
  expect_equal(nrow(specData), 1024*7)
})
