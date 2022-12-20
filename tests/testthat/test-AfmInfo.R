test_that("AFMinfo", {
  h = AFMinfo(AFM.getSampleImages(type='ibw'))
  expect_equal(h$resFrequency, 257190.3, tolerance = 1e-6)
  expect_equal(h$widthPixel, 128)
  expect_true(nchar(h$note)>5)

  h = AFMinfo(AFM.getSampleImages(type='tiff'))
  expect_equal(h$resFrequency, 280655, tolerance = 1e-6)
  expect_equal(h$scanRate.Hz, 1.248, tolerance = 1e-3)
  # expect_true(nchar(h$note)>5)
})
