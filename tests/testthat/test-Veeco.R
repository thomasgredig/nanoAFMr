context("Veeco AFM image check")

filename = AFM.getSampleImages(type='veeco')

test_that("check Veeco AFM image exists", {
  expect_true(file.exists(filename))
})

test_that("check if Veeco AFM file reads", {
  d = read.Nanoscope_file(filename)
  expect_equal(nrow(d),512*512)
})



test_that("use general AFM reading function to read file", {
  d = AFM.read(filename)
  expect_equal(length(d), 6)
  expect_equal(nrow(d),512*512)
  expect_equal(max(d$z),3121)
})


test_that("Veeco image roughness check", {
  d = AFM.math.params(AFM.import(filename))
  expect_equal(d$Ra, 3.05, tolerance = 1e-3)
  expect_equal(d$Rq, 10.18, tolerance = 1e-3)
})

