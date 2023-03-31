test_that("Height height correlation function: check roughness", {
  filename = AFM.getSampleImages(type='tiff')
  a = AFM.import(filename)
  a = AFM.flatten(a)
  h1 = AFM.hhcf(a, numIterations = 1e5, degRes=1000, dataOnly = TRUE)
  # AFM.hhcf(a, numIterations = 4e4, degRes=1000, addFit=TRUE)

  expect_equal(mean(h1$g) / (2*AFM.math.params(a)$Rq^2), 1, tolerance = 2e-2)
})

test_that("HHCF should output fit parameters", {
  filename = AFM.getSampleImages(type='tiff')
  a = AFM.import(filename)
  h1 = AFM.hhcf(a, numIterations = 1e5, degRes=1000, allResults = TRUE)
  fit = h1$fitParams
  expect_equal(fit$xi,40.0, tolerance = 0.02)
  expect_equal(fit$H, 1.73, tolerance = 0.05)
})
