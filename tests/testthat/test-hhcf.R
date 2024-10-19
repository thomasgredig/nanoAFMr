filename = AFM.getSampleImages(type='tiff')
a = AFM.import(filename)
a = AFM.flatten(a)

test_that("Height height correlation function: check roughness", {
  h1 = AFM.hhcf(a, numIterations = 3e4, degRes=1000, dataOnly = TRUE, randomSeed = 45792231)
  expect_equal(mean(h1$g) / (2*AFM.math.params(a)$Rq^2), 1.00, tolerance = 2e-3)
})



test_that("Height height correlation function: reproducibility", {
  h1 = AFM.hhcf(a, numIterations = 1e4, degRes=1000, randomSeed = 45792231, allResults = TRUE)
  h2 = AFM.hhcf(a, numIterations = 1e4, degRes=1000, randomSeed = 45792231, allResults = TRUE)
  
  expect_equal(h1$fitParams, h2$fitParams)
  expect_equal(AFM.math.params(a)$Rq, h1$fitParams$sigma, tolerance = 1e-2)
  expect_equal(40.67411, h1$fitParams$xi, tolerance = 1e-4)
  expect_equal(0.8572, h1$fitParams$Hurst, tolerance = 1e-4)
})



