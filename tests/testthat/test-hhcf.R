filename = AFM.getSampleImages(type='tiff')
a = AFM.import(filename)
a = AFM.flatten(a)

test_that("Height height correlation function: check roughness", {
  h1 = AFM.hhcf(a, numIterations = 1e5, degRes=1000, dataOnly = TRUE)
  # AFM.hhcf(a, numIterations = 4e4, degRes=1000, addFit=TRUE)

  expect_equal(mean(h1$g) / (2*AFM.math.params(a)$Rq^2), 1, tolerance = 2e-2)
})



test_that("Height height correlation function: reproducibility", {
  h1 = AFM.hhcf(a, numIterations = 1e4, degRes=1000, randomSeed = 45792231, allResults = TRUE)
  # AFM.hhcf(a, numIterations = 4e4, degRes=1000, addFit=TRUE)
  m1 =  h1$fitParams
  h2 = AFM.hhcf(a, numIterations = 1e4, degRes=1000, randomSeed = 45792231, allResults = TRUE)
  m2 =  h1$fitParams
  expect_equal(m1,m2)
})
