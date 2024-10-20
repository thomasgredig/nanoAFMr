test_that("check particle size", {
  a <- AFM.artificialImage(21,21, type="calibration", verbose=FALSE)
  q <- AFM.particleSize(a, floorHeight = 10)
  pTable = q$ps.table

  expect_equal(nrow(pTable),1)
  expect_equal(pTable$area.pixel, 12*12)
})
