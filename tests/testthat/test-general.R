filename = AFM.getSampleImages(type='ibw')
afmd = AFM.import(filename[1])

test_that("AFMdata object", {
  a = AFMdata(instrument='Cypher')
  expect_true(isS4(a))
})


test_that("AFM.flatten: flattening image", {
  filename = AFM.getSampleImages('nid')
  d = AFM.import(filename)
  d2 = AFM.flatten(d)
  expect_equal(nrow(d@data),nrow(d2@data))
})

test_that("AFM.getSampleImages: find sample files", {
  file.list = AFM.getSampleImages()
  expect_equal(length(file.list),6)
  expect_true(file.exists(AFM.getSampleImages(type='ibw')))
})

test_that("line Profile", {
  AFM.lineProfile(afmd, 0,0, 2000,2000) -> d1
  AFM.lineProfile(d1, 0,0, 100,2500) -> d2
  # plot(d2, addLines = TRUE)
  q = AFM.linePlot(d2, dataOnly=TRUE)
  expect_equal(sum(q$z), -159.4684, tolerance = 1e-4)
  expect_equal(nlevels(as.factor(q$type)), 2)
})

test_that("getLine for particular pixel", {
  # choose a random line
  selLine = floor(runif(1,min=1,max=afmd@y.pixels))
  afmd2 = AFM.getLine(afmd, selLine)
  q = AFM.linePlot(afmd2, dataOnly = TRUE)
  expect_equal(nrow(q), afmd2@x.pixels)
})


test_that("print AFMdata", {
  expect_output(print(afmd), "Cypher AFM image")
  expect_output(print(afmd), "HeightRetrace")
})


test_that("summary AFMinfo", {
  h = AFMinfo(AFM.getSampleImages(type='ibw'))
  expect_output(summary(h), "Scan Angle")
})



test_that("Graphing ggplot2 AFM image", {
  p = plot(afmd, quiet = TRUE)
  expect_equal(class(p$layers[[1]]$geom), c("GeomRaster", "Geom","ggproto","gg"))
  p = plot(afmd, graphType = 2, quiet = TRUE)
  expect_equal(class(p$layers[[1]]$geom), c("GeomRaster", "Geom","ggproto","gg"))
  p = plot(afmd, graphType = 3, quiet = TRUE)
  expect_equal(class(p$layers[[1]]$geom), c("GeomRaster", "Geom","ggproto","gg"))
})




test_that("test histogram for image and data", {
  expect_equal( sum(AFM.histogram(afmd, dataOnly=TRUE)$zDensity), 1)
  p = AFM.histogram(afmd)
  expect_equal(class(p$layers[[1]]$geom), c("GeomBar", "GeomRect", "Geom","ggproto","gg"))
})


test_that("valid AFM files", {
  file.list = AFM.getSampleImages()
  for(filename in file.list) {
    expect_true(AFM.isFileValid(filename))
  }
  expect_true(!AFM.isFileValid("random-nonexistant-file.txt"))
})


test_that("AFM history", {
  fname = AFM.getSampleImages('nid')
  a = AFM.import(fname)
  a = AFM.flatten(a)
  a = AFM.getLine(a, xPixel=3)
  hs = AFM.history(a)
  expect_equal(length(hs), 4)
})


test_that("Byte to Double Conversion", {
  expect_equal(byte2double(c(0,0,0,0,0,0,8,64)),3)
  expect_equal(int2double(c(0,0,0,64*256+8)),3)
  expect_equal(int2double(c(0,0,0,16368)),1)
})


test_that("Check AFM file extension", {
  expect_true(AFM.isFileValid(AFM.getSampleImages('veeco')))
  fileTest = file.path(tempdir(), 'afm.023')
  write.csv(data.frame(a=1), fileTest)
  expect_true(AFM.isFileValid(fileTest))
})

