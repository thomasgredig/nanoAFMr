test_that("plot.AFMdata returns a ggplot object for supported graph types", {
  d <- AFM.import(AFM.getSampleImages(type = "ibw"))
  
  for (gt in c(1, 2, 3, 4, 5)) {
    p <- plot(d, graphType = gt, quiet=TRUE)
    expect_s3_class(p, "ggplot")
  }
})
