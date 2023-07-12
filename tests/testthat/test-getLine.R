test_that("getLine produces proper cut-offs", {
  # generate a 10x10 image
  a = AFM.artificialImage(type="calibration", verbose=FALSE)
  
  # extract data points for line 4
  a3 = AFM.getLine(a, y=4)
  # plot(a3, addLines = TRUE)
  df4a <- AFM.getLine(a3, y=4, dataOnly = TRUE)

  # extract all data points with getLine
  a2 = a
  for(j in 1:a@y.pixels) {
    a2 = AFM.getLine(a2, y=j)
  }
  AFM.linePlot(a2, dataOnly = TRUE) -> df
  
  # extract data points for line 4
  df4b = subset(df,type==4)

  # make sure AFM.linePlot and AFM.getLine agree
  expect_equal(df4a$z,df4b$z)
  
  # expect first two rows from bottom to be substrate
  expect_true(mean(subset(df, type<=2)$z) < 4)
  
  # expect next three rows from bottom to be mostly calibration sample
  expect_equal(mean(subset(df, type>=3 & type<=5)$z),72, tolerance=0.2)
  
  # expect left and right side to be substrate
  expect_true(mean(subset(df, x==0)$z) < 5)
  expect_true(mean(subset(df, x>=900)$z) < 5)
  
  # check diagonal
  # a = AFM.artificialImage(12,12, type="calibration", invert = TRUE, verbose=FALSE)
  # plot(a)
  # summary(a)
  # 
  # AFM.lineProfile(a, 1,1, a@x.pixels, a@y.pixels, unitPixels = TRUE, verbose=TRUE) -> a4
  # a4@data$line
  # AFM.lineProfile(a, 0,0, 1000,1000, unitPixels = FALSE) -> a4
  # t(summary(a4))
  # plot(a4, addLines = TRUE)
})
