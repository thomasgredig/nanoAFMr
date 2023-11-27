test_that("AFM Ratings read and write", {
  fname = file.path(tempdir(), "afm00.sqlite")
  df = data.frame(
    ID = c(7,8),
    user = c("user1","user1"),
    quality = c(3.5,4.)
  )
  AFM.writeRatings(fname, df, verbose=FALSE)
  df2 <- AFM.readRatings(fname, verbose=FALSE)
 
  expect_equal(nrow(df), nrow(df2))
})
