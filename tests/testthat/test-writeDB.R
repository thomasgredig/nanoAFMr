context("SQL database read/write")

test_that("check writing and reading of SQL DB", {
  fname = file.path(tempdir(), "afm.sqlite")
  mydb <- DBI::dbConnect(RSQLite::SQLite(), fname)
  afmFile = AFM.getSampleImages(type='tiff')[1]
  a = AFM.import(afmFile)
  AFM.writeDB(a, mydb, 45, verbose=FALSE)
  q = AFM.readDB(mydb)
  b = AFM.readDB(mydb, 45)
  DBI::dbDisconnect(mydb)
  
  expect_equal(q, 45)
  expect_equal(summary(b)$objectect,"Park image")
  expect_equal(summary(b)$resolution,"256 x 256")
  expect_equal(summary(b)$channel,"Topography")
  expect_equal(summary(b)$z.units,"nm")
  
  # remove image
  AFM.add2DB(fname, IDs=c(-45), verbose=FALSE)
  mydb <- DBI::dbConnect(RSQLite::SQLite(), fname)
  expect_warning( AFM.readDB(mydb, 45) )
  DBI::dbDisconnect(mydb)
  
})



