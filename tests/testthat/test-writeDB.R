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
})
