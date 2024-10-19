context("SQL database read/write")

test_that("check writing and reading of SQL DB", {
  sql_filename = tempfile(fileext="sqlite")
  mydb <- DBI::dbConnect(RSQLite::SQLite(), sql_filename)
  afmFile = AFM.getSampleImages(type='tiff')[1]
  a = AFM.import(afmFile)
  AFM.writeDB(a, mydb, 45, verbose=FALSE)
  q = AFM.readDB(mydb)
  b = AFM.readDB(mydb, 45)
  DBI::dbDisconnect(mydb)
  
  # chck that the image can be found in the database
  expect_equal(q, 45)
  expect_equal(summary(b)$objectect,"Park image")
  expect_equal(summary(b)$resolution,"256 x 256")
  expect_equal(summary(b)$channel,"Topography")
  expect_equal(summary(b)$z.units,"nm")
  
  # remove image
  mydb <- DBI::dbConnect(RSQLite::SQLite(), sql_filename)
  AFM.writeDB(NULL, mydb, 45, verbose=FALSE)
  expect_warning(AFM.readDB(mydb, 45))
  q = AFM.readDB(mydb)
  DBI::dbDisconnect(mydb)
  
  # the database should be empty now
  expect_equal(length(q),0)
})



