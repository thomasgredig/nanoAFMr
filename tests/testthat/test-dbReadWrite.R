test_that("Read and write of AFM image to database", {
  # write an AFM image to SQL database, then read back
  fname = file.path(tempdir(), "afm4.sqlite")
  mydb <- dbConnect(RSQLite::SQLite(), fname)
  
  # save a Park TIFF AFM image
  afmFile = AFM.getSampleImages(type='tiff')[1]
  a1 = AFM.import(afmFile)
  expect_type(a1, "S4")
  AFM.writeDB(a1, mydb, 45, verbose=FALSE)
  
  # save a Cypher AFM image
  afmFile = AFM.getSampleImages(type='ibw')[1]
  b1 = AFM.import(afmFile)
  expect_type(b1, "S4")
  AFM.writeDB(b1, mydb, 46, verbose=FALSE)
  
  # read the images back from the database
  a2 = AFM.readDB(mydb, 45)
  b2 = AFM.readDB(mydb, 46)
  dbDisconnect(mydb)
  
  # compare the images
  expect_equal(summary(a1), summary(a2))
  expect_equal(summary(b1), summary(b2))
  
})
