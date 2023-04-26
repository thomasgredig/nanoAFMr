#' Reads AFM object from SQL DB
#' 
#' @description
#' AFMdata S4 images can be read from an SQL database using their unique ID.
#' Since the database can only store tables, the image is split into two tables,
#' one for the data and one for the metadata that contains the description, units
#' and size of the image. This function puts the image back together using specific
#' tables in the database; this function only works with \code{\link{AFM.writeDB}}
#' 
#' @author Thomas Gredig
#'
#' @param mydb database connection from DBI package
#' @param ID unique object file ID
#' @param verbose if \code{TRUE} outputs verbose comments
#'
#' @importFrom DBI dbReadTable dbGetQuery dbListTables dbConnect dbDisconnect
#'
#' @returns S4 AFM object from nanoAFMr package
#' 
#' @seealso [\code{\link{AFM.writeDB}}]
#'
#' @examples
#' fname = file.path(tempdir(), "afm.sqlite")
#' mydb <- DBI::dbConnect(RSQLite::SQLite(), fname)
#' afmFile = AFM.getSampleImages(type='tiff')[1]
#' a = AFM.import(afmFile)
#' AFM.writeDB(a, mydb, 45, verbose=FALSE)
#' b = AFM.readDB(mydb, 45)
#' DBI::dbDisconnect(mydb)
#' plot(b)
#'
#' @export
AFM.readDB <- function(mydb, ID, verbose=TRUE) {
  # define table names in DB
  myTableAFMname = paste0('afm',ID)
  myTableDataName = paste0('afmData')
  
  # check that database has the tables for this AFM object
  t1 <- dbListTables(mydb)
  if ((myTableAFMname %in% t1) & (myTableDataName %in% t1)) {

    dbGetQuery(mydb, paste('SELECT * FROM ',
                           myTableDataName,'WHERE ID=',ID)) -> dfData
    dfData = dfData[1,]
    # load data
    if (dfData$instrument == "Park") {
      dfAFM <- dbReadTable(mydb, myTableAFMname)
      z = as.vector(dfAFM$z)
      zList = list(z)
    } else if (dfData$instrument == "Cypher") {
      dfAFM <- dbReadTable(mydb, myTableAFMname)
      zList = list(dfAFM$V1, dfAFM$V2, dfAFM$V3, dfAFM$V4)
    }
    
    # create AFMdata S4 class
    obj = AFMdata(
      data = list(z=zList),
      channel = unlist(strsplit(dfData$channel,',')[[1]]),
      x.conv = dfData$x.conv,
      y.conv = dfData$y.conv,
      x.pixels = dfData$x.pixels,
      y.pixels = dfData$y.pixels,
      z.conv = 1,
      z.units = unlist(strsplit(dfData$z.units,',')[[1]]),
      instrument = dfData$instrument,
      history = '',
      description = dfData$description,
      fullFilename = dfData$fullFilename
    )
  } else {
    warning("AFM object is not found in database.")
    obj = NULL
  }
  obj
}
