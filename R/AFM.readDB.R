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
#' @param ID unique object file ID, if not provided, will return all available IDs in database
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
#' cat("Available IDs in database: ",AFM.readDB(mydb))
#' b = AFM.readDB(mydb, 45)
#' DBI::dbDisconnect(mydb)
#' plot(b)
#'
#' @export
AFM.readDB <- function(mydb, ID = NA, verbose=TRUE) {
  # define table names in DB
  myTableAFMname = paste0('afm',ID)
  myTableDataName = paste0('afmData')
  t1 <- DBI::dbListTables(mydb)
  
  # check if list of available IDs should be returned?
  if (is.na(ID)) {
    # return IDs of AFM images
    if (myTableDataName %in% t1) {
      return(as.numeric(gsub('afm','',t1[!t1=='afmData' & !t1=='afmRating' & !t1=='sqlHistory'])))
    } else {
      stop("Database is not an AFM SQL database.")
    }
  }
  
  # check that database has the tables for this AFM object
  if ((myTableAFMname %in% t1) & (myTableDataName %in% t1)) {

    DBI::dbGetQuery(mydb, paste('SELECT * FROM ',
                           myTableDataName,'WHERE ID=',ID)) -> dfData
    dfData = dfData[1,]
    channelData = unlist(strsplit(dfData$channel,',')[[1]])
    # load data
    if (dfData$instrument == "Park") {
      dfAFM <- DBI::dbReadTable(mydb, myTableAFMname)
      z = as.vector(dfAFM$z)
      zList = list(z)
    } else if (dfData$instrument == "Cypher") {
      dfAFM <- dbReadTable(mydb, myTableAFMname)
      # find out how many?
      if (length(channelData)==4) {
        zList = list(dfAFM$V1, dfAFM$V2, dfAFM$V3, dfAFM$V4)
      } else if (length(channelData)==5) {
        zList = list(dfAFM$V1, dfAFM$V2, dfAFM$V3, dfAFM$V4, dfAFM$V5)
      } else if (length(channelData)==6) {
        zList = list(dfAFM$V1, dfAFM$V2, dfAFM$V3, dfAFM$V4, dfAFM$V5, dfAFM$V6)
      } else if (length(channelData)==1) {
        zList = list(dfAFM$V1)
      }
    }
    
    str_creation_date = ""
    if ("date" %in% names(dfData)) {
      # some older versions did not save the date
      str_creation_date = dfData$date 
    } 
    
    
    # create AFMdata S4 class
    obj = AFMdata(
      data = list(z=zList),
      channel = channelData,
      x.conv = dfData$x.conv,
      y.conv = dfData$y.conv,
      x.pixels = dfData$x.pixels,
      y.pixels = dfData$y.pixels,
      z.conv = 1,
      z.units = unlist(strsplit(dfData$z.units,',')[[1]]),
      instrument = dfData$instrument,
      history = '',
      date = str_creation_date,
      description = dfData$description,
      fullFilename = dfData$fullFilename
    )
  } else {
    warning("AFM object is not found in database.")
    obj = NULL
  }
  invisible(obj)
}
