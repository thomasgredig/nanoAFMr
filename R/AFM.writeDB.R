#' Write AFM object to SQL DB
#'
#' @description
#' An AFMdata S4 image is written to an SQL database; the ID will 
#' save it in a particular table in the database, if another AFM image 
#' with the same ID exists, it will be overwritten. 
#' 
#' Open: mydb <- DBI::dbConnect(RSQLite::SQLite(), "myFile.sqlite")
#' 
#' Close: DBI::dbDisconnect(mydb)
#'
#' @param obj S4 AFM object from nanoAFMr package, if object is \code{NULL}, then ID will be removed from database
#' @param mydb database connection from DBI package
#' @param ID unique object file ID
#' @param vacuum vacuums the database, if obj is \code{NULL} and something is deleted; this option saves space
#' @param verbose if \code{TRUE} outputs verbose comments
#'
#' @importFrom DBI dbRemoveTable dbWriteTable dbListTables dbCreateTable dbAppendTable
#' @author Thomas Gredig
#'
#' @seealso [\code{\link{AFM.readDB}}]
#'
#' @export
AFM.writeDB <- function(obj, mydb, ID, vacuum = TRUE, verbose=FALSE) {
  # define table names in DB
  myTableAFMname = paste0('afm',ID)
  myTableDataName = paste0('afmData')
  
  t1 <- dbListTables(mydb)
  if (!myTableDataName %in% t1) {
    DFempty = data.frame(ID = integer(),
                         channel = character(),
                         x.conv = integer(),
                         y.conv = integer(),
                         x.pixels = integer(),
                         y.pixels = integer(),
                         z.units = character(),
                         instrument = character(),
                         history = character(),
                         description = character(),
                         fullFilename = character())
    dbCreateTable(mydb, myTableDataName, DFempty)
    if (verbose) print(paste("Created data table:",myTableDataName))
  }
  
  # remove AFM image data from DB, if it exists
  if (myTableAFMname %in% t1) {
    dbRemoveTable(mydb, myTableAFMname)
    if (verbose) print(paste("Removed existing table:", myTableAFMname))
  }
  # exit if there is no object to be added; this can be used to delete an image
  if (is.null(obj)) {
    if (vacuum) dbGetQuery(mydb, "VACUUM;")
    return(NULL)
  }
  
  # save the image data
  if (obj@instrument == "Park") {
    z = obj@data$z[[1]]
    dfZ = as.data.frame(z)
  } else if (obj@instrument == "Cypher") {
    dfZ = as.data.frame(sapply(obj@data$z, rbind))
  } else {
    warning(paste("File format",
                  obj@instrument,
                  "is not supported for database output."))
    return(NULL)
  }
  dbWriteTable(mydb, myTableAFMname, dfZ)
  
  dfData = data.frame(ID = ID,
                      channel = paste(obj@channel, collapse = ','),
                      x.conv = obj@x.conv,
                      y.conv = obj@y.conv,
                      x.pixels = obj@x.pixels,
                      y.pixels = obj@y.pixels,
                      z.units = paste(obj@z.units, collapse = ','),
                      instrument = obj@instrument,
                      description = obj@description,
                      fullFilename = obj@fullFilename
  )
  dbAppendTable(mydb, myTableDataName, dfData)
  
  invisible(TRUE)
}
