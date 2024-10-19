#' Write AFM Object to SQL Database
#'
#' @description
#' Writes and AFM image to an SQL database; the ID is used to store the image. If an image
#' already exists with this ID, then it will be overwritten. If you use \code{NULL} as the
#' \code{obj}, then it will delete that ID. The space is only made available, if you vacuum
#' the database.
#' 
#'
#' @param obj S4 AFMdata object, if object is \code{NULL}, then ID will be removed from database
#' @param con database connection from DBI package
#' @param ID unique object file ID
#' @param vacuum vacuums the database, if obj is \code{NULL} and something is deleted; this option saves space
#' @param verbose if \code{TRUE} outputs verbose comments
#'
#' @importFrom DBI dbRemoveTable dbWriteTable dbListTables dbCreateTable dbAppendTable dbExecute
#' @author Thomas Gredig
#' 
#' @examples
#' sql_filename = tempfile(fileext = "sqlite")
#' con <- DBI::dbConnect(RSQLite::SQLite(), sql_filename)
#' AFM.writeDB(NULL, con, 0, vacuum=FALSE)
#' DBI::dbDisconnect(con)
#'
#' @seealso [AFM.readDB()]
#'
#' @export
AFM.writeDB <- function(obj, con, ID, vacuum = TRUE, verbose=FALSE) {
  # define table names in DB
  myTableAFMname = paste0('afm',ID)
  myTableDataName = paste0('afmData')
  
  t1 <- dbListTables(con)
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
                         date = character(),
                         description = character(),
                         fullFilename = character())
    dbCreateTable(con, myTableDataName, DFempty)
    if (verbose) print(paste("Created data table:",myTableDataName))
  }
  
  # remove AFM image data from DB, if it exists
  if (myTableAFMname %in% t1) {
    dbRemoveTable(con, myTableAFMname)
    if (verbose) print(paste("Removed existing table:", myTableAFMname))
  }
  
  # exit if there is no object to be added; 
  # this can be used to delete an image
  if (is.null(obj)) {
    if (vacuum) dbExecute(con, "VACUUM")
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
  dbWriteTable(con, myTableAFMname, dfZ)
  
  dfData = data.frame(ID = ID,
                      channel = paste(obj@channel, collapse = ','),
                      x.conv = obj@x.conv,
                      y.conv = obj@y.conv,
                      x.pixels = obj@x.pixels,
                      y.pixels = obj@y.pixels,
                      z.units = paste(obj@z.units, collapse = ','),
                      instrument = obj@instrument,
                      history = obj@history,
                      date = obj@date,
                      description = obj@description,
                      fullFilename = obj@fullFilename
  )
  dbAppendTable(con, myTableDataName, dfData)
  
  invisible(TRUE)
}
