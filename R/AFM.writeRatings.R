#' Write AFM Rating File to SQL Database
#'
#' @description
#' User rating information of specific AFM images is stored separately
#' in the AFM SQLite database.
#' 
#'
#' @param dbFileName full path and name of SQLite database
#' @param df_ratings data frame with ratings to be saved
#' @param verbose if \code{TRUE} outputs verbose comments
#'
#' @importFrom DBI dbConnect dbRemoveTable dbWriteTable dbListTables dbCreateTable dbDisconnect
#' @importFrom RSQLite SQLite
#' @author Thomas Gredig
#'
#' @export
AFM.writeRatings <- function(dbFileName, df_ratings, verbose=FALSE) {
  if (!file.exists(dbFileName)) {
    if (verbose) cat("DB file does not exist. Creating file:", dbFileName,"\n")
    file.create(dbFileName)
  }

  # define table names in DB
  myTableDataName = paste0('afmRating')
  
  mydb <- DBI::dbConnect(RSQLite::SQLite(), dbFileName)
  t1 <- dbListTables(mydb)
  if (!myTableDataName %in% t1) {
    # dbCreateTable(mydb, myTableDataName, df_ratings)
   # if (verbose) print(paste("Created data table:",myTableDataName))
  } else {
    dbRemoveTable(mydb, myTableDataName)
    if (verbose) print(paste("Removed existing table:", myTableDataName))  
  }
  dbWriteTable(mydb, myTableDataName, df_ratings)
  DBI::dbDisconnect(mydb)
  
  invisible(TRUE)
}
