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
#' @returns Invisibly returns `TRUE` after writing ratings.
#'
#' @importFrom DBI dbConnect dbRemoveTable dbWriteTable dbListTables dbCreateTable dbDisconnect
#' @importFrom RSQLite SQLite
#' @author Thomas Gredig
#'
#' @export
AFM.writeRatings <- function(dbFileName, df_ratings, verbose=FALSE) {
  if (!is.data.frame(df_ratings)) {
    stop("df_ratings must be a data.frame.")
  }
  required_cols <- c("ID", "user", "quality")
  missing_cols <- setdiff(required_cols, names(df_ratings))
  if (length(missing_cols) > 0) {
    stop("df_ratings is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
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
