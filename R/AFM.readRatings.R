#' Reads AFM Ratings from SQLite Database
#' 
#' @description
#' Returns AFM ratings for each AFM image and user.
#' 
#' @author Thomas Gredig
#'
#' @param dbFileName full path and name of SQLite database
#' @param verbose if \code{TRUE} outputs verbose comments
#' 
#' @returns  data frame with ratings to be saved
#'
#' @importFrom DBI dbReadTable dbGetQuery dbListTables dbConnect dbDisconnect
#' 
#' @seealso [\code{\link{AFM.writeRatings}}]
#'
#'
#' @export
AFM.readRatings <- function(dbFileName, verbose=FALSE) {
  if (!file.exists(dbFileName)) stop("Cannot find DB file:", dbFileName)
  # define table names in DB
  myTableDataName = paste0('afmRating')
  mydb <- DBI::dbConnect(RSQLite::SQLite(), dbFileName)
  
  t1 <- dbListTables(mydb)
  # return IDs of AFM images
  df_ratings <- NULL
  if (myTableDataName %in% t1) {
    df_ratings <- dbReadTable(mydb, myTableDataName)
  } 
  DBI::dbDisconnect(mydb)
  
  df_ratings
}
