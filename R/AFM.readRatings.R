#' Reads AFM Ratings from SQLite Database
#' 
#' @description
#' Returns AFM ratings for each AFM image and user.
#' 
#' @author Thomas Gredig
#'
#' @param dbFileName full path and name of SQLite database
#' @param meanValues logical, if \code{TRUE} returns only average for each ID
#' @param verbose if \code{TRUE} outputs verbose comments
#' 
#' @returns  data frame with ratings to be saved
#'
#' @importFrom DBI dbReadTable dbGetQuery dbListTables dbConnect dbDisconnect
#' @importFrom dplyr group_by "%>%" summarize
#' @importFrom RSQLite SQLite
#' @importFrom rlang .data
#' 
#' @seealso [AFM.writeRatings()]
#'
#' @export
AFM.readRatings <- function(dbFileName, meanValues = FALSE, verbose=FALSE) {
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
  
  if (meanValues) {
    if (!is.null(df_ratings)) {
      # compute mean values for ratinges
      df_ratings <- df_ratings %>%
        group_by(.data$ID) %>%
        summarize(quality = mean(as.numeric(.data$quality))) %>%
        as.data.frame()
    } else {
      df_ratings=data.frame()
    }
  }
  
  df_ratings
}
