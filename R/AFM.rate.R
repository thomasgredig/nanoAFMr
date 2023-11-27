#' Rate AFM Images
#'
#' @description
#' The images are displayed and rated by a user; if the user has rated
#' an image already, it will not show up anymore, if the image has not
#' yet been rated by that user, it will show and the user can enter a
#'
#' rating usually: `high`, `ok`, `low`
#'
#' @param dbFileName full path and name of SQLite database
#' @param IDs vector with AFM file IDs
#' @param verbose if TRUE additional information is output
#'
#' @importFrom RAWdataR raw.getFileByID
#' @importFrom utils write.csv read.csv
#'
#'
#' @export
AFM.rate <- function(dbFileName, IDs=NA, verbose = FALSE) {
  if (!file.exists(dbFileName)) stop("Cannot find DB file:", dbFileName)
  
  # load existing ratings
  df_ratings <- AFM.readRatings(dbFileName)
  if (is.null(df_ratings)) df_ratings = data.frame()
  
  # define table names in DB
  mydb <- DBI::dbConnect(RSQLite::SQLite(), dbFileName)
  IDs.all <- AFM.readDB(mydb)
  if (is.na(IDs)) {
    IDs <- IDs.all
  } else {
    IDs <- IDs %in% IDs.all
  }

  cat("\n\nRating AFM images.\n")
  user.name <- readline("Last name of user: ")
  
  
  for(i in 1:length(IDs)) {
    # skip if this user has rated the image already
    m1 <- which(df_ratings$ID == IDs[i] & df_ratings$user==user.name)
    if (length(m1)>0) next
    
    cat("Rating for ID=",IDs[i],"\n")
    a = AFM.readDB(mydb, IDs[i])
    a = AFM.flatten(a)
    print(plot(a))
    print(a)
    qual <- readline("Quality, q=exit, 1=high, 2=ok, 3=low [low]: ")
    if (qual=='q') break
    if (qual=="") qual=3
    r = data.frame(ID = IDs[i], user=user.name, quality=qual, timestamp=format(Sys.time(), "%Y-%m-%d %X"))
    df_ratings = rbind(df_ratings, r)
  }
  DBI::dbDisconnect(mydb)
  
  AFM.writeRatings(dbFileName, df_ratings)
}
