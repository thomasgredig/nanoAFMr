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
#' @param fIDfile path and filename for RAW-ID.csv file
#' @param add2DB logical, if TRUE highly rated AFM images are added to SQL database
#' @param verbose logical, if TRUE additional information is output
#'
#' @importFrom utils write.csv read.csv
#'
#' @export
AFM.rate <- function(dbFileName, IDs=NA, fIDfile = NA, add2DB = FALSE, verbose = FALSE) {
  if (!file.exists(dbFileName)) stop("Cannot find DB file:", dbFileName)
  
  # load existing ratings
  df_ratings <- AFM.readRatings(dbFileName)
  if (is.null(df_ratings)) df_ratings = data.frame()
  
  # define table names in DB
  mydb <- DBI::dbConnect(RSQLite::SQLite(), dbFileName)
  IDs.all <- AFM.readDB(mydb)
  if (length(IDs)==0) {
    IDs <- IDs.all
  } 
  if (!is.na(fIDfile)) rID <- raw.readRAWIDfile(fIDfile) else rID = NULL
  if (is.null(rID)) warning("RAW-ID.csv data cannot be read.")

  cat("\n\n--> Rating AFM images.\n")
  saveIDList = c()  # list of IDs that are high-quality
  user.name = 'anonymous'
  if (add2DB == FALSE) {
    if (interactive()) user.name <- readline("Last name of user: ")
  }
  
  for(i in 1:length(IDs)) {
    # skip if this user has rated the image already
    m1 <- which(df_ratings$ID == IDs[i] & df_ratings$user==user.name)
    if (length(m1)>0) next
    
    if (verbose) cat("Rating for ID=",IDs[i],"\n")
    if (IDs[i] %in% IDs.all) {
      a = AFM.readDB(mydb, ID = IDs[i])
    } else {
      # try to read from local file
      if (is.null(rID)) {
        cat("RAW-ID file is empty.")
        next
      }
      m <- which(rID$ID==IDs[i])
      if (length(m)==0) {
        cat("ID ",IDs[i]," not found.\n")
        next
      }
      fname = file.path(rID$path[m], rID$filename[m])
      if (!file.exists(fname)) {
        cat("AFM file is not found:", fname, "\n")
        next
      }
      a = AFM.import(fname)
    }
    a = AFM.flatten(a)
    print(plot(a))
    print(a)
    if (interactive()) qual <- readline("Quality, q=exit, 1=high, 2=ok, 3=low [low]: ")
    if (qual=='q') break
    if (qual=="") qual=3
    if (qual==1) saveIDList = c(saveIDList, IDs[i])
    r = data.frame(ID = IDs[i], user=user.name, quality=qual, timestamp=format(Sys.time(), "%Y-%m-%d %X"))
    df_ratings = rbind(df_ratings, r)
  }
  DBI::dbDisconnect(mydb)
  
  if (verbose) cat("Writing",nrow(df_ratings),"to SQLite database.\n")
  if (nrow(df_ratings)>0) AFM.writeRatings(dbFileName, df_ratings)
  
  if (add2DB & length(saveIDList)>0) {
    if (verbose) cat("Saving high-quality AFM images to SQLite database.\n")
    AFM.add2DB(dbFileName, IDs=saveIDList, fIDfile, verbose=verbose)
  }
  
  invisible(df_ratings)
}
