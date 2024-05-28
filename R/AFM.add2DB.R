#' Add or Remove AFM Images to the SQL Database
#' 
#' If the ID is negative, then the image will be removed, any positive IDs are added.
#' If the IDs can be both positive and negative numbers, then images are both added and
#' removed; save any image, even partial AFM images.
#'
#' @param baseSQLfile path and file name of the SQLite database with the AFM images
#' @param IDs vector with list of unique file IDs to add
#' @param fIDfile location of RAW-ID.csv file with path information for files
#' @param verbose logical, for verbose output
#'
#' @importFrom dplyr "%>%" mutate filter select
#' @importFrom DBI dbConnect dbDisconnect dbExecute
#'
#' @author Thomas Gredig
#'
#' @export
AFM.add2DB <- function(baseSQLfile, IDs, fIDfile = "data-raw/RAW-ID.csv", verbose=TRUE) {
  if (verbose) cat("AFM SQL dbname:", baseSQLfile,'\n')
  if (!file.exists(baseSQLfile)) stop("AFM database not found.")
  
  # check if any AFM images by ID are to be removed
  # if the ID is negative, then it will be removed, if it exists
  removeIDlist = abs( IDs[IDs<0] )
  if (length(removeIDlist)>0) {
    # remove some AFM images
    mydb <- dbConnect(RSQLite::SQLite(), baseSQLfile)
    for(remID in removeIDlist) {
      myTableAFMname = paste0('afm',remID)
      dbRemoveTable(mydb, myTableAFMname)
    }
    dbExecute(mydb, "VACUUM;")
    DBI::dbDisconnect(mydb)
  }
  
  IDs = IDs[IDs>0]
  if (length(IDs)==0) return(0)
  
  # find all AFM files, then add them to the SQL DB
  df <- raw.readRAWIDfile(fIDfile)
  if (nrow(df)==0) stop("RAW-ID.csv file not found in ", fIDfile)

  fileList <- df %>%
    filter(ID %in% IDs) %>%
    mutate(fname = file.path(path,filename)) %>%
    filter(missing == FALSE) %>%
    select(ID, missing, fname)
  
  if (verbose) cat("Found", nrow(fileList), "AFM files.\n")
  if (nrow(fileList)==0) return(0)
  
  mydb <- dbConnect(RSQLite::SQLite(), baseSQLfile)
  savedIDs = AFM.readDB(mydb)
  
  if (verbose) cat("--> Will update DB only; . = added, - = skipped, X = error\n\n")
  savedImageCounter = 0
  for(i in 1:nrow(fileList)) {
    if (i %% 40 == 0) cat("\n")
    # check whether file is already in DB
    ID = fileList$ID[i]
    if (ID %in% savedIDs) {
      cat("-")
      next
    }
    
    # get filename
    afmFile = fileList$fname[i]
    # try loading the AFM image
    try({ a = NULL; a = AFM.import(afmFile) })
    if (is.null(a)) { cat("X") }
    else {
      #if (!AFM.partial(a)) {
        AFM.writeDB(a, mydb, ID, verbose=FALSE)
        savedImageCounter = savedImageCounter + 1
        cat(".")
      #}
    }
  }
  cat("\n")
  
  DBI::dbDisconnect(mydb)
  
  if (verbose) cat("Saved ",savedImageCounter," AFM images in database:", baseSQLfile,'\n')
  savedImageCounter
}
