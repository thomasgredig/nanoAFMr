#' History of AFM file generation
#' 
#' @description outputs commands to recreate this particular AFM
#'   data image; if line profiles were added or the image was cropped
#'   that history will be shown; this allows reproducibility of the
#'   image.
#'    
#' @param obj AFMdata object to be cropped
#' @param verbose output additional information if \code{TRUE}
#' @returns image history
#'
#' @author Thomas Gredig
#' 
#' @examples
#' d <- AFM.import(AFM.getSampleImages('tiff'))
#' d <- AFM.lineProfile(d, 50,50,75,75, unitPixels = T)
#' d <- AFM.lineProfile(d, 15,125,75,75, unitPixels = T)
#' AFM.history(d)
#'
#' @importFrom raster rasterFromXYZ click
#' @export
AFM.history <- function(obj, verbose=FALSE) {
  h <- obj@history

  cmdLines = strsplit(h,";")[[1]]
  
  # first load the image
  hLines = c(paste0("obj = AFM.import('",obj@fullFilename,"')"))
  for(ln in cmdLines) { 
    hLines = c(hLines, paste0(ln," -> obj"))
  }
  
  hLines
}

#
#
# ADD to history of object
add.AFM.history <- function(obj, txt) {
  if (purrr::is_empty(obj@history)) return("")
  paste(obj@history,txt,";")
}
