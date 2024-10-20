#' Find Lines on AFM Image
#' 
#' @description
#' Use interactive mode to display an AFM image, then 
#' use a mouse to select a particular line and return
#' the pixel location as output. 
#' 
#' 
#' @param obj AFMdata object
#' @param numLines number of clicks to find the line number
#' 
#' @returns 
#' pixel number of the selected line in the image
#' 
#' @examples
#' filename = AFM.getSampleImages('tiff')
#' a = AFM.import(filename)
#' AFM.flattenFindLine(a)
#' 
#' @importFrom terra plot click rast
#' @importFrom grDevices dev.new
#' 
#' @export
AFM.flattenFindLine <- function(obj, numLines=1) {
  AFMcopy <- obj
  d = AFM.raster(AFMcopy)
  
  line_clicked = NA
  width.x = AFMcopy@x.pixels
  width.y = AFMcopy@y.pixels

  if(interactive()) {
    dev.new(noRStudioGD = TRUE)
    dfr <- rast(d)
    terra::plot(dfr)
    click(dfr, n=numLines, xy=TRUE, show=FALSE) -> xy
    line_clicked = xy$y / AFMcopy@y.conv
  }
  
  line_clicked
}