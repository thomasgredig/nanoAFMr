#' Find Lines on AFM Image
#' 
#' @param obj AFMdata object
#' @param numLines number of clicks to find the line number
#' 
#' @importFrom terra click rast
#' 
#' 
#' @export
AFM.flattenFindLine <- function(obj, numLines=1) {
  AFMcopy <- obj
  d = AFM.raster(AFMcopy)
  
  width.x = AFMcopy@x.pixels
  width.y = AFMcopy@y.pixels

  dfr <- rast(d)
  terra::plot(dfr)
  click(dfr, n=numLines, xy=TRUE, show=FALSE) -> xy
  
  xy$y / AFMcopy@y.conv
}