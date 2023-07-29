#' Find Lines to Fix
#' 
#' @export
AFM.flattenFindLine <- function(obj, numLines=1) {
  AFMcopy <- obj
  d = AFM.raster(AFMcopy)
  
  width.x = AFMcopy@x.pixels
  width.y = AFMcopy@y.pixels

  dfr = raster::rasterFromXYZ(d)
  sp::plot(dfr)
  raster::click(dfr, n=numLines, xy=TRUE, show=FALSE) -> xy

  xy$y / AFMcopy@y.conv
}