#' Crops an AFM image
#' 
#' @description Creates a subset of an AFM image by cropping the original
#'   image into a smaller AFM image object
#'    
#' @param obj AFMdata object to be cropped
#' @param x0 x origin location in pixels, if \code{NA} use GUI to click on origin
#' @param y0 y origin location in pixels, if \code{NA} use GUI to click on origin
#' @param width.pixels width of cropped image in pixels
#' @param height.pixels height of cropped image in pixels
#' @param verbose output fitting parameters
#' 
#' @returns returns cropped image
#'
#' @author Thomas Gredig
#' 
#' @examples
#' d <- AFM.artificialImage(type='calibration', width=100, height=100, verbose=FALSE)
#' plot(d)
#' d1 = AFM.crop(d, x0=45, y0=45, width.pixels = 50, height.pixels = 50)
#' plot(d1)
#'
#' @importFrom terra click rast
#' @export
AFM.crop <- function(obj,
                     x0=NA,y0=NA,
                     width.pixels=1024, 
                     height.pixels=1024,
                     verbose=FALSE) {
  AFMcopy <- obj
  
  AFMcopy@data$z -> dat
  AFMcopy@x.pixels -> xPixels
  AFMcopy@y.pixels -> yPixels
  
  # if no origin is specified, plot the graph and
  # have the user select the origin via the GUI
  if ( (is.na(x0)) | (is.na(y0)) ) {
    d = AFM.raster(obj)
    dfr = rast(d)
    terra::plot(dfr)
    # returns coordinates according to (x,y)
    print("Click on crop image origin point:")
    click(dfr, n=1, xy=TRUE, show=FALSE) -> xy
    if (is.null(xy)) { x0 = 1; y0 = 1; } else {
      x0 = round(xy$x[1]/obj@x.conv)
      y0 = round(xy$y[1]/obj@y.conv)
    }
    if (verbose) { print(paste("Crop from:",x0,"/",y0,
                               " to:", x0+width.pixels,"/",y0+height.pixels)) }
  } 
  
  # starting position in array
  startLoc = (y0-1)*yPixels + x0
  
  
  # check boundaries
  # width must be <= right border
  if (width.pixels<2) {width.pixels = 2}
  if (height.pixels<2) {height.pixels = 2}
  if (x0+width.pixels > xPixels) { width.pixels = xPixels - x0 }
  if (y0+height.pixels > yPixels) { height.pixels = yPixels - y0 }
  
  # add to history
  AFMcopy@history <- add.AFM.history(AFMcopy, paste0("AFM.crop(obj,x0=",x0,
                                  ", y0=",y0,
                                  ", width.pixels=",width.pixels,
                                  ", height.pixels=",height.pixels,")"))
  
  
  p = c()
  for(j in 0:(height.pixels-1)) {
    p = c(p,(startLoc + j*xPixels):(j*xPixels + startLoc+width.pixels-1))
  }
  q = lapply(dat,function(x) { x[p] })
  AFMcopy@data$z <- q

  AFMcopy@x.pixels = width.pixels
  AFMcopy@y.pixels = height.pixels
  AFMcopy@x.nm = (width.pixels-1)*AFMcopy@x.conv
  AFMcopy@y.nm = (height.pixels-1)*AFMcopy@y.conv

  AFMcopy
}
