#' Set Pixels along Horizontal or Vertial Line of the AFM image
#' 
#' AFM.getLine returns the data from either a horizontal or vertial
#' line. The AFM.setLine sets the data for that line. Only a vector
#' with the data is submitted. 
#'
#'
#' @param obj AFMdata object
#' @param lineData vector with new values
#' @param xPixel vertical line at this pixel (1 to image resolution), if \code{NA} will use yPixel
#' @param yPixel horizontal line at this pixel (1 to image resolution), if \code{NA} will use xPixel
#' @param no channel number
#' @param verbose if \code{TRUE}, output additional information
#' 
#' @returns AFMdata object with new line
#' 
#' @examples
#' a = AFM.artificialImage(height=10, width=10, verbose=FALSE)
#' b = AFM.getLine(a, yPixel = 1, dataOnly = TRUE)
#' lineData = rep(b$z[1], length(b$x))
#' a1 <- AFM.setLine(a, lineData, yPixel = 1)
#' plot(a1)
#' 
#'
#' @author Thomas Gredig
#' @seealso \code{\link{AFM.getLine}}
#'
#'
#' @export
AFM.setLine <- function(obj,
                        lineData, 
                        xPixel = NA,
                        yPixel = NA,
                        no = 1,
                        verbose=FALSE) {
  # check inputs
  if(!isS4(obj)) { stop("Not an S4 object, AFMdata object expected.") }
  if(!AFM.isImage(obj)) { stop("AFM Image expected; object has AFM data but not an image.") }
  if(!is.vector(lineData)) { stop("lineData must be a vector with the new data.") }
  
  # copy object and add new data to line
  AFMcopy <- obj
  zData = AFMcopy@data$z[[no]]
  
  if (is.na(xPixel) && (!is.na(yPixel))) {
    # horizonal line
    mPixelPos = (yPixel-1)*obj@x.pixels + 1
    mPixelRange = mPixelPos:(mPixelPos+obj@x.pixels-1)
    
    historyProfileLine <- paste("AFM.setLine(obj,yPixel=",yPixel,")")
  } else if (!is.na(xPixel) && (is.na(yPixel))) {
    # vertical line
    mPixelPos = xPixel
    mPixelRange = (0:(obj@x.pixels-1))*obj@y.pixels + xPixel
    
    historyProfileLine <- paste("AFM.getLine(obj,xPixel=",xPixel,")")
  } else {
    stop("Neither horizonal nor vertial line selected; add xPixel parameter.")
  }
  
  if (verbose) cat("Saving channel",no," to:",AFMcopy@fullFilename,"\n")

  AFMcopy@history <- add.AFM.history(AFMcopy, historyProfileLine)
  
  if (length(lineData) != length(mPixelRange)) { stop("New lineData vector does not have length of line, which is ",length(mPixelRange)," data points.")}
  zData[mPixelRange] <- lineData
  AFMcopy@data$z[[no]] <- zData
  AFMcopy
}
