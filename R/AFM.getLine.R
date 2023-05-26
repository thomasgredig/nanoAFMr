#' AFM horizontal or vertical profile line
#'
#' create a profile data line for a particular
#' position given in pixels; either a horizontal or a vertical
#' line; this is also an easy way to reiterate through all lines of an image
#'
#' @param obj AFMdata object
#' @param xPixel vertical line at this pixel (1 to image resolution), if \code{NA} will use yPixel
#' @param yPixel horizontal line at this pixel (1 to image resolution), if \code{NA} will use xPixel
#' @param no channel number
#' @param dataOnly if \code{TRUE}, returns data instead of AFMdata object
#' @param verbose if \code{TRUE}, output additional information
#' @returns AFMdata object with line data or data frame with data
#'
#' @author Thomas Gredig
#' @seealso \code{\link{AFM.lineProfile}}
#'
#' @examples
#' filename = AFM.getSampleImages('ibw')[1]
#' afmd = AFM.import(filename)
#' afmd2 = AFM.getLine(afmd, 50)
#' plot(afmd2, addLines = TRUE)
#' head(AFM.linePlot(afmd2, dataOnly = TRUE))
#'
#'
#' @export
AFM.getLine <- function(obj,
                        xPixel = NA,
                        yPixel = NA,
                        no = 1,
                        dataOnly = FALSE,
                        verbose=FALSE) {
  AFMcopy <- obj

  
  if (is.na(xPixel) && (!is.na(yPixel))) {
    # horizonal line
    # distance from one to the next one in nm:
    r2 = 0:(obj@x.pixels-1)*obj@x.conv
    # pixels selected
    r = obj@y.pixels*(yPixel-1)+1:(obj@x.pixels)
    if (dataOnly) {
      d = AFM.raster(AFMcopy, no)
      m1 = which(d$y == obj@y.conv*(yPixel-1))
    }
    
    historyProfileLine <- paste(AFMcopy@history,
          "AFM.getLine(obj,yPixel=",yPixel,")")
  } else if (!is.na(xPixel) && (is.na(yPixel))) {
    # vertical line
    r2 = 0:(obj@y.pixels-1)*obj@y.conv
    r = obj@y.pixels*(0:(obj@y.pixels-1)) + (xPixel)
    if (dataOnly) {
      d = AFM.raster(AFMcopy, no)
      m1 = which(d$x == obj@x.conv*(xPixel-1))
    }
    historyProfileLine <- paste(AFMcopy@history,
                                "AFM.getLine(obj,xPixel=",xPixel,")")
  } else {
    stop("Neither horizonal nor vertial line selected; add xPixel parameter.")
  }

  if (verbose) print(paste("delta Y:",signif(AFMcopy@y.conv,4),
                           "nm/px and delta X:",signif(AFMcopy@x.conv,4),"nm/px"))
  if(dataOnly) {
    if (verbose) cat("Extracting channel",no," from:",AFMcopy@fullFilename,"\n")
    return(d[m1,])
  }

  AFMcopy@history <- add.AFM.history(AFMcopy, historyProfileLine)
  
  
  if (is.null(AFMcopy@data$line)) AFMcopy@data$line = list()
  AFMcopy@data$line = append(AFMcopy@data$line,list(r))
  if (is.null(AFMcopy@data$line.nm)) AFMcopy@data$line.nm = list()
  AFMcopy@data$line.nm = append(AFMcopy@data$line.nm,list(r2))
  AFMcopy
}
