#' Flattens AFM Image
#'
#' @description
#' Flatten the data of a particular channel for an AFM image. You can use an appropriate
#' method to flatten the image. The default flatten is a plane fit to the full dataset. 
#' This, however, does not work well for samples that have 2 levels, for example. You can
#' also select lineByLine method to fit each line separately, you can check this approach
#' with `AFM.flattenCheck()` and if some lines need to be excluded, then use `AFM.flattenLine()`
#' to return the slope for each line of the image. Use this dataset with (m,b) values to 
#' subtract the slope manually with the slope method. 
#' 
#' It is possible to offset the data with a shift using the `zShift` parameter.
#' 
#'
#' @param obj AFMdata object
#' @param no channel number
#' @param method use the method to flatten the image:
#'   \itemize{
#'      \item{"plane"}{`Default`: Fit a flat plane to the entire image and subtract}
#'      \item{"lineByLine"}{Fit each line and substract a linear fit}
#'      \item{"slope"}{Remove given slopes from each line, must provide `slope` parameter}
#'   }
#' @param zShift vertical offset in the same units as the channel units
#' @param slope data.frame obtained from `AFM.flattenLine()`
#' @param verbose output fitting parameters
#' @param ... additional arguments for method, such as tau_lower
#' 
#' @return AFMdata object 
#' 
#' @author Thomas Gredig
#' 
#' @seealso [AFM.flattenLine()]
#' 
#' @examples
#' d = AFM.import(AFM.getSampleImages(type='ibw'))
#' d2 = AFM.flatten(d)
#' plot(d2,graphType=2)
#' 
#' @export
AFM.flatten <- function(obj, no=1, method = c('plane','lineByLine','slope'), zShift = 0, slope=NULL, verbose=FALSE, ...) {
  # set default method
  if (length(method)>1) method='plane'
  
  # set history
  AFMcopy <- obj
  AFMcopy@history <- add.AFM.history(AFMcopy, paste0("AFM.flatten(obj,",no,",method='",method,"')"))
  
  if (method == 'lineByLine') {
    if (verbose) cat("Method: Line by Line\n")
    z.new = .flattenMethodLineByLine(AFMcopy, verbose=verbose, ...)
  } else if (method == 'slope') {
    if (verbose) cat("Method: Slope\n")
    z.new = .flattenMethodSlope(AFMcopy, no, slope)
  } else { 
    if (verbose) cat("Method: Plane\n")
    d = AFM.raster(AFMcopy,no)
    z.new = .flattenMethodPlane(d)
  }

  AFMcopy@data$z[[no]] =  z.new + zShift
  AFMcopy
}



#' Line Flatten Fit Parameters
#' 
#' @param obj AFMdata image
#' @param no channel number
#' @param skip positions to skip (incorrect fit)
#' @param region dataframe with `lines` and `fit.px.lower` and `fit.px.upper` for each line
#' @param tau_lower percentage of data points to fit (1=100 percent)
#' @param verbose logical, output useful information if TRUE
#' @param ... additional parameters like lowLimit, upperLimit, outGraphs
#' 
#' @author Thomas Gredig
#' 
#' @returns data frame with `m` (slope) and `b` (offset) for each line of the image
#' 
#' @importFrom stats approx
#' 
#' @export
AFM.flattenLine <- function(obj, no=1, skip = c(), region = NULL, tau_lower = 0.01, verbose=FALSE, ...) {
  m = c()
  b = c()
  
  for(j in 1:obj@y.pixels) {
    if (j %in% region$lines) {
      .flattenLine(obj, j, lowLimit = region$fit.px.lower[which(region$lines==j)],
                   upperLimit = region$fit.px.upper[which(region$lines==j)],
                   outGraphs = FALSE, tau_lower = tau_lower, ...) -> d
    } else {
      .flattenLine(obj, j, outGraphs = FALSE, tau_lower = tau_lower, ...) -> d
    }
    m = c(m, d$m)
    b = c(b, d$b)
  }
  slope = data.frame(m,b)
  
  if (length(skip)>0) {
    slope$x = 1:nrow(slope)
    slope1 <- slope[-skip,]
    
    m <- approx(x=slope1$x, y=slope1$m, xout=1:nrow(slope))
    b <- approx(x=slope1$x, y=slope1$b, xout=1:nrow(slope))
    
    slope <- data.frame(m = m$y, b = b$y)
  }
  
  slope
}



#' Check AFM image Flattening Process
#' 
#' @description
#' Creates a graph that shows the fit for each line in a grid plot
#' 
#' @param obj AFMdata object
#' @param lns lines to select to be fitted (vector)
#' @param no channel number
#' @param tau_lower percentage of data points to fit (1 = 100 percent)
#' 
#' @importFrom ggplot2 ggtitle
#' 
#' @returns graph (ggplot2 object)
#' 
#' @export
AFM.flattenCheck <- function(obj, lns = c(1:4*5), no=1, tau_lower=0.01) {
  g = list()
  k = 1
  for(j in lns) {
    .flattenLine(obj, j, outGraphs = TRUE, tau_lower = tau_lower) -> d
    g[[k]] = d$g2 + ggtitle(paste0("Line:",j))
    k = k + 1
  }
  g
}
