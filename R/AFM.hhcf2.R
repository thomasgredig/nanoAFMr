###################################################
#'
#' Height-Height Correlation Function 2
#'
#' @description
#' Changes the structure of the output variable for the AFM.hhcf function
#' from nanoAFMr package by Thomas Gredig et. al. Like its parent function, this
#' computes g(r) correlation function for an AFM
#' image with height information. The height information
#' must be encoded in a BMP file with a linear scale
#' Publication: http://iopscience.iop.org/article/10.1088/1742-6596/417/1/012069
#
#' Title: Height-Height Correlation Function to Determine Grain
#'        Size in Iron Phthalocyanine Thin Films
#' Authors: Thomas Gredig, Evan A. Silverstein, Matthew P Byrne
#' Amendments: Ryan Mizukami
#' Journal: J of Phys: Conf. Ser. Vol 417, p. 012069 (2013).
#'
#' @author Thomas Gredig, Ryan Mizukami
#' @param obj AFMdata object
#' @param no Channel number
#' @param degRes resolution of angle, the higher the more, should be >100, 1000 is also good
#' @param addFit if \code{TRUE} a fit is added to the data
#' @param numIterations Number of iterations (must be > 1000), but 1e6 recommended
#' @param r.percentage a number from 10 to 100 representing the distance to compute, since the image is
#'    square, there are not as many points that are separated by the full length, 80 is a good value, if there
#'    is no fit, the value can be reduced to 70 or 60.
#' @param xi.percentage a number from 10 to 100 representing where correlation length could be found from maximum (used for fitting)
#' @param dataOnly if \code{TRUE} only return data frame, otherwise returns a graph
#' @param fitparameters if \code{TRUE} dataOnly will be set to \code{TRUE} and a list with data, fit, fit labels and fit parameters
#'    will be returned.
#' @param verbose output time if \code{TRUE}
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_path scale_x_log10 scale_y_log10 theme_bw geom_label theme
#' @importFrom stats runif nls predict coef
#'
#' @return Returns a graph with g(r) and $num indicating number of computations used for r. If dataOnly is TRUE then a data frame with only the
#'    x,y,z data is returned. If fitparameters is TRUE then a list that has the x,y,z data in the first element, the fitted curve is stored in the
#'    second element. The fit labels are stored in the third element, and the fit parameters (xi, sigma, H) are stored in the fourth element.
#'
#'
#' @examples
#' filename = AFM.getSampleImages(type='tiff')
#' a = AFM.import(filename)
#' a = AFM.flatten(a)
#' r = AFM.hhcf2(a, numIterations = 5e5, fitparameters = TRUE)
#' head(r)                           # outputs HHCF data as a list
#' 
#' AFM.hhcf2(a, dataOnly = FALSE)    # outputs graph
#'
#'
##################################################
#' @export
AFM.hhcf2 <- function(obj, no=1,
                      degRes = 100,
                      numIterations=1000,
                      addFit = TRUE,
                      r.percentage = 80,
                      xi.percentage = 70,
                      dataOnly = FALSE,
                      fitparameters = FALSE,
                      verbose=FALSE) {
  
  
  r.nm <- myLabel <- NULL
  
  if (!is(obj, "AFMdata")) return(NULL)
  if (obj@x.conv != obj@y.conv) warning('AFM image is distorted in x- and y-direction; HHCF is not correct.')
  dimx = obj@x.pixels
  dimy = obj@y.pixels
  if (verbose) print(paste("AFM image is ",dimx, "by",dimy," pixels."))
  q = obj@data$z[[no]]
  if (numIterations<1000) {
    numIterations=1000
    if (verbose) print("numIterations must be at least 1000, reset to 1000.")
  }
  
  # generate random numbers to pick starting positions
  # faster than computing all possible locations
  px1 = round(runif(numIterations, min=1, max=dimx))
  py1 = round(runif(numIterations, min=1, max=dimy))
  theta = round(runif(numIterations, min=0, max=2*pi*degRes))/degRes
  
  lg = c()
  lq = c()
  t.start = as.numeric(Sys.time())
  maxR = round(dimx*r.percentage/100)
  if (verbose) print("Computing r from 1 pixel to",maxR,"pixels maximum. Adjust with r.percentage parameter.")
  for(r in 1:maxR) {
    # compute second point
    px2 = round(px1+r*cos(theta))
    py2 = round(py1+r*sin(theta))
    qq = which(px2>0 & px2 <= dimx & py2>0 & py2 <= dimy)
    
    p1 = px1[qq]+(py1[qq]-1)*dimx
    p2 = px2[qq]+(py2[qq]-1)*dimy
    
    # compute height-height difference
    g = sum((q[p1]-q[p2])^2) / length(qq)
    lg = c(lg, g)
    lq = c(lq, length(qq))
  }
  t.end = as.numeric(Sys.time())
  if (verbose) print(paste('Time used: ',round(t.end-t.start,1), ' seconds'))
  
  r = data.frame(
    r.nm = (1:maxR)*obj@x.conv,
    g = lg,
    num = lq
  )
  
  
  totalData = list()
  totalData[[1]] = r
  totalDataOnly = r
  
  
  if (addFit) {
    # starting fit parameters
    AFM.math.params(obj) -> m1
    m1$Rq^2*2 -> ss
    xi = r$r.nm[min(which(r$g > (xi.percentage*ss)/100))]
    
    # fit the data using
    # 2*sigma^2 = ss, sigma = roughness
    # xi = correlation length
    # H = 2*Hurst parameter
    fit <- NULL
    try({
      nls(data=r,
          g ~ ss * (1 - exp(-(r.nm/xi)^H)),
          start = list(ss = ss, xi = xi,
                       H=2)) -> fit
    });
    
    # fit was successful
    if (!is.null(fit)) {
      fitRnm = seq(from=round(min(r$r.nm)*0.9), to=max(r$r.nm), by=1)
      dFit = data.frame(
        r.nm = fitRnm,
        g = predict(fit, list(r.nm=fitRnm))
      )
      fitNames = c('sigma', 'xi','H')
      fitNamesUnits = c('nm','nm','')
      fitParams = coef(fit)
      fitParams[1]=sqrt(fitParams[1]/2)
      fitParams[3]=fitParams[3]/2
      dFitLabels = data.frame(
        r.nm = r$r.nm[1:3],
        g = r$g[1:3],
        myLabel = paste(fitNames,'=',signif(fitParams,4),fitNamesUnits)
      )
      totalData[[2]] = dFit
      totalData[[3]] = dFitLabels
      totalData[[4]] = data.frame(sigma = fitParams[1],
                                  xi = fitParams[2],
                                  H = fitParams[3]
      )
      
    } else {
      if (verbose) print("Cannot fit data => setting addFit=FALSE")
      addFit = FALSE
    }
  }
  
  if(fitparameters){
    return(totalData)
  } else if(dataOnly & !fitparameters){
    return(totalDataOnly)
  }
  
  
  g = ggplot(r, aes(r.nm, g)) +
    geom_point(col='blue', size=2) +
    scale_x_log10() +
    scale_y_log10() + ylab('g(r)') + xlab('r (nm)') +
    theme_bw()
  
  if (addFit) g = g +
    geom_path(data=dFit, col='red') +
    geom_label(data = dFitLabels,
               aes(fill = 'white',label=myLabel),
               colour = "white",
               fontface = "bold", hjust=-0.1) +
    theme(legend.position = 'none')
  
  g
}






