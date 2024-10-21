###################################################
#'
#' Height-Height Correlation Function for AFM Image
#'
#' @description
#' Computes the height-height correlation function for an AFM data object; note
#' that any background should be removed first. Since the full computation would 
#' be lengthy, yet a random subset generally converges to the full result, the
#' \code{numIterations} parameter is used to limit the iterations. It should be 
#' increased for images with more pixel resolution. With higher resolution, the 
#' \code{degRes} should also be increased. For each iteration a random pixel 
#' and a random angle with resolution \code{degRes} is generated Then, the height-height 
#' correlation \eqn{g(r) = |h(x) - h(x+r)|^2} for that point is computed,
#' where \eqn{r} stretches from 1 to pixel resolution of the image (scaled by 
#' \code{r.percentage}). Since the AFM image is generally square, some locations / angles 
#' will not have data for large r and are ignored. Given the random numbers, slightly
#' different results may be obtained in different runs. To limit the variation, the 
#' same random numbers can be used, when the \code{randomSeed} is populated with a
#' prime number. It is recommended to run with \code{allResults = TRUE}.
#' 
#' The resulting data curve \eqn{g(r)} is fit to the following equation:
#' \eqn{2 \sigma^2  (1 - \exp \left[ -(\frac{r}{\xi})^{2H} \right] ) }
#' 
#' \itemize{
#'   \item \eqn{\sigma}: roughness
#'   \item \eqn{\xi}: correlation length
#'   \item \eqn{H}: Hurst parameter, \eqn{\alpha=2H}
#' }
#' 
#' Publication: http://iopscience.iop.org/article/10.1088/1742-6596/417/1/012069
#
#' Title: Height-Height Correlation Function to Determine Grain
#'        Size in Iron Phthalocyanine Thin Films
#' Authors: Thomas Gredig, Evan A. Silverstein, Matthew P Byrne
#' Journal: J of Phys: Conf. Ser. Vol 417, p. 012069 (2013).
#'
#' @author Thomas Gredig
#'
#' @param obj AFMdata object
#' @param no channel number
#' @param addFit if \code{TRUE} a fit is added to the data
#' @param dataOnly if \code{TRUE} returns data frame, otherwise returns a graph (OBSOLETE, use allResults)
#' @param numIterations Number of iterations (must be > 1000), but 10^6 recommended
#' @param degRes resolution of angle, the higher the better, should be >100, 1000 is also good, but takes more time
#' @param r.percentage a number from 10 to 100 representing the distance to compute, since the image is
#'    square, there are not as many points that are separated by the full length, 80 is a good value, if there
#'    is no fit, the value can be reduced to 70 or 60.
#' @param xi.percentage a number from 10 to 100 representing where correlation length could be found from maximum (used for fitting)
#' @param randomSeed (optional) a large number, if set, the random numbers are seeded and the results are reproducible
#' @param allResults if \code{TRUE} returns graph, data and fit parameters as list
#' @param verbose output time if \code{TRUE}
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_path scale_x_log10 scale_y_log10 theme_bw geom_label theme
#' @importFrom stats runif nls predict coef 
#'
#' @return graph, data frame with g(r) and $num indicating number of computations used for r, or
#'    a list with graph, data.frame, fit parameters
#'
#' @examples
#' filename = AFM.getSampleImages(type='tiff')
#' a = AFM.import(filename)
#' a = AFM.flatten(a)
#' r = AFM.hhcf(a, numIterations = 1e4, allResults = TRUE)
#' head(r$data)        # output HHCF data
#' head(r$fitData)     # fit data curve data
#' head(r$fitParams)   # output fit parameters
#' # r$graph           # output ggplot2 graph
#'
##################################################
#' @export
AFM.hhcf <- function(obj, no=1,
                     numIterations=10000,
                     addFit = TRUE,
                     dataOnly = FALSE,
                     degRes = 100,
                     r.percentage = 80,
                     xi.percentage = 70,
                     randomSeed = NA,
                     allResults = FALSE,
                     verbose=FALSE) {
  r.nm <- myLabel <- NULL
  results = list()  # keeps track of all results

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
  if (!is.na(randomSeed)) set.seed(randomSeed)
  px1 = round(runif(numIterations, min=1, max=dimx))
  py1 = round(runif(numIterations, min=1, max=dimy))
  theta = round(runif(numIterations, min=0, max=2*pi*degRes))/degRes

  lg = c()
  lq = c()
  t.start = as.numeric(Sys.time())
  maxR = round(dimx*r.percentage/100)
  if (verbose) cat("Computing r from 1 pixel to",maxR,"pixels maximum. Adjust with r.percentage parameter.\n")
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
  results$data = r # add HHCF data to the results list
  if ((dataOnly) & (!allResults)) return(r)

  if (addFit) {
    # starting fit parameters
    AFM.math.params(obj) -> m1
    m1$Rq -> sigmaGuess
    xiGuess = r$r.nm[min(which(r$g > (xi.percentage*sigmaGuess*sigmaGuess*2)/100))]

    # fit the data using
    fit <- NULL
    try({
      nls(data=r,
          g ~ 2 * (sigma*sigma) * (1 - exp(-(r.nm/xi)^(2*H))),
          start = list(sigma = sigmaGuess, xi = xiGuess, H=1)
          ) -> fit
    });

    # fit was successful
    if (!is.null(fit)) {
      if (verbose) cat("Fit is successful.\n")
      fitRnm = seq(from=round(min(r$r.nm)*0.9), to=max(r$r.nm), by=1)
      dFit = data.frame(
        r.nm = fitRnm,
        g = predict(fit, list(r.nm=fitRnm))
      )
      fitNames = c('sigma', 'xi','H')
      fitNamesUnits = c('nm','nm','')
      fitParams = coef(fit)
      dFitLabels = data.frame(
        r.nm = r$r.nm[1:3],
        g = r$g[1:3],
        myLabel = paste(fitNames,'=',signif(fitParams,4),fitNamesUnits)
      )
      
      # add fit to the results list
      results$fitData = dFit
      
      # add fitting parameters
      fitParams = data.frame(rbind(summary(fit)$coef[1:6]))
      names(fitParams) = c('sigma','xi','Hurst','sigma.err','xi.err','Hurst.err')
      results$fitParams = fitParams
    } else {
      if (verbose) print("Cannot fit data => setting addFit=FALSE")
      addFit = FALSE
    }
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
    theme(legend.position="none")

  results$graph = g
  
  if (allResults) return(results)
  
  g
}


