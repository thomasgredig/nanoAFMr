#' Histogram of AFM image
#'
#' @param obj AFMdata object
#' @param no channel number of the image
#' @param binNo number of bins in the histogram
#' @param dataOnly logical, if \code{TRUE} a data frame with the histogram
#' data is returned
#' @return data frame or ggplot
#'
#' @author Thomas Gredig
#'
#' @examples
#' d = AFM.import(AFM.getSampleImages(type='ibw'))
#' head(AFM.histogram(d, dataOnly=TRUE),n=20)
#' AFM.histogram(d)
#'
#' @seealso \code{\link{plot.AFMdata}}
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density
#' @importFrom graphics hist
#'
#' @export
AFM.histogram <- function(obj, no=1, binNo = 200, dataOnly=FALSE) {
  z <- NULL

  dr = AFM.raster(obj,no)
  if (dataOnly) {
    graphics::hist(dr$z, breaks=binNo, plot=FALSE) -> q
    result = data.frame(mids = q$mids , zDensity = q$density/sum(q$density))
  } else {
    result =
      ggplot(dr, aes(x=z)) +
      geom_histogram(aes(y=..density..),
                     colour="black", fill="pink", bins=binNo)+
      geom_density(alpha=0.2, fill='red') +
      theme_bw()
  }
  result
}

globalVariables(c("..density.."))
