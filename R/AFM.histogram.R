#' Histogram of AFM height data
#'
#' @description
#' Computes and optionally plots a histogram of one channel in an `AFMdata`
#' object. This is useful for checking whether a few outlier pixels dominate
#' the dynamic range before choosing `trimPeaks` or `setRange` in [plot()].
#'
#' @param obj An `AFMdata` object.
#' @param no Integer channel number.
#' @param binNo Integer number of bins to use.
#' @param dataOnly Logical; if `TRUE`, return histogram summary data instead of
#'   a plot.
#'
#' @return A `ggplot` object when `dataOnly = FALSE`, otherwise a data frame
#'   with columns `mids` and `zDensity`.
#'
#' @author Thomas Gredig
#'
#' @examples
#' img <- AFM.artificialImage(
#'   width = 64, height = 64, type = "gradient",
#'   addNoise = FALSE, verbose = FALSE
#' )
#' hist_data <- AFM.histogram(img, binNo = 40, dataOnly = TRUE)
#' head(hist_data, n = 6)
#' AFM.histogram(img, binNo = 40)
#'
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density after_stat 
#' @importFrom graphics hist
#' @importFrom stats density
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
      geom_histogram(aes(y=after_stat(density)),
                     colour="black", fill="pink", bins=binNo)+
      geom_density(alpha=0.2, fill='red') +
      theme_bw()
  }
  result
}
