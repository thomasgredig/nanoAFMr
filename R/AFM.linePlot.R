#' Plot AFM line profiles
#'
#' @description
#' Plots all profile lines stored in an `AFMdata` object for one channel.
#' Use [AFM.lineProfile()] or [AFM.getLine()] first to attach one or more lines.
#'
#' @param obj An `AFMdata` object.
#' @param no Integer channel number.
#' @author Thomas Gredig
#' @param dataOnly Logical; if `TRUE`, return the underlying data and do not
#'   draw a plot.
#' @returns If `dataOnly = TRUE`, a data frame with columns `x`, `z`, and
#'   `type`; otherwise a `ggplot2` object.
#' @importFrom ggplot2 ggplot geom_path scale_color_discrete xlab theme_bw theme
#'
#' @seealso \code{\link{AFM.lineProfile}}
#' @examples
#' img <- AFM.artificialImage(
#'   width = 80, height = 80, type = "gradient",
#'   addNoise = FALSE, verbose = FALSE
#' )
#' img <- AFM.lineProfile(img, 0, 100, 1000, 100, unitPixels = FALSE)
#' img <- AFM.lineProfile(img, 0, 300, 1000, 300, unitPixels = FALSE)
#' AFM.linePlot(img)
#' head(AFM.linePlot(img, dataOnly = TRUE))
#' @export
AFM.linePlot <- function(obj,no=1,dataOnly=FALSE) {
  if (is.null(obj@data$line)) { warning("No lines in object."); return() }
  x <- z <- type <- NULL
  zData = obj@data$z[[no]]
  i=1
  r = data.frame()
  for(ln in obj@data$line) {
    dz = data.frame(x=obj@data$line.nm[[i]],z=zData[ln])
    dz$type=i
    i=i+1
    r=rbind(r, dz)
  }
  if (dataOnly) return(r)
  ggplot(r, aes(x,z,col=as.factor(type))) +
    geom_path() +
    xlab('d (nm)') +
    scale_color_discrete('No') +
    theme_bw() +
    theme(legend.position.inside = c(0.01,0.99),
          legend.justification = c(0,1))
}
