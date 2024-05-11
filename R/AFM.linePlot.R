#' Plots AFM line
#'
#' @param obj AFMdata object
#' @param no channel number
#' @author Thomas Gredig
#' @param dataOnly if \code{TRUE} no graph is returned
#' @importFrom ggplot2 ggplot geom_path scale_color_discrete xlab theme_bw theme
#'
#' @seealso \code{\link{AFM.lineProfile}}, \code{\link{plot.AFMdata}}
#' @examples
#' filename = AFM.getSampleImages(type='ibw')
#' d = AFM.import(filename)
#' AFM.lineProfile(d, 0,0, 2000,2000) -> d1
#' AFM.lineProfile(d1, 0,0, 100,2500) -> d2
#' AFM.linePlot(d2)
#' plot(d2,addLines=TRUE)
#' head(AFM.linePlot(d2, dataOnly=TRUE))
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
