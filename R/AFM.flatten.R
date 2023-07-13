#' Flattens an AFM image using a plane fit
#'
#' uses the AFM.raster() function, makes a copy of the object
#' and fits a plane, returns the flattened object
#'
#' @param obj AFMdata object
#' @param no channel number
#' @param verbose output fitting parameters
#' @param lineByLine logical, if \code{TRUE}, the flattening method is line-by-line
#' 
#' @return flattened matrix with AFM image
#' 
#' @author Thomas Gredig
#' 
#' @examples
#' d = AFM.import(AFM.getSampleImages(type='ibw'))
#' d2 = AFM.flatten(d)
#' plot(d2,graphType=2)
#' 
#' @export
AFM.flatten <- function(obj, no=1, method = c('plane','lineByLine'), verbose=FALSE, ...) {
  AFMcopy <- obj
  AFMcopy@history <- add.AFM.history(AFMcopy, paste0("AFM.flatten(obj,",no,")"))
  
  if (length(method)>1) method='plane'
  if (method == 'lineByLine') {
    z.new = .flattenLineByLine(AFMcopy, no=no, verbose=verbose, ...)
  } else { 
    d = AFM.raster(AFMcopy,no)
    # use 
    SZ = nrow(d)
    x = d$x
    y = d$y
    z = d$z
  
    b = c(sum(x*z), sum(y*z), sum(z))
    a = matrix(data = c(sum(x*x), sum(x*y), sum(x),
                        sum(x*y), sum(y*y), sum(y),
                        sum(x), sum(y), SZ),
               nrow=3)
    a
    solvX = solve(a,b)
  
    if (verbose) print(paste("Plane fit:", solvX))
    z.new = z - (x*solvX[1] + y*solvX[2] + solvX[3])
  }

  AFMcopy@data$z[[no]] =  z.new 
  AFMcopy
}


####
.flattenLineByLine <- function(obj,no=1, tau_lower = 0.01, verbose=FALSE) {
  z = c()
  
  for(j in 1:obj@y.pixels) {
    .flattenLine(obj, j, outGraphs = FALSE, tau_lower = tau_lower) -> d
    z = c(z, d$z)
  }
  z
}

.flattenLine <- function(afmd, lineNo, outGraphs=TRUE, tau_lower = 0.01) {
  c2 = AFM.getLine(afmd, yPixel= lineNo)
  ldf = AFM.linePlot(c2, dataOnly = TRUE)
  fit_lower <- quantreg::rq(ldf$z ~ ldf$x, tau = tau_lower)

  ldf$bgd = predict(fit_lower, list(x=ldf$x))

  ldf$z.flat = ldf$z - ldf$bgd
  
  if (outGraphs) {
    ldf %>%
      ggplot(aes(x,z)) +
      geom_point(col='red', size=1.5) +
      geom_line(data=data.frame(x=ldf$x, z=ldf$bgd), col='blue') +
      theme_bw() -> g.Flat
  } else {
    g.Flat = NULL
  }
  
  list(z = ldf$z.flat,
       fit = fit_lower,
       g2 = g.Flat,
       b= coef(fit_lower)[1],
       m=coef(fit_lower)[2])
}

