#' Flattens an AFM image using a plane fit
#'
#' uses the AFM.raster() function, makes a copy of the object
#' and fits a plane, returns the flattened object
#'
#' @param obj AFMdata object
#' @param no channel number
#' @param verbose output fitting parameters
#' @return flattened matrix with AFM image
#' @author thomasgredig
#' @importFrom purrr is_empty
#' @examples
#' d = AFM.import(AFM.getSampleImages(type='ibw'))
#' d2 = AFM.flatten(d)
#' plot(d2,graphType=2)
#' @export
AFM.flatten <- function(obj,no=1,verbose=FALSE) {
  AFMcopy <- obj
  # if (purrr::is_empty(AFMcopy@history)) AFMcopy@history <- ""
  # AFMcopy@history = paste(AFMcopy@history,"AFM.flatten(obj,",no,");")
  AFMcopy@history <- add.AFM.history(AFMcopy, paste0("AFM.flatten(obj,",no,")"))
  
  d = AFM.raster(AFMcopy,no)
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

  AFMcopy@data$z[[no]] =  z - (x*solvX[1] + y*solvX[2] + solvX[3])
  AFMcopy
}
