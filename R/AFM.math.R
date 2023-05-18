#' Math Parameters for AFMdata Object
#' 
#' @description
#' Computes various math parameters for an AFM image object.
#' Including the arithmetic roughnes (Ra), the geometrical
#' roughness (Rq), the skewness (Rsk), the kurtosis (K),
#' the Binder cumulant (U4),
#' the standard height deviation (Hsd), and the outlier 
#' ratio (AR)
#' 
#'
#' @param obj AFMdata class
#' @return structure with various computed AFM image parameters, such
#' as roughness
#' 
#' @examples
#' filename = AFM.getSampleImages(type='ibw')
#' AFM.math.params(AFM.import(filename)) -> q
#' summary(q)
#' 
#' @export
AFM.math.params <- function(obj) {
  Ra = get.Ra(obj@data$z[[1]])
  Rq = get.Rq(obj@data$z[[1]])
  Rsk = get.Skewness(obj@data$z[[1]])
  Hsd = get.Hsd(obj@data$z[[1]])
  K = get.MomentN(obj@data$z[[1]], 4) / (( Rq*Rq)^2)
  U4 = (1-(K/(3*Rq*Rq)))
  AR = get.AR(obj@data$z[[1]])
  structure(
    list(
      basename = basename(obj@fullFilename),
      Ra = Ra,
      Rq = Rq,
      Rsk = Rsk,
      Hsd = Hsd,
      K = K,
      U4 = U4,
      AR = AR
    ),
    class = 'AFMmath'
  )
}

#' Summary of computed AFM image parameters
#'
#' @param object AFMmath object
#' @param ... other arguments
#' @return prints a summary
#' @examples
#' filename = AFM.getSampleImages(type='ibw')
#' summary(AFM.math.params(AFM.import(filename)))
#' @export
summary.AFMmath <- function(object, ...) {
  cat("Basename:", object$basename,"\n")
  cat("Roughness Ra ............:", object$Ra," nm \n")
  cat("Roughness Rq ............:", object$Rq," nm \n")
  cat("Skewness Rsk ............:", object$Rsk," \n")
  cat("Kurtosis K ..............:", object$K," \n")
  cat("Binder Cumulant U4 ......:", object$U4," \n")
  cat("Std. Dev of Height Hsd ..:", object$Hsd," nm \n")
  cat("Deep Area Ratio .........:", object$AR,"\n")
}


#
NULL

# computes the average roughness Ra
get.Ra <- function(z) { sum(abs(z-mean(z)))/length(z) }
# computes the root mean square roughness Rq
get.Rq <- function(z) { sqrt(sum((z-mean(z))^2)/length(z)) }
# get the n-th moment
get.MomentN <- function(z, n) { 1/length(z)*sum((z-mean(z))^n) }
# computes the skewness or asymmetry
get.Skewness <- function(z) {
  get.MomentN(z,3)/(get.MomentN(z,2)^1.5)
}

#' Computes the standard deviation in height Hsd
#' 
#' @param z z component vector
#'
#' @importFrom stats sd
#'
get.Hsd <- function(z) {sd(z)}

# computes the ratio of pin hole area (height lower than 3 sigma) to total area through pixel counting
get.AR <- function(z) {
  count <- 0
  Hsd <- get.Hsd(z)
  for (val in z) {
    if(val <= -3*Hsd)  count = count+1
  }
  count/length(z)
}
