#' Remove horizontal streak artifacts from an AFM image
#'
#' Detects stripe/streak artifacts by thresholding a discrete second-difference
#' along the \emph{y} direction (row-wise) and replaces flagged pixels with the
#' value from the next row. The operation is recorded in the object's history.
#'
#' @param obj An AFM object containing raster data in \code{obj@data$z}.
#' @param no Integer index of the image/channel in \code{obj@data$z} to process.
#'   Defaults to \code{1}.
#' @param threshold Numeric in \eqn{[0,1]} giving the fraction of the maximum
#'   second-difference used as a detection cutoff. Larger values are more
#'   conservative. Default is \code{0.4}.
#' @param verbose Logical; currently unused. Default \code{FALSE}.
#'
#' @return A modified copy of \code{obj} with \code{obj@data$z[[no]]} updated and
#'   \code{obj@history} appended.
#'
#' @details
#' The discrete second-difference is computed for interior rows as
#' \eqn{2 m[i,] - m[i-1,] - m[i+1,]}. Values above \code{threshold * max(.)} are
#' flagged as streak pixels. Flagged pixels are replaced with the pixel directly
#' below (row \code{i+1}) at the same column.
#' 
#' @author Thomas Gredig
#'
#' @examples
#' f <- AFM.getSampleImages()[2]
#' a <- AFM.import(f)
#' a2 <- AFM.removeStreaks(a)
#' plot(a2)
#'
#' @export
AFM.removeStreaks <- function(obj, no=1,
                              threshold = 0.4,
                              verbose=FALSE) {
  AFMcopy <- obj
  AFMcopy@history <- add.AFM.history(
    AFMcopy,
    sprintf("AFM.removeStreaks(obj,%g,threshold=%g)", no, threshold)
  )
  
  # convert to matrix
  d <- AFM.raster(AFMcopy, no=no)
  m <- matrix(d$z, nrow = AFMcopy@y.pixels, ncol = AFMcopy@x.pixels, byrow = TRUE)
  m_diff = m
  
  for (i in 2:(AFMcopy@y.pixels-1)) {
    m_diff[i,] = 2*m[i,] - m[i-1,] - m[i+1,]
  }
  m_diff[1,] = 0
  m_diff[AFMcopy@y.pixels,] = 0
  
  thr <- threshold * max(m_diff, na.rm = TRUE)
  m_diff[m_diff <= thr] <- 0
  m_diff[m_diff > thr] <- 1
  
  idx <- which(m_diff == 1, arr.ind = TRUE)
  idx <- idx[idx[, 1] < nrow(m), , drop = FALSE]
  m[idx] <- m[cbind(idx[, 1] + 1, idx[, 2])]

  # 90 deg counterclockwise then flip left-to-right
  m <- t(m)[ncol(m):1, ]
  m <- m[nrow(m):1, ]
  
  AFMcopy@data$z[[no]] <- as.numeric(m)
  AFMcopy
}