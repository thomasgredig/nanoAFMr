# Methods to AFM.flatten
# ======================
NULL

#### METHOD: lineByLine
.flattenMethodLineByLine <- function(obj, tau_lower = 0.01, verbose=FALSE, ...) {
  z = c()
  
  for(j in 1:obj@y.pixels) {
    .flattenLine(obj, j, outGraphs = FALSE, tau_lower = tau_lower, ...) -> d
    z = c(z, d$z)
  }
  z
}

#### METHOD: Slope
.flattenMethodSlope <- function(AFMcopy, no, slope) {
  if (nrow(slope) != AFMcopy@x.pixels) warning("Slope parameter is incorrect length.")
  
  d = AFM.raster(AFMcopy,no)
  m.vec = rep(slope$m, each = AFMcopy@y.pixels)
  b.vec = rep(slope$b, each = AFMcopy@y.pixels)
  z = d$z - m.vec*d$x - b.vec
  
  z
}

#### METHOD: Slope
.flattenMethodPlane <- function(d, verbose=FALSE) {
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
  z - (x*solvX[1] + y*solvX[2] + solvX[3])
}



#### METHOD: autoMask
.flattenAutoMask <- function(obj, no, verbose=FALSE, quartile_cutoff = 0.94, fit_order = 4) {
  # store new data in z
  z = c()
  
  # get image size
  maxY = obj@y.pixels
  q=c()
  for (i in 1:maxY) {
    l1 = AFM.getLine(obj, y=i, dataOnly = TRUE, no=no)
    q = c(q,abs(diff(l1$z)))
  }
  # this is the cut-off to mask objects
  m_high = quantile(q, quartile_cutoff)
  if (verbose) { 
    cat("Cut-off z position is [quartile_cutoff=",quartile_cutoff,"]", m_high," nm.\n") 
    cat("Fitting: [fit_order=", fit_order, "] th order polynomial.\n")
  }
  
  # fit each line to a polynomial and then remove the residuals
  for (i in 1:maxY) {
    l1 = AFM.getLine(obj, yPixel=i, dataOnly = TRUE, no=no)
    zhigh = which(c(0, abs(diff(l1$z)))>m_high)
    # fill in any gaps, if needed
    zhigh_dx = diff(zhigh)
    zhigh = sort( c(zhigh, zhigh[which(zhigh_dx==2)]+1) )
    zhigh = c(zhigh, zhigh[which(zhigh_dx==2)]+1)

    # these are the background pixels    
    l2 = l1[-zhigh,]
    if (nrow(l2)>0) {
      fit <- lm(z ~ poly(x, fit_order, raw = TRUE), data = l2)
  
      # Get fitted (predicted) values from the model
      fitted_vals <- predict(fit, newdata = l1)
    } else {
      fitted_vals = rep(0, length(l1$x))
      if (verbose) {
        warning("Cannot fit line: ",i,"\n")
      }
    }
    
    # Remove fitted trend (residuals)
    lineData <- l1$z - fitted_vals
    z = c(z, lineData)
    # plot(l1$x, lineData)
    # plot(l1$x, l1$z)
    # a1 <- AFM.setLine(a1, lineData, yPixel=i)
  }
  
  z
}


###################################################
# Helper Functions
###################################################
#' @importFrom quantreg rq
#' @importFrom ggplot2 ggplot geom_point aes geom_line theme_bw
#' @importFrom rlang .data
#' @noRd
.flattenLine <- function(afmd, lineNo, lowLimit=NA, upperLimit=NA, 
                         no=1, outGraphs=TRUE, tau_lower = 0.01) {
  
  ldf = AFM.getLine(afmd, no=no, yPixel= lineNo, dataOnly = TRUE)
  if (!is.na(lowLimit) & !is.na(upperLimit)) {
    ldf <- subset(ldf, .data$x >= lowLimit & .data$x <= upperLimit)
  }
  fit_lower <- quantreg::rq(ldf$z ~ ldf$x, tau = tau_lower)
  
  ldf$bgd = predict(fit_lower, list(x=ldf$x))
  ldf$z.flat = ldf$z - ldf$bgd
  
  # if (outGraphs) {
  #   ggplot(ldf,aes(x,z)) +
  #     geom_point(col='red', size=1.5) +
  #     geom_line(data=data.frame(x=ldf$x, z=ldf$bgd), col='blue') +
  #     theme_bw() -> g.Flat
  # } else {
  #   g.Flat = NULL
  # }
  
  list(z = ldf$z.flat,
       fit = fit_lower,
       g2 = NULL, #g.Flat,
       b = coef(fit_lower)[1],
       m = coef(fit_lower)[2])
}

