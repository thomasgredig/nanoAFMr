# Methods to AFM.flatten
# ======================


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

###################################################
# Helper Functions
###################################################
.flattenLine <- function(afmd, lineNo, lowLimit=NA, upperLimit=NA, no=1, outGraphs=TRUE, tau_lower = 0.01) {
  ldf = AFM.getLine(afmd, no=no, yPixel= lineNo, dataOnly = TRUE)
  if (!is.na(lowLimit) & !is.na(upperLimit)) {
    ldf <- subset(ldf, x >= lowLimit & x <= upperLimit)
  }
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

