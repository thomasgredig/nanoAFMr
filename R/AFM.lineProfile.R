#' Line Profile
#'
#' create a profile data line across an image (d), providing
#'   the starting point (x1,y1) and end point (x2,y2). The start and end
#'   points are provided in units of nanometers or pixels. If the starting
#'   and end point coordinates are not provided, it will use the \code{raster::click()}
#'   function to prompt the user to click on two points on the graph.
#'   
#'   Note: the convention is that the bottom left corner is (1,1) in pixels and (0,0) in nanometers
#'
#' @param obj AFMdata object
#' @param x1 start x position in  units of nm/pixels from bottom left, if \code{NA}, user will need to click on two points to define profile line
#' @param y1 start y position in  units of nm/pixels from bottom left, if \code{NA}, user will need to click on two points to define profile line
#' @param x2 end x position in  units of nm/pixels from bottom left, if \code{NA}, user will need to click on two points to define profile line
#' @param y2 end y position in  units of nm/pixels from bottom left, if \code{NA}, user will need to click on two points to define profile line
#' @param N thickness of line in pixels, for high-resolution images, increase
#' @param unitPixels logical, if \code{TRUE}, then coordinates are in units of pixels otherwise nm
#' @param verbose logical, if \code{TRUE}, output additional information
#' @returns AFMdata object with line data, use \code{AFM.linePlot()} to graph / tabulate data or \code{plot(addLines=TRUE)} to graph image with lines
#'
#' @author Thomas Gredig
#'
#' @examples
#' afmd = AFM.artificialImage(width=128, height=128, type='calibration', verbose=FALSE)
#' AFM.lineProfile(afmd, 100, 500, 900, 500) -> afmd2
#' AFM.linePlot(afmd2)
#'
#' AFM.lineProfile(afmd, 1, 1, 128, 128, unitPixels=TRUE) -> afmd2
#' AFM.linePlot(afmd2)
#' head(AFM.linePlot(afmd2, dataOnly=TRUE))
#'
#' @seealso [AFM.getLine(), AFM.liniePlot(), plot.AFMdata()]
#'
#' @importFrom terra rast click
#' @export
AFM.lineProfile <- function(obj,x1=NA,y1=NA,x2=NA,y2=NA,
                            N=1,
                            unitPixels = FALSE, verbose=FALSE) {
  
  if (unitPixels) xD = 0 else xD = 0 # define left bottom corner as (1,1) pixels and (0,0)nm
  N = min(obj@y.pixels, N)
  for(i in 1:N) {
    obj = .singleLineProfile(obj, x1, y1 + xD, x2, y2 + xD, unitPixels, verbose)
    xD = xD + ceiling(obj@y.conv)
  }
  
  obj
}


# ==============================================================
.singleLineProfile <- function(obj,x1=NA,y1=NA,x2=NA,y2=NA,
                            unitPixels = FALSE, verbose=FALSE) {
  AFMcopy <- obj
  d = AFM.raster(AFMcopy)

  width.x = AFMcopy@x.pixels
  width.y = AFMcopy@y.pixels

  # if no coordinates are provided, use graphical interface
  # to prompt for coordinates
  if (is.na(x1) | is.na(x2) | is.na(y1) | is.na(y2)) {
    dfr = rast(d)
    terra::plot(dfr)
    click(dfr, n=2, xy=TRUE, show=FALSE) -> xy
    x1 = xy$x[1]
    x2 = xy$x[2]
    y1 = xy$y[1]
    y2 = xy$y[2]
    # units are [nm] in this case
    unitPixels = FALSE
  }

  if (unitPixels) { # input is in units of pixels, from (1,1)
    if ((x1<1) | (x2<1) | (y1<1) | (y2<1)) { warning("Must have non-zero positive pixel coordinates."); x1=min(1,x1); x2=min(1,x2); y1=min(1,y1); y2=min(1,y2); }
    if (x1 > width.x) { warning("x1: Out of range"); x1 = width.x }
    if (x2 > width.x) { warning("x2: Out of range"); x2 = width.x }
    if (y1 > width.y) { warning("y1: Out of range"); y1 = width.y }
    if (y2 > width.y) { warning("y2: Out of range"); y2 = width.y }
    x1.pixel = x1
    y1.pixel = y1
    x2.pixel = x2
    y2.pixel = y2
  } else {
    range.x = AFMcopy@x.conv * AFMcopy@x.pixels # max(d$x) - min(d$x)
    range.y = AFMcopy@y.conv * AFMcopy@y.pixels # max(d$y) - min(d$y)
    if(x1 > range.x) { warning("x1: Out of range"); x1=range.x }
    if(y1 > range.y) { warning("y1: Out of range"); y1=range.y }
    if(x2 > range.x) { warning("x2: Out of range"); x2=range.x}
    if(y2 > range.y) { warning("y2: Out of range"); y2=range.y}

    x1.pixel = ceiling((x1+1)/range.x*width.x)
    y1.pixel = ceiling((y1+1)/range.y*width.y)
    x2.pixel = floor((x2+1)/range.x*width.x)
    y2.pixel = floor((y2+1)/range.y*width.y)
  }

  AFMcopy@history <- paste(AFMcopy@history,
                           "AFM.lineProfile(obj,",x1.pixel,",",y1.pixel,",",x2.pixel,",",y2.pixel,",unitPixels = T);")

  if (verbose) print(paste("Pixels: (",x1.pixel,",",y1.pixel,") - (",x2.pixel,",",y2.pixel,")"))

  Dx = abs(x2.pixel - x1.pixel)
  sx = sign(x2.pixel - x1.pixel)
  Dy = - abs(y2.pixel - y1.pixel)
  sy = sign(y2.pixel - y1.pixel)
  er = Dx + Dy
  r = c(x1.pixel + (y1.pixel-1) * width.x)
  q2=0
  r2 = c(q2)
  # Bresenham's Line Algorithm
  while (!((x1.pixel == x2.pixel) & (y1.pixel == y2.pixel))) {
    er2 = 2*er
    lx = ly = 0
    if(er2 >= Dy) {
      er = er + Dy
      x1.pixel = x1.pixel + sx
      lx = AFMcopy@x.conv
    }
    if(er2 <= Dx) {
      er = er + Dx
      y1.pixel = y1.pixel + sy
      ly = AFMcopy@y.conv
    }
    # add data point
    q1 = x1.pixel + (y1.pixel-1) * width.x
    q2 = q2+sqrt(lx^2 + ly^2)
    r=c(r, q1)
    r2 = c(r2, q2)
  }
  if (verbose) cat(paste("delta Y:",signif(AFMcopy@y.conv,4),
                           "nm/px and delta X:",signif(AFMcopy@x.conv,4),"nm/px"))

  if (is.null(AFMcopy@data$line)) AFMcopy@data$line = list()
  AFMcopy@data$line = append(AFMcopy@data$line,list(r))
  if (is.null(AFMcopy@data$line.nm)) AFMcopy@data$line.nm = list()
  AFMcopy@data$line.nm = append(AFMcopy@data$line.nm,list(r2))
  AFMcopy
}

