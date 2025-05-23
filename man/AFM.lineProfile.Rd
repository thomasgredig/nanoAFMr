% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AFM.lineProfile.R
\name{AFM.lineProfile}
\alias{AFM.lineProfile}
\title{Line Profile}
\usage{
AFM.lineProfile(
  obj,
  x1 = NA,
  y1 = NA,
  x2 = NA,
  y2 = NA,
  N = 1,
  unitPixels = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{obj}{AFMdata object}

\item{x1}{start x position in  units of nm/pixels from bottom left, if \code{NA}, user will need to click on two points to define profile line}

\item{y1}{start y position in  units of nm/pixels from bottom left, if \code{NA}, user will need to click on two points to define profile line}

\item{x2}{end x position in  units of nm/pixels from bottom left, if \code{NA}, user will need to click on two points to define profile line}

\item{y2}{end y position in  units of nm/pixels from bottom left, if \code{NA}, user will need to click on two points to define profile line}

\item{N}{thickness of line in pixels, for high-resolution images, increase}

\item{unitPixels}{logical, if \code{TRUE}, then coordinates are in units of pixels otherwise nm}

\item{verbose}{logical, if \code{TRUE}, output additional information}
}
\value{
AFMdata object with line data, use \code{AFM.linePlot()} to graph / tabulate data or \code{plot(addLines=TRUE)} to graph image with lines
}
\description{
create a profile data line across an image (d), providing
  the starting point (x1,y1) and end point (x2,y2). The start and end
  points are provided in units of nanometers or pixels. If the starting
  and end point coordinates are not provided, it will use the \code{raster::click()}
  function to prompt the user to click on two points on the graph.
  
  Note: the convention is that the bottom left corner is (1,1) in pixels and (0,0) in nanometers
}
\examples{
afmd = AFM.artificialImage(width=128, height=128, type='calibration', verbose=FALSE)
AFM.lineProfile(afmd, 100, 500, 900, 500) -> afmd2
AFM.linePlot(afmd2)

AFM.lineProfile(afmd, 1, 1, 128, 128, unitPixels=TRUE) -> afmd2
AFM.linePlot(afmd2)
head(AFM.linePlot(afmd2, dataOnly=TRUE))

}
\seealso{
\code{\link{AFM.getLine}}, \code{\link{AFM.linePlot}}
}
\author{
Thomas Gredig
}
