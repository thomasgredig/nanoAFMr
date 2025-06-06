% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AFM.flattenFindLine.R
\name{AFM.flattenFindLine}
\alias{AFM.flattenFindLine}
\title{Find Lines on AFM Image}
\usage{
AFM.flattenFindLine(obj, numLines = 1)
}
\arguments{
\item{obj}{AFMdata object}

\item{numLines}{number of clicks to find the line number}
}
\value{
pixel number of the selected line in the image
}
\description{
Use interactive mode to display an AFM image, then 
use a mouse to select a particular line and return
the pixel location as output.
}
\examples{
filename = AFM.getSampleImages('tiff')
a = AFM.import(filename)
AFM.flattenFindLine(a)

}
