% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AFM.flatten.R
\name{AFM.flattenCheck}
\alias{AFM.flattenCheck}
\title{Check AFM image Flattening Process}
\usage{
AFM.flattenCheck(obj, lns = c(1:4 * 5), no = 1, tau_lower = 0.01)
}
\arguments{
\item{obj}{AFMdata object}

\item{lns}{lines to select to be fitted (vector)}

\item{no}{channel number}

\item{tau_lower}{percentage of data points to fit (1 = 100 percent)}
}
\value{
graph (ggplot2 object)
}
\description{
Creates a graph that shows the fit for each line in a grid plot
}
